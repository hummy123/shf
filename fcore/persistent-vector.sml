structure PersistentVector =
struct
  (* Clojure-style persistent vector, for building search list.
   * There is an "int table" too, which stores the last index 
   * at the node with the same index.
   * We can use the size table for binary search.
   * *)
  datatype t =
    BRANCH of t vector * int vector
  | LEAF of {start: int, finish: int} vector * int vector

  val maxSize = 32
  val halfSize = 16

  fun isEmpty t =
    case t of
      LEAF (_, sizes) => Vector.length sizes = 0
    | _ => false

  val empty = LEAF (#[], #[])

  datatype append_result = APPEND of t | UPDATE of t

  fun isInRange (checkIdx, t) =
    case t of
      BRANCH (nodes, sizes) =>
        let
          val searchIdx = BinSearch.equalOrMore (checkIdx, sizes)
        in
          if searchIdx = ~1 then
            false
          else if searchIdx = 0 then
            isInRange (checkIdx, Vector.sub (nodes, searchIdx))
          else
            let
              val nextCheckIdx = checkIdx - Vector.sub (sizes, searchIdx - 1)
            in
              isInRange (nextCheckIdx, Vector.sub (nodes, searchIdx))
            end
        end
    | LEAF (values, sizes) =>
        let
          val searchIdx = BinSearch.equalOrMore (checkIdx, sizes)
        in
          if searchIdx = ~1 then
            false
          else
            let
              val {start, finish} = Vector.sub (values, searchIdx)
            in
              checkIdx >= start andalso checkIdx <= finish
            end
        end

  fun getFinishIdx t =
    case t of
      BRANCH (_, sizes) => Vector.sub (sizes, Vector.length sizes - 1)
    | LEAF (_, sizes) => Vector.sub (sizes, Vector.length sizes - 1)

  fun helpAppend (start, finish, tree) =
    case tree of
      BRANCH (nodes, sizes) =>
        let
          val lastNode = Vector.sub (nodes, Vector.length nodes - 1)
          val prevSize =
            if Vector.length sizes > 1 then
              Vector.sub (sizes, Vector.length sizes - 2)
            else
              0
        in
          case helpAppend (start - prevSize, finish - prevSize, lastNode) of
            UPDATE newLast =>
              let
                val lastPos = Vector.length nodes - 1
                val newNode = Vector.update (nodes, lastPos, newLast)
                val newSizes = Vector.update (sizes, lastPos, finish)
                val newNode = BRANCH (newNode, newSizes)
              in
                UPDATE newNode
              end
          | APPEND newVec =>
              if Vector.length nodes = maxSize then
                let 
                  (* adjust "finish" so that it does not consider
                   * offset for "lower" vector *)
                  val finish = finish - Vector.sub (sizes, Vector.length sizes - 1)
                  val newNode = BRANCH (#[newVec], #[finish])
                in 
                  APPEND newNode
                end
              else
                let
                  val newNodes = Vector.concat [nodes, #[newVec]]
                  val newSizes = Vector.concat [sizes, #[finish]]
                  val newNodes = BRANCH (newNodes, newSizes)
                in
                  UPDATE newNodes
                end
        end
    | LEAF (values, sizes) =>
        if Vector.length values + 1 > maxSize then
          (* when we split a leaf into two vectors, 
           * we want to adjust the start and finish parameters
           * so that they don't contain the offset relevant to the
           * "lower" vector, which was split from *)
          let 
            val prevFinish = Vector.sub (sizes, Vector.length sizes - 1)
            val start = start - prevFinish
            val finish = finish - prevFinish
            val newNode = LEAF (#[{start = start, finish = finish}], #[finish])
          in 
            APPEND newNode
          end
        else
          let
            val newNode = Vector.concat
              [values, #[{start = start, finish = finish}]]
            val newSizes = Vector.concat [sizes, #[finish]]
            val newNode = LEAF (newNode, newSizes)
          in
            UPDATE newNode
          end

  fun append (start, finish, tree) =
    case helpAppend (start, finish, tree) of
      UPDATE t => t
    | APPEND newNode => 
        let
          val maxSize = getFinishIdx tree
        in
          BRANCH (#[tree, newNode], #[maxSize, finish])
        end

  (* todo: modify below functions so that they also 
   * use rope-like metadata *)

  fun getStart tree =
    case tree of
      LEAF (values, _) => Vector.sub (values, 0)
    | BRANCH (nodes, _) => getStart (Vector.sub (nodes, 0))

  fun helpNextMatch (cursorIdx, tree) =
    case tree of
      LEAF (values, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx = ~1 then {start = ~1, finish = ~1}
          else Vector.sub (values, idx)
        end
    | BRANCH (nodes, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx = ~1 then {start = ~1, finish = ~1}
          else helpNextMatch (cursorIdx, Vector.sub (nodes, idx))
        end

  fun startNextMatch (cursorIdx, tree) =
    case tree of
      LEAF (values, sizes) =>
        if Vector.length sizes = 0 then
          {start = ~1, finish = ~1}
        else
          let
            val idx = BinSearch.equalOrMore (cursorIdx, sizes)
            val idx = if idx = ~1 then 0 else idx
          in
            Vector.sub (values, idx)
          end
    | BRANCH (nodes, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx = ~1 then {start = ~1, finish = ~1}
          else helpNextMatch (cursorIdx, Vector.sub (nodes, idx))
        end

  fun loopNextMatch (prevStart, prevFinish, tree, count) =
    if count = 0 then
      prevStart
    else
      let
        val {start, finish} = startNextMatch (prevFinish + 1, tree)
      in
        if start = ~1 then
          let val {start, finish} = getStart tree
          in loopNextMatch (start, finish, tree, count - 1)
          end
        else
          loopNextMatch (start, finish, tree, count - 1)
      end

  fun nextMatch (cursorIdx, tree, count) =
    if isEmpty tree then
      ~1
    else
      let
        val {start, finish} = startNextMatch (cursorIdx, tree)
      in
        if start = ~1 then
          let val {start, finish} = getStart tree
          in loopNextMatch (start, finish, tree, count - 1)
          end
        else if cursorIdx >= start andalso cursorIdx <= finish then
          loopNextMatch (start, finish, tree, count)
        else
          loopNextMatch (start, finish, tree, count - 1)
      end

  fun getLast tree =
    case tree of
      LEAF (values, _) => Vector.sub (values, Vector.length values - 1)
    | BRANCH (nodes, _) => getLast (Vector.sub (nodes, Vector.length nodes - 1))

  (* slightly tricky.
  * The `sizes` vector contains the last/finish position of the item
  * at the corresponding index in the `nodes` or `values` vector
  * However, what we when searching for the previous match
  * is different: we want the node that has a start prior
  * to the cursorIdx.
  * This information cannot be retrieved with 100% accuracy 
  * using the `sizes` vector.
  * To get what we want, we recurse downwards using the `sizes` vector.
  * If we found the node we want, we return it. 
  * Otherwise, we return a state meaning "no node at this position"
  * and we use the call stack to descend down the node at the previous index.
  * There might not be a previous index because the current index is 0.
  * In this case, either the call stack will handle it,
  * or the caller to `helpPrevMatch` will. *)
  fun helpPrevMatch (cursorIdx, tree) =
    case tree of
      LEAF (values, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx < 0 then
            {start = ~1, finish = ~1}
          else if idx = 0 then
            let
              val result = Vector.sub (values, 0)
            in
              if #start result < cursorIdx then result
              else {start = ~1, finish = ~1}
            end
          else
            let
              val current = Vector.sub (values, idx)
            in
              if cursorIdx > #start current then current
              else Vector.sub (values, idx - 1)
            end
        end
    | BRANCH (nodes, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx < 0 then
            {start = ~1, finish = ~1}
          else if idx = 0 then
            helpPrevMatch (cursorIdx, Vector.sub (nodes, idx))
          else
            let
              val node = Vector.sub (nodes, idx)
              val result = helpPrevMatch (cursorIdx, node)
            in
              if #start result = ~1 then getLast (Vector.sub (nodes, idx - 1))
              else result
            end
        end

  fun loopPrevMatch (prevStart, prevFinish, tree, count) =
    if count = 0 then
      prevStart
    else
      let
        val {start, finish} = helpPrevMatch (prevFinish - 1, tree)
      in
        if start = ~1 then
          let val {start, finish} = getLast tree
          in loopPrevMatch (start, finish, tree, count - 1)
          end
        else
          loopPrevMatch (start, finish, tree, count - 1)
      end

  fun prevMatch (cursorIdx, tree, count) =
    if isEmpty tree then
      ~1
    else
      let
        val {start, finish} = helpPrevMatch (cursorIdx, tree)
      in
        if start = ~1 then
          let val {start, finish} = getLast tree
          in loopPrevMatch (start, finish, tree, count - 1)
          end
        else if cursorIdx >= start andalso cursorIdx <= finish then
          loopPrevMatch (start, finish, tree, count)
        else
          loopPrevMatch (start, finish, tree, count - 1)
      end

  datatype insert_result = INSERT_UPDATE of t | INSERT_SPLIT of t * t

  fun getMaxSize tree =
    case tree of
      LEAF (_, sizes) => Vector.sub (sizes, Vector.length sizes - 1)
    | BRANCH (_, sizes) => Vector.sub (sizes, Vector.length sizes - 1)

  fun helpInsert (start, finish, tree) =
    case tree of
      BRANCH (nodes, sizes) =>
        if finish >= Vector.sub (sizes, Vector.length sizes - 1) then
          (* if we want to append *)
          case
            helpAppend (start, finish, Vector.sub
              (nodes, Vector.length sizes - 1))
          of
            UPDATE newLast =>
              let
                val sizes = Vector.update
                  (sizes, Vector.length sizes - 1, finish)
                val nodes = Vector.update
                  (nodes, Vector.length nodes - 1, newLast)
              in
                INSERT_UPDATE (BRANCH (nodes, sizes))
              end
          | APPEND newLast =>
              if Vector.length nodes = maxSize then
                (* have to split *)
                let
                  val leftLen = SOME halfSize
                  val rightLen = SOME (Vector.length nodes - halfSize)

                  val leftNodeSlice = VectorSlice.slice (nodes, 0, leftLen)
                  val rightNodeSlice =
                    VectorSlice.slice (nodes, halfSize, rightLen)

                  val leftSizeSlice = VectorSlice.slice(sizes, 0, leftLen)
                  val rightSizeSlice =
                    VectorSlice.slice (sizes, halfSize, rightLen)

                  val leftNodes = VectorSlice.vector leftNodeSlice
                  val leftSizes = VectorSlice.vector leftSizeSlice

                  val newLast = VectorSlice.full (#[newLast])
                  val finish = VectorSlice.full (#[finish])
                  val rightNodes = VectorSlice.concat [rightNodeSlice, newLast]
                  val rightSizes = VectorSlice.concat [rightSizeSlice, finish]

                  val left = BRANCH (leftNodes, leftSizes)
                  val right = BRANCH (rightNodes, rightSizes)
                in
                  INSERT_SPLIT (left, right)
                end
              else
                (* append newLast to current node *)
                let
                  val newLast = #[newLast]
                  val finish = #[finish]

                  val nodes = Vector.concat [nodes, newLast]
                  val sizes = Vector.concat [sizes, finish]
                in
                  INSERT_UPDATE (BRANCH (nodes, sizes))
                end
        else
          let
            val idx = BinSearch.equalOrMore (finish, sizes)
            val idx = if idx = ~1 then 0 else idx
          in
            case helpInsert (start, finish, tree) of
              INSERT_UPDATE newNode =>
                let
                  val sizes =
                    if finish > Vector.sub (sizes, idx) then
                      Vector.update (sizes, idx, finish)
                    else
                      sizes
                  val nodes = Vector.update (nodes, idx, newNode)
                in
                  INSERT_UPDATE (BRANCH (nodes, sizes))
                end
            | INSERT_SPLIT (left, right) =>
                if Vector.length nodes = maxSize then
                  (* have to split this node too *)
                  let
                    (* slice sizes *)
                    val leftSize = VectorSlice.full #[getMaxSize left]
                    val rightSize = VectorSlice.full #[getMaxSize right]

                    val leftLen = SOME idx
                    val rightLen = SOME (Vector.length nodes - idx - 1)

                    val leftSizeSlice = VectorSlice.slice (sizes, 0, leftLen)
                    val rightSizeSlice = VectorSlice.slice (sizes, idx + 1, rightLen)

                    val leftSizes = VectorSlice.concat [leftSizeSlice, leftSize]
                    val rightSizes = VectorSlice.concat [rightSizeSlice, rightSize]

                    (* slice nodes *)
                    val left = VectorSlice.full #[left]
                    val right = VectorSlice.full #[right]

                    val leftNodesSlice = VectorSlice.slice (nodes, 0, leftLen)
                    val rightNodesSlice = VectorSlice.slice (nodes, idx + 1, rightLen)

                    val leftNodes = VectorSlice.concat [leftNodesSlice, left]
                    val rightNodes = VectorSlice.concat [right, rightNodesSlice]

                    (* join sizes and nodes *)
                    val left = BRANCH (leftNodes, leftSizes)
                    val right = BRANCH (rightNodes, rightSizes)
                  in
                    INSERT_SPLIT (left, right)
                  end
                else
                  (* can join children into parent *)
                  let
                    val midSizes = #[getMaxSize left, getMaxSize right]
                    val midSizes = VectorSlice.full midSizes
                    val midNodes = #[left, right]
                    val midNodes = VectorSlice.full midNodes

                    val leftLen = SOME idx
                    val rightLen = SOME (Vector.length sizes - idx)

                    val leftSizes = VectorSlice.slice (sizes, 0, leftLen)
                    val rightSizes = VectorSlice.slice (sizes, idx, rightLen)

                    val leftNodes = VectorSlice.slice (nodes, 0, leftLen)
                    val rightNodes = VectorSlice.slice (nodes, idx, rightLen)

                    val sizes = 
                      VectorSlice.concat [leftSizes, midSizes, rightSizes]
                    val nodes =
                      VectorSlice.concat [leftNodes, midNodes, rightNodes]
                  in
                    INSERT_UPDATE (BRANCH (nodes, sizes))
                  end
          end
    | LEAF (items, sizes) =>
        if Vector.length items = 0 then
          (* leaf is empty, so return leaf containing one item *)
          let
            val item = #[{start = start, finish = finish}]
            val size = #[finish]
          in
            INSERT_UPDATE (LEAF (item, size))
          end
        else
          if finish > Vector.sub (sizes, Vector.length sizes - 1) then
            if Vector.length sizes = maxSize then
              (* have to split *)
              let
                val startLen = SOME halfSize
                val midLen = SOME (Vector.length items - halfSize)

                val leftSizes = VectorSlice.slice (sizes, 0, startLen)
                val leftItems = VectorSlice.slice (items, 0, startLen)

                val midSizes = VectorSlice.slice (sizes, halfSize, midLen)
                val midItems = VectorSlice.slice (items, halfSize, midLen)

                val rightSizes = VectorSlice.full #[finish]
                val rightItems = VectorSlice.full #[{start = start, finish = finish}]

                val rightItems = VectorSlice.concat [midItems, rightItems]
                val leftItems = VectorSlice.vector leftItems

                val rightSizes = VectorSlice.concat [midSizes, rightSizes]
                val leftSizes = VectorSlice.vector leftSizes

                val left = LEAF (leftItems, leftSizes)
                val right = LEAF (rightItems, rightSizes)
              in
                INSERT_SPLIT (left, right)
              end
            else
              (* can just append *)
              let
                val sizes = Vector.concat [sizes, #[finish]]
                val item = #[{start = start, finish = finish}]
                val items = Vector.concat [items, item]
              in
                INSERT_UPDATE (LEAF (items, sizes))
              end
          else if finish < #start (Vector.sub (items, 0)) then
            (* prepend *)
            if Vector.length sizes = maxSize then
              (* have to split *)
              let
                val leftSizes = VectorSlice.full #[finish]
                val leftItems = VectorSlice.full #[{start = start, finish = finish}]

                val midLen = SOME halfSize
                val rightLen = SOME (Vector.length items - halfSize)

                val midSizes = VectorSlice.slice (sizes, 0, midLen)
                val midItems = VectorSlice.slice (items, 0, midLen)

                val rightSizes = VectorSlice.slice (sizes, halfSize, rightLen)
                val rightItems = VectorSlice.slice (items, halfSize, rightLen)

                val leftSizes = VectorSlice.concat [leftSizes, midSizes]
                val rightSizes = VectorSlice.vector rightSizes

                val leftItems = VectorSlice.concat [leftItems, midItems]
                val rightItems = VectorSlice.vector rightItems

                val left = LEAF (leftItems, leftSizes)
                val right = LEAF (rightItems, rightSizes)
              in
                INSERT_SPLIT (left, right)
              end
            else
              (* just prepend *)
              let
                val sizes = Vector.concat [#[finish], sizes]
                val item = {start = start, finish = finish}
                val items = Vector.concat [#[item], items]
              in
                INSERT_UPDATE (LEAF (items, sizes))
              end
        else
          (* insert into middle *)
          let
            val idx = BinSearch.equalOrMore (finish, sizes)
            val leftLen = SOME idx
            val rightLen = SOME (Vector.length sizes - idx)

            val leftSizes = VectorSlice.slice (sizes, 0, leftLen)
            val rightSizes = VectorSlice.slice (sizes, idx, rightLen)

            val leftItems = VectorSlice.slice (items, 0, leftLen)
            val rightItems = VectorSlice.slice (items, idx, rightLen)
            val midSize = VectorSlice.full #[finish]
            val midItem = VectorSlice.full #[{start = start, finish = finish}]
          in
            if Vector.length items = maxSize then
              (* have to return split *)
              let
                val leftSizes = VectorSlice.concat [leftSizes, midSize]
                val rightSizes = VectorSlice.vector rightSizes

                val leftItems = VectorSlice.concat [leftItems, midItem]
                val rightItems = VectorSlice.vector rightItems

                val left = LEAF (leftItems, leftSizes)
                val right = LEAF (rightItems, rightSizes)
              in
                INSERT_SPLIT (left, right)
              end
            else
              (* have to return update *)
              let
                val sizes = VectorSlice.concat [leftSizes, midSize, rightSizes]
                val items = VectorSlice.concat [leftItems, midItem, rightItems]
              in
                INSERT_UPDATE (LEAF (items, sizes))
              end
          end

    fun insert (start, finish, tree) =
      case helpInsert (start, finish, tree) of
        INSERT_UPDATE tree => tree
      | INSERT_SPLIT (left, right) =>
          let
            val sizes = #[getMaxSize left, getMaxSize right]
            val nodes = #[left, right]
          in
            BRANCH (nodes, sizes)
          end
end
