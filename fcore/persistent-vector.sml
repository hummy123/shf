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

  fun getStart tree =
    case tree of
      LEAF (values, _) => Vector.sub (values, 0)
    | BRANCH (nodes, _) => getStart (Vector.sub (nodes, 0))

  fun helpNextMatch (cursorIdx, tree, absOffset) =
    case tree of
      LEAF (values, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx = ~1 then {start = ~1, finish = ~1}
          else 
            let
              val {start, finish} = Vector.sub (values, idx)
            in
              {start = start + absOffset, finish = finish + absOffset}
            end
        end
    | BRANCH (nodes, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx = ~1 then 
            {start = ~1, finish = ~1}
          else if idx = 0 then 
            helpNextMatch (cursorIdx, Vector.sub (nodes, idx), absOffset)
          else
            let
              val prevSize = Vector.sub (sizes, idx - 1)
              val cursorIdx = cursorIdx - prevSize
              val absOffset = absOffset + prevSize
            in
              helpNextMatch (cursorIdx, Vector.sub (nodes, idx), absOffset)
            end
        end

  fun loopNextMatch (prevStart, prevFinish, tree, count) =
    if count = 0 then
      prevStart
    else
      let
        val {start, finish} = helpNextMatch (prevFinish + 1, tree, 0)
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
        val {start, finish} = helpNextMatch (cursorIdx, tree, 0)
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

  fun getLast (tree, absOffset) =
    case tree of
      LEAF (values, _) => 
        let
          val {start, finish} = Vector.sub (values, Vector.length values - 1)
        in
          {start = start + absOffset, finish = finish + absOffset}
        end
    | BRANCH (nodes, sizes) => 
        let
          val prevSize =
            if Vector.length sizes - 2 >= 0 then
              Vector.sub (sizes, Vector.length sizes - 2)
            else
              0
          val absOffset = absOffset + prevSize
        in
          getLast (Vector.sub (nodes, Vector.length nodes - 1), absOffset)
        end

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
  fun helpPrevMatch (cursorIdx, tree, absOffset) =
    case tree of
      LEAF (values, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx < 0 then
            {start = ~1, finish = ~1}
          else if idx = 0 then
            let
              val {start, finish} = Vector.sub (values, 0)
            in
              if start < cursorIdx then 
                {start = start + absOffset, finish = finish + absOffset}
              else 
                {start = ~1, finish = ~1}
            end
          else
            let
              val {start, finish} = Vector.sub (values, idx)
            in
              if cursorIdx > start then
                {start = start + absOffset, finish = finish + absOffset}
              else 
                let
                  val {start, finish} = Vector.sub (values, idx - 1)
                in
                  {start = start + absOffset, finish = finish + absOffset}
                end
            end
        end
    | BRANCH (nodes, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx < 0 then
            {start = ~1, finish = ~1}
          else if idx = 0 then
            helpPrevMatch (cursorIdx, Vector.sub (nodes, idx), absOffset)
          else
            let
              val prevSize = Vector.sub (sizes, idx - 1)
              val node = Vector.sub (nodes, idx)
              val result = 
                helpPrevMatch (cursorIdx - prevSize, node, absOffset + prevSize)
            in
              if #start result = ~1 then 
                let
                  val prevSize =
                    if idx - 2 >= 0 then
                      Vector.sub (sizes, idx - 2)
                    else
                      0
                  val absOffset = absOffset + prevSize
                in
                  getLast (Vector.sub (nodes, idx - 1), absOffset)
                end
              else result
            end
        end

  fun loopPrevMatch (prevStart, prevFinish, tree, count) =
    if count = 0 then
      prevStart
    else
      let
        val {start, finish} = helpPrevMatch (prevFinish - 1, tree, 0)
      in
        if start = ~1 then
          let val {start, finish} = getLast (tree, 0)
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
        val {start, finish} = helpPrevMatch (cursorIdx, tree, 0)
      in
        if start = ~1 then
          let val {start, finish} = getLast (tree, 0)
          in loopPrevMatch (start, finish, tree, count - 1)
          end
        else if cursorIdx >= start andalso cursorIdx <= finish then
          loopPrevMatch (start, finish, tree, count)
        else
          loopPrevMatch (start, finish, tree, count - 1)
      end

    fun splitLeft (cursorIdx, tree) =
      case tree of
        LEAF (items, sizes) =>
          if Vector.length items = 0 then
            (* if tree is empty, then just return tree *)
            tree
          else 
            let
              val {start, ...} = Vector.sub (items, 0)
            in
              (* if all items are after cursorIdx,
               * then we want to return an empty tree,
               * splitting everything *)
              if cursorIdx < start then
                empty
              else if cursorIdx > Vector.sub (sizes, Vector.length sizes - 1) then
                (* if all items are before cursorIdx,
                 * then we want to return the same tree,
                 * splitting nothing *)
                tree
              else
                (* we want to split from somewhere in middle, keeping left *)
                let
                  val idx = BinSearch.equalOrMore (cursorIdx, sizes)
                  val idx = SOME idx

                  val items = VectorSlice.slice (items, 0, idx)
                  val items = VectorSlice.vector items

                  val sizes = VectorSlice.slice (sizes, 0, idx)
                  val sizes = VectorSlice.vector sizes
                in
                  LEAF (items, sizes)
                end
            end
end
