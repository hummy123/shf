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
            let val nextCheckIdx = checkIdx - Vector.sub (sizes, searchIdx - 1)
            in isInRange (nextCheckIdx, Vector.sub (nodes, searchIdx))
            end
        end
    | LEAF (values, sizes) =>
        let
          val searchIdx = BinSearch.equalOrMore (checkIdx, sizes)
        in
          if searchIdx = ~1 then
            false
          else
            let val {start, finish} = Vector.sub (values, searchIdx)
            in checkIdx >= start andalso checkIdx <= finish
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
                  val finish =
                    finish - Vector.sub (sizes, Vector.length sizes - 1)
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
        let val maxSize = getFinishIdx tree
        in BRANCH (#[tree, newNode], #[maxSize, finish])
        end

  fun getStart tree =
    case tree of
      LEAF (values, _) => Vector.sub (values, 0)
    | BRANCH (nodes, _) => getStart (Vector.sub (nodes, 0))

  fun helpNextMatch (cursorIdx, tree, acc) =
    case tree of
      LEAF (values, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx = ~1 then
            {start = ~1, finish = ~1}
          else
            let val {start, finish} = Vector.sub (values, idx)
            in {start = start + acc, finish = finish + acc}
            end
        end
    | BRANCH (nodes, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx = ~1 then
            {start = ~1, finish = ~1}
          else
            let
              val prevSize = if idx = 0 then 0 else Vector.sub (sizes, idx - 1)
              val acc = acc + prevSize
              val cursorIdx = cursorIdx - prevSize
            in
              helpNextMatch (cursorIdx, Vector.sub (nodes, idx), acc)
            end
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
          if idx = ~1 then
            {start = ~1, finish = ~1}
          else
            let
              val prevSize = if idx = 0 then 0 else Vector.sub (sizes, idx - 1)
              val cursorIdx = cursorIdx - prevSize
            in
              helpNextMatch (cursorIdx, Vector.sub (nodes, idx), prevSize)
            end
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
        else
          let in
            if cursorIdx >= start andalso cursorIdx <= finish then
              loopNextMatch (start, finish, tree, count)
            else
              loopNextMatch (start, finish, tree, count - 1)
          end
      end

  fun getLast (tree, acc) =
    case tree of
      LEAF (values, _) =>
        let val {start, finish} = Vector.sub (values, Vector.length values - 1)
        in {start = start + acc, finish = finish + acc}
        end
    | BRANCH (nodes, sizes) =>
        let
          val acc =
            if Vector.length sizes > 1 then
              acc + Vector.sub (sizes, Vector.length sizes - 1)
            else
              acc
        in
          getLast (Vector.sub (nodes, Vector.length nodes - 1), acc)
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
  fun helpPrevMatch (cursorIdx, tree, acc) =
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
              if #start result < cursorIdx then
                {start = #start result + acc, finish = #finish result + acc}
              else
                {start = ~1, finish = ~1}
            end
          else
            let
              val current = Vector.sub (values, idx)
              val {start, finish} =
                if cursorIdx > #start current then current
                else Vector.sub (values, idx - 1)
            in
              {start = start + acc, finish = finish + acc}
            end
        end
    | BRANCH (nodes, sizes) =>
        let
          val idx = BinSearch.equalOrMore (cursorIdx, sizes)
        in
          if idx < 0 then
            {start = ~1, finish = ~1}
          else if idx = 0 then
            helpPrevMatch (cursorIdx, Vector.sub (nodes, idx), acc)
          else
            let
              val node = Vector.sub (nodes, idx)
              val prevSize = Vector.sub (sizes, idx - 1)
              val result =
                helpPrevMatch (cursorIdx - prevSize, node, acc + prevSize)
            in
              if #start result = ~1 then
                let
                  val prevPrevSize =
                    if idx - 2 < 0 then 0 else Vector.sub (sizes, idx - 2)
                in
                  getLast (Vector.sub (nodes, idx - 1), acc + prevPrevSize)
                end
              else
                result
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
          let val {start, finish} = getLast (tree, ~1)
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
          let val {start, finish} = getLast (tree, ~1)
          in loopPrevMatch (start, finish, tree, count - 1)
          end
        else if cursorIdx >= start andalso cursorIdx <= finish then
          loopPrevMatch (start, finish, tree, count)
        else
          loopPrevMatch (start, finish, tree, count - 1)
      end

  fun getFirstItem tree =
    case tree of
      BRANCH (nodes, _) => getFirstItem (Vector.sub (nodes, 0))
    | LEAF (items, _) =>
        if Vector.length items = 0 then {start = ~1, finish = ~1}
        else Vector.sub (items, 0)

  fun getMaxSize tree =
    case tree of
      BRANCH (_, sizes) => 
        if Vector.length sizes = 0 then
          0
        else
          Vector.sub (sizes, Vector.length sizes - 1)
    | LEAF (_, sizes) => 
        if Vector.length sizes = 0 then
          0
        else
          Vector.sub (sizes, Vector.length sizes - 1)

  fun splitLeft (offset, tree) =
    case tree of
      BRANCH (nodes, sizes) =>
        if offset <= Vector.sub (sizes, 0) then
          splitLeft (offset, Vector.sub (nodes, 0))
        else if offset >= Vector.sub (sizes, Vector.length sizes - 1) then
          let
            val prevSize =
              if Vector.length sizes > 1 then
                Vector.sub (sizes, Vector.length sizes - 1)
              else
                0
            val result = splitLeft (offset - prevSize, Vector.sub
              (nodes, Vector.length nodes - 1))
          in
            if isEmpty result then
              let
                val len = SOME (Vector.length sizes - 1)
                val sizeSlice = VectorSlice.slice (sizes, 0, len)
                val sizes = VectorSlice.vector sizeSlice
                val nodeSlice = VectorSlice.slice (nodes, 0, len)
                val nodes = VectorSlice.vector nodeSlice
              in
                BRANCH (nodes, sizes)
              end
            else
              let
                val newChildSize = getMaxSize result + prevSize
                val sizes = Vector.update
                  (sizes, Vector.length sizes - 1, newChildSize)
                val nodes = Vector.update
                  (nodes, Vector.length nodes - 1, result)
              in
                BRANCH (nodes, sizes)
              end
          end
        else
          let
            val idx = BinSearch.equalOrMore (offset, sizes)
            val prevSize = if idx > 1 then Vector.sub (sizes, idx - 1) else 0
            val result = splitLeft (offset - prevSize, Vector.sub (nodes, idx))
          in
            if isEmpty result then
              let
                val len = SOME idx
                val sizeSlice = VectorSlice.slice (sizes, 0, len)
                val sizes = VectorSlice.vector sizeSlice
                val nodeSlice = VectorSlice.slice (nodes, 0, len)
                val nodes = VectorSlice.vector nodeSlice
              in
                BRANCH (nodes, sizes)
              end
            else
              let
                val len = idx + 1
                val sizes = Vector.tabulate (len, fn i =>
                  if i = idx then getMaxSize result + prevSize
                  else Vector.sub (sizes, i))
                val nodes = Vector.tabulate (len, fn i =>
                  if i = idx then result else Vector.sub (nodes, i))
              in
                BRANCH (nodes, sizes)
              end
          end
    | LEAF (items, sizes) =>
        if Vector.length sizes > 0 then
          if offset > Vector.sub (sizes, Vector.length sizes - 1) then
            tree
          else if offset <= Vector.sub (sizes, 0) then
            LEAF (#[], #[])
          else
            let
              val len = BinSearch.equalOrMore (offset, sizes)
              val sizes = VectorSlice.slice (sizes, 0, SOME len)
              val sizes = VectorSlice.vector sizes
              val items = VectorSlice.slice (items, 0, SOME len)
              val items = VectorSlice.vector items
            in
              LEAF (items, sizes)
            end
        else
          tree

  (* Unlike 'splitLeft' which leaves the size metadata alone
   * (except for splitting it),
   * we want splitRight to decrement the size metadata 
   * by the largest entry in the size table that was split.
   * This is so that we can maintain relative indexing metadata. *)
  fun splitRight (offset, tree) =
    case tree of
      BRANCH (nodes, sizes) =>
        if offset <= Vector.sub (sizes, 0) then
          (* we want to split first node *)
          let
            val firstSizeBefore = Vector.sub (sizes, 0)
            val result = splitRight (offset, Vector.sub (nodes, 0))
          in
            if isEmpty result then
              let
                val len = Vector.length sizes - 1

                val sizeSlice = VectorSlice.slice (sizes, 1, SOME len)
                val sizes =
                  VectorSlice.map (fn el => el - firstSizeBefore) sizeSlice

                val nodeSlice = VectorSlice.slice (nodes, 1, SOME len)
                val nodes = VectorSlice.vector nodeSlice
              in
                BRANCH (nodes, sizes)
              end
            else
              let
                val firstSizeAfter = getMaxSize result
                val sizeDiff = firstSizeBefore - firstSizeAfter

                val sizes =
                  Vector.mapi
                    (fn (idx, el) =>
                       if idx = 0 then firstSizeAfter else el - sizeDiff) sizes

                val nodes = Vector.update (nodes, 0, result)
              in
                BRANCH (nodes, sizes)
              end
          end
        else if offset >= Vector.sub (sizes, Vector.length sizes - 1) then
          (* we want to split after last node, leaving empty *)
          BRANCH (#[], #[])
        else
          let
            val idx = BinSearch.equalOrMore (offset, sizes)
            val prevSize = Vector.sub (sizes, idx - 1)
            val curSize = Vector.sub (sizes, idx)
            val result = splitRight (offset - prevSize, Vector.sub (nodes, idx))
          in
            if isEmpty result then
              let
                val startIdx = idx + 1
                val len = Vector.length sizes - startIdx

                val sizeSlice = VectorSlice.slice (sizes, startIdx, SOME len)
                val sizes = VectorSlice.map (fn el => el - curSize) sizeSlice

                val nodeSlice = VectorSlice.slice (nodes, startIdx, SOME len)
                val nodes = VectorSlice.vector nodeSlice
              in
                BRANCH (nodes, sizes)
              end
            else
              let
                val newCurSize = getMaxSize result
                val sizeDiff = curSize - newCurSize

                val len = Vector.length sizes - idx
                val sizeSlice = VectorSlice.slice (sizes, idx, SOME len)
                val sizes = VectorSlice.map (fn el => el - sizeDiff) sizeSlice

                val nodeSlice = VectorSlice.slice (nodes, idx, SOME len)
                val nodes = VectorSlice.vector nodeSlice
              in
                BRANCH (nodes, sizes)
              end
          end
    | LEAF (items, sizes) =>
        if offset > Vector.sub (sizes, Vector.length sizes - 1) then
          LEAF (#[], #[])
        else if offset <= Vector.sub (sizes, 0) then
          tree
        else
          let
            val idx = BinSearch.equalOrMore (offset, sizes)
            val len = Vector.length sizes - idx
            val len = SOME len

            val prevSize = if idx < 1 then 0 else Vector.sub (sizes, idx - 1)
            val sizes = VectorSlice.slice (sizes, idx, len)
            val sizes = VectorSlice.map (fn i => i - prevSize) sizes

            val items = VectorSlice.slice (items, idx, len)
            val items =
              VectorSlice.map
                (fn {start, finish} =>
                   {start = start - prevSize, finish = finish - prevSize}) items
          in
            LEAF (items, sizes)
          end

  fun getDepth (tree, acc) =
    case tree of
      BRANCH (nodes, _) => getDepth (Vector.sub (nodes, 0), acc + 1)
    | LEAF (_, _) => acc

  fun getRootVecLength tree =
    case tree of
      BRANCH (_, sizes) => Vector.length sizes
    | LEAF (_, sizes) => Vector.length sizes

  fun joinSameDepth (left, right) =
    case (left, right) of
      (BRANCH (leftNodes, leftSizes), BRANCH (rightNodes, rightSizes)) =>
        let
          val nodes = Vector.concat [leftNodes, rightNodes]
          val maxLeftSize = Vector.sub (leftSizes, Vector.length leftSizes - 1)
          val sizes =
            Vector.tabulate
              ( Vector.length leftSizes + Vector.length rightSizes
              , fn i =>
                  if i < Vector.length leftSizes then
                    Vector.sub (leftSizes, i)
                  else
                    Vector.sub (rightSizes, i - Vector.length leftSizes)
                    + maxLeftSize
              )
        in
          BRANCH (nodes, sizes)
        end
    | (LEAF (leftItems, leftSizes), LEAF (rightItems, rightSizes)) =>
        let
          val maxLeftSize = Vector.sub (leftSizes, Vector.length leftSizes - 1)
          val len = Vector.length leftItems + Vector.length rightItems
          val items = Vector.tabulate (len, fn i =>
            if i < Vector.length leftItems then
              Vector.sub (leftItems, i)
            else
              let
                val {start, finish} = Vector.sub
                  (rightItems, i - Vector.length leftItems)
              in
                {start = start + maxLeftSize, finish = finish + maxLeftSize}
              end)
          val sizes = Vector.tabulate (len, fn i =>
            if i < Vector.length leftSizes then
              Vector.sub (leftSizes, i)
            else
              Vector.sub (rightSizes, i - Vector.length leftSizes) + maxLeftSize)
        in
          LEAF (items, sizes)
        end
    | _ =>
        raise Fail
          "PersistentVector.joinSameDepth: one is BRANCH and other is LEAF"

  datatype append_join_result =
    APPEND_JOIN_APPEND of t
  | APPEND_JOIN_UPDATE of t

  fun appendJoin (left, right, joinDepth, rightLength) =
    case left of
      BRANCH (nodes, sizes) =>
        if joinDepth = 0 then
          (* base case: should join at this depth *)
          if Vector.length nodes + rightLength > maxSize then
            APPEND_JOIN_APPEND right
          else
            (case right of
               BRANCH (rightNodes, rightSizes) =>
                 let
                   val nodes = Vector.concat [nodes, rightNodes]

                   val lastLeftSize = Vector.sub
                     (sizes, Vector.length sizes - 1)
                   val sizes = Vector.tabulate (Vector.length nodes, fn i =>
                     if i < Vector.length sizes then
                       Vector.sub (sizes, i)
                     else
                       Vector.sub (rightSizes, i - Vector.length sizes)
                       + lastLeftSize)
                 in
                   APPEND_JOIN_UPDATE (BRANCH (nodes, sizes))
                 end
             | LEAF _ =>
                 raise Fail
                   "persistent-vector.sml appendJoin: \
                   \expected to join when left and right are both BRANCH \
                   \but left is BRANCH and right is LEAF")
        else
          (* recursion case: join below *)
          let
            val lastIdx = Vector.length nodes - 1
            val lastNode = Vector.sub (nodes, lastIdx)
          in
            case appendJoin (lastNode, right, joinDepth - 1, rightLength) of
              APPEND_JOIN_UPDATE newLast =>
                let
                  val prevSize =
                    if lastIdx > 0 then Vector.sub (sizes, lastIdx - 1) else 0
                  val newLastSize = getMaxSize newLast + prevSize
                  val sizes = Vector.update (sizes, lastIdx, newLastSize)
                  val nodes = Vector.update (nodes, lastIdx, newLast)
                in
                  APPEND_JOIN_UPDATE (BRANCH (nodes, sizes))
                end
            | APPEND_JOIN_APPEND newNode =>
                if Vector.length nodes = maxSize then
                  (* parent has to append insead as this node
                   * would exceed capacity if appended here *)
                  APPEND_JOIN_APPEND (BRANCH
                    (#[newNode], #[getMaxSize newNode]))
                else
                  let
                    val prevSize = Vector.sub (sizes, Vector.length sizes - 1)
                    val newNodeSize = #[getMaxSize newNode + prevSize]
                    val sizes = Vector.concat [sizes, newNodeSize]

                    val newNode = #[newNode]
                    val nodes = Vector.concat [nodes, newNode]
                  in
                    APPEND_JOIN_UPDATE (BRANCH (nodes, sizes))
                  end
          end
    | LEAF (items, sizes) =>
        (* joinDepth should = 0, and we assume it is *)
        if Vector.length items + rightLength > maxSize then
          APPEND_JOIN_APPEND right
        else
          (case right of
             LEAF (rightItems, rightSizes) =>
               let
                 val leftMaxSize = Vector.sub (sizes, Vector.length sizes - 1)

                 val newLen = Vector.length items + Vector.length rightItems
                 val items = Vector.tabulate (newLen, fn i =>
                   if i < Vector.length items then
                     Vector.sub (items, i)
                   else
                     let
                       val {start, finish} = Vector.sub
                         (rightItems, i - Vector.length items)
                     in
                       { start = start + leftMaxSize
                       , finish = finish + leftMaxSize
                       }
                     end)
                 val sizes = Vector.tabulate (newLen, fn i =>
                   if i < Vector.length sizes then
                     Vector.sub (sizes, i)
                   else
                     Vector.sub (rightSizes, i - Vector.length sizes)
                     + leftMaxSize)
               in
                 APPEND_JOIN_UPDATE (LEAF (items, sizes))
               end
           | BRANCH _ =>
               raise Fail
                 "persistent-vector.sml appendJoin: \
                 \left is LEAF and expected right to also be LEAF \
                 \but right is BRANCH")

  datatype prepend_join_result =
    PREPEND_JOIN_PREPEND of t
  | PREPEND_JOIN_UPDATE of t

  fun prependJoin (left, right, joinDepth, leftLength) =
    case right of
      BRANCH (rightNodes, rightSizes) =>
        if joinDepth = 0 then
          (* base case: join *)
          if Vector.length rightNodes + leftLength > maxSize then
            PREPEND_JOIN_PREPEND left
          else
            (case left of
               BRANCH (leftNodes, leftSizes) =>
                 let
                   val nodes = Vector.concat [leftNodes, rightNodes]
                   val maxLeftSize = Vector.sub
                     (leftSizes, Vector.length leftSizes - 1)
                   val sizes = Vector.tabulate (Vector.length nodes, fn i =>
                     if i < Vector.length leftSizes then
                       Vector.sub (leftSizes, i)
                     else
                       Vector.sub (rightSizes, i - Vector.length leftSizes)
                       + maxLeftSize)
                 in
                   PREPEND_JOIN_UPDATE (BRANCH (nodes, sizes))
                 end
             | LEAF _ =>
                 raise Fail
                   "persistent-vector.sml prependJoin: \
                   \expected left and right to be BRANCH \
                   \but right is BRANCH while left is LEAF")
        else
          (* recursive case *)
          let
            val firstRightNode = Vector.sub (rightNodes, 0)
            val firstRightSize = getMaxSize firstRightNode
          in
            (case prependJoin (left, firstRightNode, joinDepth - 1, leftLength) of
               PREPEND_JOIN_UPDATE newFirst =>
                 let
                   val newFirstSize = getMaxSize newFirst
                   val sizeDiff = newFirstSize - firstRightSize
                   val sizes = Vector.map (fn el => el + sizeDiff) rightSizes
                   val nodes = Vector.update (rightNodes, 0, newFirst)
                 in
                   PREPEND_JOIN_UPDATE (BRANCH (nodes, sizes))
                 end
             | PREPEND_JOIN_PREPEND newFirst =>
                 if Vector.length rightSizes = maxSize then
                   PREPEND_JOIN_PREPEND (BRANCH
                     (#[newFirst], #[getMaxSize newFirst]))
                 else
                   let
                     val nodes = Vector.concat [#[newFirst], rightNodes]
                     val newFirstSize = getMaxSize newFirst

                     val sizes = Vector.tabulate (Vector.length nodes, fn i =>
                       if i = 0 then newFirstSize
                       else Vector.sub (rightSizes, i - 1) + newFirstSize)
                   in
                     PREPEND_JOIN_UPDATE (BRANCH (nodes, sizes))
                   end)
          end
    | LEAF (rightItems, rightSizes) =>
        (* joinDepth should = 0, and we assume it is *)
        if Vector.length rightItems + leftLength > maxSize then
          PREPEND_JOIN_PREPEND left
        else
          (case left of
             LEAF (leftItems, leftSizes) =>
               let
                 val maxLeftSize = Vector.sub
                   (leftSizes, Vector.length leftSizes - 1)
                 val len = Vector.length leftSizes + Vector.length rightItems
                 val sizes = Vector.tabulate (len, fn i =>
                   if i < Vector.length leftSizes then
                     Vector.sub (leftSizes, i)
                   else
                     Vector.sub (rightSizes, i - Vector.length leftSizes)
                     + maxLeftSize)
                 val items = Vector.tabulate (len, fn i =>
                   if i < Vector.length leftItems then
                     Vector.sub (leftItems, i)
                   else
                     let
                       val {start, finish} = Vector.sub
                         (rightItems, i - Vector.length leftItems)
                     in
                       { start = start + maxLeftSize
                       , finish = finish + maxLeftSize
                       }
                     end)
               in
                 PREPEND_JOIN_UPDATE (LEAF (items, sizes))
               end
           | BRANCH _ =>
               raise Fail
                 "persistent-vector.sml prependJoin: \
                 \expected left and right to be LEAF \
                 \but right is LEAF while left is BRANCH")

  fun join (left, right) =
    if isEmpty left then
      right
    else if isEmpty right then
      left
    else
      let
        val leftDepth = getDepth (left, 0)
        val rightDepth = getDepth (right, 0)
      in
        if leftDepth = rightDepth then
          if getRootVecLength left + getRootVecLength right <= maxSize then
            joinSameDepth (left, right)
          else
            let
              val ls = getMaxSize left
              val rs = getMaxSize right + ls
              val sizes = #[ls, rs]
              val nodes = #[left, right]
            in
              BRANCH (nodes, sizes)
            end
        else if leftDepth > rightDepth then
          let
            val joinDepth = leftDepth - rightDepth
            val rightLength = getRootVecLength right
          in
            case appendJoin (left, right, joinDepth, rightLength) of
              APPEND_JOIN_UPDATE t => t
            | APPEND_JOIN_APPEND newRight =>
                let
                  val ls = getMaxSize left
                  val rs = getMaxSize right + ls
                  val sizes = #[ls, rs]
                  val nodes = #[left, newRight]
                in
                  BRANCH (nodes, sizes)
                end
          end
        else
          let
            val joinDepth = rightDepth - leftDepth
            val leftLength = getRootVecLength left
          in
            case prependJoin (left, right, joinDepth, leftLength) of
              PREPEND_JOIN_UPDATE t => t
            | PREPEND_JOIN_PREPEND newLeft =>
                let
                  val ls = getMaxSize newLeft
                  val rs = getMaxSize right + ls
                  val sizes = #[ls, rs]
                  val nodes = #[newLeft, right]
                in
                  BRANCH (nodes, sizes)
                end
          end
      end

    fun root tree =
      case tree of
        BRANCH (nodes, sizes) =>
          if Vector.length nodes = 1 then
            Vector.sub (nodes, 0)
          else
            tree
      | LEAF _ => tree

    fun decrement (minusBy, tree) =
      case tree of
        BRANCH (nodes, sizes) =>
          let
            val sizes = Vector.map (fn el => el - minusBy) sizes
            val newFirstNode = decrement (minusBy, Vector.sub (nodes, 0))
            val nodes = Vector.update (nodes, 0, newFirstNode)
          in
            BRANCH (nodes, sizes)
          end
      | LEAF (items, sizes) =>
          let
            val sizes = Vector.map (fn el => el - minusBy) sizes
            val items = Vector.map 
              (fn {start, finish} =>
                {start = start - minusBy, finish = finish - minusBy})
              items
          in
            LEAF (items, sizes)
          end

    fun delete (start, length, tree) =
      let
        val finishIdx = start + length
        val {start, finish} = startNextMatch (start, tree)
      in
        if start = ~1 then
          (* nothing to delete *)
          tree
        else
          let
            (* split left and right side *)
            val left = splitLeft (start, tree)
            val right = splitRight (finishIdx, tree)
          in
            if isEmpty right then
              root left
            else
              let
                val left = root left
                val right = root right

                (* calculate what the new index should be
                 * for the first match in the right tree,
                 * and decrement the right tree to reach this index
                 * if necessary. 
                 *
                 * We calculate the index the right node should start at
                 * using this method:
                 * 1. We get the first match after finishIdx from the original * tree
                 * 2. We subtract this by the length provided to us,
                 *    which tells us where the right node should start at.
                 * 3. We get the first item in the right node,
                 *    and add the last item in the left node 
                 *    to get an absolute value.
                 * 4. We subtract the match from the original tree
                 *    by the first item in the right node.
                 *    This step gives us the difference: how much
                 *    we need to decrement the right node by.
                 * *)
                val {start = shouldBeStart, ...} = 
                  startNextMatch (finishIdx, tree)
                val shouldBeStart = shouldBeStart - length

                val leftSize = getMaxSize left
                val {start = rightStart, ...} = getFirstItem right
                val difference = rightStart - shouldBeStart
                val right =
                  if difference = 0 then
                    right
                  else
                    decrement (difference, right)
              in
                join (left, right)
              end
          end
      end

  fun insert (start, length, tree) =
    let
      val left = splitLeft (start, tree)
      val left = append (start, start + length, tree)
      val right = splitRight (start + 1, tree)
    in
      join (left, right)
    end

  (* conversion functions for testing *)
  fun helpFromList ([], acc) = acc
    | helpFromList ((start, finish) :: tl, acc) =
      let
        val acc = append (start, finish, acc)
      in
        helpFromList (tl, acc)
      end

  fun fromList lst = helpFromList (lst, empty)

  fun helpToList (tree, acc): (int * int) list =
    case tree of
      BRANCH (nodes, _) => 
        helpToListNodes (Vector.length nodes - 1, nodes, acc)
    | LEAF (items, _) =>
        helpToListItems (Vector.length items - 1, items, acc)

  and helpToListItems (pos, items, acc) =
    if pos < 0 then
      acc
    else
      let
        val {start, finish} = Vector.sub (items, pos)
        val acc = (start, finish) :: acc
      in
        helpToListItems (pos - 1, items, acc)
      end

  and helpToListNodes (pos, nodes, acc) =
    if pos < 0 then
      acc
    else
      let
        val node = Vector.sub (nodes, pos)
        val acc = helpToList (node, acc)
      in
        helpToListNodes (pos - 1, nodes, acc)
      end

  fun toList tree = helpToList (tree, [])
end
