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
    | BRANCH (_, sizes) => Vector.length sizes = 0

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

  fun getStartIdx t =
    case t of
      BRANCH (nodes, _) => getStartIdx (Vector.sub (nodes,  0))
    | LEAF (items, _) => 
        if Vector.length items = 0 then
          0
        else
          #start (Vector.sub (items, 0))

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

    fun splitLeft (splitIdx, tree) =
      case tree of
        LEAF (items, sizes) =>
          if Vector.length items = 0 then
            (* if tree is empty, then just return tree *)
            tree
          else 
            let
              val {start, ...} = Vector.sub (items, 0)
            in
              (* if all items are after splitIdx,
               * then we want to return an empty tree,
               * splitting everything *)
              if splitIdx < start then
                empty
              else if splitIdx > Vector.sub (sizes, Vector.length sizes - 1) then
                (* if all items are before splitIdx,
                 * then we want to return the same tree,
                 * splitting nothing *)
                tree
              else
                (* we want to split from somewhere in middle, keeping left *)
                let
                  val idx = BinSearch.equalOrMore (splitIdx, sizes)
                  val idx = SOME idx

                  val items = VectorSlice.slice (items, 0, idx)
                  val items = VectorSlice.vector items

                  val sizes = VectorSlice.slice (sizes, 0, idx)
                  val sizes = VectorSlice.vector sizes
                in
                  LEAF (items, sizes)
                end
            end
     | BRANCH (nodes, sizes) =>
         if Vector.length nodes = 0 then
           tree
         else 
           if splitIdx < Vector.sub (sizes, 0) then
             (* we want to split first node from rest *)
             splitLeft (splitIdx, Vector.sub (nodes, 0))
           else if splitIdx > Vector.sub (sizes, Vector.length sizes - 1) then
             (* split point is after this subtree,
              * so return this subtree unchanged *)
             tree
           else
             (* we want to split from somewhere in middle *)
             let
               val idx = BinSearch.equalOrMore (splitIdx, sizes)
               val prevSize =
                 if idx = 0 then
                   0
                 else
                   Vector.sub (sizes, idx - 1)
               val child = 
                 splitLeft (splitIdx - prevSize, Vector.sub (nodes, idx))

                val sizes = VectorSlice.slice (sizes, 0, SOME idx)
                val nodes = VectorSlice.slice (nodes, 0, SOME idx)
             in
               if isEmpty child then
                 let
                   val sizes = VectorSlice.vector sizes
                   val nodes = VectorSlice.vector nodes
                 in
                   BRANCH (nodes, sizes)
                 end
               else
                 let
                   val childSize = VectorSlice.full #[getFinishIdx child + prevSize]
                   val sizes =VectorSlice.concat [sizes, childSize]

                   val childNode = VectorSlice.full #[child]
                   val nodes = VectorSlice.concat [nodes, childNode]
                 in
                   BRANCH (nodes, sizes)
                 end
             end

  (* When we split in this function,
   * we always want to update the sizes vector
   * so that the relative rope-like metadata is valid *)
  fun splitRight (splitIdx, tree) =
    case tree of
      BRANCH (nodes, sizes) =>
        if splitIdx > Vector.sub (sizes, Vector.length sizes - 1) then
          (* splitIdx is greater than largest element,
           * so we want to remove everything;
           * or, in other words, we want to return an empty vec *)
          empty
        else
          let
            val idx = BinSearch.equalOrMore (splitIdx, sizes)
            val prevSize =
              if idx = 0 then
                0
              else
                Vector.sub (sizes, idx - 1)

            val oldChildSize = Vector.sub (sizes, idx)
            val child = splitRight (splitIdx - prevSize, Vector.sub (nodes, idx))

            val len = Vector.length nodes - (idx + 1)
            val sizesSlice = VectorSlice.slice (sizes, idx + 1, SOME len)
            val nodesSlice = VectorSlice.slice (nodes, idx + 1, SOME len)
          in
            if isEmpty child then
              if VectorSlice.length sizesSlice = 0 then
                (* if we descended down last node and last node became empty,
                 * then return empty vector *)
                empty
              else
                let
                  val nodes = VectorSlice.vector nodesSlice
                  val sizes = VectorSlice.map (fn el => el - oldChildSize) sizesSlice
                in
                  BRANCH (nodes, sizes)
                end
            else
              let
                val newChildSize = getFinishIdx child
                val sizes = Vector.tabulate (VectorSlice.length sizesSlice + 1,
                  fn i =>
                    if i = 0 then
                      newChildSize
                    else
                      let
                        val el = VectorSlice.sub (sizesSlice, i - 1)
                      in
                        el - oldChildSize + newChildSize
                      end
                )

                val child = VectorSlice.full #[child]
                val nodes = VectorSlice.concat [child, nodesSlice]
              in
                BRANCH (nodes, sizes)
              end
          end
    | LEAF (items, sizes) =>
        if Vector.length items = 0 then
          tree
        else
          if splitIdx > Vector.sub (sizes, Vector.length sizes - 1) then
            empty
          else if splitIdx < Vector.sub (sizes, 0) then
            tree
          else
            let
              val idx = BinSearch.equalOrMore (splitIdx, sizes)
              val {start, finish} = Vector.sub (items, idx)
              val idx =
                if splitIdx >= start then
                  idx + 1
                else
                  idx
            in
              if idx >= Vector.length items then
                empty
              else
                let
                  val prevSize =
                    if idx > 0 then
                      Vector.sub (sizes, idx - 1)
                    else
                      0
                  val len = Vector.length items - idx
                  val itemsSlice = VectorSlice.slice (items, idx, SOME len)
                  val items = VectorSlice.map 
                    (fn {start, finish} => 
                      {start = start - prevSize, finish = finish - prevSize}
                    ) 
                    itemsSlice
                  val sizes = Vector.map #finish items
                in
                  LEAF (items, sizes)
                end
            end

  fun decrementBy (decBy, tree) =
    case tree of
      BRANCH (nodes, sizes) =>
        let
          val child = decrementBy (decBy, Vector.sub (nodes, 0))
          val nodes = Vector.update (nodes, 0, child)
          val sizes = Vector.map (fn sz => sz - decBy) sizes
        in
          BRANCH (nodes, sizes)
        end
    | LEAF (items, sizes) =>
        let
          val items = Vector.map 
            (fn {start, finish} =>
              {start = start - decBy, finish = finish - decBy}
            ) items
          val sizes = Vector.map #finish items
        in
          LEAF (items, sizes)
        end

  fun countDepthLoop (counter, tree) =
    case tree of
      BRANCH (nodes, _) => countDepthLoop (counter + 1, Vector.sub (nodes, 0))
    | LEAF (_, _) => counter + 1

  fun countDepth tree = countDepthLoop (0, tree)

  datatype merge_same_depth_result =
    MERGE_SAME_DEPTH_UPDATE of t
  | MERGE_SAME_DEPTH_FULL

  fun mergeSameDepth (left, right) =
    case (left, right) of
      (LEAF (leftItems, leftSizes), LEAF (rightItems, rightSizes)) =>
        if Vector.length leftItems + Vector.length rightItems <= maxSize then
          let
            val offset = Vector.sub (leftSizes, Vector.length leftSizes - 1)
            val newVecLen = Vector.length leftItems + Vector.length rightItems
            val items = Vector.tabulate (newVecLen,
              fn i =>
                if i < Vector.length leftItems then
                  Vector.sub (leftItems, i)
                else
                  let
                    val {start, finish} = 
                      Vector.sub (rightItems, i - Vector.length leftItems) 
                  in
                    {start = start + offset, finish = finish + offset}
                  end
            )
            val sizes = Vector.map #finish items
          in
            MERGE_SAME_DEPTH_UPDATE (LEAF (items, sizes))
          end
        else
          MERGE_SAME_DEPTH_FULL
      | (BRANCH (leftNodes, leftSizes), BRANCH (rightNodes, rightSizes)) => 
          if Vector.length leftNodes + Vector.length rightNodes <= maxSize then
            let
              val offset = Vector.sub (leftSizes, Vector.length leftSizes - 1)
              val nodes = Vector.concat [leftNodes, rightNodes]

              val sizes = Vector.tabulate (Vector.length nodes, 
                fn i =>
                  if i < Vector.length leftSizes then
                    Vector.sub (leftSizes, i)
                  else
                    Vector.sub (rightSizes, i - Vector.length leftSizes) + offset
              )
            in
              MERGE_SAME_DEPTH_UPDATE (BRANCH (nodes, sizes))
            end
          else
            MERGE_SAME_DEPTH_FULL
      | _ => 
          raise Fail "PersistentVector.mergeSameDepth: \
          \left and right should both be BRANCH or both be LEAF \
          \but one is BRANCH and one is LEAF"

  datatype merge_diff_depth_result =
    MERGE_DIFF_DEPTH_UPDATE of t
  | MERGE_DIFF_DEPTH_FULL

  fun mergeWhenRightDepthIsGreater (left, right, targetDepth, curDepth) =
    if curDepth = targetDepth then
      case mergeSameDepth (left, right) of
        MERGE_SAME_DEPTH_UPDATE tree => MERGE_DIFF_DEPTH_UPDATE tree
      | MERGE_SAME_DEPTH_FULL => MERGE_DIFF_DEPTH_FULL
    else
      case right of
        BRANCH (nodes, sizes) =>
          (case mergeWhenRightDepthIsGreater 
            (left, Vector.sub (nodes, 0), targetDepth, curDepth + 1) of
              MERGE_DIFF_DEPTH_UPDATE child =>
                let
                  val oldChildSize = Vector.sub (sizes, 0)
                  val newChildSize = getFinishIdx child
                  val difference = newChildSize - oldChildSize

                  val nodes = Vector.update (nodes, 0, child)
                  val sizes = Vector.map (fn el => el + difference) sizes
                in
                  MERGE_DIFF_DEPTH_UPDATE (BRANCH (nodes, sizes))
                end
            | MERGE_DIFF_DEPTH_FULL =>
                let
                  val leftSize = getFinishIdx left
                  val sizes = Vector.tabulate (Vector.length nodes + 1,
                    fn i =>
                      if i = 0 then
                        leftSize
                      else
                        Vector.sub (sizes, i - 1) + leftSize
                  )
                  val nodes = Vector.concat [#[left], nodes]
                in
                  MERGE_DIFF_DEPTH_UPDATE (BRANCH (nodes, sizes))
                end)
      | LEAF _ =>
          raise Fail "PersistentVector.mergeWhenRightDepthIsGreater: \
          \reached LEAF before (curDepth = targetDepth)"

  fun mergeWhenLeftDepthIsGreater (left, right, targetDepth, curDepth) =
    if targetDepth = curDepth then
      case mergeSameDepth (left, right) of
        MERGE_SAME_DEPTH_UPDATE tree => MERGE_DIFF_DEPTH_UPDATE tree
      | MERGE_SAME_DEPTH_FULL => MERGE_DIFF_DEPTH_FULL
    else
      case left of
        BRANCH (nodes, sizes) =>
          (case 
            mergeWhenLeftDepthIsGreater (
              Vector.sub (nodes, Vector.length nodes - 1),
              right,
              targetDepth,
              curDepth + 1) of
            MERGE_DIFF_DEPTH_UPDATE  child =>
              let
                val lastIdx = Vector.length sizes - 1
                val oldChildSize = Vector.sub (sizes, lastIdx)
                val newChildSize = getFinishIdx child
                val difference = newChildSize - oldChildSize

                val nodes = Vector.update (nodes, lastIdx, child)
                val sizes = Vector.map (fn el => el + difference) sizes
              in
                MERGE_DIFF_DEPTH_UPDATE (BRANCH (nodes, sizes))
              end
          | MERGE_DIFF_DEPTH_FULL =>
              let
                val maxLeftSize = Vector.sub (sizes, Vector.length sizes - 1)
                val rightSize = getFinishIdx right + maxLeftSize
                val sizes = Vector.concat [sizes, #[rightSize]]
                val nodes = Vector.concat [nodes, #[right]]
              in
                MERGE_DIFF_DEPTH_UPDATE (BRANCH (nodes, sizes))
              end)
      | LEAF _ =>
          raise Fail "PersistentVector.mergeWhenLeftDepthIsGreater: \
          \reached LEAF before (curDepth = targetDepth)"

  fun merge (left, right) = 
    let
      val leftDepth = countDepth left
      val rightDepth = countDepth right
    in
      if leftDepth = rightDepth then
        case mergeSameDepth (left, right) of
          MERGE_SAME_DEPTH_UPDATE t => t
        | MERGE_SAME_DEPTH_FULL =>
            let
              val leftSize = getFinishIdx left
              val sizes = #[leftSize, getFinishIdx right + leftSize]
              val nodes = #[left, right]
            in
              BRANCH (nodes, sizes)
            end
      else if leftDepth < rightDepth then
        let
          val targetDepth = rightDepth - leftDepth
        in
          case mergeWhenRightDepthIsGreater 
            (left, right, targetDepth, 0) of
              MERGE_DIFF_DEPTH_UPDATE t => t
            | MERGE_DIFF_DEPTH_FULL => empty
        end
      else
        let
          val targetDepth = leftDepth - rightDepth
        in
          case mergeWhenLeftDepthIsGreater 
            (left, right, targetDepth, 0) of
              MERGE_DIFF_DEPTH_UPDATE t => t
            | MERGE_DIFF_DEPTH_FULL => empty
        end
    end

  fun delete (start, length, tree) =
    if isEmpty tree then
      empty
    else
      let
        val finish = start + length

        val matchBeforeStart = prevMatch (start, tree,  1)
        val matchBeforeStart =
          if matchBeforeStart >= start then
            ~1
          else
            matchBeforeStart

        val matchAfterFinish = nextMatch (finish, tree, 1)
        val matchAfterFinish =
          if matchAfterFinish < finish then
            ~1
          else 
            matchAfterFinish
      in
        if matchBeforeStart = ~1 andalso matchAfterFinish = ~1 then
          empty
        else if matchAfterFinish = ~1 then
          (* there is no match after 'finish', so just split left. 
           * No need to decrement or split right, 
           * because the right vector would be empty. *)
          splitLeft (start, tree)
        else if matchBeforeStart = ~1 then
          (* We don't want to use left split
           * because there are no elements 
           * before the deletion range's 'start'.
           * So we make a right split and then decrement it. *)
          let
            val right = splitRight (finish, tree)
          in
            if isEmpty right then
              empty
            else
              let
                val startIdx = getStartIdx right
                val shouldBeStartIdx = matchAfterFinish - length
                val difference = startIdx - shouldBeStartIdx
              in
                if difference = 0 then
                  right
                else if isEmpty right then
                  empty
                else
                  decrementBy (difference, right)
              end
          end
        else
          let
            val left = splitLeft (start, tree)
            val right = splitRight (finish, tree)
          in
            if isEmpty right then
              left
            else
              let
                val leftSize = getFinishIdx left
                val rightStartRelative = getStartIdx right
                val rightStartAbsolute = leftSize + rightStartRelative

                val shouldBeStartIdx = matchAfterFinish - length
                val difference = rightStartAbsolute - shouldBeStartIdx
              in
                if difference = 0 then
                  merge (left, right)
                else
                  let
                    val right = decrementBy (difference, right)
                  in
                    merge (left, right)
                  end
              end
          end
      end

  (* Usually, when inserting, we want the absolute metadata 
   * to be adjusted appropriately. 
   * An insertion should cause the absolute metadata to increment.
   * However, we sometimes want to insert a match without adjusting
   * the absolute metadata in this way. 
   * We want to do this when deleting some part of the buffer
   * would cause a new match to be found, for example. *)
  fun insertMatchKeepingAbsoluteInddices (start, finish, tree) =
    let
      val matchAfterFinish = nextMatch (finish, tree, 1)
    in
      if matchAfterFinish <= finish then
        (* no match after the 'finish', so we just append *)
        append (start, finish, tree)
      else
        let
          val left = splitLeft (start, tree)
          val right = splitRight (finish, tree)

          val left = append (start, finish, tree)

          val rightStartRelative = getStartIdx right
          val rightStartAbsolute = rightStartRelative + finish
          val difference = rightStartAbsolute - matchAfterFinish
          val right = decrementBy (difference, right)
        in
          merge (left, right)
        end
    end

  (* functions only for testing *)
  fun childrenHaveSameDepth (pos, nodes, expectedDepth) =
    if pos = Vector.length nodes then
      true
    else
      let
        val node = Vector.sub (nodes, pos)
      in
        if allLeavesAtSameDepth node then
          let
            val nodeDepth = countDepth node
          in
            if nodeDepth = expectedDepth then
              childrenHaveSameDepth (pos + 1, nodes, expectedDepth)
            else
              false
          end
        else
          false
      end

  and allLeavesAtSameDepth tree =
    case tree of
      BRANCH (nodes, _) => 
        let
          val expectedDepth = countDepth (Vector.sub (nodes, 0))
        in
          childrenHaveSameDepth (0, nodes, expectedDepth)
        end
    | LEAF _ => true

  fun fromListLoop (lst, acc) =
    case lst of
      {start, finish} :: tl =>
        let
          val acc = append (start, finish, acc)
        in
          fromListLoop (tl, acc)
        end
    | [] => acc

  fun fromList coords = fromListLoop (coords, empty)

  fun toListLoop (tree, acc) =
    case tree of
      BRANCH (nodes, _) =>
        let
          fun branchLoop (pos, acc) =
            if pos = Vector.length nodes then
              acc
            else
              let
                val acc = toListLoop (Vector.sub (nodes, pos), acc)
              in
                branchLoop (pos + 1, acc)
              end
        in
          branchLoop (0, acc)
        end
    | LEAF (items, _)  =>
        let
          fun itemLoop (pos, acc, offset) =
            if pos = Vector.length items then
              acc
            else
              let
                val {start, finish} = Vector.sub (items, pos)
                val item = {start = start + offset, finish = finish + offset}
              in
                itemLoop (pos + 1, item :: acc, offset)
              end

          val offset =
            case acc of
              {finish, ...} :: _ => finish
            | [] => 0
        in
          itemLoop (0, acc, offset)
        end

  fun toList tree = 
    let
      val result = toListLoop (tree, [])
    in
      List.rev result
    end
end
