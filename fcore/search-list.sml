signature SEARCH_LIST =
sig
  type t = {left: int vector list, right: int vector list}
  val empty: t
  val insert: int * t -> t
  val delete: int * int * t -> t
  val mapFromNum: int * int * t -> t
end

structure SearchList =
struct
  type t = {left: int vector list, right: int vector list}

  val targetLength = 1024

  val empty: t = {left = [], right = []}

  fun isLessThanTarget (v1, v2) =
    Vector.length v1 + Vector.length v2 <= targetLength

  fun isThreeLessThanTarget (v1, v2, v3) =
    Vector.length v1 + Vector.length v2 + Vector.length v3 <= targetLength

  fun joinEndOfLeft (new, left) =
    case left of
      hd :: tail =>
        if isLessThanTarget (new, hd) then
          let val newHd = Vector.concat [hd, new]
          in newHd :: tail
          end
        else
          new :: left
    | [] => new :: left

  fun joinStartOfRight (new, right) =
    case right of
      hd :: tail =>
        if isLessThanTarget (new, hd) then
          let val newHd = Vector.concat [new, hd]
          in newHd :: tail
          end
        else
          new :: right
    | [] => new :: right

  fun insMiddle (new, hd, tl, left, right) =
    let
      val middle = BinSearch.equalOrMore (new, hd)
      val leftSlice = VectorSlice.slice (hd, 0, SOME middle)
      val rightLength = Vector.length hd - middle
      val rightSlice = VectorSlice.slice (hd, middle, SOME rightLength)

      val new = Vector.fromList [new]
      val new = VectorSlice.full new
    in
      if Vector.length hd < targetLength then
        let val newHd = VectorSlice.concat [leftSlice, new, rightSlice]
        in {left = joinEndOfLeft (newHd, tl), right = right}
        end
      else if middle < targetLength then
        (* leftSlice is less than targetLength *)
        let
          val hd1 = VectorSlice.concat [leftSlice, new]
          val hd2 = VectorSlice.vector rightSlice
        in
          { left = joinEndOfLeft (hd1, tl)
          , right = joinStartOfRight (hd2, right)
          }
        end
      else
        let
          val hd1 = VectorSlice.vector leftSlice
          val hd2 = VectorSlice.concat [new, rightSlice]
        in
          { left = joinEndOfLeft (hd1, tl)
          , right = joinStartOfRight (hd2, right)
          }
        end
    end

  fun insLeft (new, left, right) =
    case left of
      hd :: tl =>
        let
          val first = Vector.sub (hd, 0)
        in
          if new < first then
            insLeft (new, tl, joinStartOfRight (hd, right))
          else if new > first then
            let
              val last = Vector.sub (hd, Vector.length hd - 1)
            in
              if new < last then
                (* have to insert in middle *)
                insMiddle (new, hd, tl, left, right)
              else if new > last then
                (* have to insert new at end of left 
                 * or start of right (both are equivalent) *)
                { left = left
                , right = joinStartOfRight (Vector.fromList [new], right)
                }
              else
                (* new = last so just return *)
                {left = left, right = right}
            end
          else
            (* new = first *)
            {left = left, right = right}
        end
    | [] => {left = left, right = right}

  fun insRight (new, left, right) =
    case right of
      hd :: tl =>
        let
          val last = Vector.sub (hd, Vector.length hd - 1)
        in
          if new > last then
            insRight (new, joinEndOfLeft (hd, left), tl)
          else if new < last then
            let
              val first = Vector.sub (hd, 0)
            in
              if new > first then
                (* have to insert in middle *)
                insMiddle (new, hd, tl, left, right)
              else if new < first then
                (* have to insert new at start of right
                 * or end of left (both are equivalent) *)
                { left = left
                , right = joinStartOfRight (Vector.fromList [new], right)
                }
              else
                (* new = first so just return *)
                {left = left, right = right}
            end
          else
            (* new = last *)
            {left = left, right = right}
        end
    | [] => {left = left, right = right}

  fun insert (new, {left, right}: t) =
    (* look at elements to see which way to traverse *)
    case right of
      hd :: _ =>
        if Vector.sub (hd, 0) >= new then insRight (new, left, right)
        else insLeft (new, left, right)
    | [] => insLeft (new, left, right)

  fun helpGoToNumLeft (num, left, right) =
    case left of
      hd :: tl =>
        if num < Vector.sub (hd, 0) then
          (* continue *)
          helpGoToNumLeft (num, tl, joinStartOfRight (hd, right))
        else
          (* greater or equal to first element so return.
           * Note: caller which destructures list expects found hd to always be
           * on right. *)
          {left = tl, right = joinStartOfRight (hd, right)}
    | [] => {left = left, right = right}

  fun helpGoToNumRight (num, left, right) =
    case right of
      hd :: tl =>
        if num > Vector.sub (hd, Vector.length hd - 1) then
          (* continue *)
          helpGoToNumRight (num, joinEndOfLeft (hd, left), tl)
        else
          (* less than or equal to last element so return *)
          {left = left, right = right}
    | [] => {left = left, right = right}

  fun goToNum (num, {left, right}: t) =
    case right of
      hd :: tl =>
        if num >= Vector.sub (hd, 0) then
          (* num is greater or equal to first el on right so go right *)
          helpGoToNumRight (num, left, right)
        else
          (* num is less than first el on right so go left *)
          helpGoToNumLeft (num, left, right)
    | [] => helpGoToNumLeft (num, left, right)

  fun delRightFromHere (finish, left, right) =
    case right of
      hd :: tl =>
        let
          val last = Vector.sub (hd, Vector.length hd - 1)
        in
          if finish > last then
            delRightFromHere (finish, left, tl)
          else if finish < Vector.sub (hd, 0) then
            (* finish < first *)
            {left = left, right = right}
          else if finish < last then
            let
              val delpoint = BinSearch.equalOrMore (finish, hd)
              val newHd = VectorSlice.slice (hd, 0, SOME delpoint)
              val newHd = VectorSlice.vector newHd
            in
              {left = left, right = joinStartOfRight (newHd, right)}
            end
          else
            (* finish = last *)
            {left = left, right = tl}
        end
    | [] => {left = left, right = right}

  fun moveRightAndDelete (start, finish, left, right) =
    case right of
      hd :: tl =>
        let
          val last = Vector.sub (hd, Vector.length hd - 1)
        in
          if start > last then
            moveRightAndDelete (start, finish, joinEndOfLeft (hd, left), tl)
          else if start > Vector.sub (hd, 0) then
            (* start > first *)
            {left = left, right = right}
          else if start < last then
            if finish > last then
              (* delete part of hd, and continue deleting rightwards *)
              let
                val delpoint = BinSearch.equalOrMore (start, hd)
                val newHd = VectorSlice.slice (hd, 0, SOME delpoint)
                val newHd = VectorSlice.vector newHd
              in
                delRightFromHere (finish, joinEndOfLeft (newHd, left), tl)
              end
            else if finish < last then
              (* have to delete from middle of hd and then return *)
              let
                val startpoint = BinSearch.equalOrMore (start, hd)
                val finishpoint = BinSearch.equalOrMore (finish, hd)
                val lhd = VectorSlice.slice (hd, 0, SOME
                  (Vector.length hd - startpoint))
                val rhd = VectorSlice.slice (hd, finishpoint, SOME
                  (Vector.length hd - finishpoint))
                val lhd = VectorSlice.vector lhd
                val rhd = VectorSlice.vector rhd
              in
                { left = joinEndOfLeft (lhd, left)
                , right = joinStartOfRight (rhd, right)
                }
              end
            else
              (* finish = last, which means delete from last part of hd *)
              let
                val startpoint = BinSearch.equalOrMore (start, hd)
                val newHd = VectorSlice.slice (hd, 0, SOME startpoint)
                val newHd = VectorSlice.vector newHd
              in
                {left = left, right = joinStartOfRight (newHd, tl)}
              end
          else
            (* start = last, meaning delete last and then continue deleting right*)
            let
              val length = Vector.length hd - 1
              val newHd = VectorSlice.slice (hd, 0, SOME length)
              val newHd = VectorSlice.vector newHd
            in
              delRightFromHere (finish, joinEndOfLeft (newHd, left), tl)
            end
        end
    | [] => {left = left, right = right}

  fun delLeftFromHere (start, left, right) =
    case left of
      hd :: tl =>
        let
          val first = Vector.sub (hd, 0)
        in
          if start < first then
            delLeftFromHere (start, tl, right)
          else if start > Vector.sub (hd, Vector.length hd - 1) then
            (* start > last *)
            {left = left, right = right}
          else if start > first then
            let
              val delpoint = BinSearch.equalOrMore (start, hd)
              val newLength = Vector.length hd - delpoint
              val newHd = VectorSlice.slice (hd, delpoint, SOME newLength)
              val newHd = VectorSlice.vector newHd
            in
              {left = joinEndOfLeft (newHd, left), right = right}
            end
          else
            (* start = first *)
            {left = tl, right = right}
        end
    | [] => {left = left, right = right}

  fun moveLeftAndDelete (start, finish, left, right) =
    case left of
      hd :: tl =>
        let
          val first = Vector.sub (hd, 0)
        in
          if finish < first then
            moveLeftAndDelete (start, finish, tl, joinStartOfRight (hd, right))
          else if finish > Vector.sub (hd, Vector.length hd - 1) then
            (* finish > last *)
            {left = left, right = right}
          else if finish > first then
            if start < first then
              (* delete from start of hd and continue deleting leftwards *)
              let
                val startpoint = BinSearch.equalOrMore (finish, hd)
                val len = Vector.length hd - startpoint
                val newHd = VectorSlice.slice (hd, startpoint, SOME len)
                val newHd = VectorSlice.vector newHd
              in
                delLeftFromHere (start, tl, joinStartOfRight (newHd, right))
              end
            else if start > first then
              (* delete from middle and then return *)
              let
                val llen = BinSearch.equalOrMore (start, hd)
                val rstart = BinSearch.equalOrMore (finish, hd)
                val rlen = Vector.length hd - rstart
                val lhd = VectorSlice.slice (hd, 0, SOME llen)
                val rhd = VectorSlice.slice (hd, rstart, SOME rlen)
                val lhd = VectorSlice.vector lhd
                val rhd = VectorSlice.vector rhd
              in
                { left = joinEndOfLeft (lhd, tl)
                , right = joinStartOfRight (rhd, right)
                }
              end
            else
              (* start = first and finish > first
               * have to delete from start of hd and return*)
              let
                val startpoint = BinSearch.equalOrMore (finish, hd)
                val len = Vector.length hd - startpoint
                val newHd = VectorSlice.slice (hd, startpoint, SOME len)
                val newHd = VectorSlice.vector newHd
              in
                {left = joinEndOfLeft (newHd, tl), right = right}
              end
          else
            (* finish = first *)
            let
              val len = Vector.length hd - 1
              val newHd = VectorSlice.slice (hd, 1, SOME len)
              val newHd = VectorSlice.vector newHd
            in
              if start < first then
                delLeftFromHere (start, tl, joinStartOfRight (newHd, right))
              else
                (* start = first *)
                {left = tl, right = joinStartOfRight (newHd, right)}
            end
        end
    | [] => {left = left, right = right}

  fun delFromLeftAndRight (start, finish, left, right) =
    let val {left, right} = delRightFromHere (finish, left, right)
    in delLeftFromHere (start, left, right)
    end

  fun del (start, finish, left, right) =
    case right of
      rhd :: rtl =>
        let
          val rfirst = Vector.sub (rhd, 0)
        in
          if start < rfirst then
            (case left of
               lhd :: ltl =>
                 let
                   val llast = Vector.sub (lhd, Vector.length lhd - 1)
                 in
                   if
                     start < llast
                   then
                     if finish < rfirst then
                       (* start < rfirst and start < llast and finish < rfirst
                        * move left and delete *)
                       moveLeftAndDelete (start, finish, left, right)
                     else
                       (* start < rfirst and start < llast and finish >= rfirst
                        * in middle; delete from both sides *)
                       delFromLeftAndRight (start, finish, left, right)
                   else if
                     start = llast
                   then
                     if finish < rfirst then
                       (* start < rfirst, start = llast, and finish < rfirst
                        * so just have to delete left from here *)
                       delLeftFromHere (start, left, right)
                     else
                       (* start < rfirst, start = llast, finish >= rfirst
                        * in middle; delete from both sides *)
                       delFromLeftAndRight (start, finish, left, right)
                   else (* start > llast and start < rfirst *) if
                     finish >= rfirst
                   then
                     (* delete right from here *)
                     delRightFromHere (finish, left, right)
                   else
                     (* start < rfirst and finish < rfirst
                      * so just return *)
                     {left = left, right = right}
                 end
             | [] =>
                 (* start < rfirst 
                  * but left is empty.
                  * All we can do is 1. Delete right or 2. Return *)
                 if finish < rfirst then {left = left, right = right}
                 else delRightFromHere (finish, left, right))
          else if start = rfirst then
            delRightFromHere (finish, left, right)
          else
            (* start > rfirst 
             * move right and then start deleting *)
            moveRightAndDelete (start, finish, left, right)
        end
    | [] =>
        (case left of
           lhd :: ltl =>
             let
               val llast = Vector.sub (lhd, Vector.length lhd - 1)
             in
               if start < llast then
                 if finish <= llast then
                   moveRightAndDelete (start, finish, left, right)
                 else
                   delLeftFromHere (finish, left, right)
               else
                 moveRightAndDelete (start, finish, left, right)
             end
         | [] =>
             (* left and right are both empty *)
             {left = left, right = right})

  fun delete (start, length, {left, right}: t) =
    if length > 0 then del (start, start + length, left, right)
    else {left = left, right = right}

  (* go all the way to the end of the list, mapping each hd,
   * joining the hd to the left,
   * and return when we have reached the end *)
  fun mapRight (mapBy, left, right) =
    case right of
      hd :: tl =>
        let val newHd = Vector.map (fn el => el + mapBy) hd
        in mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
        end
    | [] => {left = left, right = right}

  fun moveRightAndMap (num, mapBy, left, right) =
    case right of
      hd :: tl =>
        let
          val lastIdx = Vector.length hd - 1
          val last = Vector.sub (hd, lastIdx)
        in
          if num > last then
            moveRightAndMap (num, mapBy, joinEndOfLeft (hd, left), tl)
          else if num < last then
            (* need to map in middle *)
            let
              val startIdx = BinSearch.equalOrMore (num, hd)
              val mapEl = Vector.sub (hd, startIdx)
              val newHd =
                Vector.map (fn el => if el < mapEl then el else mapEl + mapBy)
                  hd
            in
              mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
            end
          else
            (* num = last *)
            let
              val newHd =
                Vector.map (fn el => if el < num then num else num + mapBy) hd
            in
              mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
            end
        end
    | [] => {left = left, right = right}

  fun mapFromNum (num, mapBy, lst) =
    let
      (* goToNum always places vector where num was found to the right list *)
      val {left, right} = goToNum (num, lst)
    in
      moveRightAndMap (num, mapBy, left, right)
    end
end
