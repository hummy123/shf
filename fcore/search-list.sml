signature SEARCH_LIST =
sig
  type t = {left: int vector list, right: int vector list}
  val empty: t

  val insert: int * t -> t
  val append: int * t -> t
  val delete: int * int * string * t -> t

  val goToNum: int * t -> t
  val mapFrom: int * int * t -> t
end

structure SearchList :> SEARCH_LIST =
struct
  type t = {left: int vector list, right: int vector list}

  (* temp function for testing *)
  fun printlst {left, right} =
    let
      val left = List.rev left
      val lst = List.concat [left, right]
      val v = Vector.concat lst
      val _ = print "\nstart print list:\n"
      val _ = Vector.map (fn el => print (" " ^ Int.toString el ^ ",")) v
      val _ = print "\nend print list\n"
    in
      ()
    end

  (* clojure's persistent vectors contain arrays of length 32
   * and this data structure is similar to that, so we also use 32 *)
  val targetLength = 32

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
    | [] =>
        let val new = Vector.fromList [new]
        in {left = left, right = joinStartOfRight (new, right)}
        end

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
    | [] =>
        let val new = Vector.fromList [new]
        in {left = joinEndOfLeft (new, left), right = right}
        end

  fun insert (new, {left, right}: t) =
    (* look at elements to see which way to traverse *)
    case right of
      hd :: _ =>
        if new > Vector.sub (hd, 0) then insRight (new, left, right)
        else insLeft (new, left, right)
    | [] => insLeft (new, left, right)

  fun helpAppend (new, left, right) =
    case right of
      hd :: tl => helpAppend (new, joinEndOfLeft (hd, left), tl)
    | [] => {left = joinEndOfLeft (Vector.fromList [new], left), right = right}

  fun append (new, {left, right}: t) = helpAppend (new, left, right)

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
              val delstart = BinSearch.equalOrMore (finish, hd)
              val dellength = Vector.length hd - delstart
              val newHd = VectorSlice.slice (hd, delstart, SOME dellength)
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
            if finish < rfirst then
              (case left of
                 lhd :: ltl =>
                   let
                     val llast = Vector.sub (lhd, Vector.length lhd - 1)
                   in
                     if finish = llast then delLeftFromHere (start, left, right)
                     else moveLeftAndDelete (start, finish, left, right)
                   end
               | [] => {left = left, right = right})
            else
              (* finish >= rfirst *)
              delFromLeftAndRight (start, finish, left, right)
          else if start = rfirst then
            delRightFromHere (finish, left, right)
          else
            (* finish > rfirst *)
            moveRightAndDelete (start, finish, left, right)
        end
    | [] =>
        (case left of
           lhd :: ltl =>
             let
               val llast = Vector.sub (lhd, Vector.length lhd - 1)
             in
               if finish >= llast then
                 delLeftFromHere (start, left, right)
               else
                 (* finish < last *)
                 moveLeftAndDelete (start, finish, left, right)
             end
         | [] =>
             (* left and right are both empty *)
             {left = left, right = right})

  fun delete (start, length, searchString, {left, right}: t) =
    if length > 0 then
      let
        val finish = start + length
        val start = start - String.size searchString + 1
      in
        del (start, finish, left, right)
      end
    else
      {left = left, right = right}

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

  fun moveRightAndMap (from, mapBy, left, right) =
    case right of
      hd :: tl =>
        let
          val lastIdx = Vector.length hd - 1
          val last = Vector.sub (hd, lastIdx)
        in
          if from > last then
            moveRightAndMap (from, mapBy, joinEndOfLeft (hd, left), tl)
          else if from < last then
            (* need to map in middle *)
            let
              val startIdx = BinSearch.equalOrMore (from, hd)
              val mapEl = Vector.sub (hd, startIdx)
              val newHd =
                Vector.map (fn el => if el < from then el else el + mapBy) hd
            in
              mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
            end
          else
            (* from = last *)
            let
              val newHd =
                Vector.map (fn el => if el < from then el else el + mapBy) hd
            in
              mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
            end
        end
    | [] => {left = left, right = right}

  fun mapFrom (num, mapBy, lst) =
    let
      (* goToNum always places vector where num was found to the right list *)
      val {left, right} = goToNum (num, lst)
    in
      moveRightAndMap (num, mapBy, left, right)
    end
end
