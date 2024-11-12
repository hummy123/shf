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
          helpGoToNumLeft 
            ( num, tl
            , joinStartOfRight (hd, right)
            )
        else
          (* greater or equal to first element so return.
           * Note: caller which destructures list expects found hd to always be
           * on right. *)
          { left = tl
          , right = joinStartOfRight (hd, right)
          }
    | [] => {left = left, right = right}

  fun helpGoToNumRight (num, left, right) =
    case right of
      hd :: tl =>
        if num > Vector.sub (hd, Vector.length hd - 1) then
          (* continue *)
          helpGoToNumRight 
            ( num
            , joinEndOfLeft (hd, left)
            , tl
            )
        else
          (* less than or equal to last element so return *)
          {left = left, right = right}
    | [] => {left = left, right = right}

  fun goToNum (num, {left, right}: t) =
    case right of
      hd :: tl =>
        if num > Vector.sub (hd, Vector.length hd - 1) then
          helpGoToNumRight (num, left, right)
        else
          helpGoToNumLeft (num, left, right)
    | [] => helpGoToNumLeft (num, left, right)
end
