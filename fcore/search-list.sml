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
        if isLessThanTarget (new, hd) then (hd ^ new) :: tail else new :: left
    | [] => new :: left

  fun joinStartOfRight (new, right) =
    case right of
      hd :: tail =>
        if isLessThanTarget (new, hd) then (new ^ hd) :: tail else new :: right
    | [] => new :: right

  fun preferInsertLeft (new, left, right) =
    case left of
      hd :: tail =>
        if isLessThanTarget (hd, new) then
          {left = (hd ^ new) :: tail, right = right}
        else
          {left = left, right = joinStartOfRight (new, right)}
    | [] => {left = left, right = joinStartOfRight (new, right)}

  fun insLeft (new, left, right) =
    case left of
      hd :: tl =>
        let
          val first = Vector.sub (hd, 0)
        in
          if first > new then
            insLeft (new, tl, joinStartOfRight (hd, right))
          else if first < new then
            let
              val last = Vector.sub (hd, Vector.length hd - 1)
            in
              if last > new then
                (* have to insert in middle *)
                let
                  val middle = BinSearch.equalOrMore (new, hd)
                  val leftSlice = VectorSlice.slice (hd, 0, SOME middle)
                  val rightLength = Vector.length hd - middle
                  val rightSlice =
                    VectorSlice.slice (hd, middle, SOME rightLength)

                  val new = Vector.fromList [new]
                  val new = VectorSlice.full new
                in
                  if Vector.length hd < targetLength then
                    let
                      val newHd =
                        VectorSlice.concat [leftSlice, new, rightSlice]
                    in
                      {left = joinEndOfLeft (newHd, tl), right = right}
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
              else if last < new then
                (* have to insert new at end *)
                if Vector.length hd < targetLength then
                  let val newHd = Vector.concat [hd, Vector.fromList [new]]
                  in {left = joinEndOfLeft (newHd, tl), right = right}
                  end
                else
                  { left = left
                  , right = joinStartOfRight (Vector.fromList [hd], right)
                  }
              else
                (* last = new so just return *)
                {left = left, right = right}
            end
          else
            (* first = new *)
            {left = left, right = right}
        end
    | [] => {left = left, right = right}
end
