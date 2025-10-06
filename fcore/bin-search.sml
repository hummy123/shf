structure BinSearch =
struct
  local
    fun reverseLinearSearch (findNum, idx, vec) =
      if idx < 0 then
        ~1
      else
        let
          val curVal = Vector.sub (vec, idx)
        in
          if curVal < findNum then idx
          else reverseLinearSearch (findNum, idx - 1, vec)
        end

    fun helpBinSearch (findNum, vec, low, high) =
      let
        val mid = low + ((high - low) div 2)
      in
        if high >= low then
          let
            val midVal = Vector.sub (vec, mid)
          in
            if midVal = findNum then
              mid
            else if midVal < findNum then
              helpBinSearch (findNum, vec, mid + 1, high)
            else
              helpBinSearch (findNum, vec, low, mid - 1)
          end
        else
          reverseLinearSearch (findNum, mid, vec)
      end
  in
    fun equalOrLess (findNum, vec) =
      helpBinSearch (findNum, vec, 0, Vector.length vec - 1)
  end

  local
    fun forwardLinearSearch (findNum, idx, vec) =
      if idx = Vector.length vec then
        ~1
      else
        let
          val curVal = Vector.sub (vec, idx)
        in
          if curVal > findNum then idx
          else forwardLinearSearch (findNum, idx + 1, vec)
        end

    fun helpBinSearch (findNum, vec, low, high) =
      let
        val mid = low + ((high - low) div 2)
      in
        if high >= low then
          let
            val midVal = Vector.sub (vec, mid)
          in
            if midVal = findNum then
              mid
            else if midVal < findNum then
              helpBinSearch (findNum, vec, mid + 1, high)
            else
              helpBinSearch (findNum, vec, low, mid - 1)
          end
        else
          forwardLinearSearch (findNum, Int.max (mid, 0), vec)
      end
  in
    fun equalOrMore (findNum, vec) =
      helpBinSearch (findNum, vec, 0, Vector.length vec - 1)
  end

  local
    fun helpExists (findNum, vec, low, high) =
      let
        val mid = low + ((high - low) div 2)
      in
        if high >= low then
          let
            val midVal = Vector.sub (vec, mid)
          in
            if midVal = findNum then
              true
            else if midVal < findNum then
              helpExists (findNum, vec, mid + 1, high)
            else
              helpExists (findNum, vec, low, mid - 1)
          end
        else
          false
      end
  in
    fun exists (findNum, vec) =
      helpExists (findNum, vec, 0, Vector.length vec - 1)
  end

  local
    fun helpEqualOrMinus1 (findNum, vec, low, high) =
      let
        val mid = low + ((high - low) div 2)
      in
        if high >= low then
          let
            val midVal = Vector.sub (vec, mid)
          in
            if midVal = findNum then
              mid
            else if midVal < findNum then
              helpEqualOrMinus1 (findNum, vec, mid + 1, high)
            else
              helpEqualOrMinus1 (findNum, vec, low, mid - 1)
          end
        else
          ~1
      end
  in
    fun equalOrMinus1 (findNum, vec) =
      helpEqualOrMinus1 (findNum, vec, 0, Vector.length vec - 1)
  end
end
