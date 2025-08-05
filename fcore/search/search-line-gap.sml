structure SearchLineGap =
struct
  fun searchStep (pos, hd, absIdx, tl, acc, searchPos, searchString) =
    if searchPos < 0 then
      (absIdx + 1) :: acc
    else if pos < 0 then
      case tl of
        hd :: tl =>
          searchStep (String.size hd - 1, hd, absIdx, tl, acc, searchPos, searchString)
      | [] => acc
    else
      let
        val bufferChr = String.sub (hd, pos)
        val searchChr = String.sub (searchString, searchPos)
      in
        if bufferChr = searchChr then
          searchStep (pos - 1, hd, absIdx - 1, tl, acc, searchPos - 1, searchString)
        else
          acc
      end

  fun loopSearch (pos, hd, absIdx, tl, acc, searchString) =
    if pos < 0 then
      case tl of
        hd :: tl =>
          loopSearch (String.size hd - 1, hd, absIdx, tl, acc, searchString)
      | [] => acc
    else
      let
        val acc = searchStep 
          (pos, hd, absIdx, tl, acc, String.size searchString - 1, searchString)
      in
        loopSearch (pos - 1, hd, absIdx - 1, tl, acc, searchString)
      end

  fun search (buffer, searchString) =
    if String.size searchString = 0 then
      []
    else
      let
        val buffer = LineGap.goToEnd buffer
        val {leftStrings, idx = absIdx, ...} = buffer
      in
        case leftStrings of
          hd :: tl =>
            loopSearch (String.size hd - 1, hd, absIdx - 1, tl, [], searchString)
        | [] => []
      end
end
