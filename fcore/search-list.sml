structure SearchList =
struct
  type t = int vector

  val empty = Vector.fromList []

  fun searchStep (pos, hd, absIdx, tl, acc, searchPos, searchString) =
    if searchPos < 0 then
      (absIdx + 1) :: acc
    else if pos < 0 then
      case tl of
        hd :: tl =>
          searchStep
            (String.size hd - 1, hd, absIdx, tl, acc, searchPos, searchString)
      | [] => acc
    else
      let
        val bufferChr = String.sub (hd, pos)
        val searchChr = String.sub (searchString, searchPos)
      in
        if bufferChr = searchChr then
          searchStep
            (pos - 1, hd, absIdx - 1, tl, acc, searchPos - 1, searchString)
        else
          acc
      end

  fun loopSearch (pos, hd, absIdx, tl, acc, searchString) =
    if pos < 0 then
      case tl of
        hd :: tl =>
          loopSearch (String.size hd - 1, hd, absIdx, tl, acc, searchString)
      | [] => Vector.fromList acc
    else
      let
        val acc = searchStep
          (pos, hd, absIdx, tl, acc, String.size searchString - 1, searchString)
      in
        loopSearch (pos - 1, hd, absIdx - 1, tl, acc, searchString)
      end

  (* Prerequisite: move buffer/LineGap to end *)
  fun search (buffer: LineGap.t, searchString) =
    if String.size searchString = 0 then
      empty
    else
      let
        val {leftStrings, idx = absIdx, ...} = buffer
      in
        case leftStrings of
          hd :: tl =>
            loopSearch
              (String.size hd - 1, hd, absIdx - 1, tl, [], searchString)
        | [] => empty
      end

  fun build (buffer, searchString) =
    if String.size searchString > 0 then
      let
        val searchList = search (buffer, searchString)
      in
        searchList
      end
    else
      empty

  fun loopNextMatch (pos, searchList, count) =
    if count = 0 then
      Vector.sub (searchList, pos)
    else
      let
        val pos = pos + 1
        val pos = if pos < Vector.length searchList then pos else 0
        val count = count - 1
      in
        loopNextMatch (pos, searchList, count)
      end

  fun nextMatch (cursorIdx, searchList, count) =
    if Vector.length searchList = 0 then
      ~1
    else
      let
        val pos = BinSearch.equalOrMore (cursorIdx + 1, searchList)
        val pos = if pos < Vector.length searchList then pos else 0
        val count = count - 1
      in
        loopNextMatch (pos, searchList, count)
      end

  fun loopPrevMatch (pos, searchList, count) =
    if count = 0 then
      Vector.sub (searchList, pos)
    else
      let
        val pos = pos - 1
        val pos = if pos < 0 then Vector.length searchList - 1 else pos
        val count = count - 1
      in
        loopPrevMatch (pos, searchList, count)
      end

  fun prevMatch (cursorIdx, searchList, count) =
    if Vector.length searchList = 0 then
      ~1
    else
      let
        val pos = BinSearch.equalOrLess (cursorIdx - 1, searchList)
        val pos = if pos < 0 then Vector.length searchList - 1 else pos
        val count = count - 1
      in
        loopPrevMatch (pos, searchList, count)
      end
end
