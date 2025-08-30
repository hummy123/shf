structure SearchList =
struct
  type t = int vector

  val empty = Vector.fromList []

  fun loopSearch (pos, hd, absIdx, tl, acc, searchPos, searchString) =
    if pos = String.size hd then
      case tl of
        hd :: tl => loopSearch (0, hd, absIdx, tl, acc, searchPos, searchString)
      | [] => acc
    else
      let
        val bufferChr = String.sub (hd, pos)
        val searchChr = String.sub (searchString, searchPos)
      in
        if bufferChr = searchChr then
          if searchPos + 1 = String.size searchString then
            (* we fully matched the search string *)
            let
              val foundIdx = absIdx - String.size searchString + 1
              val acc = PersistentVector.append (foundIdx, acc)
            in
              loopSearch (pos + 1, hd, absIdx + 1, tl, acc, 0, searchString)
            end
          else
            loopSearch
              (pos + 1, hd, absIdx + 1, tl, acc, searchPos + 1, searchString)
        else if searchPos = 0 then
          loopSearch (pos + 1, hd, absIdx + 1, tl, acc, 0, searchString)
        else
          loopSearch (pos, hd, absIdx, tl, acc, 0, searchString)
      end

  fun search ({rightStrings, leftStrings, ...}: LineGap.t, searchString) =
    case rightStrings of
      hd :: tl =>
        let
          val result = loopSearch
            (0, hd, 0, tl, PersistentVector.empty, 0, searchString)
        in
          PersistentVector.toVector result
        end
    | [] => empty

  (* Prerequisite: move buffer/LineGap to end *)
  fun build (buffer, searchString) =
    if String.size searchString > 0 then search (buffer, searchString)
    else empty

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

  fun rangeSearchStep (pos, hd, absIdx, tl, acc, searchPos, searchString, low) =
    if searchPos < 0 then
      raise Fail "todo"
    else if absIdx < low then
      acc
    else if pos < 0 then
      case tl of
        hd :: tl =>
          rangeSearchStep
            ( String.size hd - 1
            , hd
            , absIdx
            , tl
            , acc
            , searchPos
            , searchString
            , low
            )
      | [] => acc
    else
      let
        val bufferChr = String.sub (hd, pos)
        val searchChr = String.sub (searchString, searchPos)
      in
        if bufferChr = searchChr then
          rangeSearchStep
            (pos - 1, hd, absIdx - 1, tl, acc, searchPos - 1, searchString, low)
        else
          acc
      end

  fun loopRange (pos, hd, absIdx, tl, acc, searchString, low) =
    if absIdx < low then
      Vector.fromList acc
    else if pos < 0 then
      case tl of
        hd :: tl =>
          loopRange (String.size hd - 1, hd, absIdx, tl, acc, searchString, low)
      | [] => Vector.fromList acc
    else
      let
        val acc = rangeSearchStep
          ( pos
          , hd
          , absIdx
          , tl
          , acc
          , String.size searchString - 1
          , searchString
          , low
          )
      in
        loopRange (pos - 1, hd, absIdx - 1, tl, acc, searchString, low)
      end

  fun searchRange (buffer: LineGap.t, searchString, low) =
    let
      val low = Int.max (low, 0)
      val {rightStrings, leftStrings, idx = absIdx, ...} = buffer
    in
      case rightStrings of
        hd :: _ =>
          let
            val pos = String.size hd - 1
            val absIdx = absIdx + String.size hd - 1
          in
            loopRange (pos, hd, absIdx, leftStrings, [], searchString, low)
          end
      | [] =>
          (case leftStrings of
             hd :: tl =>
               let
                 val pos = String.size hd - 1
                 val absIdx = absIdx - 1
               in
                 loopRange (pos, hd, absIdx, tl, [], searchString, low)
               end
           | [] => empty)
    end

  fun buildRange (buffer, searchString, low) =
    if String.size searchString > 0 then searchRange (buffer, searchString, low)
    else empty
end
