structure SearchList =
struct
  type t = int vector

  val empty = Vector.fromList []

  fun backtrackFull (pos, hd, absIdx, tl, acc, searchPos, searchString, prevTl) =
    if searchPos <= 1 then
      (* we are trying to backtrack to index 1, 
       * and then continue are search from here *)
      loopSearch (pos, hd, absIdx, tl, acc, 0, searchString, prevTl)
    else if pos < 0 then
      case prevTl of
        prevHd :: prevTl =>
          let
            val tl = hd :: tl
          in
            backtrackFull
              ( String.size prevHd - 1
              , prevHd
              , absIdx
              , tl
              , acc
              , searchPos
              , searchString
              , prevTl
              )
          end
      | [] =>
          (* Should never be called *)
          raise Fail "SearchList.backtrackFull error: line 24\n"
    else
      backtrackFull
        (pos - 1, hd, absIdx - 1, tl, acc, searchPos - 1, searchString, prevTl)

  and loopSearch (pos, hd, absIdx, tl, acc, searchPos, searchString, prevTl) =
    if pos = String.size hd then
      case tl of
        newHd :: newTl =>
          loopSearch
            ( 0
            , newHd
            , absIdx
            , newTl
            , acc
            , searchPos
            , searchString
            , hd :: prevTl
            )
      | [] => PersistentVector.toVector acc
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
              loopSearch
                (pos + 1, hd, absIdx + 1, tl, acc, 0, searchString, prevTl)
            end
          else
            loopSearch
              ( pos + 1
              , hd
              , absIdx + 1
              , tl
              , acc
              , searchPos + 1
              , searchString
              , prevTl
              )
        else
          (if searchPos = 0 then
             loopSearch
               (pos + 1, hd, absIdx + 1, tl, acc, 0, searchString, prevTl)
           else
             backtrackFull
               (pos, hd, absIdx, tl, acc, searchPos, searchString, prevTl))
      end

  fun search ({rightStrings, leftStrings, ...}: LineGap.t, searchString) =
    case rightStrings of
      hd :: tl =>
        loopSearch (0, hd, 0, tl, PersistentVector.empty, 0, searchString, [])
    | [] => empty

  (* Prerequisite: move buffer/LineGap to start *)
  fun build (buffer, searchString) =
    if String.size searchString > 0 then search (buffer, searchString)
    else empty

  fun backtrackRange
    (pos, hd, absIdx, tl, acc, searchPos, searchString, finish, prevTl) =
    if searchPos <= 1 then
      loopRange (pos, hd, absIdx, tl, acc, 0, searchString, finish, prevTl)
    else if pos < 0 then
      case prevTl of
        prevHd :: prevTl =>
          let
            val tl = hd :: tl
          in
            backtrackRange
              ( String.size prevHd - 1
              , prevHd
              , absIdx
              , tl
              , acc
              , searchPos
              , searchString
              , finish
              , prevTl
              )
          end
      | [] =>
          (* Should never be called *)
          raise Fail "SearchList.backtrackRange error: line 120\n"
    else
      backtrackRange
        ( pos - 1
        , hd
        , absIdx - 1
        , tl
        , acc
        , searchPos - 1
        , searchString
        , finish
        , prevTl
        )

  and loopRange
    (pos, hd, absIdx, tl, acc, searchPos, searchString, finish, prevTl) =
    if pos = String.size hd then
      case tl of
        newHd :: newTl =>
          let
            val prevTl = hd :: prevTl
          in
            loopRange
              ( 0
              , newHd
              , absIdx
              , newTl
              , acc
              , searchPos
              , searchString
              , finish
              , prevTl
              )
          end
      | [] => PersistentVector.toVector acc
    else if absIdx = finish then
      PersistentVector.toVector acc
    else
      let
        val bufferChr = String.sub (hd, pos)
        val searchChr = String.sub (searchString, searchPos)
      in
        if bufferChr = searchChr then
          if searchPos + 1 = String.size searchString then
            (* full match *)
            let
              val foundIdx = absIdx - String.size searchString + 1
              val acc = PersistentVector.append (foundIdx, acc)
            in
              loopRange
                ( pos + 1
                , hd
                , absIdx + 1
                , tl
                , acc
                , 0
                , searchString
                , finish
                , prevTl
                )
            end
          else
            loopRange
              ( pos + 1
              , hd
              , absIdx + 1
              , tl
              , acc
              , searchPos + 1
              , searchString
              , finish
              , prevTl
              )
        else
          ((if searchPos = 0 then
              loopRange
                ( pos + 1
                , hd
                , absIdx + 1
                , tl
                , acc
                , 0
                , searchString
                , finish
                , prevTl
                )
            else
              backtrackRange
                ( pos
                , hd
                , absIdx
                , tl
                , acc
                , searchPos
                , searchString
                , finish
                , prevTl
                )))
      end

  fun searchRange (buffer: LineGap.t, searchString, finish) =
    let
      val {rightStrings, idx = absIdx, ...} = buffer
    in
      case rightStrings of
        hd :: tl =>
          loopRange
            ( 0
            , hd
            , absIdx
            , tl
            , PersistentVector.empty
            , 0
            , searchString
            , finish
            , []
            )
      | [] => empty
    end

  fun buildRange (buffer, searchString, finish) =
    if String.size searchString > 0 then
      searchRange (buffer, searchString, finish)
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
