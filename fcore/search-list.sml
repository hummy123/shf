structure SearchList =
struct
  val empty = PersistentVector.empty

  fun backtrackFull (pos, hd, absIdx, tl, acc, searchPos, searchString, prevTl) =
    if pos < 0 then
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
    else if searchPos <= 1 then
      (* we are trying to backtrack to index 1, 
       * and then continue are search from here *)
      loopSearch (pos, hd, absIdx, tl, acc, 0, searchString, prevTl)
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
              val acc = PersistentVector.append (foundIdx, absIdx, acc)
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
    | [] => PersistentVector.empty

  (* Prerequisite: move buffer/LineGap to start *)
  fun build (buffer, searchString) =
    if String.size searchString > 0 then search (buffer, searchString)
    else PersistentVector.empty

  fun backtrackRange
    (pos, hd, absIdx, tl, acc, searchPos, searchString, finish, prevTl) =
    if pos < 0 then
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
    else if searchPos <= 1 then
      loopRange (pos, hd, absIdx, tl, acc, 0, searchString, finish, prevTl)
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
      | [] => acc
    else if absIdx = finish then
      acc
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
              val acc = PersistentVector.append (foundIdx, absIdx, acc)
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
      | [] => PersistentVector.empty
    end

  fun buildRange (buffer, searchString, finishIdx) =
    if String.size searchString > 0 then
      case Nfa.parse searchString of
        SOME nfa =>
          Nfa.getMatchesInRange
            (#idx buffer, finishIdx, buffer : LineGap.t, nfa)
      | NONE => PersistentVector.empty
    else
      PersistentVector.empty

  fun nextMatch (cursorIdx, searchList, count) = raise Fail "todo: reimplement"

  fun prevMatch (cursorIdx, searchList, count) = raise Fail "todo: reimplement"
end
