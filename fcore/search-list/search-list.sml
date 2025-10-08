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

  structure DfaGen = CaseInsensitiveDfa

  fun rangeLoop
    ( dfa
    , bufferPos
    , buffer
    , finishIdx
    , searchList
    , curState
    , startPos
    , prevFinalPos
    ) =
    if bufferPos = #textLength buffer orelse bufferPos > finishIdx then
      let
        val searchList =
          if prevFinalPos = ~1 then searchList
          else PersistentVector.append (startPos, prevFinalPos, searchList)
      in
        (buffer, searchList)
      end
    else
      let
        val buffer = LineGap.goToIdx (bufferPos, buffer)
        val chr = LineGap.sub (bufferPos, buffer)
        val newState = DfaGen.nextState (dfa, curState, chr)
        val prevFinalPos =
          if DfaGen.isFinal (dfa, newState) then bufferPos else prevFinalPos
      in
        if DfaGen.isDead newState then
          if prevFinalPos = ~1 then
            (* no match found: restart search from `startPos + 1` *)
            rangeLoop
              ( dfa
              , startPos + 1
              , buffer
              , finishIdx
              , searchList
              , 0
              , startPos + 1
              , ~1
              )
          else
            (* match found: append and continue *)
            let
              val searchList =
                PersistentVector.append (startPos, prevFinalPos, searchList)

              (* we start 1 idx after the final position we found *)
              val newStart = prevFinalPos + 1
            in
              rangeLoop
                (dfa, newStart, buffer, finishIdx, searchList, 0, newStart, ~1)
            end
        else
          (* continue searching for match *)
          rangeLoop
            ( dfa
            , bufferPos + 1
            , buffer
            , finishIdx
            , searchList
            , newState
            , startPos
            , prevFinalPos
            )
      end

  fun buildRange (buffer, searchString, finishIdx, dfa) =
    if String.size searchString > 0 andalso Vector.length dfa > 0 then
      rangeLoop
        ( dfa
        , #idx buffer
        , buffer
        , finishIdx
        , PersistentVector.empty
        , 0
        , #idx buffer
        , ~1
        )
    else
      (buffer, PersistentVector.empty)

  fun nextMatch (cursorIdx, searchList, count) = raise Fail "todo: reimplement"

  fun prevMatch (cursorIdx, searchList, count) = raise Fail "todo: reimplement"
end
