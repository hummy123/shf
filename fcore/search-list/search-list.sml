structure SearchList =
struct
  structure DfaGen = CaseInsensitiveDfa

  fun buildLoop (idx, iterator, dfa, acc, curState, startPos, prevFinalPos) =
    let
      val iterator = LineGap.moveIteratorToIdx (idx, iterator)
    in
      if idx = #textLength iterator then
        if prevFinalPos < 0 then acc
        else PersistentVector.append (startPos, prevFinalPos, acc)
      else
        let
          val chr = LineGap.subIterator (idx, iterator)
          val newState = DfaGen.nextState (dfa, curState, chr)
          val prevFinalPos =
            if DfaGen.isFinal (dfa, newState) then idx else prevFinalPos
        in
          if DfaGen.isDead newState then
            if prevFinalPos = ~1 then
              (* no match found: restart search from `startPos + 1` *)
              buildLoop (startPos + 1, iterator, dfa, acc, 0, startPos + 1, ~1)
            else
              (* match found: append and continue *)
              let
                val acc = PersistentVector.append (startPos, prevFinalPos, acc)

                (* we start 1 idx after the final position we found *)
                val newStart = prevFinalPos + 1
              in
                buildLoop (newStart, iterator, dfa, acc, 0, newStart, ~1)
              end
          else
            buildLoop
              (idx + 1, iterator, dfa, acc, newState, startPos, prevFinalPos)
        end
    end

  fun build (iterator, dfa) =
    if Vector.length dfa > 0 then
      buildLoop (0, iterator, dfa, PersistentVector.empty, 0, 0, ~1)
    else
      PersistentVector.empty

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

  fun buildRange (buffer, finishIdx, dfa) =
    if Vector.length dfa > 0 then
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

  fun prevMatch (cursorIdx, searchList, count) = raise Fail "todo: reimplement"
end
