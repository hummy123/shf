structure SearchList =
struct
  structure Dfa = CaseInsensitiveDfa

  fun buildLoop (idx, buffer, dfa, acc, curState, startPos, prevFinalPos) =
    let
      val buffer = LineGap.goToIdx (idx, buffer)
    in
      if idx = #textLength buffer then
        let
          val acc =
            if prevFinalPos < 0 then acc
            else PersistentVector.append (startPos, prevFinalPos, acc)
        in
          (buffer, acc)
        end
      else
        let
          val chr = LineGap.sub (idx, buffer)
          val newState = Dfa.nextState (dfa, curState, chr)
          val prevFinalPos =
            if Dfa.isFinal (dfa, newState) then idx else prevFinalPos
        in
          if Dfa.isDead newState then
            if prevFinalPos = ~1 then
              (* no match found: restart search from `startPos + 1` *)
              buildLoop (startPos + 1, buffer, dfa, acc, 0, startPos + 1, ~1)
            else
              (* match found: append and continue *)
              let
                val acc = PersistentVector.append (startPos, prevFinalPos, acc)

                (* we start 1 idx after the final position we found *)
                val newStart = prevFinalPos + 1
              in
                buildLoop (newStart, buffer, dfa, acc, 0, newStart, ~1)
              end
          else
            buildLoop
              (idx + 1, buffer, dfa, acc, newState, startPos, prevFinalPos)
        end
    end

  fun build (buffer, dfa) =
    if Vector.length dfa > 0 then
      let val buffer = LineGap.goToStart buffer
      in buildLoop (0, buffer, dfa, PersistentVector.empty, 0, 0, ~1)
      end
    else
      (buffer, PersistentVector.empty)

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
        val newState = Dfa.nextState (dfa, curState, chr)
        val prevFinalPos =
          if Dfa.isFinal (dfa, newState) then bufferPos else prevFinalPos
      in
        if Dfa.isDead newState then
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
end
