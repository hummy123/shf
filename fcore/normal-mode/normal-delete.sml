structure NormalDelete =
struct
  open AppType
  open MailboxType

  fun deleteAndFinish (app: app_type, low, length, buffer, time) =
    let
      val buffer = LineGap.delete (low, length, buffer)

      val searchString = #searchString app
      val buffer = LineGap.goToStart buffer
      val initialMsg = [SEARCH (buffer, searchString)]

      val buffer = LineGap.goToIdx (low - 1111, buffer)
      val searchList = SearchList.buildRange (buffer, searchString, low + 1111)

      val buffer = LineGap.goToIdx (low, buffer)
    in
      NormalFinish.buildTextAndClear
        (app, buffer, low, searchList, initialMsg, time)
    end

  (* equivalent of vi's 'x' command **)
  fun helpRemoveChr (app: app_type, buffer, cursorIdx, count, time) =
    if count = 0 then
      let
        val searchString = #searchString app
        val buffer = LineGap.goToStart buffer
        val initialMsg = [SEARCH (buffer, searchString)]

        val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
        val searchList =
          SearchList.buildRange (buffer, searchString, cursorIdx + 1111)
      in
        NormalFinish.buildTextAndClear
          (app, buffer, cursorIdx, searchList, initialMsg, time)
      end
    else
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)

        (* Explanation of how Vi's 'x' command behaves:
        * If the cursor is at the end of the file, 
        * then it is decremented by 1.
        * If the character after the cursor is a line break,
        * then it is also decremented by 1.
        * If the character before the cursor is a linee break, the cursor stays
        * where it is.
        * If the chracter AT the cursor is a line break and the characater
        * AFTER the cursor is also a line break, then nothing is deleted. 
        * Otherwise, the same cursor is returned.
        * All decrement cases do not decrement when the cursor is 0. *)
        val cursorIsStart = Cursor.isCursorAtStartOfLine (buffer, cursorIdx)
        val nextIsEnd = Cursor.isNextChrEndOfLine (buffer, cursorIdx)
      in
        if nextIsEnd andalso cursorIsStart then
          (* vi simply doesn't do anything on 'x' command
          * when cursor is at start of line, and next chr is line break 
          * so skip to end of loop by passing count of 0 *)
          helpRemoveChr (app, buffer, cursorIdx, 0, time)
        else if cursorIsStart then
          helpRemoveChr (app, buffer, cursorIdx, 0, time)
        else if nextIsEnd then
          let
            (* delete char at cursor and then decrement cursorIdx by 1
            * if cursorIdx is not 0 *)
            val searchString = #searchString app
            val buffer = LineGap.delete (cursorIdx, 1, buffer)

            val cursorIdx =
              if
                Cursor.isPrevChrStartOfLine (buffer, cursorIdx)
                orelse cursorIdx = 0
              then cursorIdx
              else cursorIdx - 1
          in
            helpRemoveChr (app, buffer, cursorIdx, count - 1, time)
          end
        else
          let
            val searchString = #searchString app
            val buffer = LineGap.delete (cursorIdx, 1, buffer)
          in
            helpRemoveChr (app, buffer, cursorIdx, count - 1, time)
          end
      end

  fun removeChr (app: app_type, count, time) =
    helpRemoveChr (app, #buffer app, #cursorIdx app, count, time)

  fun helpDelete
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, time) =
    (* As a small optimisation to reduce allocations, 
    * we accumulate otherIdx by calling fMove with it and the buffer
    * on each loop.
    * Then, at the end of the loop, we perform the actual deletion.
    * This is faster than performing the actual deletion on every loop
    * because we only delete once, and avoid allocating intermediary buffers.
    * The behaviour between the two is equivalent. *)
    if count = 0 then
      let
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val high = Cursor.clipIdx (buffer, high)
        val length = high - low

        val buffer = LineGap.delete (low, length, buffer)

        val buffer = LineGap.goToStart buffer
        val searchString = #searchString app
        val initialMsg = [SEARCH (buffer, searchString)]

        val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
        val searchList =
          SearchList.buildRange (buffer, searchString, cursorIdx + 1111)

        (* If we have deleted from the buffer so that cursorIdx
        * is no longer a valid idx,
        * clip cursorIdx to the end. *)
        val buffer = LineGap.goToIdx (low, buffer)
        val cursorIdx = Cursor.clipIdx (buffer, low)
      in
        NormalFinish.buildTextAndClear
          (app, buffer, cursorIdx, searchList, initialMsg, time)
      end
    else
      let
        (* get otherIdx, where cursor will want to go after motion. *)
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
      in
        helpDelete (app, buffer, cursorIdx, newOtherIdx, newCount, fMove, time)
      end

  fun delete (app: app_type, count, fMove, time) =
    let val {buffer, cursorIdx, ...} = app
    in helpDelete (app, buffer, cursorIdx, cursorIdx, count, fMove, time)
    end

  fun deleteByDfa (app: app_type, count, fMove, time) =
    let
      val {buffer, cursorIdx, searchString, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = fMove (buffer, cursorIdx, count)

      val low = Int.min (cursorIdx, otherIdx)
      val high = Int.max (cursorIdx, otherIdx)
      val length = high - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun deleteToEndOfPrevWord (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, searchString, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.endOfPrevWord (buffer, cursorIdx, count)

      val length = (cursorIdx + 1) - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun deleteToEndOfPrevWORD (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, searchString, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.endOfPrevWORD (buffer, cursorIdx, count)

      val length = (cursorIdx + 1) - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun deleteToEndOfLine (app: app_type, time) =
    let
      val {buffer, cursorIdx, ...} = app
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        (* if we are on \n, we don't want to delete or do anything
        * so reset the mode *)
        NormalFinish.clearMode app
      else
        let
          (* viDlr takes us to the last chr in the line 
          * but does not delete that last chr
          * so we call helpRemoveChr to delete that last chr. 
          * We also rely on helpRemoveChr to handle backwards-movement logic:
          * If cursorIdx is at \n after deletion, then stop.
          * Else, move back one chr. *)
          val lastChr = Cursor.viDlr (buffer, cursorIdx, 1)
          val length = lastChr - cursorIdx
          val buffer = LineGap.delete (cursorIdx, length, buffer)
        in
          helpRemoveChr (app, buffer, cursorIdx, 1, time)
        end
    end

  fun deleteLine (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, searchString, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val startIdx = Cursor.vi0 (buffer, cursorIdx)
      val finishIdx = Cursor.viDlrForDelete (buffer, cursorIdx, count)

      val length = finishIdx - startIdx
    in
      deleteAndFinish (app, startIdx, length, buffer, time)
    end

  fun helpDeleteLineBack (app, buffer, low, high, count, time) =
    if count = 0 then
      let
        val low = Int.max (low, 0)
        val length = high - low
      in
        deleteAndFinish (app, low, length, buffer, time)
      end
    else
      let
        val buffer = LineGap.goToIdx (low, buffer)
        val low = Cursor.viH (buffer, low)
        val buffer = LineGap.goToIdx (low, buffer)
        val low = Cursor.vi0 (buffer, low)
        val newCount = if low = 0 then 0 else count - 1
      in
        helpDeleteLineBack (app, buffer, low, high, newCount, time)
      end

  fun deleteLineBack (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, ...} = app
      val low = Cursor.vi0 (buffer, cursorIdx)
      val high = Cursor.viDlr (buffer, cursorIdx, 1) + 1
    in
      helpDeleteLineBack (app, buffer, low, high, count, time)
    end

  fun deleteToFirstNonSpaceChr (app: app_type, time) =
    let
      val
        { buffer
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , searchString
        , ...
        } = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = Cursor.vi0 (buffer, cursorIdx)

      (* move cursorIdx to first character on line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = Cursor.firstNonSpaceChr (buffer, otherIdx)

      val low = Int.min (cursorIdx, otherIdx)
      val high = Int.max (cursorIdx, otherIdx)
      val length = high - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun helpDeleteToChr
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, fInc, chr, time) =
    if count = 0 then
      let
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val length = high - low
      in
        deleteAndFinish (app, low, length, buffer, time)
      end
    else
      let
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx, chr)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
        val newOtherIdx = fInc (newOtherIdx, 1)
      in
        helpDeleteToChr
          ( app
          , buffer
          , cursorIdx
          , newOtherIdx
          , newCount
          , fMove
          , fInc
          , chr
          , time
          )
      end

  fun deleteToChr (app: app_type, count, fMove, fInc, chr, time) =
    helpDeleteToChr
      ( app
      , #buffer app
      , #cursorIdx app
      , #cursorIdx app
      , count
      , fMove
      , fInc
      , chr
      , time
      )

  fun deleteToStart (app: app_type, time) =
    let
      val {cursorIdx, buffer, windowWidth, windowHeight, searchString, ...} =
        app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viDlrForDelete (buffer, cursorIdx, 1)

      val buffer = LineGap.delete (0, cursorIdx, buffer)

      val buffer = LineGap.goToStart buffer
      val initialMsg = [SEARCH (buffer, #searchString app)]

      val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
      val searchList =
        SearchList.buildRange (buffer, searchString, cursorIdx + 1111)

      val cursorIdx = 0
      val startLine = 0
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , initialMsg
        )

      val mode = NORMAL_MODE ""
    in
      NormalModeWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg, time)
    end

  fun helpDeleteToMatch (app: app_type, low, high, time) =
    let
      val {buffer, searchString, ...} = app
      val length = high - low
      val buffer = LineGap.delete (low, length, buffer)

      val buffer = LineGap.goToStart buffer
      val initialMsg = [SEARCH (buffer, searchString)]

      val buffer = LineGap.goToIdx (low - 1111, buffer)
      val searchList = SearchList.buildRange (buffer, searchString, low + 1111)

      val buffer = LineGap.goToIdx (low, buffer)
    in
      NormalFinish.buildTextAndClear
        (app, buffer, low, searchList, initialMsg, time)
    end

  fun deleteToNextMatch (app: app_type, count, time) =
    let
      val {cursorIdx, searchList, ...} = app
      val newCursorIdx = SearchList.nextMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 orelse newCursorIdx <= cursorIdx then
        NormalFinish.clearMode app
      else
        helpDeleteToMatch (app, cursorIdx, newCursorIdx, time)
    end

  fun deleteToPrevMatch (app: app_type, count, time) =
    let
      val {cursorIdx, searchList, ...} = app
      val newCursorIdx = SearchList.prevMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 orelse newCursorIdx >= cursorIdx then
        NormalFinish.clearMode app
      else
        helpDeleteToMatch (app, newCursorIdx, cursorIdx, time)
    end

  (* check if we are trying to delete from an empty buffer
   * or a buffer which consists of only one character which is \n *)
  fun canDeleteInsideOrAround (buffer, low, length) =
    not (length = 1 andalso LineGap.substring (low, 1, buffer) = "\n")

  fun deleteInsideWord (app: app_type, time) =
    let
      val {buffer, cursorIdx, searchString, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val low = Cursor.prevWordStrict (buffer, cursorIdx, 1)
      val high = Cursor.endOfWordStrict (buffer, cursorIdx, 1) + 1

      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low
    in
      if canDeleteInsideOrAround (buffer, low, length) then
        let
          val length = high - low
          val buffer = LineGap.delete (low, length, buffer)

          val buffer = LineGap.goToStart buffer
          val initialMsg = [SEARCH (buffer, searchString)]

          val buffer = LineGap.goToIdx (low - 1111, buffer)
          val searchList =
            SearchList.buildRange (buffer, searchString, low + 1111)

          val buffer = LineGap.goToIdx (low, buffer)
        in
          NormalFinish.buildTextAndClear
            (app, buffer, low, searchList, initialMsg, time)
        end
      else
        app
    end

  fun deleteInsideWORD (app: app_type, time) =
    let
      val {buffer, cursorIdx, searchString, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val low = Cursor.prevWordStrict (buffer, cursorIdx, 1)
      val high = Cursor.endOfWordStrict (buffer, cursorIdx, 1) + 1

      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low
    in
      if canDeleteInsideOrAround (buffer, low, length) then
        let
          val buffer = LineGap.delete (low, length, buffer)

          val buffer = LineGap.goToStart buffer
          val initialMsg = [SEARCH (buffer, searchString)]

          val buffer = LineGap.goToIdx (low - 1111, buffer)
          val searchList =
            SearchList.buildRange (buffer, searchString, low + 1111)

          val buffer = LineGap.goToIdx (low, buffer)
        in
          NormalFinish.buildTextAndClear
            (app, buffer, low, searchList, initialMsg, time)
        end
      else
        app
    end

  fun finishAfterDeleteInside (app: app_type, origLow, high, time) =
    if origLow = high then
      NormalFinish.clearMode app
    else
      let
        val {cursorIdx, buffer, searchString, ...} = app
        val low = origLow + 1
        val length = high - low
        val buffer = LineGap.delete (low, length, buffer)

        val buffer = LineGap.goToStart buffer
        val initialMsg = [SEARCH (buffer, searchString)]

        val buffer = LineGap.goToIdx (low - 1111, buffer)
        val searchList =
          SearchList.buildRange (buffer, searchString, low + 1111)

        val buffer = LineGap.goToIdx (origLow, buffer)
      in
        NormalFinish.buildTextAndClear
          (app, buffer, origLow, searchList, initialMsg, time)
      end

  fun deleteInsideChrOpen (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = cursorIdx + 1
      val buffer = LineGap.goToIdx (start, buffer)

      val origLow = Cursor.toPrevChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (origLow, buffer)
      val high = Cursor.matchPair (buffer, origLow)
    in
      finishAfterDeleteInside (app, origLow, high, time)
    end

  fun deleteInsideChrClose (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = Int.max (cursorIdx - 1, 0)
      val buffer = LineGap.goToIdx (start, buffer)

      val high = Cursor.toNextChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (high, buffer)
      val origLow = Cursor.matchPair (buffer, high)
    in
      finishAfterDeleteInside (app, origLow, high, time)
    end

  fun deleteAroundChrOpen (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = cursorIdx + 1
      val buffer = LineGap.goToIdx (start, buffer)

      val low = Cursor.toPrevChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (low, buffer)
      val high = Cursor.matchPair (buffer, low)
    in
      if low = high then NormalFinish.clearMode app
      else deleteAndFinish (app, low, high - low + 1, buffer, time)
    end

  fun deleteAroundChrClose (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = Int.max (cursorIdx - 1, 0)
      val buffer = LineGap.goToIdx (start, buffer)

      val high = Cursor.toNextChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (high, buffer)
      val low = Cursor.matchPair (buffer, high)
    in
      if low = high then NormalFinish.clearMode app
      else deleteAndFinish (app, low, high - low + 1, buffer, time)
    end

  fun deletePair (app: app_type, time) =
    let
      val {cursorIdx, buffer, ...} = app
      val otherIdx = Cursor.matchPair (buffer, cursorIdx)
    in
      if otherIdx = cursorIdx then
        NormalFinish.clearMode app
      else
        let
          val low = Int.min (cursorIdx, otherIdx)
          val high = Int.max (cursorIdx, otherIdx)
          val length = high - low + 1

          val buffer = LineGap.delete (low, length, buffer)
          val buffer = LineGap.goToIdx (low, buffer)

          val low =
            if Cursor.isCursorAtStartOfLine (buffer, low) then
              (* we may have deleted the last character of this current line,
               * and if we did, we have to move the cursor back by 1 *)
              Int.max (low - 1, 0)
            else
              low

          val searchString = #searchString app
          val buffer = LineGap.goToStart buffer
          val initialMsg = [SEARCH (buffer, searchString)]

          val buffer = LineGap.goToIdx (low - 1111, buffer)
          val searchList =
            SearchList.buildRange (buffer, searchString, low + 1111)

          val buffer = LineGap.goToIdx (low, buffer)
        in
          NormalFinish.buildTextAndClear
            (app, buffer, low, searchList, initialMsg, time)
        end
    end
end
