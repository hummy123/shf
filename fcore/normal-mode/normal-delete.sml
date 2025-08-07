structure NormalDelete =
struct
  open AppType
  open MailboxType

  (* equivalent of vi's 'x' command **)
  fun helpRemoveChr (app: app_type, buffer, cursorIdx, count) =
    if count = 0 then
      let
        val searchString = #searchString app
        val buffer = LineGap.goToEnd buffer
        val initialMsg = [SEARCH (buffer, searchString)]

        val buffer = LineGap.goToIdx (cursorIdx + 777, buffer)
        val searchList =
          SearchList.buildRange (buffer, searchString, cursorIdx - 777)
      in
        Finish.buildTextAndClear
          (app, buffer, cursorIdx, searchList, initialMsg)
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
          helpRemoveChr (app, buffer, cursorIdx, 0)
        else if cursorIsStart then
          helpRemoveChr (app, buffer, cursorIdx, 0)
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
            helpRemoveChr (app, buffer, cursorIdx, count - 1)
          end
        else
          let
            val searchString = #searchString app
            val buffer = LineGap.delete (cursorIdx, 1, buffer)
          in
            helpRemoveChr (app, buffer, cursorIdx, count - 1)
          end
      end

  fun removeChr (app: app_type, count) =
    helpRemoveChr (app, #buffer app, #cursorIdx app, count)

  fun helpDelete (app: app_type, buffer, cursorIdx, otherIdx, count, fMove) =
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

        val buffer = LineGap.goToEnd buffer
        val searchString = #searchString app
        val initialMsg = [SEARCH (buffer, searchString)]

        val buffer = LineGap.goToIdx (cursorIdx + 777, buffer)
        val searchList =
          SearchList.buildRange (buffer, searchString, cursorIdx - 777)

        (* If we have deleted from the buffer so that cursorIdx
        * is no longer a valid idx,
        * clip cursorIdx to the end. *)
        val buffer = LineGap.goToIdx (low, buffer)
        val cursorIdx = Cursor.clipIdx (buffer, low)
      in
        Finish.buildTextAndClear
          (app, buffer, cursorIdx, searchList, initialMsg)
      end
    else
      let
        (* get otherIdx, where cursor will want to go after motion. *)
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
      in
        helpDelete (app, buffer, cursorIdx, newOtherIdx, newCount, fMove)
      end

  fun delete (app: app_type, count, fMove) =
    helpDelete (app, #buffer app, #cursorIdx app, #cursorIdx app, count, fMove)

  fun deleteByDfa (app: app_type, count, fMove) =
    let
      val {buffer, cursorIdx, searchString, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = fMove (buffer, cursorIdx, count)

      val low = Int.min (cursorIdx, otherIdx)
      val high = Int.max (cursorIdx, otherIdx)
      val length = high - low

      val buffer = LineGap.delete (low, length, buffer)

      val buffer = LineGap.goToEnd buffer
      val initialMsg = [SEARCH (buffer, searchString)]

      val buffer = LineGap.goToIdx (cursorIdx + 777, buffer)
      val searchList =
        SearchList.buildRange (buffer, searchString, cursorIdx - 777)

      val buffer = LineGap.goToIdx (low, buffer)
    in
      Finish.buildTextAndClear (app, buffer, low, searchList, initialMsg)
    end

  fun deleteToEndOfLine (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        (* if we are on \n, we don't want to delete or do anything
        * so reset the mode *)
        Finish.clearMode app
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
          helpRemoveChr (app, buffer, cursorIdx, 1)
        end
    end

  fun deleteLine (app: app_type, count) =
    let
      val {buffer, cursorIdx, searchString, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val startIdx = Cursor.vi0 (buffer, cursorIdx)
      val finishIdx = Cursor.viDlrForDelete (buffer, cursorIdx, count)

      val length = finishIdx - startIdx
      val buffer = LineGap.delete (startIdx, length, buffer)

      val buffer = LineGap.goToEnd buffer
      val initialMsg = [SEARCH (buffer, searchString)]

      val buffer = LineGap.goToIdx (cursorIdx + 777, buffer)
      val searchList =
        SearchList.buildRange (buffer, searchString, cursorIdx - 777)

      val buffer = LineGap.goToIdx (startIdx, buffer)
    in
      Finish.buildTextAndClear (app, buffer, startIdx, searchList, initialMsg)
    end

  fun helpDeleteLineBack (app, buffer, low, high, count) =
    if count = 0 then
      let
        val low = Int.max (low, 0)
        val length = high - low
        val buffer = LineGap.delete (low, length, buffer)

        val buffer = LineGap.goToEnd buffer
        val searchString = #searchString app
        val initialMsg = [SEARCH (buffer, searchString)]

        val buffer = LineGap.goToIdx (low + 777, buffer)
        val searchList = SearchList.buildRange (buffer, searchString, low - 777)

        val buffer = LineGap.goToIdx (low, buffer)
      in
        Finish.buildTextAndClear (app, buffer, low, searchList, initialMsg)
      end
    else
      let
        val buffer = LineGap.goToIdx (low, buffer)
        val low = Cursor.viH (buffer, low)
        val buffer = LineGap.goToIdx (low, buffer)
        val low = Cursor.vi0 (buffer, low)
        val newCount = if low = 0 then 0 else count - 1
      in
        helpDeleteLineBack (app, buffer, low, high, newCount)
      end

  fun deleteLineBack (app: app_type, count) =
    let
      val {buffer, cursorIdx, ...} = app
      val low = Cursor.vi0 (buffer, cursorIdx)
      val high = Cursor.viDlr (buffer, cursorIdx, 1) + 1
    in
      helpDeleteLineBack (app, buffer, low, high, count)
    end

  fun deleteToFirstNonSpaceChr (app: app_type) =
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

      val buffer = LineGap.delete (low, length, buffer)

      val buffer = LineGap.goToEnd buffer
      val initialMsg = [SEARCH (buffer, searchString)]

      val buffer = LineGap.goToIdx (cursorIdx + 777, buffer)
      val searchList =
        SearchList.buildRange (buffer, searchString, cursorIdx - 777)
    in
      Finish.buildTextAndClear (app, buffer, low, searchList, initialMsg)
    end

  fun helpDeleteToChr
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, fInc, chr) =
    if count = 0 then
      let
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val length = high - low
        val buffer = LineGap.delete (low, length, buffer)

        val buffer = LineGap.goToEnd buffer
        val searchString = #searchString app
        val initialMsg = [SEARCH (buffer, searchString)]

        val buffer = LineGap.goToIdx (cursorIdx + 777, buffer)
        val searchList =
          SearchList.buildRange (buffer, searchString, cursorIdx - 777)
      in
        Finish.buildTextAndClearAfterChr
          (app, buffer, low, searchList, initialMsg)
      end
    else
      let
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx, chr)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
        val newOtherIdx = fInc (newOtherIdx, 1)
      in
        helpDeleteToChr
          (app, buffer, cursorIdx, newOtherIdx, newCount, fMove, fInc, chr)
      end

  fun deleteToChr (app: app_type, count, fMove, fInc, chr) =
    helpDeleteToChr
      ( app
      , #buffer app
      , #cursorIdx app
      , #cursorIdx app
      , count
      , fMove
      , fInc
      , chr
      )

  fun deleteToStart (app: app_type) =
    let
      val {cursorIdx, buffer, windowWidth, windowHeight, searchString, ...} =
        app

      val buffer = LineGap.delete (0, cursorIdx, buffer)

      val buffer = LineGap.goToEnd buffer
      val initialMsg = [SEARCH (buffer, #searchString app)]

      val buffer = LineGap.goToIdx (cursorIdx + 777, buffer)
      val searchList =
        SearchList.buildRange (buffer, searchString, cursorIdx - 777)

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
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg)
    end

  fun helpDeleteToMatch (app: app_type, low, high) =
    let
      val {buffer, searchString, ...} = app
      val length = high - low
      val buffer = LineGap.delete (low, length, buffer)

      val buffer = LineGap.goToEnd buffer
      val initialMsg = [SEARCH (buffer, searchString)]

      val buffer = LineGap.goToIdx (low + 777, buffer)
      val searchList = SearchList.buildRange (buffer, searchString, low - 777)

      val buffer = LineGap.goToIdx (low, buffer)
    in
      Finish.buildTextAndClear (app, buffer, low, searchList, initialMsg)
    end

  fun deleteToNextMatch (app: app_type, count) =
    let
      val {cursorIdx, searchList, ...} = app
      val newCursorIdx = SearchList.nextMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 orelse newCursorIdx <= cursorIdx then
        Finish.clearMode app
      else
        helpDeleteToMatch (app, cursorIdx, newCursorIdx)
    end

  fun deleteToPrevMatch (app: app_type, count) =
    let
      val {cursorIdx, searchList, ...} = app
      val newCursorIdx = SearchList.prevMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 orelse newCursorIdx >= cursorIdx then
        Finish.clearMode app
      else
        helpDeleteToMatch (app, newCursorIdx, cursorIdx)
    end
end
