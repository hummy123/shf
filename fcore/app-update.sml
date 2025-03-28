structure AppUpdate =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

  fun clearMode app =
    AppWith.mode (app, NORMAL_MODE "", [])

  fun resizeText (app: app_type, newWidth, newHeight) =
    let
      val
        { buffer
        , windowWidth
        , windowHeight
        , startLine
        , cursorIdx
        , searchList
        , searchString
        , ...
        } = app

      val newBuffer = LineGap.goToLine (startLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)
      val searchList = SearchList.goToNum (lineIdx, searchList)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , newBuffer
        , newWidth
        , newHeight
        , searchList
        , searchString
        )
    in
      AppWith.bufferAndSize
        (app, newBuffer, newWidth, newHeight, searchList, drawMsg)
    end

  (* Difference between this and buildTextAndClear is that 
   * this is meant to be called after a chr movement, 
   * where the cursor may possibly jump off window by a wide marigin.
   * Since the cursor may move away a lot, it is best to recenter.
   * *)
  fun buildTextAndClearAfterChr (app: app_type, buffer, cursorIdx, searchList) =
    let
      val {windowWidth, windowHeight, startLine, searchString, ...} = app

      (* move LineGap to first line displayed on screen *)
      val buffer = LineGap.goToLine (startLine, buffer)

      (* get new startLine which may move screen depending on cursor movements *)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

      (* move buffer to new startLine as required by TextBuilder.build 
      * and move searchList to idx where line starts as well *)
      val buffer = LineGap.goToLine (startLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)
      val searchList = SearchList.goToNum (lineIdx, searchList)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        )

      val mode = NORMAL_MODE ""
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg)
    end

  fun centreToCursor (app: app_type) =
    let
      val
        { buffer
        , windowWidth
        , windowHeight
        , startLine = origLine
        , cursorIdx
        , searchList
        , searchString
        , ...
        } = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val startLine = TextWindow.getStartLineWithCursorCentered
        (buffer, cursorIdx, origLine, windowWidth, windowHeight div 2)

      val buffer = LineGap.goToLine (startLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)
      val searchList = SearchList.goToNum (lineIdx, searchList)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        )
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, NORMAL_MODE "", startLine, searchList, drawMsg)
    end

  (* movement functions *)

  fun moveToStart (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, searchList, searchString, ...} =
        app

      val cursorIdx = 0
      val startLine = 0
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val searchList = SearchList.goToNum (0, searchList)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        )

      val mode = NORMAL_MODE ""
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg)
    end

  fun moveToEnd (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, searchList, searchString, ...} =
        app

      val buffer = LineGap.goToEnd buffer
      val {line = bufferLine, idx = bufferIdx, ...} = buffer

      val bufferIdx = bufferIdx - 1
      val bufferIdx = Cursor.clipIdx (buffer, bufferIdx)
      val bufferLine = bufferLine - 1

      val buffer = LineGap.goToIdx (bufferIdx, buffer)
      val bufferLine =
        let
          val maxHeight = windowHeight - TextConstants.ySpace
        in
          TextWindow.getStartLineWithCursorCentered
            (buffer, bufferIdx, bufferLine, windowWidth, maxHeight)
        end

      val buffer = LineGap.goToLine (bufferLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (bufferLine, buffer)
      val searchList = SearchList.goToNum (lineIdx, searchList)

      val drawMsg = TextBuilder.build
        ( bufferLine
        , bufferIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        )

      val mode = NORMAL_MODE ""
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, bufferIdx, mode, bufferLine, searchList, drawMsg)
    end

  fun moveToLine (app: app_type, reqLine) =
    if reqLine = 0 then
      moveToStart app
    else
      let
        val
          { windowWidth
          , windowHeight
          , buffer
          , startLine = origLine
          , searchList
          , searchString
          , ...
          } = app
        val buffer = LineGap.goToLine (reqLine, buffer)

        (* get idx of first chr after linebreak *)
        val cursorIdx = Cursor.getLineStartIdx (buffer, reqLine)

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val startLine = TextWindow.getStartLineWithCursorCentered
          (buffer, cursorIdx, origLine, windowWidth, windowHeight div 2)

        val buffer = LineGap.goToLine (startLine, buffer)
        val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)
        val searchList = SearchList.goToNum (lineIdx, searchList)

        val drawMsg = TextBuilder.build
          ( startLine
          , cursorIdx
          , buffer
          , windowWidth
          , windowHeight
          , searchList
          , searchString
          )

        val mode = NORMAL_MODE ""
      in
        AppWith.bufferAndCursorIdx
          (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg)
      end

  fun moveToMatchingPair (app: app_type) =
    let
      val
        { buffer
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , searchList
        , searchString
        , ...
        } = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.matchPair (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)
      val searchList = SearchList.goToNum (lineIdx, searchList)
    in
      if
        TextWindow.isCursorVisible
          (buffer, cursorIdx, startLine, windowWidth, windowHeight)
      then
        (* if visible, just need to redraw; no need to get line *)
        let
          val drawMsg = TextBuilder.build
            ( startLine
            , cursorIdx
            , buffer
            , windowWidth
            , windowHeight
            , searchList
            , searchString
            )
        in
          AppWith.bufferAndCursorIdx
            ( app
            , buffer
            , cursorIdx
            , NORMAL_MODE ""
            , startLine
            , searchList
            , drawMsg
            )
        end
      else
        (* not visible, so need to get startLine where cursor is visible *)
        let
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val startLine = TextWindow.getStartLineWithCursorCentered
            (buffer, cursorIdx, startLine, windowWidth, windowHeight div 2)

          val buffer = LineGap.goToLine (startLine, buffer)
          val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)
          val searchList = SearchList.goToNum (lineIdx, searchList)

          val drawMsg = TextBuilder.build
            ( startLine
            , cursorIdx
            , buffer
            , windowWidth
            , windowHeight
            , searchList
            , searchString
            )
        in
          AppWith.bufferAndCursorIdx
            ( app
            , buffer
            , cursorIdx
            , NORMAL_MODE ""
            , startLine
            , searchList
            , drawMsg
            )
        end
    end

  fun firstNonSpaceChr (app: app_type) =
    let
      val {buffer, cursorIdx, windowWidth, windowHeight, startLine, ...} = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.vi0 (buffer, cursorIdx)

      (* move cursorIdx to first character on line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.firstNonSpaceChr (buffer, cursorIdx)
    in
      Finish.buildTextAndClear (app, buffer, cursorIdx, #searchList app)
    end

  fun helpMoveToChr (app: app_type, buffer, cursorIdx, count, fMove, chr) =
    if count = 0 then
      buildTextAndClearAfterChr (app, buffer, cursorIdx, #searchList app)
    else
      let
        (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val newCursorIdx = fMove (buffer, cursorIdx, chr)
        val newCount = if cursorIdx = newCursorIdx then 0 else count - 1
      in
        helpMoveToChr (app, buffer, newCursorIdx, newCount, fMove, chr)
      end

  fun moveToChr (app: app_type, count, fMove, chr) =
    let val {cursorIdx, buffer, ...} = app
    in helpMoveToChr (app, buffer, cursorIdx, count, fMove, chr)
    end

  fun handleMoveToChr (count, app, fMove, newCmd) =
    case newCmd of
      CHAR_EVENT chr => moveToChr (app, count, fMove, chr)
    | KEY_ESC => clearMode app
    | RESIZE_EVENT (width, height) => resizeText (app, width, height)

  fun handleGo (count, app, newCmd) =
    case newCmd of
      CHAR_EVENT chr =>
        (case chr of
           #"e" => MoveToEndOfPrevWord.move (app, count)
         | #"E" => MoveToEndOfPrevWORD.move (app, count)
         | #"g" => moveToStart app
         | _ => clearMode app)
    | KEY_ESC => clearMode app
    | RESIZE_EVENT (width, height) => resizeText (app, width, height)

  (* text-delete functions *)
  (** equivalent of vi's 'x' command **)

  fun deleteSearchList (cursorIdx, length, searchString, searchList, buffer) =
    let
      val searchList =
        SearchList.delete (cursorIdx, length, searchString, searchList)
      val searchList = SearchList.mapFrom (cursorIdx, ~length, searchList)
    in
      BuildSearchList.fromRange
        (cursorIdx, length, buffer, searchString, searchList)
    end

  fun helpRemoveChr (app: app_type, buffer, searchList, cursorIdx, count) =
    if count = 0 then
      Finish.buildTextAndClear (app, buffer, cursorIdx, searchList)
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
          helpRemoveChr (app, buffer, searchList, cursorIdx, 0)
        else if cursorIsStart then
          helpRemoveChr (app, buffer, searchList, cursorIdx, 0)
        else if nextIsEnd then
          let
            (* delete char at cursor and then decrement cursorIdx by 1
            * if cursorIdx is not 0 *)
            val {searchString, ...} = app
            val buffer = LineGap.delete (cursorIdx, 1, buffer)

            val (buffer, searchList) = deleteSearchList
              (cursorIdx, 1, searchString, searchList, buffer)

            val cursorIdx =
              if
                Cursor.isPrevChrStartOfLine (buffer, cursorIdx)
                orelse cursorIdx = 0
              then cursorIdx
              else cursorIdx - 1
          in
            helpRemoveChr (app, buffer, searchList, cursorIdx, count - 1)
          end
        else
          let
            val {searchString, ...} = app
            val buffer = LineGap.delete (cursorIdx, 1, buffer)

            val (buffer, searchList) = deleteSearchList
              (cursorIdx, 1, searchString, searchList, buffer)
          in
            helpRemoveChr (app, buffer, searchList, cursorIdx, count - 1)
          end
      end

  fun removeChr (app: app_type, count) =
    helpRemoveChr (app, #buffer app, #searchList app, #cursorIdx app, count)

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

        val {searchList, searchString, ...} = app
        val (buffer, searchList) = deleteSearchList
          (low, length, searchString, searchList, buffer)

        (* If we have deleted from the buffer so that cursorIdx
        * is no longer a valid idx,
        * clip cursorIdx to the end. *)
        val buffer = LineGap.goToIdx (low, buffer)
        val cursorIdx = Cursor.clipIdx (buffer, low)
      in
        Finish.buildTextAndClear (app, buffer, cursorIdx, searchList)
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

  fun deleteToEndOfLine (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        (* if we are on \n, we don't want to delete or do anything
        * so reset the mode *)
        clearMode app
      else
        let
          (* viDlr takes us to the last chr in the line 
          * but does not delete that last chr
          * so we call helpRemoveChr to delete that last chr. 
          * We also rely on helpRemoveChr to handle backwards-movement logic:
          * If cursorIdx is at \n after deletion, then stop.
          * Else, move back one chr. *)
          val lastChr = Cursor.viDlr (buffer, cursorIdx)
          val length = lastChr - cursorIdx
          val buffer = LineGap.delete (cursorIdx, length, buffer)

          (* delete from searchList and map *)
          val {searchList, searchString, ...} = app
          val (buffer, searchList) = deleteSearchList
            (cursorIdx, length, searchString, searchList, buffer)
        in
          helpRemoveChr (app, buffer, searchList, cursorIdx, 1)
        end
    end

  fun helpDeleteLine (app: app_type, buffer, cursorIdx, otherIdx, count) =
    if count = 0 then
      let
        val otherIdx = Cursor.clipIdx (buffer, otherIdx)
        val length = otherIdx - cursorIdx
        val buffer = LineGap.delete (cursorIdx, length, buffer)

        val {searchList, searchString, ...} = app
        val (buffer, searchList) = deleteSearchList
          (cursorIdx, length, searchString, searchList, buffer)

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val cursorIdx = Cursor.clipIdx (buffer, cursorIdx)
      in
        Finish.buildTextAndClear (app, buffer, cursorIdx, searchList)
      end
    else
      let
        (* get otherIdx, where cursor will want to go after motion. *)
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = Cursor.viDlr (buffer, otherIdx)
        val newOtherIdx = Cursor.viL (buffer, newOtherIdx)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
      in
        helpDeleteLine (app, buffer, cursorIdx, newOtherIdx, newCount)
      end

  fun deleteLine (app: app_type, count) =
    let
      val {buffer, cursorIdx, ...} = app
      val cursorIdx = Cursor.vi0 (buffer, cursorIdx)
    in
      helpDeleteLine (app, buffer, cursorIdx, cursorIdx, count)
    end

  fun helpDeleteLineBack (app, buffer, low, high, count) =
    if count = 0 then
      let
        val low = Int.max (low, 0)
        val length = high - low
        val buffer = LineGap.delete (low, length, buffer)

        val {searchList, searchString, ...} = app
        val (buffer, searchList) = deleteSearchList
          (low, length, searchString, searchList, buffer)

        val buffer = LineGap.goToIdx (low, buffer)
      in
        Finish.buildTextAndClear (app, buffer, low, searchList)
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
      val high = Cursor.viDlr (buffer, cursorIdx) + 1
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
        , searchList
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
      val (buffer, searchList) = deleteSearchList
        (low, length, searchString, searchList, buffer)
    in
      Finish.buildTextAndClear (app, buffer, low, searchList)
    end

  fun helpDeleteToChr
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, fInc, chr) =
    if count = 0 then
      let
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val length = high - low
        val buffer = LineGap.delete (low, length, buffer)

        val {searchString, searchList, ...} = app
        val (buffer, searchList) = deleteSearchList
          (low, length, searchString, searchList, buffer)
      in
        buildTextAndClearAfterChr (app, buffer, low, searchList)
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
      val
        { cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , ...
        } = app

      val buffer = LineGap.delete (0, cursorIdx, buffer)
      val (buffer, searchList) = deleteSearchList
        (0, cursorIdx, searchString, searchList, buffer)

      val cursorIdx = 0
      val startLine = 0
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val searchList = SearchList.goToNum (0, searchList)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        )

      val mode = NORMAL_MODE ""
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg)
    end

  (* command-parsing functions *)
  (** number of characters which are integers *)
  fun getNumLength (pos, str) =
    if pos = String.size str then
      pos
    else
      let val chr = String.sub (str, pos)
      in if Char.isDigit chr then getNumLength (pos + 1, str) else pos
      end

  fun appendChr (app: app_type, chr, str) =
    let
      val str = str ^ Char.toString chr
      val mode = NORMAL_MODE str
    in
      AppWith.mode (app, mode, [])
    end

  fun handleChr (app: app_type, count, chr, str) =
    case chr of
      #"h" => MoveViH.move (app, count)
    | #"j" => MoveViJ.move (app, count)
    | #"k" => MoveViK.move (app, count)
    | #"l" => MoveViL.move (app, count)
    | #"w" => MoveToNextWord.move (app, count)
    | #"W" => MoveToNextWORD.move (app, count)
    | #"b" => MoveToPrevWord.move (app, count)
    | #"B" => MoveToPrevWORD.move (app, count)
    | #"e" => MoveToEndOfWord.move (app, count)
    | #"E" => MoveToEndOfWORD.move (app, count)
    | #"z" => centreToCursor app
    (* can only move to start or end of line once 
     * so hardcode count as 1 *)
    | #"0" =>
        (* 0 is a bit of a special case.
         * If 0 is pressed without any preceding characters,
         * then it should move cursor to the start of the line.
         * However, if a number was pressed previously before 0 was,
         * then this means user is entering a count.
         * In that case, we append 0 to the string. *)
        if String.size str > 0 then
          let
            val lastChr = String.sub (str, String.size str - 1)
          in
            if Char.isDigit lastChr then
              let
                val chr = Char.toString chr
                val str = str ^ chr
                val mode = NORMAL_MODE str
              in
                AppWith.mode (app, mode, [])
              end
            else
              MoveToStartOfLine.move (app, 1)
          end
        else
          MoveToStartOfLine.move (app, 1)
    | #"$" => MoveToEndOfLine.move (app, 1)
    | #"^" => firstNonSpaceChr app
    | #"G" =>
        (* if str has a size larger than 0,
         * interpret as "go to line" command;
         * else, interpret as a command to move to end *)
        if String.size str = 0 then moveToEnd app
        else moveToLine (app, count - 1)
    | #"%" => moveToMatchingPair app
    | #"x" => removeChr (app, count)
    (* multi-char commands which can be appended *)
    | #"t" => appendChr (app, chr, str)
    | #"T" => appendChr (app, chr, str)
    | #"y" => appendChr (app, chr, str)
    | #"d" => appendChr (app, chr, str)
    | #"f" => appendChr (app, chr, str)
    | #"F" => appendChr (app, chr, str)
    | #"g" => appendChr (app, chr, str)
    | #"c" => appendChr (app, chr, str)
    | _ =>
        (* user may be entering a cmd with more than one chr
         * such as "2dw" to delete two word
         * so add current chr to mode, and save it in the app state *)
        let
          val str = if Char.isDigit chr then str ^ Char.toString chr else ""
          val mode = NORMAL_MODE str
        in
          AppWith.mode (app, mode, [])
        end

  fun parseDelete (strPos, str, count, app, newCmd) =
    if strPos = String.size str - 1 then
      (* have to check newCmd *)
      case newCmd of
        CHAR_EVENT chr =>
          (case chr of
           (* terminal commands: require no input after *)
             #"h" => delete (app, count, Cursor.viH)
           | #"l" => delete (app, count, Cursor.viL)
           (* vi's 'j' and 'k' commands move up or down a column
            * but 'dj' or 'dk' delete whole lines
            * so their implementation differs from
            * other cursor motions *)
           | #"j" => deleteLine (app, count + 1)
           | #"k" => deleteLineBack (app, count)
           | #"w" => delete (app, count, Cursor.nextWord)
           | #"W" => delete (app, count, Cursor.nextWORD)
           | #"b" => delete (app, count, Cursor.prevWord)
           | #"B" => delete (app, count, Cursor.prevWORD)
           | #"e" => delete (app, count, Cursor.endOfWordPlusOne)
           | #"E" => delete (app, count, Cursor.endOfWORDPlusOne)
           | #"0" => delete (app, 1, Cursor.vi0)
           | #"$" => deleteToEndOfLine app
           | #"^" => deleteToFirstNonSpaceChr app
           | #"d" => deleteLine (app, count)
           (* non-terminal commands which require appending chr *)
           | #"t" => appendChr (app, chr, str)
           | #"T" => appendChr (app, chr, str)
           | #"f" => appendChr (app, chr, str)
           | #"F" => appendChr (app, chr, str)
           | #"g" => appendChr (app, chr, str)
           (* invalid command: reset mode *)
           | _ => clearMode app)
      | KEY_ESC => clearMode app
      | RESIZE_EVENT (width, height) => resizeText (app, width, height)
    else
      (* have to continue parsing string *)
      case String.sub (str, strPos + 1) of
        #"t" =>
          (* delete till chr, forwards *)
          (case newCmd of
             CHAR_EVENT chr =>
               deleteToChr (app, 1, Cursor.tillNextChr, op+, chr)
           | KEY_ESC => clearMode app
           | RESIZE_EVENT (width, height) => resizeText (app, width, height))
      | #"T" =>
          (* delete till chr, backwards *)
          (case newCmd of
             CHAR_EVENT chr =>
               deleteToChr (app, 1, Cursor.tillPrevChr, op-, chr)
           | KEY_ESC => clearMode app
           | RESIZE_EVENT (width, height) => resizeText (app, width, height))
      | #"f" =>
          (case newCmd of
             CHAR_EVENT chr =>
               deleteToChr (app, count, Cursor.toNextChr, op+, chr)
           | KEY_ESC => clearMode app
           | RESIZE_EVENT (width, height) => resizeText (app, width, height))
      | #"F" =>
          (* delete to chr, backwards *)
          (case newCmd of
             CHAR_EVENT chr =>
               deleteToChr (app, count, Cursor.toPrevChr, op-, chr)
           | KEY_ESC => clearMode app
           | RESIZE_EVENT (width, height) => resizeText (app, width, height))
      | #"g" =>
          (* same events as handleGo *)
          (case newCmd of
             CHAR_EVENT chr =>
               (case chr of
                  #"e" => delete (app, count, Cursor.endOfPrevWord)
                | #"E" => delete (app, count, Cursor.endOfPrevWORD)
                | #"g" => deleteToStart app
                | _ => clearMode app)
           | KEY_ESC => clearMode app
           | RESIZE_EVENT (width, height) => resizeText (app, width, height))
      | _ => clearMode app

  (* useful reference as list of non-terminal commands *)
  fun parseAfterCount (strPos, str, count, app, newCmd) =
    (* we are trying to parse multi-char but non-terminal strings here.
     * For example, we don't want to parse 3w which is a terminal commmand
     * to go 3 words forwards
     * but we do want to parse 3d which is a non-terminal command
     * which can be made terminal by adding "w" or "e" at the end. 
     * *)
    case String.sub (str, strPos) of
      #"t" =>
        (* to just before char, forward 
         * tillNextChr with count of 1 has same effect
         * as tillNextChr with any count above 1
         * so just hardcode 1 *)
        handleMoveToChr (1, app, Cursor.tillNextChr, newCmd)
    | #"T" =>
        (* to just before chr, backward *)
        handleMoveToChr (1, app, Cursor.tillPrevChr, newCmd)
    | #"y" => (* yank *) clearMode app
    | #"d" => (* delete *) parseDelete (strPos, str, count, app, newCmd)
    | #"f" =>
        (* to chr, forward *)
        handleMoveToChr (count, app, Cursor.toNextChr, newCmd)
    | #"F" =>
        (* to chr, backward *)
        handleMoveToChr (count, app, Cursor.toPrevChr, newCmd)
    | #"g" => (* go *) handleGo (count, app, newCmd)
    | #"c" => (* change *) clearMode app
    | _ =>
        (* isn't a non-terminal cmd
         * this case should never happen*)
        clearMode app

  fun parseNormalModeCommand (app, str, newCmd) =
    if String.size str = 0 then
      case newCmd of
        CHAR_EVENT chr => handleChr (app, 1, chr, str)
      | KEY_ESC => clearMode app
      | RESIZE_EVENT (width, height) => resizeText (app, width, height)
    else if String.size str = 1 then
      case newCmd of
        CHAR_EVENT chr =>
          (case Int.fromString str of
             SOME count => handleChr (app, count, chr, str)
           | NONE => parseAfterCount (0, str, 1, app, newCmd))
      | KEY_ESC => clearMode app
      | RESIZE_EVENT (width, height) => resizeText (app, width, height)
    else
      let
        val numLength = getNumLength (0, str)
        val count = String.substring (str, 0, numLength)
        val count =
          case Int.fromString count of
            SOME x => x
          | NONE => 1
      in
        if numLength = String.size str then
          (* reached end of str; str only contained numbers *)
          case newCmd of
            CHAR_EVENT chr => handleChr (app, count, chr, str)
          | KEY_ESC => clearMode app
          | RESIZE_EVENT (width, height) => resizeText (app, width, height)
        else
          (* continue parsing. *)
          parseAfterCount (numLength, str, count, app, newCmd)
      end

  fun updateNormalMode (app, str, msg) = parseNormalModeCommand (app, str, msg)

  fun update (app, msg) =
    case #mode app of NORMAL_MODE str => updateNormalMode (app, str, msg)
end
