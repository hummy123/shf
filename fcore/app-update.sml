structure AppUpdate =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

  fun clearMode app =
    let
      val mode = NORMAL_MODE ""
      val newApp = AppWith.mode (app, mode)
    in
      (newApp, [])
    end

  fun resizeText (app: app_type, newWidth, newHeight) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val newBuffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, newBuffer, newWidth, newHeight)

      val newApp = AppWith.bufferAndSize (app, newBuffer, newWidth, newHeight)
    in
      (newApp, drawMsg)
    end

  fun buildTextAndClear (app: app_type, buffer, cursorIdx) =
    let
      val {windowWidth, windowHeight, startLine, ...} = app

      (* move LineGap to first line displayed on screen *)
      val buffer = LineGap.goToLine (startLine, buffer)

      (* get new startLine which may move screen depending on cursor movements *)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

      (* move buffer to new startLine as required by TextBuilder.build *)
      val buffer = LineGap.goToLine (startLine, buffer)

      val drawMsg = 
        TextBuilder.build
          (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val mode = NORMAL_MODE ""
      val newApp = AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine)
    in
      (newApp, drawMsg)
    end

  fun centreToCursor (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine = origLine, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val startLine = TextWindow.getStartLineWithCursorCentered 
        (buffer, cursorIdx, origLine, windowWidth, windowHeight div 2)

      val buffer = LineGap.goToLine (startLine, buffer)

      val newApp = AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, NORMAL_MODE "", startLine)

      val drawMsg = 
        TextBuilder.build
          (startLine, cursorIdx, buffer, windowWidth, windowHeight)
    in
      (newApp, drawMsg)
    end

  (* movement functions *)

  fun moveToStart (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, ...} = app

      val cursorIdx = 0
      val startLine = 0
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val drawMsg = 
        TextBuilder.build
          (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val mode = NORMAL_MODE ""
      val newApp = AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine)
    in
      (newApp, drawMsg)
    end

  fun moveToEnd (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, ...} = app

      val buffer = LineGap.goToEnd buffer
      val {line = bufferLine, idx = bufferIdx, ...} = buffer

      val bufferIdx = bufferIdx - 1
      val bufferLine = bufferLine - 1

      val buffer = LineGap.goToIdx (bufferIdx, buffer)
      val bufferLine = 
        let
          val maxHeight = windowHeight - (TextConstants.ySpace * 2)
        in
          TextWindow.getStartLineWithCursorCentered 
            (buffer, bufferIdx, bufferLine, windowWidth, maxHeight)
        end

      val buffer = LineGap.goToLine (bufferLine, buffer)
      val drawMsg = 
        TextBuilder.build
          (bufferLine, bufferIdx, buffer, windowWidth, windowHeight)

      val mode = NORMAL_MODE ""
      val newApp = AppWith.bufferAndCursorIdx
        (app, buffer, bufferIdx, mode, bufferLine)
    in
      (newApp, drawMsg)
    end

  fun moveToLine (app: app_type, reqLine) =
    if reqLine = 0 then
      moveToStart app
    else
      let
        val {windowWidth, windowHeight, buffer, startLine = origLine, ...} = app
        val buffer = LineGap.goToLine (reqLine, buffer)

        (* get idx of first chr after linebreak *)
        val cursorIdx = Cursor.getLineStartIdx (buffer, reqLine)

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val startLine = TextWindow.getStartLineWithCursorCentered 
          (buffer, cursorIdx, origLine, windowWidth, windowHeight div 2)

        val buffer = LineGap.goToLine (startLine, buffer)

        val newApp = AppWith.bufferAndCursorIdx
          (app, buffer, cursorIdx, NORMAL_MODE "", startLine)

        val drawMsg = 
          TextBuilder.build
            (startLine, cursorIdx, buffer, windowWidth, windowHeight)
      in
        (newApp, drawMsg)
      end

  fun helpMove (app: app_type, buffer, cursorIdx, count, fMove) =
    if count = 0 then
      buildTextAndClear (app, buffer, cursorIdx)
    else
      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val cursorIdx = fMove (buffer, cursorIdx)
      in
        helpMove (app, buffer, cursorIdx, count - 1, fMove)
      end

  fun move (app: app_type, count, fMove) =
    let val {cursorIdx, buffer, ...} = app
    in helpMove (app, buffer, cursorIdx, count, fMove)
    end

  fun moveToMatchingPair (app: app_type) =
    let
      val {buffer, cursorIdx, windowWidth, windowHeight, startLine, ...} = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.matchPair (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
    in
      if TextWindow.isCursorVisible 
        (buffer, cursorIdx, startLine, windowWidth, windowHeight)
      then
        (* if visible, just need to redraw; no need to get line *)
        let
          val newApp = AppWith.bufferAndCursorIdx
            (app, buffer, cursorIdx, NORMAL_MODE "", startLine)

          val drawMsg = 
            TextBuilder.build
              (startLine, cursorIdx, buffer, windowWidth, windowHeight)
        in
          (newApp, drawMsg)
        end
      else
        (* not visible, so need to get startLine where cursor is visible *)
        let
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val startLine = 
            TextWindow.getStartLineWithCursorCentered 
            (buffer, cursorIdx, startLine, windowWidth, windowHeight div 2)

          val buffer = LineGap.goToLine (startLine, buffer)

          val newApp = AppWith.bufferAndCursorIdx
            (app, buffer, cursorIdx, NORMAL_MODE "", startLine)

          val drawMsg = 
            TextBuilder.build
              (startLine, cursorIdx, buffer, windowWidth, windowHeight)
        in
          (newApp, drawMsg)
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
      buildTextAndClear (app, buffer, cursorIdx)
    end

  fun helpMoveToChr (app: app_type, buffer, cursorIdx, count, fMove, chr) =
    if count = 0 then
      let
        val {windowWidth, windowHeight, startLine, ...} = app

        (* move LineGap to first line displayed on screen *)
        val buffer = LineGap.goToLine (startLine, buffer)

        (* get new startLine which may move screen depending on cursor movements *)
        val startLine = TextWindow.getStartLine
          (buffer, startLine, cursorIdx, windowWidth, windowHeight)

        (* move buffer to new startLine as required by TextBuilder.build *)
        val buffer = LineGap.goToLine (startLine, buffer)

        val drawMsg = 
          TextBuilder.build
            (startLine, cursorIdx, buffer, windowWidth, windowHeight)

        val mode = NORMAL_MODE ""
        val newApp = AppWith.bufferAndCursorIdx
          (app, buffer, cursorIdx, mode, startLine)
      in
        (newApp, drawMsg)
      end
    else
      let
        (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val cursorIdx = fMove (buffer, cursorIdx, chr)
      in
        helpMoveToChr (app, buffer, cursorIdx, count - 1, fMove, chr)
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
           #"e" => move (app, count, Cursor.endOfPrevWord)
         | #"E" => move (app, count, Cursor.endOfPrevWORD)
         | #"g" => moveToStart app
         | _ => clearMode app)
    | KEY_ESC => clearMode app
    | RESIZE_EVENT (width, height) => resizeText (app, width, height)

  (* text-delete functions *)
  (** equivalent of vi's 'x' command **)
  fun helpRemoveChr (app: app_type, buffer, cursorIdx, count) =
    if count = 0 then
      buildTextAndClear (app, buffer, cursorIdx)
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
            val buffer = LineGap.delete (cursorIdx, 1, buffer)
            val cursorIdx = 
              if Cursor.isPrevChrStartOfLine (buffer, cursorIdx)
              orelse cursorIdx = 0 then
                cursorIdx
              else cursorIdx - 1
          in
            helpRemoveChr (app, buffer, cursorIdx, count - 1)
          end
       else
         let
           val buffer = LineGap.delete (cursorIdx, 1, buffer)
         in
            helpRemoveChr (app, buffer, cursorIdx, count - 1)
         end
      end

  fun removeChr (app: app_type, count) =
    helpRemoveChr (app, #buffer app, #cursorIdx app, count)

  fun helpDelete (app: app_type, buffer, cursorIdx, count, fMove) =
    if count = 0 then
      let
        (* If we have deleted from the buffer so that cursorIdx
         * is no longer a valid idx,
         * clip cursorIdx to the end. *)
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val cursorIdx = Cursor.clipIdxAfterDelete (buffer, cursorIdx)
      in
        buildTextAndClear (app, buffer, cursorIdx)
      end
    else
      let
        (* get otherIdx, where cursor will want to go after motion. *)
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val otherIdx = fMove (buffer, cursorIdx)

        (* Because some motions like 'b' take us backwards,
         * and some motions like 'w' take us forwards,
         * we need to find out which idx is lower and higher
         * for proper deletion and seeing where to set cursorIdx to *)
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val length = high - low

        val buffer = LineGap.delete (low, length, buffer)

        (* todo: possibly decrement cursorIdx by 1 
         * if deleting put cursorIdx past end of file.
         * else, if deleting put cursorIdx before 0,
         * ensure that it is clipped to 0.
         * else, leave cursorIdx alone.
         * *)
      in
        helpDelete (app, buffer, low, count - 1, fMove)
      end

  fun delete (app: app_type, count, fMove) =
    helpDelete (app, #buffer app, #cursorIdx app, count, fMove)


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
      val newApp = AppWith.mode (app, mode)
    in
      (newApp, [])
    end

  fun handleChr (app: app_type, count, chr, str) =
    case chr of
      #"h" => move (app, count, Cursor.viH)
    | #"j" => move (app, count, Cursor.viJ)
    | #"k" => move (app, count, Cursor.viK)
    | #"l" => move (app, count, Cursor.viL)
    | #"w" => move (app, count, Cursor.nextWord)
    | #"W" => move (app, count, Cursor.nextWORD)
    | #"b" => move (app, count, Cursor.prevWord)
    | #"B" => move (app, count, Cursor.prevWORD)
    | #"e" => move (app, count, Cursor.endOfWord)
    | #"E" => move (app, count, Cursor.endOfWORD)
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
                val newApp = AppWith.mode (app, mode)
              in
                (newApp, [])
              end
            else
              move (app, 1, Cursor.vi0)
          end
        else
          move (app, 1, Cursor.vi0)
    | #"$" => move (app, 1, Cursor.viDlr)
    | #"^" => firstNonSpaceChr app
    | #"G" => 
        (* if str has a size larger than 0,
         * interpret as "go to line" command;
         * else, interpret as a command to move to end *)
         if String.size str = 0 then
           moveToEnd app
         else
           moveToLine (app, count - 1)
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
          val newApp = AppWith.mode (app, mode)
        in
          (newApp, [])
        end

  fun parseDelete (strPos, str, count, app, newCmd) =
    if strPos = String.size str - 1 then
      (* have to check newCmd *)
      case newCmd of
        CHAR_EVENT chr =>
          (case chr of
            (* terminal commands: require no input after *)
            #"h" => delete (app, count, Cursor.viH)
          | #"j" => delete (app, count, Cursor.viJ)
          | #"k" => delete (app, count, Cursor.viK)
          | #"l" => delete (app, count, Cursor.viL)
          | #"w" => delete (app, count, Cursor.nextWord)
          | #"W" => delete (app, count, Cursor.nextWORD)
          | #"b" => delete (app, count, Cursor.prevWord)
          | #"B" => delete (app, count, Cursor.prevWORD)
          | #"e" => delete (app, count, Cursor.endOfWord)
          | #"E" => delete (app, count, Cursor.endOfWORD)
          | #"0" => delete (app, 1, Cursor.vi0)
          (* todo for '$': 
           * Cursor.viDlr takes us to last chr on line
           * but it leaves last chr on line alone.
           * Have to increment by 1. *)
          | #"$" => delete (app, 1, Cursor.viDlr)
          (* todo: requires custom delete function
          | #"^" => firstNonSpaceChr app
          *)
          (* non-terminal commands which require appending chr *)
          | #"t" => appendChr (app, chr, str)
          | #"T" => appendChr (app, chr, str)
          | #"y" => appendChr (app, chr, str)
          | #"d" => appendChr (app, chr, str)
          | #"f" => appendChr (app, chr, str)
          | #"F" => appendChr (app, chr, str)
          | #"g" => appendChr (app, chr, str)
          | #"c" => appendChr (app, chr, str)
          (* invalid command: reset mode *)
          | _ => clearMode app)
      | KEY_ESC => clearMode app
      | RESIZE_EVENT (width, height) => resizeText (app, width, height)
    else
      (* have to continue parsing string *)
      (print "app-update.sml line 527 temp\n"; clearMode app)

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
    | #"y" => 
        (* yank *) 
        clearMode app
    | #"d" => 
        (* delete *) 
        parseDelete (strPos, str, count, app, newCmd)
    | #"f" =>
        (* to chr, forward *)
        handleMoveToChr (count, app, Cursor.toNextChr, newCmd)
    | #"F" =>
        (* to chr, backward *)
        handleMoveToChr (count, app, Cursor.toPrevChr, newCmd)
    | #"g" => 
        (* go *) 
        handleGo (count, app, newCmd)
    | #"c" => 
        (* change *) 
        clearMode app
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
        else if numLength + 1 < String.size str then
          (* continue parsing. *)
          parseAfterCount (numLength + 1, str, count, app, newCmd)
        else
          (* continue parsing. *)
          parseAfterCount (numLength, str, count, app, newCmd)
      end

  fun updateNormalMode (app, str, msg) = parseNormalModeCommand (app, str, msg)

  fun update (app, msg) =
    case #mode app of NORMAL_MODE str => updateNormalMode (app, str, msg)
end
