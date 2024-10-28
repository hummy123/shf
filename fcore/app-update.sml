structure AppUpdate =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

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

  fun helpMoveBackward (app: app_type, buffer, cursorIdx, count, fMove) =
    if count = 0 then
      let
        val {windowWidth, windowHeight, startLine, ...} = app

        (* move LineGap to first line displayed on screen, and build new text *)
        val buffer = 
          LineGap.goToLine (startLine, buffer)

        val startLine = TextWindow.getStartLine
          (buffer, startLine, cursorIdx, windowWidth, windowHeight)

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
      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val cursorIdx = fMove (buffer, cursorIdx)
      in
        helpMoveBackward (app, buffer, cursorIdx, count - 1, fMove)
      end

  fun moveBackward (app: app_type, count, fMove) =
    let val {cursorIdx, buffer, ...} = app
    in helpMoveBackward (app, buffer, cursorIdx, count, fMove)
    end

  fun helpMoveForwards (app: app_type, buffer, cursorIdx, count, fMove) =
    if count = 0 then
      let
        val {windowWidth, windowHeight, startLine, ...} = app
        (* todo: get new startLine if cursor has moved out of screen *)

        (* move LineGap to first line displayed on screen, and build new text *)
        val buffer = LineGap.goToLine (startLine, buffer)

        val startLine = TextWindow.getStartLine
          (buffer, startLine, cursorIdx, windowWidth, windowHeight)

        val drawMsg = TextBuilder.build
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
        val cursorIdx = fMove (buffer, cursorIdx)
      in
        helpMoveForwards (app, buffer, cursorIdx, count - 1, fMove)
      end

  fun moveForwards (app: app_type, count, fMove) =
    let val {cursorIdx, buffer, ...} = app
    in helpMoveForwards (app, buffer, cursorIdx, count, fMove)
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

      (* todo: get new startLine if cursor has moved out of screen *)

      (* move LineGap to first line displayed on screen, and build new text *)
      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val mode = NORMAL_MODE ""
      val newApp = AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine)
    in
      (newApp, drawMsg)
    end

  (* number of characters which are integers *)
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

  fun moveLine (app: app_type, f) =
    let
      val {cursorIdx, buffer, windowWidth, windowHeight, startLine, ...} = app
      val startLine = f (startLine, 1)

      val buffer = LineGap.goToLine (startLine, buffer)
      val newApp = AppWith.startLine (app, startLine, buffer)

      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)
    in
      (newApp, drawMsg)
    end

  fun handleChr (app: app_type, count, chr, str) =
    case chr of
      #"h" => moveBackward (app, count, Cursor.viH)
    | #"j" => moveForwards (app, count, Cursor.viJ)
    | #"k" => moveBackward (app, count, Cursor.viK)
    | #"l" => moveForwards (app, count, Cursor.viL)
    | #"w" => moveForwards (app, count, Cursor.nextWord)
    | #"W" => moveForwards (app, count, Cursor.nextWORD)
    | #"b" => moveBackward (app, count, Cursor.prevWord)
    | #"B" => moveBackward (app, count, Cursor.prevWORD)
    | #"e" => moveForwards (app, count, Cursor.endOfWord)
    | #"E" => moveForwards (app, count, Cursor.endOfWORD)
    (* PLACEHOLDER *)
    | #"," => moveLine (app, op+)
    | #"." => moveLine (app, op-)
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
              moveBackward (app, 1, Cursor.vi0)
          end
        else
          moveBackward (app, 1, Cursor.vi0)
    | #"$" => moveForwards (app, 1, Cursor.viDlr)
    | #"^" => firstNonSpaceChr app
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

  fun helpMoveToChrNext (app: app_type, buffer, cursorIdx, count, fMove, chr) =
    if count = 0 then
      let
        val {windowWidth, windowHeight, startLine, ...} = app
        (* todo: get new startLine if cursor has moved out of screen *)

        (* move LineGap to first line displayed on screen, and build new text *)
        val buffer = LineGap.goToLine (startLine, buffer)
        val drawMsg = TextBuilder.build
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
        helpMoveToChrNext (app, buffer, cursorIdx, count - 1, fMove, chr)
      end

  fun moveToChrNext (app: app_type, count, fMove, chr) =
    let val {cursorIdx, buffer, ...} = app
    in helpMoveToChrNext (app, buffer, cursorIdx, count, fMove, chr)
    end

  (* temp placeholder function *)
  fun clearMode app =
    let
      val mode = NORMAL_MODE ""
      val newApp = AppWith.mode (app, mode)
    in
      (newApp, [])
    end

  fun handleNextChr (count, app, fMove, newCmd) =
    case newCmd of
      CHAR_EVENT chr => moveToChrNext (app, count, fMove, chr)
    | RESIZE_EVENT (width, height) => resizeText (app, width, height)

  fun handleGo (count, app, newCmd) =
    case newCmd of
      CHAR_EVENT chr =>
        (case chr of
           #"e" => moveBackward (app, count, Cursor.endOfPrevWord)
         | #"E" => moveBackward (app, count, Cursor.endOfPrevWORD)
         | _ => clearMode app)
    | RESIZE_EVENT (width, height) => resizeText (app, width, height)

  (* useful reference as list of non-terminal commands *)
  (* todo: actually parse, checking if there are further strings or input *)
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
        handleNextChr (1, app, Cursor.tillNextChr, newCmd)
    | #"T" =>
        (* to just before chr, backward *)
        handleNextChr (1, app, Cursor.tillPrevChr, newCmd)
    | #"y" => (* yank *) clearMode app
    | #"d" => (* delete *) clearMode app
    | #"f" =>
        (* to chr, forward *)
        handleNextChr (count, app, Cursor.toNextChr, newCmd)
    | #"F" =>
        (* to chr, backward *)
        handleNextChr (count, app, Cursor.toPrevChr, newCmd)
    | #"g" => (* go *) handleGo (count, app, newCmd)
    | #"c" => (* change *) clearMode app
    | _ =>
        (* isn't a non-terminal cmd
         * this case should never happen*)
        clearMode app

  fun parseNormalModeCommand (app, str, newCmd) =
    if String.size str = 0 then
      case newCmd of
        RESIZE_EVENT (width, height) => resizeText (app, width, height)
      | CHAR_EVENT chr => handleChr (app, 1, chr, str)
    else if String.size str = 1 then
      case newCmd of
        RESIZE_EVENT (width, height) => resizeText (app, width, height)
      | CHAR_EVENT chr =>
          (case Int.fromString str of
             SOME count => handleChr (app, count, chr, str)
           | NONE => parseAfterCount (0, str, 1, app, newCmd))
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
            RESIZE_EVENT (width, height) => resizeText (app, width, height)
          | CHAR_EVENT chr => handleChr (app, count, chr, str)
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
