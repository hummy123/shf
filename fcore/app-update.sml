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
        (* todo: get new startLine if cursor has moved out of screen *)

        (* move LineGap to first line displayed on screen, and build new text *)
        val buffer = LineGap.goToLine (startLine, buffer)
        val drawMsg = TextBuilder.build
          (startLine, cursorIdx, buffer, windowWidth, windowHeight)

        val mode = NORMAL_MODE ""
        val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx, mode)
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
        val drawMsg = TextBuilder.build
          (startLine, cursorIdx, buffer, windowWidth, windowHeight)

        val mode = NORMAL_MODE ""
        val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx, mode)
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
      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx, mode)
    in
      (newApp, drawMsg)
    end

  (* number of characters which are integers *)
  fun getNumLength (pos, str) =
    if pos = String.size str then
      pos
    else
      let
        val chr = String.sub (str, pos)
      in
        if chr >= #"0" andalso chr <= #"9" then getNumLength (pos + 1, str)
        else pos
      end

  fun handleChr (app: app_type, count, chr, str) =
    case chr of
      #"h" => moveBackward (app, count, Cursor.viH)
    | #"j" => moveForwards (app, count, Cursor.viJ)
    | #"k" => moveBackward (app, count, Cursor.viK)
    | #"l" => moveForwards (app, count, Cursor.viL)
    | #"0" => moveBackward (app, 1, Cursor.vi0)
    | #"$" => moveForwards (app, 1, Cursor.viDlr)
    | #"w" => moveForwards (app, count, Cursor.nextWord)
    | #"W" => moveForwards (app, count, Cursor.nextWORD)
    | #"b" => moveBackward (app, count, Cursor.prevWord)
    | #"B" => moveBackward (app, count, Cursor.prevWORD)
    | #"e" => moveForwards (app, count, Cursor.endOfWord)
    | #"E" => moveForwards (app, count, Cursor.endOfWORD)
    | #"^" => firstNonSpaceChr app
    | _ =>
        (* user may be entering a cmd with more than one chr
         * such as "2dw" to delete two word
         * so add current chr to mode, and save it in the app state *)
        let
          val str =
            if chr >= #"0" andalso chr <= #"9" then str ^ Char.toString chr
            else ""
          val mode = NORMAL_MODE str
          val newApp = AppWith.mode (app, mode)
        in
          (newApp, [])
        end

  fun parseNormalModeCommand (app, str, newCmd) =
    if String.size str = 0 then
      case newCmd of
        RESIZE_EVENT (width, height) => resizeText (app, width, height)
      | CHAR_EVENT chr => handleChr (app, 1, chr, str)
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
        else
          (* todo: continue parsing. *)
          raise Match
      end

  fun updateNormalMode (app, str, msg) = parseNormalModeCommand (app, str, msg)

  fun update (app, msg) =
    case #mode app of NORMAL_MODE str => updateNormalMode (app, str, msg)
end
