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

  fun moveBackward (app: app_type, fMove) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = fMove (buffer, cursorIdx)

      (* todo: get new startLine if cursor has moved out of screen *)

      (* move LineGap to first line displayed on screen, and build new text *)
      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun moveFowrards (app: app_type, fMove) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = fMove (buffer, cursorIdx)

      (* todo: get new startLine if cursor has moved out of screen *)

      (* move LineGap to first line displayed on screen, and build new text *)
      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun firstNonSpaceChr (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

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

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun handleChr (app: app_type, chr) =
    case chr of
      #"h" => moveBackward (app, Cursor.viH) 
    | #"j" => moveFowrards (app, Cursor.viJ)
    | #"k" => moveBackward (app, Cursor.viK)
    | #"l" => moveFowrards (app, Cursor.viL)
    | #"0" => moveBackward (app, Cursor.vi0)
    | #"$" => moveFowrards (app, Cursor.viDlr)
    | #"w" => moveFowrards (app, Cursor.nextWord)
    | #"W" => moveFowrards (app, Cursor.nextWORD)
    | #"b" => moveBackward (app, Cursor.prevWord)
    | #"B" => moveBackward (app, Cursor.prevWORD)
    | #"e" => moveFowrards (app, Cursor.endOfWord)
    | #"E" => moveFowrards (app, Cursor.endOfWORD)
    | #"^" => firstNonSpaceChr app
    | _ => (app, [])

  fun update (app, msg) =
    case msg of
      RESIZE_EVENT (width, height) => resizeText (app, width, height)
    | CHAR_EVENT chr => handleChr (app, chr)
end
