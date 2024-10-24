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

  fun moveRight (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viL (buffer, cursorIdx)

      (* move LineGap to first line displayed on screen, and build new text *)
      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun moveLeft (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viH (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun moveDown (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viJ (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun moveUp (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viK (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun moveToLineStart (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.vi0 (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun moveToLineEnd (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viDlr (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end
    
  fun nextWord (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.nextWord (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end
    
  fun prevWord (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.prevWord (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, buffer, cursorIdx)
    in
      (newApp, drawMsg)
    end

  fun handleChr (app: app_type, chr) =
    case chr of
      #"h" => moveLeft app
    | #"j" => moveDown app
    | #"k" => moveUp app
    | #"l" => moveRight app
    | #"0" => moveToLineStart app
    | #"$" => moveToLineEnd app
    | #"w" => nextWord app
    | #"b" => prevWord app
    | _ => (app, [])

  fun update (app, msg) =
    case msg of
      RESIZE_EVENT (width, height) => resizeText (app, width, height)
    | CHAR_EVENT chr => handleChr (app, chr)
end
