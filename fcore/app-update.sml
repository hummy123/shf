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
      val preferredColumn = Cursor.getCursorColumn (buffer, cursorIdx)

      (* move LineGap to first line displayed on screen, and build new text *)
      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp =
        AppWith.bufferAndCursorIdx (app, buffer, cursorIdx, preferredColumn)
    in
      (newApp, drawMsg)
    end

  fun moveLeft (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viH (buffer, cursorIdx)
      val preferredColumn = Cursor.getCursorColumn (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, buffer, windowWidth, windowHeight)

      val newApp =
        AppWith.bufferAndCursorIdx (app, buffer, cursorIdx, preferredColumn)
    in
      (newApp, drawMsg)
    end

  fun handleChr (app: app_type, chr) =
    case chr of
      #"h" => moveLeft app
    | #"l" => moveRight app
    | _ => (app, [])

  fun update (app, msg) =
    case msg of
      RESIZE_EVENT (width, height) => resizeText (app, width, height)
    | CHAR_EVENT chr => handleChr (app, chr)
end
