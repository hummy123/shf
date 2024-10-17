structure AppUpdate =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

  fun resizeText (app: app_type, newWidth, newHeight) =
    let
      val {buffer, windowWidth, windowHeight, startLine, cursorIdx} = app

      val newBuffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, newBuffer, newWidth, newHeight)

      val newApp = AppWith.bufferAndSize (app, newBuffer, newWidth, newHeight)
    in
      (newApp, drawMsg)
    end

  fun moveRight (app: app_type) =
    let
      (* todo: proper implementation of moveRight
       * currently, we also retrieve the newCursorIdx improperly.
       * To do it properly, we have to look inside LineGap
       * instead of just incrementing cursorIdx by 1.
       * So:
       * - Find newCursorIdx from LineGap
       * *)

      val {buffer, windowWidth, windowHeight, startLine, cursorIdx} = app

      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      val newBuffer = LineGap.goToIdx (cursorIdx, buffer)
      (* todo: call to retrieve newCursorIdx should be below *)
      val newCursorIdx = cursorIdx + 1

      (* move LineGap to first line displayed on screen, and build new text *)
      val newBuffer = LineGap.goToLine (startLine, newBuffer)
      val drawMsg = TextBuilder.build
        (startLine, cursorIdx, newBuffer, windowWidth, windowHeight)

      val newApp = AppWith.bufferAndCursorIdx (app, newBuffer, newCursorIdx)
    in
      (newApp, drawMsg)
    end

  fun handleChr (app: app_type, chr) =
    case chr of
      #"l" => moveRight app
    | _ => (app, [])

  fun update (app, msg) =
    case msg of
      RESIZE_EVENT (width, height) => resizeText (app, width, height)
    | CHAR_EVENT chr => handleChr (app, chr)
end
