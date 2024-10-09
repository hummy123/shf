structure AppUpdate =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

  fun resizeText (app: app_type, newWidth, newHeight) =
    let
      val {buffer, windowWidth, windowHeight, startLine} = app

      val newBuffer = LineGap.goToLine (startLine, buffer)
      val cursorIdx = 0 (* TEMP *)
      val textVec = TextBuilder.build
        (startLine, cursorIdx, newBuffer, newWidth, newHeight)

      val newApp = AppWith.bufferAndSize (app, newBuffer, newWidth, newHeight)
      val msg = REDRAW_TEXT textVec
    in
      (newApp, [DRAW msg])
    end

  fun update (app, msg) =
    case msg of RESIZE_EVENT (width, height) => resizeText (app, width, height)
end
