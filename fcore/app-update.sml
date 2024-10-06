structure AppUpdate =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

  fun resizeText (app: app_type, newWidth, newHeight) =
    let
      val {buffer, windowWidth, windowHeight} = app
      val (textVec, newBuffer) =
        TextBuilder.build (0, buffer, newWidth, newHeight)

      val newApp =
        {buffer = newBuffer, windowWidth = newWidth, windowHeight = newHeight}
      val msg = REDRAW_TEXT textVec
    in
      (newApp, [DRAW msg])
    end

  fun update (app, msg) =
    case msg of RESIZE_EVENT (width, height) => resizeText (app, width, height)
end
