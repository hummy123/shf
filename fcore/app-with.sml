structure AppWith =
struct
  open AppType

  fun bufferAndSize (app: app_type, newBuffer, newWidth, newHeight) =
    let
      val {buffer = _, windowWidth = _, windowHeight = _, startLine, cursorIdx} =
        app
    in
      { buffer = newBuffer
      , windowWidth = newWidth
      , windowHeight = newHeight
      , startLine = startLine
      , cursorIdx = cursorIdx
      }
    end
end
