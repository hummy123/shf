structure AppWith =
struct
  open AppType

  fun bufferAndSize (app: app_type, newBuffer, newWidth, newHeight) =
    let
      val
        { mode
        , buffer = _
        , windowWidth = _
        , windowHeight = _
        , startLine
        , cursorIdx
        } = app
    in
      { mode = mode
      , buffer = newBuffer
      , windowWidth = newWidth
      , windowHeight = newHeight
      , startLine = startLine
      , cursorIdx = cursorIdx
      }
    end

  fun bufferAndCursorIdx (app: app_type, newBuffer, newCursorIdx) =
    let
      val
        {mode, buffer = _, cursorIdx = _, windowWidth, windowHeight, startLine} =
        app
    in
      { mode = mode
      , buffer = newBuffer
      , cursorIdx = newCursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      }
    end
end
