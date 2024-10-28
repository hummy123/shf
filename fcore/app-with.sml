structure AppWith =
struct
  open AppType

  fun startLine (app: app_type, startLine, newBuffer) =
    let
      val
        { startLine = _
        , buffer = _
        , mode
        , windowWidth
        , windowHeight
        , cursorIdx
        } = app
    in
      { startLine = startLine
      , buffer = newBuffer
      , mode = mode
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , cursorIdx = cursorIdx
      }
    end

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

  fun bufferAndCursorIdx
    (app: app_type, newBuffer, newCursorIdx, newMode, newStartLine) =
    let
      val
        { mode = _
        , buffer = _
        , cursorIdx = _
        , windowWidth
        , windowHeight
        , startLine = _
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , cursorIdx = newCursorIdx
      , startLine = newStartLine
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      }
    end

  fun mode (app: app_type, newMode) =
    let
      val {mode = _, buffer, cursorIdx, windowWidth, windowHeight, startLine} =
        app
    in
      { mode = newMode
      , buffer = buffer
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      }
    end
end
