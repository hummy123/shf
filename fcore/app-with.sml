structure AppWith =
struct
  open AppType

  fun bufferAndSize (app: app_type, newBuffer, newWidth, newHeight) =
    let
      val
        { buffer = _
        , windowWidth = _
        , windowHeight = _
        , startLine
        , cursorIdx
        , preferredColumn
        } = app
    in
      { buffer = newBuffer
      , windowWidth = newWidth
      , windowHeight = newHeight
      , startLine = startLine
      , cursorIdx = cursorIdx
      , preferredColumn = preferredColumn
      }
    end

  fun bufferAndCursorIdx (app: app_type, newBuffer, newCursorIdx) =
    let
      val
        { buffer = _
        , cursorIdx = _
        , windowWidth
        , windowHeight
        , startLine
        , preferredColumn
        } = app
    in
      { buffer = newBuffer
      , cursorIdx = newCursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      , preferredColumn = preferredColumn
      }
    end
end
