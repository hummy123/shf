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

  fun bufferAndCursorIdx (app: app_type, newBuffer, newCursorIdx, newColumn) =
    let
      val
        { buffer = _
        , cursorIdx = _
        , preferredColumn = _
        , windowWidth
        , windowHeight
        , startLine
        } = app
    in
      { buffer = newBuffer
      , cursorIdx = newCursorIdx
      , preferredColumn = newColumn
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      }
    end
end
