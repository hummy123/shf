structure AppWith =
struct
  open AppType

  fun startLine (app: app_type, startLine, newBuffer) =
    let
      val
        { startLine = _
        , buffer = _
        , searchList
        , searchString
        , mode
        , windowWidth
        , windowHeight
        , cursorIdx
        } = app
    in
      { startLine = startLine
      , buffer = newBuffer
      , searchList = searchList
      , searchString = searchString
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
        , searchList
        , searchString
        , startLine
        , cursorIdx
        } = app
    in
      { mode = mode
      , buffer = newBuffer
      , windowWidth = newWidth
      , windowHeight = newHeight
      , searchList = searchList
      , searchString = searchString
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
        , startLine = _
        , searchList
        , searchString
        , windowWidth
        , windowHeight
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , cursorIdx = newCursorIdx
      , startLine = newStartLine
      , searchList = searchList
      , searchString = searchString
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      }
    end

  fun onDelete
    (app: app_type, newBuffer, newCursorIdx, newMode, newStartLine, newSearchList) =
    let
      val
        { mode = _
        , buffer = _
        , cursorIdx = _
        , startLine = _
        , searchList = _
        , searchString
        , windowWidth
        , windowHeight
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , cursorIdx = newCursorIdx
      , startLine = newStartLine
      , searchList = newSearchList
      , searchString = searchString
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      }
    end

  fun mode (app: app_type, newMode) =
    let
      val
        { mode = _
        , buffer
        , searchList
        , searchString
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        } = app
    in
      { mode = newMode
      , buffer = buffer
      , searchList = searchList
      , searchString = searchString
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      }
    end

  fun searchList (app: app_type, newSearchList, newBuffer, newSearchString) =
    let
      val
        { searchList = _
        , buffer = _
        , searchString = _
        , mode
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        } = app
    in
      { searchList = newSearchList
      , buffer = newBuffer
      , searchString = newSearchString
      , mode = mode
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      }
    end
end
