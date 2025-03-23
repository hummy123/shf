structure AppWith =
struct
  open AppType

  fun bufferAndSize
    (app: app_type, newBuffer, newWidth, newHeight, newSearchList, newMsgs) =
    let
      val
        { mode
        , buffer = _
        , windowWidth = _
        , windowHeight = _
        , searchList = _
        , msgs = _
        , searchString
        , startLine
        , cursorIdx
        } = app
    in
      { mode = mode
      , buffer = newBuffer
      , windowWidth = newWidth
      , windowHeight = newHeight
      , searchList = newSearchList
      , msgs = newMsgs
      , searchString = searchString
      , startLine = startLine
      , cursorIdx = cursorIdx
      }
    end

  fun bufferAndCursorIdx
    ( app: app_type
    , newBuffer
    , newCursorIdx
    , newMode
    , newStartLine
    , newSearchList
    , newMsgs
    ) =
    let
      val
        { mode = _
        , buffer = _
        , cursorIdx = _
        , startLine = _
        , searchList = _
        , msgs = _
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
      , msgs = newMsgs
      , searchString = searchString
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      }
    end

  fun mode (app: app_type, newMode, newMsgs) =
    let
      val
        { mode = _
        , msgs = _
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
      , msgs = newMsgs
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
        , msgs
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
      , msgs = msgs
      , mode = mode
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      }
    end

  fun idx (app, newIdx) =
    let
      val
        { startLine
        , buffer
        , searchList
        , searchString
        , mode
        , windowWidth
        , windowHeight
        , msgs
        , cursorIdx = _
        } = app
    in
      { startLine = startLine
      , buffer = buffer
      , searchList = searchList
      , searchString = searchString
      , mode = mode
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , msgs = msgs
      , cursorIdx = newIdx
      }
    end
end
