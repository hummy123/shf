structure NormalSearchModeWith =
struct
  open AppType

  fun returnToNormalMode
    ( app: app_type
    , newBuffer
    , newSearchString
    , newSearchList
    , newStartLine
    , newMode
    , newMsgs
    ) =
    let
      val
        { mode = _
        , buffer = _
        , searchString = _
        , searchList = _
        , startLine = _
        , msgs = _
        , bufferModifyTime
        , windowWidth
        , windowHeight
        , cursorIdx
        , visualScrollColumn
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , searchString = newSearchString
      , searchList = newSearchList
      , startLine = newStartLine
      , bufferModifyTime = bufferModifyTime
      , msgs = newMsgs
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , cursorIdx = cursorIdx
      , visualScrollColumn = visualScrollColumn
      }
    end

  fun changeTempSearchString
    (app: app_type, newBuffer, newStartLine, newMode, newMsgs) =
    let
      val
        { mode = _
        , buffer = _
        , searchString
        , searchList
        , startLine = _
        , msgs = _
        , bufferModifyTime
        , windowWidth
        , windowHeight
        , cursorIdx
        , visualScrollColumn
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , startLine = newStartLine
      , msgs = newMsgs
      , searchString = searchString
      , searchList = searchList
      , bufferModifyTime = bufferModifyTime
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , cursorIdx = cursorIdx
      , visualScrollColumn = visualScrollColumn
      }
    end

  fun searchList (app: app_type, newSearchList) =
    let
      val
        { mode
        , buffer
        , searchString
        , searchList = _
        , startLine
        , msgs
        , bufferModifyTime
        , windowWidth
        , windowHeight
        , cursorIdx
        , visualScrollColumn
        } = app
    in
      { mode = mode
      , searchList = newSearchList
      , buffer = buffer
      , startLine = startLine
      , msgs = msgs
      , searchString = searchString
      , bufferModifyTime = bufferModifyTime
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , cursorIdx = cursorIdx
      , visualScrollColumn = visualScrollColumn
      }
    end

  fun bufferAndSize
    ( app: app_type
    , newMode
    , newBuffer
    , newWindowWidth
    , newWindowHeight
    , newMsgs
    ) =
    let
      val
        { mode = _
        , windowWidth = _
        , windowHeight = _
        , msgs = _
        , buffer = _
        , searchString
        , searchList
        , startLine
        , bufferModifyTime
        , cursorIdx
        , visualScrollColumn
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , windowWidth = newWindowWidth
      , windowHeight = newWindowHeight
      , msgs = newMsgs
      , searchList = searchList
      , startLine = startLine
      , searchString = searchString
      , bufferModifyTime = bufferModifyTime
      , cursorIdx = cursorIdx
      , visualScrollColumn = visualScrollColumn
      }
    end
end
