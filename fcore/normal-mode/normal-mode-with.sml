structure NormalModeWith =
struct
  open AppType

  fun bufferAndSize
    ( app: app_type
    , newBuffer
    , newWidth
    , newHeight
    , newSearchList
    , newMsgs
    , newBufferModifyTime
    , newVisualScrollColumn
    ) =
    let
      val
        { mode
        , buffer = _
        , bufferModifyTime = _
        , windowWidth = _
        , windowHeight = _
        , searchList = _
        , visualScrollColumn = _
        , msgs = _
        , searchString
        , startLine
        , cursorIdx
        } = app
    in
      { mode = mode
      , buffer = newBuffer
      , bufferModifyTime = newBufferModifyTime
      , windowWidth = newWidth
      , windowHeight = newHeight
      , searchList = newSearchList
      , visualScrollColumn = newVisualScrollColumn
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
    , newBufferModifyTime
    , newVisualScrollColumn
    ) =
    let
      val
        { mode = _
        , buffer = _
        , bufferModifyTime = _
        , cursorIdx = _
        , startLine = _
        , searchList = _
        , visualScrollColumn = _
        , msgs = _
        , searchString
        , windowWidth
        , windowHeight
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , bufferModifyTime = newBufferModifyTime
      , cursorIdx = newCursorIdx
      , startLine = newStartLine
      , searchList = newSearchList
      , visualScrollColumn = newVisualScrollColumn
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
        , bufferModifyTime
        , searchList
        , searchString
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , visualScrollColumn
        } = app
    in
      { mode = newMode
      , msgs = newMsgs
      , buffer = buffer
      , bufferModifyTime = bufferModifyTime
      , searchList = searchList
      , searchString = searchString
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      , visualScrollColumn = visualScrollColumn
      }
    end

  fun modeAndBuffer (app: app_type, newBuffer, newMode, newMsgs) =
    let
      val
        { mode = _
        , msgs = _
        , buffer = _
        , bufferModifyTime
        , searchList
        , searchString
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , visualScrollColumn
        } = app
    in
      { mode = newMode
      , msgs = newMsgs
      , buffer = newBuffer
      , bufferModifyTime = bufferModifyTime
      , searchList = searchList
      , searchString = searchString
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      , visualScrollColumn = visualScrollColumn
      }
    end


  fun searchList
    ( app: app_type
    , newSearchList
    , newBuffer
    , newSearchString
    , newBufferModifyTime
    ) =
    let
      val
        { searchList = _
        , buffer = _
        , bufferModifyTime
        , searchString = _
        , msgs
        , mode
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , visualScrollColumn
        } = app
    in
      { searchList = newSearchList
      , buffer = newBuffer
      , bufferModifyTime = newBufferModifyTime
      , searchString = newSearchString
      , msgs = msgs
      , mode = mode
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      , visualScrollColumn = visualScrollColumn
      }
    end
end
