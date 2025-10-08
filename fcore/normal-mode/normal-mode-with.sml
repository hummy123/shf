structure NormalModeWith =
struct
  open AppType

  fun bufferMsgsAndMode (app: app_type, newBuffer, newMsgs, newMode) =
    let
      val
        { mode = _
        , buffer = _
        , msgs = _
        , bufferModifyTime
        , windowWidth
        , windowHeight
        , searchList
        , visualScrollColumn
        , startLine
        , cursorIdx
        , dfa
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , msgs = newMsgs
      , bufferModifyTime = bufferModifyTime
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , searchList = searchList
      , visualScrollColumn = visualScrollColumn
      , startLine = startLine
      , cursorIdx = cursorIdx
      , dfa = dfa
      }
    end

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
        , startLine
        , cursorIdx
        , dfa
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
      , startLine = startLine
      , cursorIdx = cursorIdx
      , dfa = dfa
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
        , windowWidth
        , windowHeight
        , dfa
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
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , dfa = dfa
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
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , visualScrollColumn
        , dfa
        } = app
    in
      { mode = newMode
      , msgs = newMsgs
      , buffer = buffer
      , bufferModifyTime = bufferModifyTime
      , searchList = searchList
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      , visualScrollColumn = visualScrollColumn
      , dfa = dfa
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
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , visualScrollColumn
        , dfa
        } = app
    in
      { mode = newMode
      , msgs = newMsgs
      , buffer = newBuffer
      , bufferModifyTime = bufferModifyTime
      , searchList = searchList
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      , visualScrollColumn = visualScrollColumn
      , dfa = dfa
      }
    end

  fun searchList (app: app_type, newSearchList, newBuffer, newBufferModifyTime) =
    let
      val
        { searchList = _
        , buffer = _
        , bufferModifyTime
        , msgs
        , mode
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , visualScrollColumn
        , dfa
        } = app
    in
      { searchList = newSearchList
      , buffer = newBuffer
      , bufferModifyTime = newBufferModifyTime
      , msgs = msgs
      , mode = mode
      , cursorIdx = cursorIdx
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , startLine = startLine
      , visualScrollColumn = visualScrollColumn
      , dfa = dfa
      }
    end
end
