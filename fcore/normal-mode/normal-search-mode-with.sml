structure NormalSearchModeWith =
struct
  open AppType

  fun returnToNormalMode
    ( app: app_type
    , newBuffer
    , newSearchList
    , newStartLine
    , newMode
    , newDfa
    , newMsgs
    ) =
    let
      val
        { mode = _
        , buffer = _
        , searchList = _
        , startLine = _
        , msgs = _
        , dfa = _
        , bufferModifyTime
        , windowWidth
        , windowHeight
        , cursorIdx
        , visualScrollColumn
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , searchList = newSearchList
      , startLine = newStartLine
      , dfa = newDfa
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
        , searchList
        , startLine = _
        , msgs = _
        , bufferModifyTime
        , windowWidth
        , windowHeight
        , cursorIdx
        , visualScrollColumn
        , dfa
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , startLine = newStartLine
      , msgs = newMsgs
      , searchList = searchList
      , bufferModifyTime = bufferModifyTime
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , cursorIdx = cursorIdx
      , visualScrollColumn = visualScrollColumn
      , dfa = dfa
      }
    end

  fun searchList (app: app_type, newSearchList) =
    let
      val
        { mode
        , buffer
        , searchList = _
        , startLine
        , msgs
        , bufferModifyTime
        , windowWidth
        , windowHeight
        , cursorIdx
        , visualScrollColumn
        , dfa
        } = app
    in
      { mode = mode
      , searchList = newSearchList
      , buffer = buffer
      , startLine = startLine
      , msgs = msgs
      , bufferModifyTime = bufferModifyTime
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , cursorIdx = cursorIdx
      , visualScrollColumn = visualScrollColumn
      , dfa = dfa
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
        , searchList
        , startLine
        , bufferModifyTime
        , cursorIdx
        , visualScrollColumn
        , dfa
        } = app
    in
      { mode = newMode
      , buffer = newBuffer
      , windowWidth = newWindowWidth
      , windowHeight = newWindowHeight
      , msgs = newMsgs
      , searchList = searchList
      , startLine = startLine
      , bufferModifyTime = bufferModifyTime
      , cursorIdx = cursorIdx
      , visualScrollColumn = visualScrollColumn
      , dfa = dfa
      }
    end
end
