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
      }
    end
end
