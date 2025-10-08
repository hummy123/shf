structure NormalSearchFinish =
struct
  open AppType
  open DrawMsg

  fun onSearchChanged
    ( app: app_type
    , searchString
    , tempSearchList
    , searchCursorIdx
    , searchScrollColumn
    , caseSensitive
    , buffer
    ) =
    let
      val
        { buffer
        , cursorIdx
        , startLine = prevLineNumber
        , windowWidth
        , windowHeight
        , visualScrollColumn
        , ...
        } = app

      val searchScrollColumn =
        TextScroll.getScrollColumnFromString
          (searchCursorIdx, windowWidth, searchScrollColumn)

      val mode = NORMAL_SEARCH_MODE
        { searchString = searchString
        , tempSearchList = tempSearchList
        , searchCursorIdx = searchCursorIdx
        , searchScrollColumn = searchScrollColumn
        , caseSensitive = caseSensitive
        }

      val floatWindowWidth = Real32.fromInt windowWidth
      val floatWindowHeight = Real32.fromInt windowHeight

      val searchStringPosY = windowHeight - TextConstants.ySpace - 5

      val initialTextAcc = SearchBar.build
        ( searchString
        , 5
        , searchStringPosY
        , windowWidth
        , floatWindowWidth
        , floatWindowHeight
        , searchCursorIdx
        , searchScrollColumn
        , caseSensitive
        )

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorLine = LineGap.idxToLineNumber (cursorIdx, buffer)
      val startLine =
        TextScroll.getStartLine
          (prevLineNumber, cursorLine, windowHeight, #lineLength buffer)
      val buffer = LineGap.goToLine (startLine, buffer)

      val remainingWindowHeight = windowHeight - (TextConstants.ySpace * 2)

      val drawMsg = NormalModeTextBuilder.startBuild
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , remainingWindowHeight
        , floatWindowWidth
        , floatWindowHeight
        , tempSearchList
        , visualScrollColumn
        , initialTextAcc
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DrawMsg.DRAW_TEXT drawMsg
      val msgs = [MailboxType.DRAW drawMsg]
    in
      NormalSearchModeWith.changeTempSearchString
        (app, buffer, startLine, mode, msgs)
    end

  fun resize
    ( app: app_type
    , newWindowWidth
    , newWindowHeight
    , searchString
    , searchCursorIdx
    , tempSearchList
    , searchScrollColumn
    , caseSensitive
    ) =
    let
      val
        {buffer, cursorIdx, startLine = prevLineNumber, visualScrollColumn, ...} =
        app

      val floatWindowWidth = Real32.fromInt newWindowWidth
      val floatWindowHeight = Real32.fromInt newWindowHeight

      val searchScrollColumn =
        TextScroll.getScrollColumnFromString
          (searchCursorIdx, newWindowWidth, searchScrollColumn)

      val mode = NORMAL_SEARCH_MODE
        { searchString = searchString
        , tempSearchList = tempSearchList
        , searchCursorIdx = searchCursorIdx
        , searchScrollColumn = searchScrollColumn
        , caseSensitive = caseSensitive
        }

      val searchStringPosY = newWindowHeight - TextConstants.ySpace - 5

      val initialTextAcc = SearchBar.build
        ( searchString
        , 5
        , searchStringPosY
        , newWindowWidth
        , floatWindowWidth
        , floatWindowHeight
        , searchCursorIdx
        , searchScrollColumn
        , caseSensitive
        )

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorLine = LineGap.idxToLineNumber (cursorIdx, buffer)
      val startLine =
        TextScroll.getStartLine
          (prevLineNumber, cursorLine, newWindowHeight, #lineLength buffer)
      val buffer = LineGap.goToLine (startLine, buffer)

      val remainingWindowHeight = newWindowHeight - (TextConstants.ySpace * 2)

      val drawMsg = NormalModeTextBuilder.startBuild
        ( startLine
        , cursorIdx
        , buffer
        , newWindowWidth
        , remainingWindowHeight
        , floatWindowWidth
        , floatWindowHeight
        , tempSearchList
        , visualScrollColumn
        , initialTextAcc
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DrawMsg.DRAW_TEXT drawMsg
      val msgs = [MailboxType.DRAW drawMsg]
    in
      NormalSearchModeWith.bufferAndSize
        (app, mode, buffer, newWindowWidth, newWindowHeight, msgs)
    end
end
