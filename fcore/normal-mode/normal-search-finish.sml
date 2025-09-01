structure NormalSearchFinish =
struct
  open AppType

  fun onSearchChanged
    (app: app_type, searchString, tempSearchList, searchCursorIdx, buffer) =
    let
      open DrawMsg

      val {buffer, cursorIdx, startLine, windowWidth, windowHeight, ...} = app
      val mode = NORMAL_SEARCH_MODE
        { searchString = searchString
        , tempSearchList = tempSearchList
        , searchCursorIdx = searchCursorIdx
        }

      val floatWindowWidth = Real32.fromInt windowWidth
      val floatWindowHeight = Real32.fromInt windowHeight

      val searchStringPosY = windowHeight - TextConstants.ySpace - 5

      val initialTextAcc = TextBuilder.buildLineToList
        ( searchString
        , 5
        , searchStringPosY
        , windowWidth
        , floatWindowWidth
        , floatWindowHeight
        )

      val buffer = LineGap.goToLine (startLine, buffer)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

      val remainingWindowHeight = windowHeight - (TextConstants.ySpace * 2)

      val msgs = TextBuilder.buildWithExisting
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , remainingWindowHeight
        , floatWindowWidth
        , floatWindowHeight
        , tempSearchList
        , searchString
        , []
        , initialTextAcc
        , []
        )
    in
      NormalSearchModeWith.changeTempSearchString
        (app, buffer, startLine, mode, msgs)
    end

end
