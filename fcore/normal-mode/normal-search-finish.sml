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
        )

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorLine = LineGap.getLineNumberOfIdx (cursorIdx, buffer)
      val startLine =
        TextScroll.getStartLine (prevLineNumber, cursorLine, windowHeight)
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
        , searchString
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
    , searchCursorIdx
    , tempSearchList
    , searchScrollColumn
    ) =
    let
      val
        { buffer
        , cursorIdx
        , startLine = prevLineNumber
        , searchString
        , visualScrollColumn
        , ...
        } = app

      val floatWindowWidth = Real32.fromInt newWindowWidth
      val floatWindowHeight = Real32.fromInt newWindowHeight

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
        )

      val cursor =
        let
          val xpos = TextConstants.xSpace * (searchCursorIdx + 1) + 5
          val x = Real32.fromInt xpos
          val y = Real32.fromInt searchStringPosY
          val r: Real32.real = 0.67
          val g: Real32.real = 0.51
          val b: Real32.real = 0.83
        in
          PipeCursor.lerp
            ( x
            , y
            , 0.01
            , TextConstants.scale
            , floatWindowWidth
            , floatWindowHeight
            , r
            , g
            , b
            )
        end

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorLine = LineGap.getLineNumberOfIdx (cursorIdx, buffer)
      val startLine =
        TextScroll.getStartLine (prevLineNumber, cursorLine, newWindowHeight)
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
        , searchString
        , visualScrollColumn
        , cursor :: initialTextAcc
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DrawMsg.DRAW_TEXT drawMsg
      val msgs = [MailboxType.DRAW drawMsg]
    in
      NormalSearchModeWith.bufferAndSize
        (app, buffer, newWindowWidth, newWindowHeight, msgs)
    end
end
