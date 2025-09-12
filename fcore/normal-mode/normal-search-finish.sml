structure NormalSearchFinish =
struct
  open AppType
  open DrawMsg

  fun onSearchChanged
    (app: app_type, searchString, tempSearchList, searchCursorIdx, buffer) =
    let
      val
        { buffer
        , cursorIdx
        , startLine
        , windowWidth
        , windowHeight
        , visualScrollColumn
        , ...
        } = app
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

      val buffer = LineGap.goToLine (startLine, buffer)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

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
        , cursor :: initialTextAcc
        )
      val drawMsg = Vector.concat drawMsg
      val msgs = [DrawMsg.DRAW_TEXT drawMsg]
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
    ) =
    let
      val {buffer, cursorIdx, startLine, searchString, visualScrollColumn, ...} =
        app

      val floatWindowWidth = Real32.fromInt newWindowWidth
      val floatWindowHeight = Real32.fromInt newWindowHeight

      val searchStringPosY = newWindowHeight - TextConstants.ySpace - 5

      val initialTextAcc = TextBuilder.buildLineToList
        ( searchString
        , 5
        , searchStringPosY
        , newWindowWidth
        , floatWindowWidth
        , floatWindowHeight
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

      val buffer = LineGap.goToLine (startLine, buffer)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, newWindowWidth, newWindowHeight)

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
      val msgs = [DrawMsg.DRAW_TEXT drawMsg]
    in
      NormalSearchModeWith.bufferAndSize
        (app, buffer, newWindowWidth, newWindowHeight, msgs)
    end
end
