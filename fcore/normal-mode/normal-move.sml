structure NormalMove =
struct
  open AppType

  fun moveToStart (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, searchList, searchString, ...} =
        app

      val cursorIdx = 0
      val startLine = 0
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , []
        )

      val mode = NORMAL_MODE ""
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg)
    end

  fun moveToEnd (app: app_type) =
    let
      val {buffer, windowWidth, windowHeight, searchList, searchString, ...} =
        app

      val buffer = LineGap.goToEnd buffer
      val {line = bufferLine, idx = bufferIdx, ...} = buffer

      val bufferIdx = bufferIdx - 1
      val bufferIdx = Cursor.clipIdx (buffer, bufferIdx)
      val bufferLine = bufferLine - 1

      val buffer = LineGap.goToIdx (bufferIdx, buffer)
      val bufferLine =
        let
          val maxHeight = windowHeight - TextConstants.ySpace
        in
          TextWindow.getStartLineWithCursorCentered
            (buffer, bufferIdx, bufferLine, windowWidth, maxHeight)
        end

      val buffer = LineGap.goToLine (bufferLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (bufferLine, buffer)

      val drawMsg = TextBuilder.build
        ( bufferLine
        , bufferIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , []
        )

      val mode = NORMAL_MODE ""
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, bufferIdx, mode, bufferLine, searchList, drawMsg)
    end

  fun moveToLine (app: app_type, reqLine) =
    if reqLine = 0 then
      moveToStart app
    else
      let
        val
          { windowWidth
          , windowHeight
          , buffer
          , startLine = origLine
          , searchList
          , searchString
          , ...
          } = app
        val buffer = LineGap.goToLine (reqLine, buffer)

        (* get idx of first chr after linebreak *)
        val cursorIdx = Cursor.getLineStartIdx (buffer, reqLine)

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val startLine = TextWindow.getStartLineWithCursorCentered
          (buffer, cursorIdx, origLine, windowWidth, windowHeight div 2)

        val buffer = LineGap.goToLine (startLine, buffer)
        val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)

        val drawMsg = TextBuilder.build
          ( startLine
          , cursorIdx
          , buffer
          , windowWidth
          , windowHeight
          , searchList
          , searchString
          , []
          )

        val mode = NORMAL_MODE ""
      in
        AppWith.bufferAndCursorIdx
          (app, buffer, cursorIdx, mode, startLine, searchList, drawMsg)
      end

  fun moveToMatchingPair (app: app_type) =
    let
      val
        { buffer
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , searchList
        , searchString
        , ...
        } = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.matchPair (buffer, cursorIdx)

      val buffer = LineGap.goToLine (startLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)
    in
      if
        TextWindow.isCursorVisible
          (buffer, cursorIdx, startLine, windowWidth, windowHeight)
      then
        (* if visible, just need to redraw; no need to get line *)
        let
          val drawMsg = TextBuilder.build
            ( startLine
            , cursorIdx
            , buffer
            , windowWidth
            , windowHeight
            , searchList
            , searchString
            , []
            )
        in
          AppWith.bufferAndCursorIdx
            ( app
            , buffer
            , cursorIdx
            , NORMAL_MODE ""
            , startLine
            , searchList
            , drawMsg
            )
        end
      else
        (* not visible, so need to get startLine where cursor is visible *)
        let
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val startLine = TextWindow.getStartLineWithCursorCentered
            (buffer, cursorIdx, startLine, windowWidth, windowHeight div 2)

          val buffer = LineGap.goToLine (startLine, buffer)
          val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)

          val drawMsg = TextBuilder.build
            ( startLine
            , cursorIdx
            , buffer
            , windowWidth
            , windowHeight
            , searchList
            , searchString
            , []
            )
        in
          AppWith.bufferAndCursorIdx
            ( app
            , buffer
            , cursorIdx
            , NORMAL_MODE ""
            , startLine
            , searchList
            , drawMsg
            )
        end
    end

  fun firstNonSpaceChr (app: app_type) =
    let
      val {buffer, cursorIdx, windowWidth, windowHeight, startLine, ...} = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.vi0 (buffer, cursorIdx)

      (* move cursorIdx to first character on line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.firstNonSpaceChr (buffer, cursorIdx)
    in
      Finish.buildTextAndClear (app, buffer, cursorIdx, #searchList app, [])
    end

  fun helpMoveToChr (app: app_type, buffer, cursorIdx, count, fMove, chr) =
    if count = 0 then
      Finish.buildTextAndClearAfterChr
        (app, buffer, cursorIdx, #searchList app, [])
    else
      let
        (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val newCursorIdx = fMove (buffer, cursorIdx, chr)
        val newCount = if cursorIdx = newCursorIdx then 0 else count - 1
      in
        helpMoveToChr (app, buffer, newCursorIdx, newCount, fMove, chr)
      end

  fun moveToChr (app: app_type, count, fMove, chr) =
    let val {cursorIdx, buffer, ...} = app
    in helpMoveToChr (app, buffer, cursorIdx, count, fMove, chr)
    end

  fun moveToNextMatch (app: app_type, count) =
    let
      val {cursorIdx, searchList, buffer, ...} = app
      val newCursorIdx = SearchList.nextMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 then
        Finish.clearMode app
      else
        Finish.buildTextAndClearAfterChr
          (app, buffer, newCursorIdx, searchList, [])
    end

  fun moveToPrevMatch (app: app_type, count) =
    let
      val {cursorIdx, searchList, buffer, ...} = app
      val newCursorIdx = SearchList.prevMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 then
        Finish.clearMode app
      else
        Finish.buildTextAndClearAfterChr
          (app, buffer, newCursorIdx, searchList, [])
    end

end
