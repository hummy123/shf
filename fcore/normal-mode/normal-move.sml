structure NormalMove =
struct
  open AppType

  fun moveToStart (app: app_type) : AppType.app_type =
    let
      val
        { buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , bufferModifyTime
        , visualScrollColumn
        , ...
        } = app

      val cursorIdx = 0
      val startLine = 0
      val buffer = LineGap.goToStart buffer

      val drawMsg = NormalModeTextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , visualScrollColumn
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DrawMsg.DRAW_TEXT drawMsg
      val drawMsg = [MailboxType.DRAW drawMsg]

      val mode = NORMAL_MODE ""
    in
      NormalModeWith.bufferAndCursorIdx
        ( app
        , buffer
        , cursorIdx
        , mode
        , startLine
        , searchList
        , drawMsg
        , bufferModifyTime
        , 0
        )
    end

  fun moveToEnd (app: app_type) =
    let
      val
        { buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , bufferModifyTime
        , visualScrollColumn = prevScrollColumn
        , startLine = prevLineNumber
        , ...
        } = app

      val buffer = LineGap.goToEnd buffer
      val {line = bufferLine, textLength, ...} = buffer

      val bufferIdx = Int.max (0, textLength - 2)
      val bufferLine = bufferLine - 1

      val buffer = LineGap.goToIdx (bufferIdx, buffer)
      val visualScrollColumn =
        TextScroll.getScrollColumn
          (buffer, bufferIdx, windowWidth, prevScrollColumn)

      val bufferLine =
        TextScroll.getStartLine (prevLineNumber, bufferLine, windowHeight)
      val buffer = LineGap.goToLine (bufferLine, buffer)

      val drawMsg = NormalModeTextBuilder.build
        ( bufferLine
        , bufferIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , visualScrollColumn
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DrawMsg.DRAW_TEXT drawMsg
      val drawMsg = [MailboxType.DRAW drawMsg]

      val mode = NORMAL_MODE ""
    in
      NormalModeWith.bufferAndCursorIdx
        ( app
        , buffer
        , bufferIdx
        , mode
        , bufferLine
        , searchList
        , drawMsg
        , bufferModifyTime
        , visualScrollColumn
        )
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
          , startLine = prevLineNumber
          , searchList
          , searchString
          , bufferModifyTime
          , visualScrollColumn = prevScrollColumn
          , ...
          } = app
        val buffer = LineGap.goToLine (reqLine, buffer)

        (* get idx of first chr after linebreak *)
        val cursorIdx = Cursor.getLineStartIdx (buffer, reqLine)

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val visualScrollColumn =
          TextScroll.getScrollColumn
            (buffer, cursorIdx, windowWidth, prevScrollColumn)

        val cursorLine = LineGap.getLineNumberOfIdx (cursorIdx, buffer)
        val startLine =
          TextScroll.getStartLine (prevLineNumber, cursorLine, windowHeight)

        val buffer = LineGap.goToLine (startLine, buffer)

        val drawMsg = NormalModeTextBuilder.build
          ( startLine
          , cursorIdx
          , buffer
          , windowWidth
          , windowHeight
          , searchList
          , searchString
          , visualScrollColumn
          )
        val drawMsg = Vector.concat drawMsg
        val drawMsg = DrawMsg.DRAW_TEXT drawMsg
        val drawMsg = [MailboxType.DRAW drawMsg]

        val mode = NORMAL_MODE ""
      in
        NormalModeWith.bufferAndCursorIdx
          ( app
          , buffer
          , cursorIdx
          , mode
          , startLine
          , searchList
          , drawMsg
          , bufferModifyTime
          , visualScrollColumn
          )
      end

  fun moveToMatchingPair (app: app_type) =
    let
      val
        { buffer
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine = prevLineNumber
        , searchList
        , searchString
        , bufferModifyTime
        , visualScrollColumn = prevScrollColumn
        , ...
        } = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.matchPair (buffer, cursorIdx)
    in
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val visualScrollColumn =
          TextScroll.getScrollColumn
            (buffer, cursorIdx, windowWidth, prevScrollColumn)

        val cursorLine = LineGap.getLineNumberOfIdx (cursorIdx, buffer)
        val startLine =
          TextScroll.getStartLine (prevLineNumber, cursorLine, windowHeight)

        val buffer = LineGap.goToLine (startLine, buffer)

        val drawMsg = NormalModeTextBuilder.build
          ( startLine
          , cursorIdx
          , buffer
          , windowWidth
          , windowHeight
          , searchList
          , searchString
          , visualScrollColumn
          )
        val drawMsg = Vector.concat drawMsg
        val drawMsg = DrawMsg.DRAW_TEXT drawMsg
        val drawMsg = [MailboxType.DRAW drawMsg]
      in
        NormalModeWith.bufferAndCursorIdx
          ( app
          , buffer
          , cursorIdx
          , NORMAL_MODE ""
          , startLine
          , searchList
          , drawMsg
          , bufferModifyTime
          , visualScrollColumn
          )
      end
    end

  fun firstNonSpaceChr (app: app_type) =
    let
      val
        { buffer
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , searchList
        , bufferModifyTime
        , ...
        } = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.vi0 (buffer, cursorIdx)

      (* move cursorIdx to first character on line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.firstNonSpaceChr (buffer, cursorIdx)
    in
      NormalFinish.buildTextAndClear
        (app, buffer, cursorIdx, searchList, [], bufferModifyTime)
    end

  fun helpMoveToChr (app: app_type, buffer, cursorIdx, count, fMove, chr) =
    if count = 0 then
      NormalFinish.buildTextAndClear
        (app, buffer, cursorIdx, #searchList app, [], #bufferModifyTime app)
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
      val
        { cursorIdx
        , searchList
        , buffer
        , bufferModifyTime
        , visualScrollColumn
        , ...
        } = app
      val newCursorIdx = SearchList.nextMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        NormalFinish.buildTextAndClear
          (app, buffer, newCursorIdx, searchList, [], bufferModifyTime)
    end

  fun moveToPrevMatch (app: app_type, count) =
    let
      val {cursorIdx, searchList, buffer, bufferModifyTime, ...} = app
      val newCursorIdx = SearchList.prevMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        NormalFinish.buildTextAndClear
          (app, buffer, newCursorIdx, searchList, [], bufferModifyTime)
    end
end
