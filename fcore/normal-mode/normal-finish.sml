structure NormalFinish =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

  fun clearMode app =
    NormalModeWith.mode (app, NORMAL_MODE "", [])

  fun buildTextAndClear
    (app: app_type, buffer, cursorIdx, searchList, msgs, bufferModifyTime) =
    let
      val
        { windowWidth
        , windowHeight
        , visualScrollColumn = prevScrollColumn
        , startLine = prevLineNumber
        , ...
        } = app

      (* calculate new scroll column and start line, if there are any changes *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val visualScrollColumn =
        TextScroll.getScrollColumn
          (buffer, cursorIdx, windowWidth, prevScrollColumn)

      val cursorLine = LineGap.idxToLineNumber (cursorIdx, buffer)
      val startLine =
        TextScroll.getStartLine
          (prevLineNumber, cursorLine, windowHeight, #lineLength buffer)

      (* move buffer to new startLine as required by TextBuilder.build *)
      val buffer = LineGap.goToLine (startLine, buffer)

      val drawMsg = NormalModeTextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , visualScrollColumn
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DRAW_TEXT drawMsg
      val msgs = DRAW drawMsg :: msgs

      val mode = NORMAL_MODE ""
    in
      NormalModeWith.bufferAndCursorIdx
        ( app
        , buffer
        , cursorIdx
        , mode
        , startLine
        , searchList
        , msgs
        , bufferModifyTime
        , visualScrollColumn
        )
    end

  fun resizeText (app: app_type, newWidth, newHeight) =
    let
      val
        { buffer
        , windowWidth
        , windowHeight
        , startLine
        , cursorIdx
        , searchList
        , bufferModifyTime
        , visualScrollColumn = prevScrollColumn
        , ...
        } = app

      val newBuffer = LineGap.goToIdx (cursorIdx, buffer)
      val visualScrollColumn =
        TextScroll.getScrollColumn
          (newBuffer, cursorIdx, newWidth, prevScrollColumn)
      val newBuffer = LineGap.goToLine (startLine, newBuffer)
      val lineIdx = TextBuilderUtils.getLineAbsIdxFromBuffer (startLine, buffer)

      val drawMsg = NormalModeTextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , newWidth
        , newHeight
        , searchList
        , visualScrollColumn
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DRAW_TEXT drawMsg
      val msgs = [DRAW drawMsg]
    in
      NormalModeWith.bufferAndSize
        ( app
        , newBuffer
        , newWidth
        , newHeight
        , searchList
        , msgs
        , bufferModifyTime
        , visualScrollColumn
        )
    end

  fun centreToCursor (app: app_type) =
    let
      val
        { buffer
        , windowWidth
        , windowHeight
        , startLine = prevLineNumber
        , cursorIdx
        , searchList
        , bufferModifyTime
        , visualScrollColumn
        , ...
        } = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorLine = LineGap.idxToLineNumber (cursorIdx, buffer)
      val startLine = TextScroll.getLineCentre (cursorLine, windowHeight)

      val buffer = LineGap.goToLine (startLine, buffer)
      val drawMsg = NormalModeTextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , visualScrollColumn
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DRAW_TEXT drawMsg
      val drawMsg = [DRAW drawMsg]
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
        , #visualScrollColumn app
        )
    end
end
