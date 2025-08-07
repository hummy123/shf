structure Finish =
struct
  open AppType

  open MailboxType
  open DrawMsg
  open InputMsg

  fun clearMode app =
    AppWith.mode (app, NORMAL_MODE "", [])

  fun buildTextAndClear (app: app_type, buffer, cursorIdx, searchList, msgs) =
    let
      val {windowWidth, windowHeight, startLine, searchString, ...} = app

      (* move LineGap to first line displayed on screen *)
      val buffer = LineGap.goToLine (startLine, buffer)

      (* get new startLine which may move screen depending on cursor movements *)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

      (* move buffer to new startLine as required by TextBuilder.build *)
      val buffer = LineGap.goToLine (startLine, buffer)

      val msgs = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , searchString
        , msgs
        )

      val mode = NORMAL_MODE ""
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, mode, startLine, searchList, msgs)
    end

  fun withSearchList (app: app_type, searchList) =
    let
      val {buffer, searchString, cursorIdx, ...} = app
      val app = AppWith.searchList (app, searchList, buffer, searchString)
    in
      buildTextAndClear (app, buffer, cursorIdx, searchList, [])
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
        , searchString
        , ...
        } = app

      val newBuffer = LineGap.goToLine (startLine, buffer)
      val lineIdx = TextBuilder.getLineAbsIdx (startLine, buffer)

      val drawMsg = TextBuilder.build
        ( startLine
        , cursorIdx
        , newBuffer
        , newWidth
        , newHeight
        , searchList
        , searchString
        , []
        )
    in
      AppWith.bufferAndSize
        (app, newBuffer, newWidth, newHeight, searchList, drawMsg)
    end

  (* Difference between this and buildTextAndClear is that 
   * this is meant to be called after a chr movement, 
   * where the cursor may possibly jump off window by a wide marigin.
   * Since the cursor may move away a lot, it is best to recenter.
   * *)
  fun buildTextAndClearAfterChr
    (app: app_type, buffer, cursorIdx, searchList, initialMsg) =
    let
      val {windowWidth, windowHeight, startLine, searchString, ...} = app

      (* move LineGap to first line displayed on screen *)
      val buffer = LineGap.goToLine (startLine, buffer)

      (* get new startLine which may move screen depending on cursor movements *)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

      (* move buffer to new startLine as required by TextBuilder.build 
      * and move searchList to idx where line starts as well *)
      val buffer = LineGap.goToLine (startLine, buffer)

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

  fun centreToCursor (app: app_type) =
    let
      val
        { buffer
        , windowWidth
        , windowHeight
        , startLine = origLine
        , cursorIdx
        , searchList
        , searchString
        , ...
        } = app
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
    in
      AppWith.bufferAndCursorIdx
        (app, buffer, cursorIdx, NORMAL_MODE "", startLine, searchList, drawMsg)
    end
end
