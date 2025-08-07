structure Finish =
struct
  open AppType

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
end
