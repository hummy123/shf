structure TextScroll =
struct
  structure TC = TextConstants

  (* Preqreuisite: move buffer to cursorIdx *)
  fun getScrollColumn (buffer, cursorIdx, windowWidth, prevScrollColumn) =
    let
      val startOfLine = Cursor.vi0 (buffer, cursorIdx)
      val newColumn = cursorIdx - startOfLine
      val howManyColumnsCanWeFit =
        if windowWidth >= TC.textLineWidth then TC.textLineCount
        else windowWidth div TC.xSpace
      val howManyColumnsCanWeFit = howManyColumnsCanWeFit - 1
    in
      if newColumn < prevScrollColumn then
        (* we are moving the cursor backwards
         * so make sure that newColumn is on the left side *)
        newColumn
      else if newColumn > prevScrollColumn + howManyColumnsCanWeFit then
        (* we are scrolling forwards *)
        newColumn - howManyColumnsCanWeFit
      else
        (* we can display the current column without moving the scroll column
         * so we do that *)
        prevScrollColumn
    end

  fun getStartLine (prevLineNumber, cursorLine, windowHeight) =
    if cursorLine <= prevLineNumber then
      (* if cursorLine is prior or same as prevLineNumber, 
       * then use cursorLine as scroll-line-start *)
      cursorLine
    else
      (* cursorLine > prevLineNumber *)
      let
        val howManyLinesWeCanFit = windowHeight div TC.ySpace
        val howManyLinesWeCanFit = howManyLinesWeCanFit - 2
      in
        if cursorLine > prevLineNumber + howManyLinesWeCanFit then
          cursorLine - howManyLinesWeCanFit
        else
          prevLineNumber
      end
end
