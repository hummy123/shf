structure TextScroll =
struct
  structure TC = TextConstants

  (* calculates new scroll column from integer arguments *)
  fun calculateScrollColumn
    (startOfLine, cursorIdx, windowWidth, prevScrollColumn) =
    let
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

  (* Preqreuisite: move buffer to cursorIdx *)
  fun getScrollColumn (buffer, cursorIdx, windowWidth, prevScrollColumn) =
    let
      val startOfLine = Cursor.vi0 (buffer, cursorIdx)
    in
      calculateScrollColumn
        (startOfLine, cursorIdx, windowWidth, prevScrollColumn)
    end

  fun getScrollColumnFromString (cursorIdx, windowWidth, prevScrollColumn) =
    calculateScrollColumn (0, cursorIdx, windowWidth, prevScrollColumn)

  fun getStartLine (prevLineNumber, cursorLine, windowHeight) =
    if cursorLine <= (prevLineNumber + 3) then
      (* cursorLine is prior to or same as prevLineNumber,
       * so use cursorLine to calculate the start line we want. *)
      Int.max (cursorLine - 3, 0)
    else
      (* cursorLine > prevLineNumber *)
      let
        val howManyLinesWeCanFit = windowHeight div TC.ySpace
      in
        if cursorLine > prevLineNumber + (howManyLinesWeCanFit - 3) then
          (* cursorLine is after the visible part of the screen
           * so return the mimimum line where cursorLine is visible *)
          cursorLine - howManyLinesWeCanFit + 3
        else
          prevLineNumber
      end

  fun getLineCentre (cursorLine, windowHeight) =
    let
      val howManyLinesWeCanFit = windowHeight div TC.ySpace
      val startLine = cursorLine - (howManyLinesWeCanFit div 2)
    in
      Int.max (startLine, 0)
    end
end
