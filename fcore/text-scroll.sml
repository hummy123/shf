structure TextScroll =
struct
  structure TC = TextConstants

  (* Preqreuisite: move buffer to cursorIdx *)
  fun getScrollColumn (buffer, cursorIdx, windowWidth) =
    let
      val startOfLine = Cursor.vi0 (buffer, cursorIdx)
      val columnDifference = cursorIdx - startOfLine
    in
      if columnDifference = 0 then
        0
      else
        let
          val howManyColumnsCanWeFit =
            if windowWidth >= TC.textLineWidth then TC.textLineCount
            else windowWidth div TC.xSpace
          val howManyColumnsCanWeFit = howManyColumnsCanWeFit - 1
        in
          if columnDifference < howManyColumnsCanWeFit then 0
          else columnDifference - howManyColumnsCanWeFit
        end
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
      in
        if prevLineNumber + howManyLinesWeCanFit >= cursorLine then
          prevLineNumber
        else
          let val lineDifference = cursorLine - prevLineNumber
          in prevLineNumber + lineDifference
          end
      end
end
