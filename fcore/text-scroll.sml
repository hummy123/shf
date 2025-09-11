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
        in
          columnDifference mod howManyColumnsCanWeFit
        end
    end
end
