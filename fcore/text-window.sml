structure TextWindow =
struct
  open TextConstants

  fun isPrevChrLn (str, strPos, strTl) =
    if strPos > 0 then
      String.sub (str, strPos - 1) = #"\n"
    else
      case strTl of
        hd :: _ =>
          String.sub (hd, String.size hd - 1) = #"\n"
      | [] => false

  fun getStartLineBefore (sIdx, shd, lineNum, absIdx, cursorIdx, stl) =
    if sIdx < 0 then
      case stl of
        hd :: tl =>
          getStartLineBefore 
            (String.size hd - 1, hd, lineNum, absIdx, cursorIdx, tl)
      | [] =>
          0
    else
      let
        val chr = String.sub (shd, sIdx)
      in
        if chr = #"\n" then
          if absIdx <> cursorIdx then
            getStartLineBefore
              (sIdx - 1, shd, lineNum - 1, absIdx - 1, cursorIdx, stl)
          else
            (* we have found cursor, and it is at \n *)
            lineNum - 1
        else
          if absIdx <> cursorIdx then
            getStartLineBefore
              (sIdx - 1, shd, lineNum, absIdx - 1, cursorIdx, stl)
          else
            (* we have found cursor; return line *)
            lineNum - 1
      end

  fun getStartLineAfter 
     ( sIdx, shd, lineNum, absIdx, cursorIdx, stl
     , maxWidth, maxHeight, curWidth, curHeight
     , origLine
     ) =
      if sIdx = String.size shd then
        case stl of
          hd :: tl =>
            getStartLineAfter 
              ( 0, hd, lineNum, absIdx, cursorIdx, tl
              , maxWidth, maxHeight, curWidth, curHeight
              , origLine
              )
        | [] =>
            origLine
      else
        if absIdx = cursorIdx then
          origLine
        else
          let
            val chr = String.sub (shd, sIdx)
          in
            if chr = #"\n" then
              if curHeight + (ySpace * 3) >= maxHeight then
                getStartLineAfter 
                  ( sIdx + 1, shd, lineNum + 1, absIdx + 1, cursorIdx, stl
                  , maxWidth, maxHeight, 0, curHeight + ySpace
                  , origLine + 1
                  )
              else
                getStartLineAfter 
                  ( sIdx + 1, shd, lineNum + 1, absIdx + 1, cursorIdx, stl
                  , maxWidth, maxHeight, 0, curHeight + ySpace
                  , origLine
                  )
            else 
              if curWidth + xSpace <= maxWidth then
                let
                  val curWidth = curWidth + xSpace
                in
                  getStartLineAfter 
                    ( sIdx + 1, shd, lineNum, absIdx + 1, cursorIdx, stl
                    , maxWidth, maxHeight, curWidth, curHeight
                    , origLine
                    )
                end
              else
                (* have to create visual line break *)
                if curHeight + (ySpace * 3) >= maxHeight then
                  getStartLineAfter
                    ( sIdx + 1, shd, lineNum + 1, absIdx + 1, cursorIdx, stl
                    , maxWidth, maxHeight, 0, curHeight + ySpace
                    , origLine + 1
                    )
                else
                  getStartLineAfter
                    ( sIdx + 1, shd, lineNum + 1, absIdx + 1, cursorIdx, stl
                    , maxWidth, maxHeight, 0, curHeight + ySpace
                    , origLine
                    )
          end

  (* Prerequisite: LineGap is moved to oldLine first. *)
  fun getStartLine (lineGap: LineGap.t, oldLine, cursorIdx, maxWidth, maxHeight) =
    let
      val {rightStrings, rightLines, line = curLine, idx = curIdx, leftStrings, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (rStrHd :: rStrTl, rLnHd :: _) =>
          let
            (* get index of line to start building from *)
            val startIdx =
              if oldLine > curLine then
                let
                  val lnPos = oldLine - curLine - 1
                  val startIdx = Vector.sub (rLnHd, lnPos)
                in
                  startIdx - 1
                end
              else
                0
              val absIdx = curIdx + startIdx
          in
            if cursorIdx < absIdx then
              (* move upwards *)
              getStartLineBefore 
                (startIdx, rStrHd, oldLine, absIdx, cursorIdx, leftStrings)
            else if cursorIdx = absIdx + 1 then
              (* double linebreak *)
              getStartLineBefore 
                (startIdx + 1, rStrHd, oldLine, absIdx + 1, cursorIdx, leftStrings)
            else if cursorIdx > absIdx then
              (* possibly move downwards *)
              getStartLineAfter 
                 ( startIdx, rStrHd, oldLine, absIdx, cursorIdx, rStrTl
                 , maxWidth, maxHeight, 0, 0
                 , oldLine
                 )
            else
              (* keep current line *)
              Int.max (oldLine - 1, 0)
          end
      | (_, _) => 
          oldLine
    end
end
