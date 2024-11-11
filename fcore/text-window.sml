structure TextWindow =
struct
  open TextConstants

  fun getStartLineBefore (sIdx, shd, lineNum, absIdx, cursorIdx, stl) =
    if sIdx < 0 then
      case stl of
        hd :: tl =>
          getStartLineBefore 
            (String.size hd - 1, hd, lineNum, absIdx, cursorIdx, tl)
      | [] =>
          0
    else
      if absIdx = cursorIdx then
        Int.max (lineNum - 1, 0)
      else
        let
          val chr = String.sub (shd, sIdx)
        in
          if chr = #"\n" then
            getStartLineBefore
              (sIdx - 1, shd, lineNum - 1, absIdx - 1, cursorIdx, stl)
          else
            getStartLineBefore
              (sIdx - 1, shd, lineNum, absIdx - 1, cursorIdx, stl)
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

  fun helpCentreCursor (strPos, str, lineNum, stl, maxW, halfH, curW, curH) =
    if strPos < 0 then
      case stl of
        hd :: tl =>
          helpCentreCursor 
            (String.size hd - 1, hd, lineNum, tl, maxW, halfH, curW, curH)
      | [] =>
          (* return 0 for start of buffer *)
          0
    else
      let
        val chr = String.sub (str, strPos)
      in
        if chr = #"\n" then
          if curH + (ySpace * 3) >= halfH then
            (* if we exceeded half the screen *)
            lineNum
          else
            helpCentreCursor 
              ( strPos - 1, str, lineNum - 1, stl, maxW, halfH
              , 0, curH + ySpace
              )
        else 
          if curW + xSpace <= maxW then
            let
              val curW = curW + xSpace
            in
              helpCentreCursor 
                ( strPos - 1, str, lineNum, stl, maxW, halfH
                , curW + xSpace, curH
                )
            end
          else
            (* have to create visual line break *)
            if curH + (ySpace * 3) >= halfH then
              (* if at limit, return current line lineNum *)
              lineNum
            else
              helpCentreCursor 
                ( strPos - 1, str, lineNum - 1, stl, maxW, halfH
                , 0, curH + ySpace
                )
      end

  (* search for prev \n, and once found,
   * call function to return startLine where cursor is centered *)
  fun getCursorStartLine (strPos, str, lineNum, stl, maxW, halfH) =
    if strPos < 0 then
      case stl of
        hd :: tl =>
          getCursorStartLine
            (String.size hd - 1, hd, lineNum, tl, maxW, halfH)
      | [] =>
          0
    else
      let
        val chr = String.sub (str, strPos)
      in
        if chr = #"\n" then
          (* \n found *)
          helpCentreCursor
            (strPos - 1, str, lineNum - 1, stl, maxW, halfH, xSpace, ySpace)
        else
          getCursorStartLine
            (strPos - 1, str, lineNum, stl, maxW, halfH)
      end

  fun getLineNum (strIdx, lhd, bufferLine) =
    if Vector.length lhd = 0 then
      bufferLine
    else if Vector.length lhd = 1 then
      let
        val lineIdx = Vector.sub (lhd, 0)
      in
        if lineIdx < strIdx then
          bufferLine + 1
        else
          bufferLine
      end
    else
      let
        val firstLineIdx = Vector.sub (lhd, 0)
      in
        if firstLineIdx > strIdx then
          bufferLine
        else if firstLineIdx < strIdx then
          BinSearch.equalOrLess (strIdx - 1, lhd) + bufferLine
        else
          bufferLine + 1
      end

  (* Prerequisite: LineGap is moved to cursor *)
  fun getStartLineWithCursorCentered 
    (lineGap: LineGap.t, cursorIdx, origLine, maxWidth, maxHeight) =
      let
        val {rightStrings, rightLines, idx = bufferIdx, line = bufferLine, leftStrings, ...} = lineGap
      in
        case (rightStrings, rightLines) of
          (shd :: stl, lhd :: ltl) =>
            let
              (* convert absolute cursorIdx to idx relative to hd string *)
              val strIdx = cursorIdx - bufferIdx
            in
              if strIdx < String.size shd then
                (* strIdx is in hd *)
                let
                  val lineNum = getLineNum (strIdx, lhd, bufferLine)
                in
                  getCursorStartLine 
                    (strIdx, shd, lineNum, leftStrings, maxWidth, maxHeight)
                end
              else
                (* strIdx is in tl *)
                case (stl, ltl) of
                  (stlhd :: stltl, ltlhd :: _) =>
                    let
                      val strIdx = strIdx - String.size shd
                      val bufferLine = bufferLine + Vector.length lhd
                      val lineNum = getLineNum (strIdx, ltlhd, bufferLine)
                      val leftStrings = shd :: leftStrings
                    in
                      getCursorStartLine 
                        (strIdx, stlhd, lineNum, leftStrings, maxWidth, maxHeight)
                    end
                | (_, _) =>
                    origLine
            end
        | (_, _) =>
            origLine
      end

  fun helpIsCursorVisible
    (strPos, str, stl, absIdx, maxW, maxH, curW, curH, newCursorIdx) =
      if strPos = String.size str then 
        case stl of 
         hd :: tl =>
           helpIsCursorVisible
             (0, hd, tl, absIdx, maxW, maxH, curW, curH, newCursorIdx)
        | [] =>
            true
      else
        if absIdx = newCursorIdx then
          true
        else
          let
            val chr = String.sub (str, strPos)
          in
            if chr = #"\n" then
              if curH + (ySpace * 3) >= maxH then
                false
              else
                helpIsCursorVisible
                  ( strPos + 1, str, stl, absIdx + 1
                  , maxW, maxH, 0, curH + ySpace, newCursorIdx
                  )
            else 
              if curW + xSpace <= maxW then
                helpIsCursorVisible
                  ( strPos + 1, str, stl, absIdx + 1
                  , maxW, maxH, curW + xSpace, curH, newCursorIdx
                  )
              else
                (* have to create visual line break *)
                if curH + (ySpace * 3) >= maxH then
                  false
                else
                  helpIsCursorVisible
                    ( strPos + 1, str, stl, absIdx + 1
                    , maxW, maxH, 0, curH + ySpace, newCursorIdx
                    )
          end

  fun startIsCursorVisible 
    (curIdx, shd, stl, lhd, startLine, curLine, maxW, maxH, newCursorIdx) =
      if startLine = curLine then
        helpIsCursorVisible
          (0, shd, stl, curIdx, maxW, maxH, 0, 0, newCursorIdx)
      else
        let
          val relativeLine = (curLine + Vector.length lhd) - startLine
          val lineIdx = Vector.sub (lhd, relativeLine)
          val absIdx = curIdx + lineIdx
        in
          helpIsCursorVisible
            (lineIdx, shd, stl, absIdx, maxW, maxH, 0, 0, newCursorIdx)
        end

  (* Prerequisite: move LineGap.t to startLine *)
  fun isCursorVisible (lineGap: LineGap.t, newCursorIdx, startLine, maxW, maxH) =
    let 
      val {rightStrings, rightLines, line = curLine, idx = curIdx, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          if startLine < curLine + Vector.length lhd then
            (* startLine in this node *)
            startIsCursorVisible 
              ( curIdx, shd, stl, lhd, startLine, curLine
              , maxW, maxH, newCursorIdx
              )
          else
            (* startLine is in stl *)
            (case (stl, ltl) of
              (stlhd :: stltl, ltlhd :: ltltl) =>
                startIsCursorVisible
                  ( curIdx, stlhd, stltl, ltlhd, startLine, curLine
                  , maxW, maxH, newCursorIdx
                  )
            | (_, _) => 
                true)
      | (_, _) => 
          true
    end
end
