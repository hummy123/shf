structure Cursor =
struct
  fun helpVi0 (strPos, str, absIdx, strTl, lineTl) =
    if strPos < 0 then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          helpVi0 (String.size shd - 1, shd, absIdx, stl, ltl)
      | (_, _) => 0
    else
      case String.sub (str, strPos) of
        #"\n" => absIdx + 1
      | _ => helpVi0 (strPos - 1, str, absIdx - 1, strTl, lineTl)

  fun vi0 (lineGap: LineGap.t, cursorIdx) =
    let
      val
        {rightStrings, idx = bufferIdx, rightLines, leftStrings, leftLines, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (strHd :: strTl, lnHd :: lnTl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size strHd then
              (* strIdx is in this string *)
              if String.sub (strHd, strIdx) = #"\n" then
                (* don't need to do anything if we are already at newline;
                 * just return current cursorIdx *)
                cursorIdx
              else
                (* not at newline so start iterating *)
                helpVi0 (strIdx - 1, strHd, cursorIdx - 1, strTl, lnTl)
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: _, nestLnHd :: _) =>
                   let
                     val strIdx = strIdx - String.size strHd
                   in
                     if String.sub (nestStrHd, strIdx) = #"\n" then
                       (* already at linebreak so return same cursorIdx *)
                       cursorIdx
                     else
                       (* not in linebreak *)
                       helpVi0
                         ( strIdx - 1
                         , nestStrHd
                         , cursorIdx - 1
                         , strHd :: leftStrings
                         , lnHd :: leftLines
                         )
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => 
          (* nowhere to go, so return cursorIdx *) 
          cursorIdx
    end

  fun helpViDlr (strPos, str, absIdx, strTl, lineTl) =
    if strPos = String.size str then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) => helpViDlr (0, shd, absIdx, stl, ltl)
      | (_, _) => absIdx - 1
    else
      case String.sub (str, strPos) of
        #"\n" => absIdx - 1
      | _ => helpViDlr (strPos + 1, str, absIdx + 1, strTl, lineTl)

  fun viDlr (lineGap: LineGap.t, cursorIdx) =
    let
      val
        {rightStrings, idx = bufferIdx, rightLines, leftStrings, leftLines, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (strHd :: strTl, lnHd :: lnTl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size strHd then
              if String.sub (strHd, strIdx) <> #"\n" then
                (* not in double linebreak *)
                helpViDlr (strIdx + 1, strHd, cursorIdx + 1, strTl, lnTl)
              else 
                (* check if we are in double linebreak *) 
                if strIdx - 1 >= 0 then
                  if String.sub (strHd, strIdx - 1) = #"\n" then
                    (* we are in double linebreak, so do nothing *)
                    cursorIdx
                  else
                    (* not in double linebreak, so iterate *)
                    helpViDlr (strIdx + 1, strHd, cursorIdx + 1, strTl, lnTl)
                else
                  (* check if double linebreak in strTl *)
                  (case strTl of
                     nestStrHd :: _ =>
                       if String.sub (nestStrHd, 0) = #"\n" then
                         cursorIdx
                       else
                         helpViDlr (strIdx + 1, strHd, cursorIdx + 1, strTl, lnTl)
                   | [] => cursorIdx)
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: nestStrTl, nestLnHd :: nestLnTl) =>
                   let
                     val strIdx = strIdx - String.size strHd
                   in
                     helpViDlr
                       ( strIdx + 1
                       , nestStrHd
                       , cursorIdx + 1
                       , nestStrTl
                       , nestLnTl
                       )
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => 
          (* nowhere to go, so return cursorIdx *) 
          cursorIdx
    end

  (* Prerequisite: lineGap is moved to requested idx first 
   * todo: check if we are in a \r\n pair, but this is not a priority *)
  fun viL (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
    in
      case rightStrings of
        hd :: tl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx + 1 < String.size hd then
              (* if there is at least one character after this idx *)
              let
                val nextChr = String.sub (hd, strIdx + 1)
              in
                (case nextChr of
                   #"\n" =>
                     if strIdx + 2 < String.size hd then
                       (* if there are at least two chars after strIdx *)
                       cursorIdx + 2
                     else
                       (* only one char after strIdx which is \n 
                        * if there is a string at the tl, can increment by 2 *)
                       (case tl of
                          _ :: _ => cursorIdx + 2
                        | [] => cursorIdx + 1)
                 | _ => cursorIdx + 1)
              end
            else
              (* no chars after this idx; have to check tl *)
              (case tl of
                 tlhd :: tltl =>
                   (* if there is another string after current head, we can increment cursorIdx 
                    * however, first we need to check if next char is \n. *)
                   let
                     val nextChr = String.sub (tlhd, 0)
                   in
                     (case nextChr of
                        #"\n" =>
                          if String.size tlhd > 2 then
                            (* if there is at least one character after \n 
                             * then increment cursorIdx by 2 *)
                            cursorIdx + 2
                          else
                            (* this string only contains \n
                             * but there is a small possibility tltl
                             * contains another string.
                             * If it does, we can increment cursorIdx by 2,
                             * moving past newline.
                             * If not, increment cursorIdx by 1,
                             * landing on newline. *)
                            (case tltl of
                               _ :: _ => cursorIdx + 2
                             | [] => cursorIdx + 1)
                      | _ =>
                          (* next char is not newline, 
                           * so we can just increment by 1 *)
                          cursorIdx + 1)
                   end
               | [] =>
                   (* if there is no string after current head, return original cursorIdx *)
                   cursorIdx)
          end
      | [] =>
          (* return original cursorIdx if there is nothing to the right *)
          cursorIdx
    end

  (* Prerequisite: lineGap is moved to requested idx first 
   * Implementation is mostly the same as the viL function, 
   * except we focus on decrementing instead,
   * and we examing leftStrings list instead of tail of rightStrings
   * if head of rightStrings is empty or does not have the idx we want.
   * todo: check if we are in a \r\n pair, but this is not a priority *)
  fun viH (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, leftStrings, idx = bufferIdx, ...} = lineGap
    in
      case rightStrings of
        hd :: _ =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx > 0 then
              (* if there is at least one char before this index *)
              let
                val prevChr = String.sub (hd, strIdx - 1)
              in
                (case prevChr of
                   #"\n" =>
                     if strIdx > 1 then
                       (* if there are at least two chars before strIdx *)
                       if String.sub (hd, strIdx - 2) = #"\n" then
                         (* if there is double line break like \n\n, 
                          * we would like to move the cursor 
                          * to the second linebreak *)
                         cursorIdx - 1
                       else
                         cursorIdx - 2
                     else
                       (* only one char before strIdx which is \n 
                        * if there is a string at the leftStrings, can decrement by 2 *)
                       (case leftStrings of
                          _ :: _ => cursorIdx - 2
                        | [] => cursorIdx - 1)
                 | _ => cursorIdx - 1)
              end
            else
              (* no chars before this idx; have to check leftStrings *)
              (case leftStrings of
                 lHd :: lTl =>
                   (* if there is another string after current head, 
                    * we can increment cursorIdx 
                    * however, first we need to check if next char is \n. *)
                   let
                     val lastChr = String.sub (lHd, String.size lHd - 1)
                   in
                     (case lastChr of
                        #"\n" =>
                          if String.size lHd > 2 then
                            (* if there at least one character before \n
                             * then decrement cursorIdx by 2
                             * or decrement cursorIdx by 1 
                             * if we are in a double linebreak
                             * if we are not in a double linebreak *)
                            if String.sub (lHd, strIdx - 2) = #"\n" then
                              cursorIdx - 1
                            else
                              cursorIdx - 2
                          else
                            (* this string only contains \n
                             * but there is a small possibility tltl
                             * contains another string.
                             * If it does, we can increment cursorIdx by 2,
                             * moving past newline.
                             * If not, increment cursorIdx by 1,
                             * landing on newline. *)
                            (case lTl of
                               ltlHd :: _ =>
                                 if
                                   String.sub (ltlHd, String.size ltlHd - 1)
                                   = #"\n"
                                 then cursorIdx - 1
                                 else cursorIdx - 2
                             | [] => cursorIdx - 1)
                      | _ =>
                          (* next char is not newline, 
                           * so we can just decrement by 1 *)
                          cursorIdx - 1)
                   end
               | [] =>
                   (* if there is no string after current head, return original cursorIdx *)
                   cursorIdx)
          end
      | [] => cursorIdx
    end

  (* below functions, until and including getCursorColumn, 
   * are all for helping to calculate the cursor's column 
   * compared to the line the cursor is currently positioned in *)
  fun reverseLinearSearch (findNum, idx, lines) =
    if idx < 0 then
      idx
    else
      let
        val curVal = Vector.sub (lines, idx)
      in
        if curVal < findNum then idx
        else reverseLinearSearch (findNum, idx, lines)
      end

  fun helpBinSearch (findNum, lines, low, high) =
    let
      val mid = low + ((high - low) div 2)
    in
      if high >= low then
        let
          val midVal = Vector.sub (lines, mid)
        in
          if midVal = findNum then
            mid
          else if midVal < findNum then
            helpBinSearch (findNum, lines, mid + 1, high)
          else
            helpBinSearch (findNum, lines, low, mid - 1)
        end
      else
        reverseLinearSearch (findNum, mid, lines)
    end

  fun binSearch (findNum, lines) =
    helpBinSearch (findNum, lines, 0, Vector.length lines - 1)

  fun helpGetCursorColumn (distanceFromLine, strList, lineList) =
    case (strList, lineList) of
      (strHd :: strTl, lnHd :: lnTl) =>
        if Vector.length lnHd = 0 then
          (* lnHd is empty, so line is not here *)
          helpGetCursorColumn
            (distanceFromLine + String.size strHd, strTl, lnTl)
        else
          (* lnHd is not empty, meaning last lineIdx is closest linebreak *)
          let
            val lineIdx = Vector.sub (lnHd, Vector.length lnHd - 1)
            (* number of chars after the lineIdx *)
            val idxAfterLn = String.size strHd - lineIdx
          in
            distanceFromLine + idxAfterLn - 1
          end
    | (_, _) => distanceFromLine

  fun helpGetCursorColumnBranch (strIdx, strHd, lnHd, leftStrings, leftLines) =
    if String.sub (strHd, strIdx) = #"\n" then
      (* If we are at newline, column is 0 *)
      0
    else if Vector.length lnHd = 1 then
      (* check if the one line idx in the vector 
       * is before the strIdx *)
      let
        val lineIdx = Vector.sub (lnHd, 0)
      in
        if lineIdx < strIdx then strIdx - lineIdx
        else helpGetCursorColumn (strIdx, leftStrings, leftLines)
      end
    else if Vector.length lnHd > 1 then
      let
        (* check if strIdx is inside line vector, 
         * and perform binary search if so *)
        val low = Vector.sub (lnHd, 0)
      in
        if low < strIdx then
          (* strIdx is less than low, so use bin search
           * to find lineIdx that is lower than strIdx *)
          let
            val lineIdx = binSearch (strIdx - 1, lnHd)
            (* linebreakPos = index of linebreak before strIdx *)
            val linebreakPos = Vector.sub (lnHd, lineIdx)
          in
            strIdx - linebreakPos - 1
          end
        else
          (* line before strIdx must be in leftStrings/lines *)
          helpGetCursorColumn (strIdx, leftStrings, leftLines)
      end
    else
      (* lnHd has length of 0, so most recent 
       * line break must be in leftStrings/lines *)
      helpGetCursorColumn (strIdx, leftStrings, leftLines)

  (* Prerequisite: lineGap is moved to cursorIdx *)
  fun getCursorColumn (lineGap: LineGap.t, cursorIdx) =
    let
      val
        {rightStrings, idx = bufferIdx, rightLines, leftStrings, leftLines, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (strHd :: strTl, lnHd :: lnTl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size strHd then
              (* strIdx is in this string *)
              helpGetCursorColumnBranch
                (strIdx, strHd, lnHd, leftStrings, leftLines)
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: nestStrTl, nestLnHd :: nestLnTl) =>
                   let
                     val strIdx = strIdx - String.size strHd
                     val leftStrings = strHd :: leftStrings
                     val leftLines = lnHd :: leftLines
                   in
                     helpGetCursorColumnBranch
                       (strIdx, nestStrHd, nestLnHd, leftStrings, leftLines)
                   end
               | (_, _) =>
                   helpGetCursorColumnBranch
                     ( String.size strHd - 1
                     , strHd
                     , lnHd
                     , leftStrings
                     , leftLines
                     ))
          end
      | (_, _) => helpGetCursorColumn (0, leftStrings, leftLines)
    end

  fun helpViJ
    ( strPos, str, absIdx
    , lineColumn, preferredColumn, hasPassedLine
    , strTl, lineTl
    ) =
    if strPos = String.size str then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          (* todo: possibly check if we have passed line,
           * and if so, if there are any line breaks in the lineHd
           * which we could use to skip searching part of the string. 
           * However, this will likely have worse cache locality 
           * as we switch to searching in string to searcing in line vector
           * so perhaps not. *)
          helpViJ
            ( 0, shd, absIdx
            , lineColumn, preferredColumn, hasPassedLine
            , stl, ltl
            )
      | (_, _) => 
          (* empty, so return end of previous string *) 
          absIdx - 1
    else
      case String.sub (str, strPos) of
        #"\n" =>
          if hasPassedLine then
            (* reached end of line twice, 
             * but line has fewer chars than preferredColumn 
             * note: if in double \n\n linebreak,
             * then return absIdx of second linebreak. *)
            if strPos = String.size str - 1 then
              if String.sub (str, strPos + 1) = #"\n" then
                (* we are in double linebreak *)
                absIdx + 1
              else
                (* not in double linebreak *)
                absIdx - 1
            else
              (* this is last chr of string; must check string's tl next *)
              (case strTl of
                 hd :: tl =>
                   if String.sub (hd, 0) = #"\n" then
                     (* in double linebreak *)
                     absIdx + 1
                   else
                     (* not in double linebreak *)
                     absIdx - 1
               | [] => 
                   (* no more strings so return last idx *) 
                   absIdx - 1)
          else 
            (* reached end of line once;
             * have to check if this is a double linebreak,
             * and return idx of second linebreak if so *) 
            if strPos < String.size str - 1 then
              if String.sub (str, strPos + 1) = #"\n" then
                absIdx + 1
              else
                helpViJ
                  ( strPos + 1, str, absIdx + 1
                  , 0, preferredColumn, true
                  , strTl, lineTl
                  )
            else
              (* this is last chr of string; must check string's tl next *)
              (case strTl of
                 hd :: tl =>
                   if String.sub (hd, 0) = #"\n" then
                     (* in double linebreak *)
                     absIdx + 1
                   else
                     (* not in double linebreak *)
                     helpViJ
                       ( strPos + 1, str, absIdx + 1
                       , 0, preferredColumn, true
                       , strTl, lineTl
                       )
               | [] => 
                   (* no more strings so return last idx *) 
                   absIdx)
      | _ =>
          if lineColumn = preferredColumn andalso hasPassedLine then
            (* we're at the preferredColumn so return absIdx *)
            absIdx
          else
            (* we're not in the preferred column, so keep iterating *)
            helpViJ
              ( strPos + 1, str, absIdx + 1
              , lineColumn + 1, preferredColumn, hasPassedLine
              , strTl, lineTl
              )

  fun viJ (lineGap: LineGap.t, cursorIdx) =
    let
      val
        {rightStrings, idx = bufferIdx, rightLines, leftStrings, leftLines, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (strHd :: strTl, lnHd :: lnTl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size strHd then
              (* strIdx is in this string *)
              if String.sub (strHd, strIdx) = #"\n" then
                (* if we are at a newline 
                 * note:
                 * don't need to check if we are a double linebreak,
                 * because cursor navigation functions already check
                 * for that condition at the end. 
                 * So there is no way at the start of a navigation function
                 * that cursor is in a double linebreak incorrectly. *)
                helpViJ
                  (strIdx + 1, strHd, cursorIdx + 1, 0, 0, true, strTl, lnTl)
              else
                (* not at newline 
                 * so get column number and start iterating *)
                let
                  val lineColumn = getCursorColumn (lineGap, cursorIdx)
                in
                  helpViJ
                    ( strIdx + 1, strHd, cursorIdx + 1
                    , lineColumn, lineColumn, false
                    , strTl, lnTl
                    )
                end
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: nestStrTl, nestLnHd :: nestLnTl) =>
                   let
                     val strIdx = strIdx - String.size strHd
                   in
                     if String.sub (nestStrHd, strIdx) = #"\n" then
                       helpViJ
                         ( strIdx + 1, nestStrHd, cursorIdx + 1
                         , 0, 0, true
                         , nestStrTl, nestLnTl
                         )
                     else
                       (* not in linebreak *)
                       let
                         val lineColumn = getCursorColumn (lineGap, cursorIdx)
                       in
                         helpViJ
                           ( strIdx + 1, nestStrHd, cursorIdx + 1
                           , lineColumn, lineColumn, false
                           , nestStrTl, nestLnTl
                           )
                       end
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => 
          (* nowhere to go rightward, so return cursorIdx *) 
          cursorIdx
    end

  fun helpViK
    ( strPos, str, absIdx
    , lineColumn, preferredColumn, hasPassedLine
    , strTl, lineHd, lineTl
    ) =
    if strPos < 0 then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          helpViK
            ( String.size shd - 1, shd, absIdx
            , lineColumn, preferredColumn, hasPassedLine
            , stl, lhd, ltl
            )
      | (_, _) => 
          (* empty, so return start of previous string *) 
          absIdx + 1
    else
      case String.sub (str, strPos) of
        #"\n" =>
          if hasPassedLine then
            (* reached end of line twice, 
             * but line has fewer chars than preferredColumn 
             * note: if in double \n\n linebreak,
             * then return absIdx of second linebreak. *)
            if strPos > 0 then
              (* check for double linebreak in string *)
              if String.sub (str, strPos - 1) = #"\n" then
                (* in double linebreak *)
                absIdx
              else
                (* not in double linebreak *)
                absIdx - 1
            else
              (* check for double linebreak in tl *)
              (case strTl of
                 hd :: _ =>
                   if String.sub (hd, String.size hd - 1) = #"\n" then
                     (* in double linebreak *)
                     absIdx
                   else
                     (* not in double linebreak *)
                     absIdx - 1
               | [] => 
                   (* tl is empty, so return start *) 
                   0)
          else 
            (* reached start of line once;
            * have to check if this is a double linebreak,
            * and return idx of second linebreak if so *) 
            if strPos > 0 then
              if String.sub (str, strPos - 1) = #"\n" then
                absIdx
              else
                let
                  (* have to calculate column of current line
                   * so we know which line to stop searching at *)
                  val lineColumn = 
                    helpGetCursorColumnBranch
                      (strPos - 1, str, lineHd, strTl, lineTl)
                in
                  helpViK
                    ( strPos - 1, str, absIdx - 1
                    , lineColumn, preferredColumn, true
                    , strTl, lineHd, lineTl
                    )
                end
          else
            (* this is first chr of string; must check string's tl next *)
            (case strTl of
               hd :: tl =>
                 if String.sub (hd, String.size hd - 1) = #"\n" then
                   (* in double linebreak *)
                   absIdx
                 else
                   (* not in double linebreak *)
                   let
                     val lineColumn = 
                       helpGetCursorColumnBranch
                         (strPos - 1, str, lineHd, strTl, lineTl)
                   in
                     helpViK
                       ( strPos - 1, str, absIdx - 1
                       , lineColumn, preferredColumn, true
                       , strTl, lineHd, lineTl
                       )
                   end
             | [] => 
                 (* no more strings so return last idx *) 
                 absIdx)
      | _ =>
          if lineColumn <= preferredColumn andalso hasPassedLine then
            (* We're at or before the preferredColumn so return absIdx
             * context: current line may have fewer columns  
             * than our preferred column value.
             * If this is the case, we want to check
             * "is lineColumn equal to or before preferredColumn?". *)
            absIdx
          else
            (* we're not in the preferred column, so keep iterating *)
            helpViK
              ( strPos - 1, str, absIdx - 1
              , lineColumn - 1, preferredColumn, hasPassedLine
              , strTl, lineHd, lineTl
              )

  fun viK (lineGap: LineGap.t, cursorIdx) =
    let
      val
        {rightStrings, idx = bufferIdx, rightLines, leftStrings, leftLines, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (strHd :: strTl, lnHd :: lnTl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size strHd then
              (* strIdx is in this string *)
              if String.sub (strHd, strIdx) = #"\n" then
                (* if we are at a newline  *)
                if strIdx > 0 then
                  if String.sub (strHd, strIdx - 1) = #"\n" then
                    (* if in double linebreak *)
                    helpVi0
                      (strIdx - 2, strHd, cursorIdx - 2, leftStrings, leftLines)
                  else
                    (* not in double linebreak *)
                    helpVi0
                      (strIdx - 1, strHd, cursorIdx - 1, leftStrings, leftLines)
                else
                  (* check leftStrings to see if we are in a double linebreak *)
                  (case (leftStrings, leftLines) of
                     (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
                       if String.sub (lStrHd, String.size lStrHd - 1) = #"\n" then
                         (* in double linebreak *)
                         helpVi0
                           ( String.size lStrHd - 2, lStrHd, cursorIdx - 2
                           , lStrTl, lLnTl
                           )
                       else
                         (* in single linebreak *)
                         helpVi0
                           ( strIdx - 1, strHd, cursorIdx - 1
                           , leftStrings, leftLines
                           )
                   | (_, _) =>
                       helpViK
                         ( strIdx - 1, strHd, cursorIdx - 1
                         , 0, 0, true
                         , leftStrings, lnHd, leftLines
                         ))
              else
                (* not at newline 
                 * so get column number and start iterating *)
                let
                  val lineColumn = getCursorColumn (lineGap, cursorIdx)
                in
                  helpViK
                    ( strIdx - 1, strHd, cursorIdx - 1
                    , lineColumn, lineColumn, false
                    , leftStrings, lnHd, leftLines
                    )
                end
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: nestStrTl, nestLnHd :: nestLnTl) =>
                   let
                     val strIdx = strIdx - String.size strHd
                   in
                     if String.sub (nestStrHd, strIdx) = #"\n" then
                       if strIdx > 0 then
                         (* if can check for double linebreak in nestStrHd *)
                         if String.sub (nestStrHd, strIdx - 1) = #"\n" then
                           (* is in double linebreak *)
                           let
                             val leftStrings = strHd :: leftStrings
                             val leftLines = lnHd :: leftLines
                           in
                             helpVi0
                               ( strIdx - 2, nestStrHd, cursorIdx - 2
                               , leftStrings, leftLines
                               )
                           end
                         else
                           (* is in single linebreak *)
                           helpVi0
                             ( strIdx - 1, nestStrHd, cursorIdx - 1
                             , leftStrings, leftLines
                             )
                       else 
                         (* must check strHd for second linebreak *) 
                         if String.sub (strHd, String.size strHd - 1) = #"\n" then
                           (* is in double linebreak *)
                           helpVi0
                             ( String.size strHd - 2, nestStrHd, cursorIdx - 2
                             , leftStrings, leftLines
                             )
                       else
                         (* is in single linebreak *)
                         helpVi0
                           ( String.size strHd - 1, nestStrHd, cursorIdx - 1
                           , leftStrings, leftLines
                           )
                     else
                       (* not in linebreak *)
                       let
                         val lineColumn = getCursorColumn (lineGap, cursorIdx)
                       in
                         helpViK
                           ( strIdx - 1, nestStrHd, cursorIdx - 1
                           , lineColumn, lineColumn, false
                           , strHd :: leftStrings
                           , nestLnHd
                           , lnHd :: leftLines
                           )
                       end
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => 
          (* nowhere to go rightward, so return cursorIdx *) 
          cursorIdx
    end

  fun isNextChrSpace (strPos, str, strTl) =
    if strPos + 1 < String.size str then
      let val chr = String.sub (str, strPos + 1)
      in Char.isSpace chr
      end
    else
      case strTl of
        hd :: _ => let val chr = String.sub (hd, 0) in Char.isSpace chr end
      | [] => false

  fun notIsNextChrSpace (strPos, str, strTl) =
    let val isSpace = isNextChrSpace (strPos, str, strTl)
    in not isSpace
    end

  fun isNextChrNonBlank (strPos, str, strTl) =
    if strPos + 1 < String.size str then
      let
        val chr = String.sub (str, strPos + 1)
        val isNotBlank =
          Char.isSpace chr orelse Char.isAlphaNum chr orelse chr = #"_"
      in
        not isNotBlank
      end
    else
      case strTl of
        hd :: _ =>
          let
            val chr = String.sub (hd, 0)
            val isNotBlank =
              Char.isSpace chr orelse Char.isAlphaNum chr orelse chr = #"_"
          in
            not isNotBlank
          end
      | [] => false

  fun isNextChrAlphaNum (strPos, str, stl) =
    if strPos + 1 < String.size str then
      let val chr = String.sub (str, strPos + 1)
      in Char.isAlphaNum chr orelse chr = #"_"
      end
    else
      case stl of
        hd :: _ =>
          let val chr = String.sub (str, 0)
          in Char.isAlphaNum chr orelse chr = #"_"
          end
      | [] => false

  fun isPrevChrSpace (strPos, str, strTl) =
    if strPos > 0 then
      let val prevChr = String.sub (str, strPos - 1)
      in Char.isSpace prevChr
      end
    else
      case strTl of
        hd :: _ =>
          let val prevChr = String.sub (hd, String.size hd - 1)
          in Char.isSpace prevChr
          end
      | [] => false

  fun notIsPrevChrSpace (strPos, str, strTl) =
    let val isSpace = isPrevChrSpace (strPos, str, strTl)
    in not isSpace
    end

  fun isPrevChrAlphaNum (strPos, str, strTl) =
    if strPos > 0 then
      let val chr = String.sub (str, strPos - 1)
      in Char.isAlphaNum chr orelse chr = #"_"
      end
    else
      case strTl of
        hd :: _ =>
          let val chr = String.sub (hd, String.size hd - 1)
          in Char.isAlphaNum chr orelse chr = #"_"
          end
      | [] => false

  fun isPrevChrNonBlank (strPos, str, strTl) =
    if strPos > 0 then
      let
        val chr = String.sub (str, strPos - 1)
        val isNotBlank =
          Char.isSpace chr orelse Char.isAlphaNum chr orelse chr = #"_"
      in
        not isNotBlank
      end
    else
      case strTl of
        hd :: _ =>
          let
            val chr = String.sub (hd, String.size hd - 1)
            val isNotBlank =
              Char.isSpace chr orelse Char.isAlphaNum chr orelse chr = #"_"
          in
            not isNotBlank
          end
      | [] => false

  fun helpNextWord (strPos, str, absIdx, strTl, lineTl) =
    if strPos = String.size str then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) => 
          helpNextWord (0, shd, absIdx, stl, ltl)
      | (_, _) =>
          (* reached end of lineGap; 
           * return last valid chr position *)
          absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isAlphaNum chr orelse chr = #"_" then
          if isNextChrNonBlank (strPos, str, strTl) then 
            absIdx + 1
          else 
            helpNextWord (strPos + 1, str, absIdx + 1, strTl, lineTl)
        else if Char.isSpace chr then
          if notIsNextChrSpace (strPos, str, strTl) then
            absIdx + 1
          else
            (* nothing to do on space, except keep iterating *)
            helpNextWord (strPos + 1, str, absIdx + 1, strTl, lineTl)
        else 
          (* chr is NON_BLANK. *) 
          if isNextChrAlphaNum (strPos, str, strTl) then
            absIdx + 1
          else
            helpNextWord (strPos + 1, str, absIdx + 1, strTl, lineTl)
      end

  fun helpNextWORD (strPos, str, absIdx, strTl, lineTl) =
    if strPos = String.size str then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) => 
          helpNextWORD (0, shd, absIdx, stl, ltl)
      | (_, _) =>
          (* reached end of lineGap; 
           * return last valid chr position *)
          absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isSpace chr then
          if notIsNextChrSpace (strPos, str, strTl) then
            absIdx + 1
          else
            helpNextWORD (strPos + 1, str, absIdx + 1, strTl, lineTl)
        else 
          helpNextWORD (strPos + 1, str, absIdx + 1, strTl, lineTl)
      end

  fun toNextWord (lineGap: LineGap.t, cursorIdx, fNext) =
    let
      val {rightStrings, rightLines, idx = bufferIdx, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              fNext (strIdx, shd, cursorIdx, stl, ltl)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let val strIdx = strIdx - String.size shd
                   in fNext (strIdx, shd, cursorIdx, stltl, ltltl)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  (* equivalent 'f vi's 'w' command *)
  fun nextWord (lineGap, cursorIdx) =
    toNextWord (lineGap, cursorIdx, helpNextWord)

  (* equivalent 'f vi's 'W' command *)
  fun nextWORD (lineGap, cursorIdx) =
    toNextWord (lineGap, cursorIdx, helpNextWORD)

  fun helpPrevWord (strPos, str, absIdx, strTl, lineTl) =
    if strPos < 0 then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          helpPrevWord (String.size shd - 1, shd, absIdx, stl, ltl)
      | (_, _) =>
          (* reached start of lineGap; 
           * return 0 which is start idx *)
          0
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isAlphaNum chr orelse chr = #"_" then
          if isPrevChrSpace (strPos, str, strTl)
            orelse isPrevChrNonBlank (strPos, str, strTl)
          then absIdx
          else helpPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
        else if Char.isSpace chr then
          helpPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
        else 
          (* is NON_BLANK *) 
          if isPrevChrSpace (strPos, str, strTl)
            orelse isPrevChrAlphaNum (strPos, str, strTl)
          then
            absIdx
          else
            helpPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
      end

  fun helpPrevWORD (strPos, str, absIdx, strTl, lineTl) =
    if strPos < 0 then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          helpPrevWORD (String.size shd - 1, shd, absIdx, stl, ltl)
      | (_, _) =>
          (* reached start of lineGap; 
           * return 0 which is start idx *)
          0
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isSpace chr then
          helpPrevWORD 
            (strPos - 1, str, absIdx - 1, strTl, lineTl)
        else 
          if isPrevChrSpace (strPos, str, strTl) then
            absIdx
          else
            helpPrevWORD 
              (strPos - 1, str, absIdx - 1, strTl, lineTl)
      end

  fun startPrevWord (shd, strIdx, absIdx, stl, ltl, fPrev) =
    (* we want to start iterating from previous character
     * and ignore the character the cursor is at 
     * so check previous character *)
    if strIdx > 0 then
      fPrev (strIdx - 1, shd, absIdx - 1, stl, ltl)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          let val prevIdx = String.size stlhd - 1
          in fPrev (prevIdx, stlhd, absIdx - 1, stltl, ltltl)
          end
      | (_, _) => 
          (* tl is empty; just return idx 0 *) 
          0

  fun toPrevWord (lineGap: LineGap.t, cursorIdx, fPrev) =
    let
      val
        {rightStrings, rightLines, leftStrings, leftLines, idx = bufferIdx, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              startPrevWord 
                (shd, strIdx, cursorIdx, leftStrings, leftLines, fPrev)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let
                     val strIdx = strIdx - String.size shd
                     val leftStrings = shd :: leftStrings
                     val leftLines = lhd :: leftLines
                   in
                     startPrevWord
                       (stlhd, strIdx, cursorIdx, leftStrings, leftLines, fPrev)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  (* equivalent of vi's 'b' command *)
  fun prevWord (lineGap, cursorIdx) =
    toPrevWord (lineGap, cursorIdx, helpPrevWord)

  (* equivalent of vi's 'B' command *)
  fun prevWORD (lineGap, cursorIdx) =
    toPrevWord (lineGap, cursorIdx, helpPrevWORD)

  fun helpEndOfWord (strPos, str, absIdx, stl, ltl) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) => helpEndOfWord (0, shd, absIdx, stl, ltl)
      | (_, _) => absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isAlphaNum chr orelse chr = #"_" then
          if isNextChrSpace (strPos, str, stl)
            orelse isNextChrNonBlank (strPos, str, stl)
          then absIdx
          else helpEndOfWord (strPos + 1, str, absIdx + 1, stl, ltl)
        else if Char.isSpace chr then
          helpEndOfWord (strPos + 1, str, absIdx + 1, stl, ltl)
        else 
          (* is NON_BLANK *) 
          if isNextChrSpace (strPos, str, stl)
            orelse isNextChrAlphaNum (strPos, str, stl)
          then
            absIdx
          else
            helpEndOfWord (strPos + 1, str, absIdx + 1, stl, ltl)
      end

  fun helpEndOfWORD (strPos, str, absIdx, stl, ltl) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) => helpEndOfWORD (0, shd, absIdx, stl, ltl)
      | (_, _) => absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isSpace chr then
          helpEndOfWORD (strPos + 1, str, absIdx + 1, stl, ltl)
        else 
          if isNextChrSpace (strPos, str, stl) then
            absIdx
          else
            helpEndOfWORD (strPos + 1, str, absIdx + 1, stl, ltl)
      end

  fun startEndOfWord (shd, strIdx, absIdx, stl, ltl, fEnd) =
    (* we want to start iterating from next char after strIdx *)
    if strIdx - 1 < String.size shd then
      fEnd (strIdx + 1, shd, absIdx + 1, stl, ltl)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          fEnd (0, stlhd, absIdx + 1, stltl, ltltl)
      | (_, _) => 
          (* tl is empty; just return absIdx *) 
          absIdx

  fun toEndOfWord (lineGap: LineGap.t, cursorIdx, fEnd) =
    let
      val
        {rightStrings, rightLines, leftStrings, leftLines, idx = bufferIdx, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              startEndOfWord
                (shd, strIdx, cursorIdx, rightStrings, rightLines, fEnd)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let 
                     val strIdx = strIdx - String.size shd
                   in 
                     startEndOfWord 
                       (stlhd, strIdx, cursorIdx, stltl, ltltl, fEnd)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  (* equivalent of vi's `e` command *)
  fun endOfWord (lineGap, cursorIdx) = 
    toEndOfWord (lineGap, cursorIdx, helpEndOfWord)

  (* equivalent of vi's `E` command *)
  fun endOfWORD (lineGap, cursorIdx) = 
    toEndOfWord (lineGap, cursorIdx, helpEndOfWORD)

  fun helpFirstNonSpaceChr (strPos, str, absIdx, stl, ltl) =
      if strPos = String.size str then
        case (stl, ltl) of
          (shd :: stl, lhd :: ltl) =>
            helpFirstNonSpaceChr
              (0, shd, absIdx, stl, ltl)
        | (_, _) =>
            absIdx - 1
      else
        let
          val chr = String.sub (str, strPos)
        in
          if Char.isSpace chr then
            helpFirstNonSpaceChr 
              (strPos + 1, str, absIdx + 1, stl, ltl)
          else
            absIdx
        end

  fun startFirstNonSpaceChr (shd, strIdx, absIdx, stl, ltl) =
    if strIdx  < String.size shd then
      helpFirstNonSpaceChr
        (strIdx, shd, absIdx, stl, ltl)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpFirstNonSpaceChr
            (0, stlhd, absIdx, stltl, ltltl)
      | (_, _) => 
          (* tl is empty; just return absIdx *) 
          absIdx

  (* Prerequisite: 
   * LineGap has been moved to start of line (provided with vi0). *)
  fun firstNonSpaceChr (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, rightLines, idx = bufferIdx, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              startFirstNonSpaceChr 
                (shd, strIdx, cursorIdx, stl, ltl)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let 
                     val strIdx = strIdx - String.size shd
                   in 
                     startFirstNonSpaceChr 
                       (stlhd, strIdx, cursorIdx, stltl, ltltl)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) =>
          cursorIdx
    end

  fun helpToNextChr (strPos, str, absIdx, stl, ltl, origIdx, findChr) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) => 
          helpToNextChr 
            (0, shd, absIdx, stl, ltl, origIdx, findChr)
      | (_, _) => 
          origIdx
    else 
      if String.sub (str, strPos) = findChr then
        absIdx 
      else
        helpToNextChr
          (strPos + 1, str, absIdx + 1, stl, ltl, origIdx, findChr)

  fun startToNextChr (shd, strIdx, absIdx, stl, ltl, findChr) =
    (* we want to start iterating from next char after strIdx *)
    if strIdx - 1 < String.size shd then
      helpToNextChr 
        (strIdx + 1, shd, absIdx + 1, stl, ltl, absIdx, findChr)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpToNextChr
            (0, stlhd, absIdx + 1, stltl, ltltl, absIdx, findChr)
      | (_, _) => 
          (* tl is empty; just return absIdx *) 
          absIdx

  fun nextChr (lineGap: LineGap.t, cursorIdx, chr, fStart) =
    let
      val {rightStrings, rightLines, idx = bufferIdx, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              fStart
                (shd, strIdx, cursorIdx, stl, ltl, chr)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let 
                     val strIdx = strIdx - String.size shd
                   in 
                     fStart
                       (shd, strIdx, cursorIdx, stltl, ltltl, chr)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  fun helpTillNextChr (strPos, str, absIdx, stl, ltl, origIdx, findChr, lastNonLine) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) => 
          helpTillNextChr 
            (0, shd, absIdx, stl, ltl, origIdx, findChr, lastNonLine)
      | (_, _) => 
          origIdx
    else 
      let
        val chr = String.sub (str, strPos)
      in
        if chr = findChr then
          lastNonLine
        else
          let
            val lastNonLine =
              if chr = #"\n" orelse chr = #"\r" then
                lastNonLine
              else
                absIdx
          in
            helpTillNextChr
              (strPos + 1, str, absIdx + 1, stl, ltl, origIdx, findChr, lastNonLine)
          end
      end

  fun startTillNextChr (shd, strIdx, absIdx, stl, ltl, findChr) =
    (* we want to start iterating from next char after strIdx *)
    if strIdx - 1 < String.size shd then
      helpTillNextChr 
        (strIdx + 1, shd, absIdx + 1, stl, ltl, absIdx, findChr, absIdx)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpTillNextChr
            (0, stlhd, absIdx + 1, stltl, ltltl, absIdx, findChr, absIdx)
      | (_, _) => 
          (* tl is empty; just return absIdx *) 
          absIdx

  fun tillNextChr (lineGap, cursorIdx, chr) =
    nextChr (lineGap, cursorIdx, chr, startTillNextChr)

  fun toNextChr (lineGap, cursorIdx, chr) =
    nextChr (lineGap, cursorIdx, chr, startToNextChr)

  fun helpToPrevChr (strPos, str, absIdx, stl, ltl, origIdx, findChr) =
    if strPos < 0 then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) => 
          helpToPrevChr 
            (String.size shd - 1, shd, absIdx, stl, ltl, origIdx, findChr)
      | (_, _) => 
          origIdx
    else 
      if String.sub (str, strPos) = findChr then
        absIdx 
      else
        helpToPrevChr
          (strPos - 1, str, absIdx - 1, stl, ltl, origIdx, findChr)

  fun startToPrevChr (shd, strIdx, absIdx, stl, ltl, findChr) =
    (* we want to start iterating from Prev char after strIdx *)
    if strIdx > 0 then
      helpToPrevChr 
        (strIdx - 1, shd, absIdx - 1, stl, ltl, absIdx, findChr)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpToPrevChr
            (String.size stlhd - 1, stlhd, absIdx - 1, stltl, ltltl, absIdx, findChr)
      | (_, _) => 
          (* tl is empty; return 0 for lineGap start *) 
          0

  fun toPrevChr (lineGap: LineGap.t, cursorIdx, chr) =
    let
      val {rightStrings, rightLines, idx = bufferIdx, leftStrings, leftLines, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              startToPrevChr 
                (shd, strIdx, cursorIdx, leftStrings, leftLines, chr)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let 
                     val strIdx = strIdx - String.size shd
                     val leftStrings = shd :: leftStrings
                     val leftLines = lhd :: leftLines
                   in 
                     startToPrevChr 
                       (shd, strIdx, cursorIdx, leftStrings, leftLines, chr)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end
end
