structure Cursor =
struct
  fun helpVi0String (strPos, str, absIdx, strTl, lineTl) =
    if strPos < 0 then
      helpVi0List (strTl, lineTl, absIdx)
    else
      case String.sub (str, strPos) of
        #"\n" =>
          absIdx + 1
      | _ =>
          helpVi0String (strPos - 1, str, absIdx - 1, strTl, lineTl)

  and helpVi0List (strings, lines, absIdx) =
    case (strings, lines) of
      (strHd::strTl, lineHd::lineTl) =>
        helpVi0String
          ( String.size strHd - 1, strHd, absIdx
          , strTl, lineTl
          )
    | (_, _) =>
        (* this case means strings and lines are empty
         * and empty means we are at first line
         * so we can return 0 *)
        0

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
                helpVi0String
                  ( strIdx - 1, strHd, cursorIdx - 1
                  , strTl, lnTl
                  )
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
                       helpVi0String
                         ( strIdx - 1, nestStrHd, cursorIdx - 1
                         , strHd :: leftStrings, lnHd :: leftLines
                         )
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => 
          (* nowhere to go, so return cursorIdx *) 
          cursorIdx
    end

  fun helpViDlrString (strPos, str, absIdx, strTl, lineTl) =
    if strPos = String.size str then
      helpViDlrList (strTl, lineTl, absIdx)
    else
      case String.sub (str, strPos) of
        #"\n" =>
          absIdx - 1
      | _ =>
          helpViDlrString (strPos + 1, str, absIdx + 1, strTl, lineTl)

  and helpViDlrList (strings, lines, absIdx) =
    case (strings, lines) of
      (strHd::strTl, lineHd::lineTl) =>
        helpViDlrString
          (0, strHd, absIdx , strTl, lineTl)
    | (_, _) =>
        (* this case means strings and lines are empty
         * and empty means we have reached end of lineGap
         * so we can return last chr *)
        absIdx - 1
        
  fun viDlr(lineGap: LineGap.t, cursorIdx) =
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
                helpViDlrString
                  (strIdx + 1, strHd, cursorIdx + 1, strTl, lnTl)
              else
                (* check if we are in double linebreak *)
                if strIdx - 1 >= 0 then
                  if String.sub (strHd, strIdx - 1) = #"\n" then
                    (* we are in double linebreak, so do nothing *)
                    cursorIdx
                  else
                    (* not in double linebreak, so iterate *)
                    helpViDlrString
                      (strIdx + 1, strHd, cursorIdx + 1 , strTl, lnTl)
                else
                  (* check if double linebreak in strTl *)
                  (case strTl of
                    nestStrHd :: _ =>
                      if String.sub (nestStrHd, 0) = #"\n" then
                        cursorIdx
                      else
                        helpViDlrString
                          (strIdx + 1, strHd, cursorIdx + 1, strTl, lnTl)
                  | [] => cursorIdx)
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: nestStrTl, nestLnHd :: nestLnTl) =>
                   let
                     val strIdx = strIdx - String.size strHd
                   in
                     helpViDlrString 
                       (strIdx + 1, nestStrHd, cursorIdx + 1, nestStrTl, nestLnTl)
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
                   (* if there is another string after current head, we can increment cursorIdx 
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

  fun helpViJString
    ( strPos, str, absIdx
    , lineColumn, preferredColumn, hasPassedLine
    , strTl, lineTl
    ) =
      if strPos = String.size str then
        helpViJList
          (absIdx, lineColumn, preferredColumn, hasPassedLine, strTl, lineTl)
      else
        case String.sub (str, strPos) of
          #"\n" =>
            if
              hasPassedLine
            then
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
                  helpViJString
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
                       helpViJString
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
              helpViJString
                ( strPos + 1, str, absIdx + 1
                , lineColumn + 1, preferredColumn, hasPassedLine
                , strTl, lineTl
                )

  and helpViJList
    (absIdx, lineColumn, preferredColumn, hasPassedLine, strings, lines) =
    case (strings, lines) of
      (strHd :: strTl, lineHd :: lineTl) =>
        (* todo: possibly check if we have passed line,
         * and if so, if there are any line breaks in the lineHd
         * which we could use to skip searching part of the string. 
         * However, this will likely have worse cache locality 
         * as we switch to searching in string to searcing in line vector
         * so perhaps not. *)
        helpViJString
          ( 0, strHd, absIdx
          , lineColumn, preferredColumn, hasPassedLine
          , strTl, lineTl
          )
    | (_, _) => 
        (* empty, so return end of previous string *) 
        absIdx - 1

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
                helpViJString
                  (strIdx + 1, strHd, cursorIdx + 1, 0, 0, true, strTl, lnTl)
              else
                (* not at newline 
                 * so get column number and start iterating *)
                let
                  val lineColumn = getCursorColumn (lineGap, cursorIdx)
                in
                  helpViJString
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
                       helpViJString
                         ( strIdx + 1, nestStrHd, cursorIdx + 1
                         , 0, 0, true
                         , nestStrTl, nestLnTl
                         )
                     else
                       (* not in linebreak *)
                       let
                         val lineColumn = getCursorColumn (lineGap, cursorIdx)
                       in
                         helpViJString
                           ( strIdx + 1, nestStrHd, cursorIdx + 1
                           , lineColumn, lineColumn, false
                           , nestStrTl, nestStrTl
                           )
                       end
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => 
          (* nowhere to go rightward, so return cursorIdx *) 
          cursorIdx
    end

  fun helpViKString
    ( strPos, str, absIdx
    , lineColumn, preferredColumn, hasPassedLine
    , strTl, lineHd, lineTl
    ) =
      if strPos < 0 then
        helpViKList
          (absIdx, lineColumn, preferredColumn, hasPassedLine, strTl, lineTl)
      else
        case String.sub (str, strPos) of
          #"\n" =>
            if
              hasPassedLine
            then
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
                    helpViKString
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
                         helpViKString
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
              helpViKString
                ( strPos - 1, str, absIdx - 1
                , lineColumn - 1, preferredColumn, hasPassedLine
                , strTl, lineHd, lineTl
                )

  and helpViKList
    (absIdx, lineColumn, preferredColumn, hasPassedLine, strings, lines) =
    case (strings, lines) of
      (strHd :: strTl, lineHd :: lineTl) =>
        (* todo: possibly check if we have passed line,
         * and if so, if there are any line breaks in the lineHd
         * which we could use to skip searching part of the string. 
         * However, this will likely have worse cache locality 
         * as we switch to searching in string to searcing in line vector
         * so perhaps not. *)
        helpViKString
          ( String.size strHd - 1, strHd, absIdx
          , lineColumn, preferredColumn, hasPassedLine
          , strTl, lineHd, lineTl
          )
    | (_, _) => 
        (* empty, so return start of previous string *) 
        absIdx + 1

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
                    helpVi0String 
                      (strIdx - 2, strHd, cursorIdx - 2, leftStrings, leftLines)
                  else
                    (* not in double linebreak *)
                    helpVi0String 
                      (strIdx - 1, strHd, cursorIdx - 1, leftStrings, leftLines)
                else
                  (* check leftStrings to see if we are in a double linebreak *)
                  (case (leftStrings, leftLines) of
                     (lStrHd :: lStrTl, lLnHd :: lLnTl) =>
                       if String.sub (lStrHd, String.size lStrHd - 1) = #"\n" then
                         (* in double linebreak *)
                        helpVi0String 
                          (String.size lStrHd - 2, lStrHd, cursorIdx - 2, lStrTl, lLnTl)
                       else
                         (* in single linebreak *)
                        helpVi0String 
                          (strIdx - 1, strHd, cursorIdx - 1, leftStrings, leftLines)
                   | (_, _) =>
                       helpViKString
                         ( strIdx - 1, strHd, cursorIdx - 1
                         , 0, 0, true, leftStrings, lnHd, leftLines 
                         ))
              else
                (* not at newline 
                 * so get column number and start iterating *)
                let
                  val lineColumn = getCursorColumn (lineGap, cursorIdx)
                in
                  helpViKString
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
                             helpVi0String 
                               ( strIdx - 2, nestStrHd, cursorIdx - 2
                               , leftStrings, leftLines 
                               )
                           end
                         else
                           (* is in single linebreak *)
                           helpVi0String 
                             ( strIdx - 1, nestStrHd, cursorIdx - 1
                             , leftStrings, leftLines 
                             )
                       else
                         (* must check strHd for second linebreak *)
                         if 
                           String.sub (strHd, String.size strHd - 1) = #"\n"
                         then
                           (* is in double linebreak *)
                           helpVi0String
                             ( String.size strHd - 2, nestStrHd, cursorIdx - 2
                             , leftStrings, leftLines 
                             )
                         else
                           (* is in single linebreak *)
                           helpVi0String
                             ( String.size strHd - 1, nestStrHd, cursorIdx - 1
                             , leftStrings, leftLines 
                             )
                     else
                       (* not in linebreak *)
                       let
                         val lineColumn = getCursorColumn (lineGap, cursorIdx)
                       in
                         helpViKString
                           ( strIdx - 1, nestStrHd, cursorIdx - 1
                           , lineColumn, lineColumn, false
                           , strHd :: leftStrings, nestLnHd, lnHd :: leftLines
                           )
                       end
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => 
          (* nowhere to go rightward, so return cursorIdx *) 
          cursorIdx
    end

  (*
   * nvim's motion.txt document describes a word as:
   * - A sequence of (letters, digits and underscores)
   * - or a sequence of other non-blank characters
   * - separated by white space (space, tab, <EOL>)
   *)
  datatype word_type =
    ALPHA_NUM
  | SPACE
  | NON_BLANK

  fun getWordType chr =
    if Char.isAlphaNum chr orelse chr = #"_" then
      ALPHA_NUM
    else if Char.isSpace chr then
      SPACE
    else
      NON_BLANK

  fun helpNextWordString 
    ( strPos, str, absIdx 
    , strTl, lineTl 
    , prevWordType, hasPassedSpace) =
      if strPos = String.size str then
        helpNextWordList 
          (strTl, lineTl, absIdx, prevWordType, hasPassedSpace)
      else
        let
          val chr = String.sub (str, strPos)
        in
          if Char.isAlphaNum chr orelse chr = #"_" then
            case prevWordType of
              ALPHA_NUM =>
                (* current chr is ALPHA_NUM
                 * and previous chr was also ALPHA_NUM
                 * so continue iterating *)
                helpNextWordString
                  ( strPos + 1, str, absIdx + 1
                  , strTl, lineTl 
                  , ALPHA_NUM, hasPassedSpace
                  )
            | SPACE =>
                (* we moved from SPACE to ALPHA_NUM, 
                 * meaning we may have reached a new word. *)
                if hasPassedSpace then
                  absIdx
                else
                  helpNextWordString
                    ( strPos + 1, str, absIdx + 1
                    , strTl, lineTl 
                    , ALPHA_NUM, true
                    )
            | NON_BLANK=>
                (* moved from NON_BLANK to ALPHA_NUM,
                 * meaning we reached new word *)
                 absIdx
          else if Char.isSpace chr then
            (* nothing to do on space, except keep iterating *)
            helpNextWordString
              ( strPos + 1, str, absIdx + 1
              , strTl, lineTl 
              , SPACE, true
              )
          else
            (* chr is NON_BLANK. *)
            case prevWordType of
              NON_BLANK =>
                (* moved from non-blank to non-blank
                 * so keep iterating *)
                helpNextWordString
                  ( strPos + 1, str, absIdx + 1
                  , strTl, lineTl
                  , NON_BLANK, hasPassedSpace
                  )
            | ALPHA_NUM => 
                (* moved from ALPHA_NUM to non-blank
                 * so we have reached new word *)
                absIdx
            | SPACE =>
                (* from space to non-blank *)
                if hasPassedSpace then
                  absIdx
                else
                  helpNextWordString
                    ( strPos + 1, str, absIdx + 1
                    , strTl, lineTl
                    , NON_BLANK, true
                    )
        end

  and helpNextWordList (strings, lines, absIdx, prevWordType, hasPassedSpace) =
    case (strings, lines) of
      (strHd::strTl, lineHd::lineTl) =>
        helpNextWordString
          ( 0, strHd, absIdx, strTl, lineTl, prevWordType, hasPassedSpace )
    | (_, _) =>
        (* reached end of lineGap; 
         * return last valid chr position *)
        absIdx - 1

  fun startNextWord (shd, strIdx, absIdx, stl, ltl) =
    let
      val chr = String.sub (shd, strIdx)
      val wordType = getWordType chr
      val isSpace = wordType = SPACE
    in
      helpNextWordString 
        (strIdx + 1, shd, absIdx + 1, stl, ltl, wordType, isSpace)
    end

  fun nextWord (lineGap: LineGap.t, cursorIdx) =
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
                startNextWord (shd, strIdx, cursorIdx, stl, ltl)
              else
                (* strIdx is in tl *)
                (case (stl, ltl) of
                    (stlhd :: stltl, ltlhd :: ltltl) =>
                      let
                        val strIdx = strIdx - String.size shd
                      in
                        startNextWord 
                          (stlhd, strIdx, cursorIdx, stltl, ltltl)
                      end
                  | (_, _) => cursorIdx)
            end
        | (_, _) => cursorIdx
    end

  fun isPrevChrSpace (strPos, str, strTl) =
    if strPos > 0 then
      let
        val prevChr = String.sub (str, strPos - 1)
      in
        Char.isSpace prevChr
      end
    else
      case strTl of
        hd :: _ =>
          let
            val prevChr = String.sub (hd, String.size hd - 1)
          in
            Char.isSpace prevChr
          end
      | [] => true

  fun helpPrevWordString 
    (strPos, str, absIdx, strTl, lineTl, lastWordType) =
      if strPos < 0 then
        helpPrevWordList 
          (strTl, lineTl, absIdx, lastWordType)
      else
        let
          val chr = String.sub (str, strPos)
        in
          if Char.isAlphaNum chr orelse chr = #"_" then
            case lastWordType of
              ALPHA_NUM =>
                (* check if prev chr is space;
                 * if it is, return current idx
                 * and if not, keep iterating *)
                 if isPrevChrSpace (strPos, str, strTl) then
                   absIdx 
                 else
                   helpPrevWordString
                    ( strPos - 1, str, absIdx - 1
                    , strTl, lineTl, ALPHA_NUM
                    )
            | SPACE =>
                (* last chr checked was space
                 * and this chr is ALPHA_NUM 
                 * so keep iterating *)
                 helpPrevWordString
                  ( strPos - 1, str, absIdx - 1
                  , strTl, lineTl, ALPHA_NUM
                  )
            | NON_BLANK =>
                (* last chr was NON_BLANK
                 * and this chr is ALPHA_NUM
                 * and this change is a word break.
                 * So return idx of last chr. *)
                 absIdx + 1
          else if Char.isSpace chr then
            case lastWordType of
              SPACE =>
                (* nothing to do on double space, 
                 * except keep iterating *)
                 helpPrevWordString
                  ( strPos - 1, str, absIdx - 1
                  , strTl, lineTl, SPACE
                  )
            | _ =>
                (* last chr was either ALPHA_NUM or NON_BLANK
                 * and current chr is space
                 * so we have word break.
                 * thus, return start of last word *)
                absIdx + 1
          else
            (* is NON_BLANK *)
            case lastWordType of
              NON_BLANK =>
                (* last and current wordType is same
                 * so keep iterating *)
                 if isPrevChrSpace (strPos, str, strTl) then
                   absIdx
                 else
                   helpPrevWordString
                    ( strPos - 1, str, absIdx - 1
                    , strTl, lineTl, NON_BLANK
                    )
            | ALPHA_NUM =>
                (* word break since we last chr was ALPHA_NUM
                 * and this one is NON_BLANK 
                 * so return idx of last chr *)
                 absIdx + 1
            | SPACE =>
                (* space means keep iterating *)
               helpPrevWordString
                ( strPos - 1, str, absIdx - 1
                , strTl, lineTl, NON_BLANK
                )
        end

  and helpPrevWordList (strings, lines, absIdx, lastWordType) =
    case (strings, lines) of
      (strHd::strTl, lineHd::lineTl) =>
        helpPrevWordString
          ( String.size strHd - 1, strHd, absIdx
          , strTl, lineTl, lastWordType 
          )
    | (_, _) =>
        (* reached start of lineGap; 
         * return 0 which is start idx *)
        0

  fun startPrevWord (shd, strIdx, absIdx, stl, ltl) =
    (* we want to start iterating from previous character
     * and ignore the character the cursor is at 
     * so check previous character *)
    if strIdx > 0 then
      let
        val prevChr = String.sub (shd, strIdx - 1)
        val prevWordType = getWordType prevChr
      in
        helpPrevWordString 
          (strIdx - 1, shd, absIdx - 1, stl, ltl, prevWordType)
      end
    else
      case (stl, ltl) of
        (stlhd::stltl, ltlhd::ltltl) =>
          let
            val prevIdx = String.size stlhd - 1
            val prevChr = String.sub (stlhd, prevIdx)
            val prevWordType = getWordType prevChr
          in
            helpPrevWordString
              (prevIdx, stlhd, absIdx - 1, stltl, ltltl, prevWordType)
          end
      | (_, _) =>
          (* tl is empty; just return idx 0 *)
          0

  (* equivalent of vi's `b` command *)
  fun prevWord (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, rightLines, leftStrings, leftLines, idx = bufferIdx, ...} = lineGap
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
                (shd, strIdx, cursorIdx, leftStrings, leftLines)
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
                        (stlhd, strIdx, cursorIdx, leftStrings, leftLines)
                    end
                | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  fun isNextChrSpace (strPos, str, strTl) =
    if strPos - 1 < String.size str then
      let
        val chr = String.sub (str, strPos + 1)
      in
        Char.isSpace chr
      end
    else
      case strTl of
        hd :: _ => 
          let
            val chr = String.sub (hd, 0)
          in
            Char.isSpace chr
          end
      | [] => false

  fun isNextChrNonBlank (strPos, str, strTl) =
    if strPos - 1 < String.size str then
      let
        val chr = String.sub (str, strPos + 1)
        val isNotBlank = 
          Char.isSpace chr 
          orelse Char.isAlphaNum chr 
          orelse chr = #"_"
      in
        not isNotBlank
      end
    else
      case strTl of
        hd :: _ =>
          let
            val chr = String.sub (hd, 0)
            val isNotBlank =
              Char.isSpace chr
              orelse Char.isAlphaNum chr
              orelse chr = #"_"
          in
            not isNotBlank
          end
      | [] => false

  fun isNextChrAlphaNum (strPos, str, stl) =
    if strPos - 1 < String.size str then
      let
        val chr = String.sub (str, strPos + 1)
      in
        Char.isAlphaNum chr orelse chr = #"_"
      end
    else
      case stl of
        hd :: _ =>
          let
            val chr = String.sub (str, strPos + 1)
          in
            Char.isAlphaNum chr orelse chr = #"_"
          end
      | [] => false

  fun helpEndOfWordString 
    (strPos, str, absIdx, stl, ltl) =
      if strPos = String.size str then
        helpEndOfWordList (stl, ltl, absIdx)
      else
        let
          val chr = String.sub (str, strPos)
        in
          if Char.isAlphaNum chr orelse chr = #"_" then
            if isNextChrSpace (strPos, str, stl) 
            orelse isNextChrNonBlank (strPos, str, stl) then
              absIdx
            else
              helpEndOfWordString
                (strPos + 1, str, absIdx + 1, stl, ltl)
          else if Char.isSpace chr then
            helpEndOfWordString
              (strPos + 1, str, absIdx + 1, stl, ltl)
          else
            (* is NON_BLANK *)
            if isNextChrSpace (strPos, str, stl) 
            orelse isNextChrAlphaNum (strPos, str, stl) then
              absIdx
            else
              helpEndOfWordString 
                (strPos + 1, str, absIdx + 1, stl, ltl)
        end

  and helpEndOfWordList (strings, lines, absIdx) =
    case (strings, lines) of
      (shd :: stl, lhd :: ltl) =>
        helpEndOfWordString (0, shd, absIdx, stl, ltl)
    | (_, _) => 
        absIdx - 1

  fun startEndOfWord (shd, strIdx, absIdx, stl, ltl) =
    (* we want to start iterating from next char after strIdx *)
    if strIdx - 1 < String.size shd then
      let
        val nextChr = String.sub (shd, strIdx + 1)
      in
        helpEndOfWordString 
          (strIdx + 1, shd, absIdx + 1, stl, ltl)
      end
    else
      case (stl, ltl) of
        (stlhd::stltl, ltlhd::ltltl) =>
          let
            val nextChr = String.sub (stlhd, 0)
            val wordType = getWordType nextChr
          in
            helpEndOfWordString
              (0, stlhd, absIdx + 1, stltl, ltltl)
          end
      | (_, _) =>
          (* tl is empty; just return absIdx *)
          absIdx

  (* equivalent of vi's `e` command *)
  fun endOfWord (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, rightLines, leftStrings, leftLines, idx = bufferIdx, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              startEndOfWord 
                (shd, strIdx, cursorIdx, rightStrings, rightLines)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                  (stlhd :: stltl, ltlhd :: ltltl) =>
                    let
                      val strIdx = strIdx - String.size shd
                    in
                      startEndOfWord
                        (stlhd, strIdx, cursorIdx, stltl, ltltl)
                    end
                | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end
end
