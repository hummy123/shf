structure Cursor =
struct
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
                            (* if there is at least one character before \n 
                             * then decrement cursorIdx by 2 *)
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
                               _ :: _ => cursorIdx - 2
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

  (* Prerequisite: lineGap is moved to cursorIdx *)
  fun getCursorColumn (lineGap: LineGap.t, cursorIdx) =
    let
      val
        {rightStrings, idx = bufferIdx, rightLines, leftStrings, leftLines, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (strHd :: _, lnHd :: _) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
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
          end
      | (_, _) => helpGetCursorColumn (0, leftStrings, leftLines)
    end
end