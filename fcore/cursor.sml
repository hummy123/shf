structure Cursor =
struct
  (* returns absolute index of previous line break in string *)
  fun helpVi0 (absIdx, stl, ltl) =
    case (stl, ltl) of
      (shd :: stl, lhd :: ltl) =>
        if Vector.length lhd > 0 then
          let
            val startAbsIdx = absIdx - String.size shd
            val lineIdx = Vector.sub (lhd, Vector.length lhd - 1)
          in
            (* found lineIdx. 
             * Need to make sure we follow cursor-on-linebreak rule:
             * If line break is preceded by non-line break char,
             * then increment by 1.
             * *)
            startAbsIdx + lineIdx + 1
          end
        else
          helpVi0 (absIdx - String.size shd, stl, ltl)
    | (_, _) => 0

  fun startVi0 (strPos, shd, lhd, absIdx, stl, ltl) =
    if String.sub (shd, strPos) = #"\n" then
      absIdx
    else if Vector.length lhd > 0 then
      if Vector.sub (lhd, 0) < strPos then
        let
          val linePos = BinSearch.equalOrLess (strPos - 1, lhd)
          val lineIdx = Vector.sub (lhd, linePos)
        in
          absIdx - strPos + lineIdx + 1
        end
      else
        helpVi0 (absIdx - strPos, stl, ltl)
    else
      helpVi0 (absIdx - strPos, stl, ltl)

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
              startVi0 (strIdx, strHd, lnHd, cursorIdx, leftStrings, leftLines)
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: _, nestLnHd :: _) =>
                   let
                     val strIdx = strIdx - String.size strHd
                   in
                     startVi0
                       ( strIdx
                       , nestStrHd
                       , nestLnHd
                       , cursorIdx
                       , strHd :: leftStrings
                       , lnHd :: leftLines
                       )
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => (* nowhere to go, so return cursorIdx *) cursorIdx
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
              else (* check if we are in double linebreak *) if strIdx - 1 >= 0 then
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
      | (_, _) => (* nowhere to go, so return cursorIdx *) cursorIdx
    end

  fun helpViL (strIdx, hd, cursorIdx, tl) =
    if String.sub (hd, strIdx) = #"\n" then
      (* if line break, go to next idx, no matter what chr is there *)
      cursorIdx + 1
    else if strIdx + 1 < String.size hd then
      (* next chr is in hd *)
      if String.sub (hd, strIdx + 1) = #"\n" then
        (* if non-line break followed by line break, go to chr + 2 *)
        cursorIdx + 2
      else
        cursorIdx + 1
    else
      (case tl of
         tlhd :: _ =>
           if String.sub (tlhd, 0) = #"\n" then
             (* non-line break followed by line break *)
             cursorIdx + 2
           else
             (* non-line break followed by non-line break *)
             cursorIdx + 1
       | [] => cursorIdx)

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
            if strIdx < String.size hd then
              (* strIdx is in hd *)
              helpViL (strIdx, hd, cursorIdx, tl)
            else
              (* strIdx is in tl *)
              (case tl of
                 tlhd :: tltl =>
                   let val strIdx = strIdx - String.size hd
                   in helpViL (strIdx, tlhd, cursorIdx, tltl)
                   end
               | [] => cursorIdx)
          end
      | [] =>
          (* return original cursorIdx if there is nothing to the right *)
          cursorIdx
    end

  fun helpViH (strIdx, hd, cursorIdx, leftStrings) =
    if strIdx > 0 then
      (* bounds check: can access prev char in hd *)
      if String.sub (hd, strIdx - 1) = #"\n" then
        (* prev char is line break *)
        if strIdx - 1 > 0 then
          (* bounds check: can access two chars back in hd *)
          if String.sub (hd, strIdx - 2) = #"\n" then
            (* line break followed by line break
             * so it is fine to decrement by 1 *)
            cursorIdx - 1
          else
            (* non-line break followed by line break 
             * so we have to decrement by two,
             * skipping over line break *)
            cursorIdx - 2
        else
          (* need to check two chars back in leftStrings *)
          (case leftStrings of
             lhd :: ltl =>
               if String.sub (lhd, String.size lhd - 1) = #"\n" then
                 (* double line break *)
                 cursorIdx - 1
               else
                 (* non-line break precedes line break *)
                 cursorIdx - 2
           | [] => cursorIdx - 1)
      else
        (* prev char is not line break so we can decrement by 1 *)
        cursorIdx - 1
    else
      (* prev char is in leftStrings *)
      (case leftStrings of
         lhd :: ltl =>
           if String.sub (lhd, String.size lhd - 1) = #"\n" then
             (* one line break *)
             if String.size lhd > 1 then
               (* bounds check: prev-prev chr is in lhd *)
               if String.sub (lhd, String.size lhd - 2) = #"\n" then
                 (* double line break *)
                 cursorIdx - 1
               else
                 (* non-line break precedes line break *)
                 cursorIdx - 2
             else
               (* prev-prev chr is in ltl *)
               (case ltl of
                  ltlhd :: _ =>
                    if String.sub (ltlhd, String.size ltlhd - 1) = #"\n" then
                      (* double line break *)
                      cursorIdx - 1
                    else
                      (* non-line break precedes line break *)
                      cursorIdx - 2
                | [] => cursorIdx - 1)
           else
             (* no line break *)
             cursorIdx - 1
       | [] => 0)

  (* Prerequisite: lineGap is moved to requested idx first. *)
  fun viH (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, leftStrings, idx = bufferIdx, ...} = lineGap
    in
      case rightStrings of
        hd :: tl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size hd then
              (* strIdx in hd *)
              helpViH (strIdx, hd, cursorIdx, leftStrings)
            else
              (* strIdx in tl *)
              (case tl of
                 tlhd :: tltl =>
                   let val strIdx = strIdx - String.size hd
                   in helpViH (strIdx, tlhd, cursorIdx, hd :: leftStrings)
                   end
               | [] => cursorIdx)
          end
      | [] => cursorIdx
    end

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

  fun helpGetCursorColumnLeft (leftStrings, leftLines, cursorIdx) =
    case (leftStrings, leftLines) of
      (lshd :: lstl, llhd :: lltl) =>
        let
          val cursorIdx = cursorIdx - String.size lshd
        in
          if Vector.length llhd > 0 then
            let val lnIdx = Vector.sub (llhd, Vector.length llhd - 1)
            in lnIdx + cursorIdx
            end
          else
            helpGetCursorColumnLeft (lstl, lltl, cursorIdx)
        end
    | (_, _) => Int.max (cursorIdx, 0)

  fun getCursorColumn (strIdx, strHd, lnHd, leftStrings, leftLines, cursorIdx) =
    if Vector.length lnHd > 0 then
      let
        val firstLn = Vector.sub (lnHd, 0)
      in
        if firstLn > strIdx then
          (* search left strings/lines *)
          let
            val lineIdx =
              helpGetCursorColumnLeft
                (leftStrings, leftLines, cursorIdx - strIdx)
          in
            if lineIdx = 0 then cursorIdx else cursorIdx - lineIdx - 1
          end
        else if firstLn < strIdx then
          (* binary search in here 
           * because we know lnHd definitely contains
           * a lineIdx less or equal to strIdx *)
          let
            val lnIdx = BinSearch.equalOrLess (strIdx, lnHd)
            val lnIdx = Vector.sub (lnHd, lnIdx)
          in
            if lnIdx < strIdx then strIdx - lnIdx - 1
            else (* firstLn = strIdx *) 0
          end
        else
          (* firstLn = strIdx
           * meaning that we are already at a line break
           * and that the column is 0 *)
          0
      end
    else
      let
        val lineIdx =
          helpGetCursorColumnLeft (leftStrings, leftLines, cursorIdx - strIdx)
      in
        if lineIdx = 0 then cursorIdx else cursorIdx - lineIdx - 1
      end

  fun helpViJ
    ( strPos
    , str
    , absIdx
    , lineColumn
    , preferredColumn
    , hasPassedLine
    , strTl
    , lineTl
    , prevIsLn
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
            ( 0
            , shd
            , absIdx
            , lineColumn
            , preferredColumn
            , hasPassedLine
            , stl
            , ltl
            , prevIsLn
            )
      | (_, _) => (* empty, so return end of previous string *) absIdx - 1
    else
      case String.sub (str, strPos) of
        #"\n" =>
          if hasPassedLine then
            (* reached end of line twice, 
             * but line has fewer chars than preferredColumn *)
            if prevIsLn then
              (* line break is preceded by linebreak *)
              absIdx
            else
              (* line break is preceded by graphical chr
               * so go to graphical chr *)
              absIdx - 1
          else
            (* reached end of line once;
             * continue iterationg *)
            helpViJ
              ( strPos + 1
              , str
              , absIdx + 1
              , 0
              , preferredColumn
              , true
              , strTl
              , lineTl
              , true
              )
      | _ =>
          if lineColumn = preferredColumn andalso hasPassedLine then
            (* we're at the preferredColumn so return absIdx *)
            absIdx
          else
            (* we're not in the preferred column, so keep iterating *)
            helpViJ
              ( strPos + 1
              , str
              , absIdx + 1
              , lineColumn + 1
              , preferredColumn
              , hasPassedLine
              , strTl
              , lineTl
              , false
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
              let
                val lineColumn = getCursorColumn
                  (strIdx, strHd, lnHd, leftStrings, leftLines, cursorIdx)
              in
                helpViJ
                  ( strIdx
                  , strHd
                  , cursorIdx
                  , lineColumn
                  , lineColumn
                  , false
                  , strTl
                  , lnTl
                  , false
                  )
              end
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: nestStrTl, nestLnHd :: nestLnTl) =>
                   let
                     val strIdx = strIdx - String.size strHd
                     val leftStrings = strHd :: leftStrings
                     val leftLines = lnHd :: leftLines
                     val lineColumn = getCursorColumn
                       ( strIdx
                       , nestStrHd
                       , nestLnHd
                       , leftStrings
                       , leftLines
                       , cursorIdx
                       )
                   in
                     helpViJ
                       ( strIdx
                       , nestStrHd
                       , cursorIdx
                       , lineColumn
                       , lineColumn
                       , false
                       , nestStrTl
                       , nestLnTl
                       , false
                       )
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => (* nowhere to go rightward, so return cursorIdx *) cursorIdx
    end

  fun helpViK
    ( strPos
    , str
    , absIdx
    , lineColumn
    , preferredColumn
    , hasPassedLine
    , strTl
    , lineHd
    , lineTl
    ) =
    if strPos < 0 then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          helpViK
            ( String.size shd - 1
            , shd
            , absIdx
            , lineColumn
            , preferredColumn
            , hasPassedLine
            , stl
            , lhd
            , ltl
            )
      | (_, _) => (* empty, so return start of previous string *) absIdx + 1
    else
      case String.sub (str, strPos) of
        #"\n" =>
          if hasPassedLine then
            (* reached line break twice, 
             * but line has fewer chars than preferredColumn 
             * so go back to chr immediately after this second line break *)
            absIdx + 1
          else
            (* reached start of line once;
            * have to check if this is a double linebreak,
            * and return idx of second linebreak if so *)
            let
              (* have to calculate column of current line
               * so we know which line to stop searching at *)
              val lineColumn = getCursorColumn
                (strPos - 1, str, lineHd, strTl, lineTl, absIdx - 1)
            in
              helpViK
                ( strPos - 1
                , str
                , absIdx - 1
                , lineColumn
                , preferredColumn
                , true
                , strTl
                , lineHd
                , lineTl
                )
            end
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
              ( strPos - 1
              , str
              , absIdx - 1
              , lineColumn - 1
              , preferredColumn
              , hasPassedLine
              , strTl
              , lineHd
              , lineTl
              )

  fun startViK (strIdx, shd, cursorIdx, leftStrings, lhd, leftLines) =
    if String.sub (shd, strIdx) = #"\n" then
      (* ? -> ? -> \n *)
      if strIdx > 0 then
        (* strIdx - 1 is in shd *)
        if String.sub (shd, strIdx - 1) = #"\n" then
          (* ? -> \n -> \n *)
          if strIdx > 1 then
            (* strIdx - 2 is in shd *)
            if String.sub (shd, strIdx - 2) = #"\n" then
              (* \n -> \n -> \n 
               * so it is safe to decrement cursorIdx by 1 *)
              cursorIdx - 1
            else
              (* graphical-chr -> \n -> \n 
               * so go to beginning of line, 
               * starting from graphical-chr *)
              startVi0
                (strIdx - 2, shd, lhd, cursorIdx - 2, leftStrings, leftLines)
          else
            (* strIdx - 2 is in leftStrings *)
            case (leftStrings, leftLines) of
              (lshd :: lstl, llhd :: lltl) =>
                if String.sub (lshd, String.size lshd - 1) = #"\n" then
                  (* \n -> \n -> \n 
                   * so it is safe to decrement cursorIdx by 1 *)
                  cursorIdx - 1
                else
                  (* graphical-chr -> \n -> \n 
                   * so go to beginning of line, 
                   * starting from graphical-chr *)
                  startVi0
                    ( String.size lshd - 1
                    , lshd
                    , llhd
                    , cursorIdx - 2
                    , lstl
                    , lltl
                    )
            | (_, _) =>
                (* nothing to the left, so we are at start of buffer *)
                0
        else
          (* ? -> graphical-chr -> \n 
           * Don't expect this case to happen
           * but if it does, go to start of line. *)
          startVi0 (strIdx - 1, shd, lhd, cursorIdx - 1, leftStrings, leftLines)
      else
        (* strIdx - 1 is in leftStrings *)
        case (leftStrings, leftLines) of
          (lshd :: lstl, llhd :: lltl) =>
            if String.sub (lshd, String.size lshd - 1) = #"\n" then
              (* ? -> \n -> \n *)
              if String.size lshd > 1 then
                (* cursorIdx - 2 is in this string *)
                if String.sub (lshd, String.size lshd - 2) = #"\n" then
                  (* \n -> \n -> \n *)
                  cursorIdx - 1
                else
                  (* graphical-chr -> \n -> \n *)
                  startVi0
                    ( String.size lshd - 2
                    , lshd
                    , llhd
                    , cursorIdx - 2
                    , lstl
                    , lltl
                    )
              else
                (* cursorIdx - 2 is in lstl *)
                (case (lstl, lltl) of
                   (stlhd :: stltl, ltlhd :: lltl) =>
                     if String.sub (stlhd, String.size stlhd - 1) = #"\n" then
                       (* \n -> \n -> \n *)
                       cursorIdx - 1
                     else
                       (* graphical-chr -> \n -> \n *)
                       startVi0
                         ( String.size stlhd - 1
                         , stlhd
                         , ltlhd
                         , cursorIdx - 2
                         , lstl
                         , lltl
                         )
                 | (_, _) => 0)
            else
              (* ? -> graphical-chr -> \n *)
              startVi0
                ( String.size lshd - 1
                , lshd
                , llhd
                , cursorIdx - 1
                , leftStrings
                , leftLines
                )
        | (_, _) => (* leftStrings is empty so go to start of buffer *) 0
    else
      (* ? -> ? -> graphical-chr 
       * Normal case where we call startViK. *)
      let
        val lineColumn = getCursorColumn
          (strIdx, shd, lhd, leftStrings, leftLines, cursorIdx)
      in
        helpViK
          ( strIdx
          , shd
          , cursorIdx
          , lineColumn
          , lineColumn
          , false
          , leftStrings
          , lhd
          , leftLines
          )
      end

  fun viK (lineGap: LineGap.t, cursorIdx) =
    let
      val
        {rightStrings, idx = bufferIdx, rightLines, leftStrings, leftLines, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              startViK (strIdx, shd, cursorIdx, leftStrings, lhd, leftLines)
            else
              case (stl, ltl) of
                (stlhd :: stltl, ltlhd :: ltltl) =>
                  let
                    val strIdx = strIdx - String.size shd
                    val leftStrings = shd :: leftStrings
                    val leftLines = lhd :: leftLines
                  in
                    startViK
                      (strIdx, stlhd, cursorIdx, leftStrings, ltlhd, leftLines)
                  end
              | (_, _) => cursorIdx
          end
      | (_, _) => (* nowhere to go rightward, so return cursorIdx *) cursorIdx
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
        (shd :: stl, lhd :: ltl) => helpNextWord (0, shd, absIdx, stl, ltl)
      | (_, _) =>
          (* reached end of lineGap; 
           * return last valid chr position *)
          absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isAlphaNum chr orelse chr = #"_" then
          if isNextChrNonBlank (strPos, str, strTl) then absIdx + 1
          else helpNextWord (strPos + 1, str, absIdx + 1, strTl, lineTl)
        else if Char.isSpace chr then
          if notIsNextChrSpace (strPos, str, strTl) then
            absIdx + 1
          else
            (* nothing to do on space, except keep iterating *)
            helpNextWord (strPos + 1, str, absIdx + 1, strTl, lineTl)
        else (* chr is NON_BLANK. *) if isNextChrAlphaNum (strPos, str, strTl) then
          absIdx + 1
        else
          helpNextWord (strPos + 1, str, absIdx + 1, strTl, lineTl)
      end

  fun helpNextWORD (strPos, str, absIdx, strTl, lineTl) =
    if strPos = String.size str then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) => helpNextWORD (0, shd, absIdx, stl, ltl)
      | (_, _) =>
          (* reached end of lineGap; 
           * return last valid chr position *)
          absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isSpace chr then
          if notIsNextChrSpace (strPos, str, strTl) then absIdx + 1
          else helpNextWORD (strPos + 1, str, absIdx + 1, strTl, lineTl)
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
                   in fNext (strIdx, stlhd, cursorIdx, stltl, ltltl)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  (* equivalent of vi's 'w' command *)
  fun nextWord (lineGap, cursorIdx) =
    toNextWord (lineGap, cursorIdx, helpNextWord)

  (* equivalent of vi's 'W' command *)
  val nextWORD = ViWORDDfa.startOfNextWORD

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
        if
          Char.isAlphaNum chr orelse chr = #"_"
        then
          if
            isPrevChrSpace (strPos, str, strTl)
            orelse isPrevChrNonBlank (strPos, str, strTl)
          then absIdx
          else helpPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
        else if
          Char.isSpace chr
        then
          helpPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
        else (* is NON_BLANK *) if
          isPrevChrSpace (strPos, str, strTl)
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
          helpPrevWORD (strPos - 1, str, absIdx - 1, strTl, lineTl)
        else if isPrevChrSpace (strPos, str, strTl) then
          absIdx
        else
          helpPrevWORD (strPos - 1, str, absIdx - 1, strTl, lineTl)
      end

  fun helpEndOfPrevWord (strPos, str, absIdx, strTl, lineTl) =
    if strPos < 0 then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          helpEndOfPrevWord (String.size shd - 1, shd, absIdx, stl, ltl)
      | (_, _) => 0
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isAlphaNum chr orelse chr = #"_" then
          if isPrevChrNonBlank (strPos, str, strTl) then absIdx - 1
          else helpEndOfPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
        else if Char.isSpace chr then
          if isPrevChrSpace (strPos, str, strTl) then
            helpEndOfPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
          else
            absIdx - 1
        else (* is NON_BLANK *) if isPrevChrAlphaNum (strPos, str, strTl) then
          absIdx - 1
        else
          helpEndOfPrevWord (strPos - 1, str, absIdx - 1, strTl, lineTl)
      end

  fun helpEndOfPrevWORD (strPos, str, absIdx, strTl, lineTl) =
    if strPos < 0 then
      case (strTl, lineTl) of
        (shd :: stl, lhd :: ltl) =>
          helpEndOfPrevWORD (String.size shd - 1, shd, absIdx, stl, ltl)
      | (_, _) => 0
    else
      let
        val chr = String.sub (str, strPos)
      in
        if Char.isSpace chr then
          if isPrevChrSpace (strPos, str, strTl) then
            helpEndOfPrevWORD (strPos - 1, str, absIdx - 1, strTl, lineTl)
          else
            absIdx - 1
        else
          helpEndOfPrevWORD (strPos - 1, str, absIdx - 1, strTl, lineTl)
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
      | (_, _) => (* tl is empty; just return idx 0 *) 0

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
  val prevWORD = ViWORDDfa.startOfCurrentWORD

  fun toEndOfPrevWord (lineGap: LineGap.t, cursorIdx, fPrev) =
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
              fPrev (strIdx, shd, cursorIdx, leftStrings, leftLines)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let
                     val strIdx = strIdx - String.size shd
                     val leftStrings = shd :: leftStrings
                     val leftLines = lhd :: leftLines
                   in
                     fPrev (strIdx, shd, cursorIdx, leftStrings, leftLines)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  (* equivalent of vi's 'ge' command *)
  fun endOfPrevWord (lineGap, cursorIdx) =
    toEndOfPrevWord (lineGap, cursorIdx, helpEndOfPrevWord)

  (* equivalent of vi's 'gE' command *)
  val endOfPrevWORD = ViWORDDfa.endOfPrevWORD

  fun helpEndOfWord (strPos, str, absIdx, stl, ltl) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) => helpEndOfWord (0, shd, absIdx, stl, ltl)
      | (_, _) => absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if
          Char.isAlphaNum chr orelse chr = #"_"
        then
          if
            isNextChrSpace (strPos, str, stl)
            orelse isNextChrNonBlank (strPos, str, stl)
          then absIdx
          else helpEndOfWord (strPos + 1, str, absIdx + 1, stl, ltl)
        else if
          Char.isSpace chr
        then
          helpEndOfWord (strPos + 1, str, absIdx + 1, stl, ltl)
        else (* is NON_BLANK *) if
          isNextChrSpace (strPos, str, stl)
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
        else if isNextChrSpace (strPos, str, stl) then
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
      | (_, _) => (* tl is empty; just return absIdx *) absIdx

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
              startEndOfWord (shd, strIdx, cursorIdx, stl, ltl, fEnd)
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
  val endOfWORD = ViWORDDfa.endOfCurrentWORD

  (* vi's 'e' command takes user last char in word
   * but 'de' deletes up to and including last char of word 
   * So we need to increment by one for deletion. *)
  fun endOfWordPlusOne (lineGap, cursorIdx) =
    endOfWord (lineGap, cursorIdx) + 1

  fun endOfWORDPlusOne (lineGap, cursorIdx) =
    endOfWORD (lineGap, cursorIdx) + 1

  fun helpFirstNonSpaceChr (strPos, str, absIdx, stl, ltl) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) =>
          helpFirstNonSpaceChr (0, shd, absIdx, stl, ltl)
      | (_, _) => absIdx - 1
    else
      let
        val chr = String.sub (str, strPos)
      in
        if chr = #" " then
          helpFirstNonSpaceChr (strPos + 1, str, absIdx + 1, stl, ltl)
        else
          absIdx
      end

  fun startFirstNonSpaceChr (shd, strIdx, absIdx, stl, ltl) =
    if strIdx < String.size shd then
      helpFirstNonSpaceChr (strIdx, shd, absIdx, stl, ltl)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpFirstNonSpaceChr (0, stlhd, absIdx, stltl, ltltl)
      | (_, _) => (* tl is empty; just return absIdx *) absIdx

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
              startFirstNonSpaceChr (shd, strIdx, cursorIdx, stl, ltl)
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
      | (_, _) => cursorIdx
    end

  fun helpToNextChr (strPos, str, absIdx, stl, ltl, origIdx, findChr) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) =>
          helpToNextChr (0, shd, absIdx, stl, ltl, origIdx, findChr)
      | (_, _) => origIdx
    else if String.sub (str, strPos) = findChr then
      absIdx
    else
      helpToNextChr (strPos + 1, str, absIdx + 1, stl, ltl, origIdx, findChr)

  fun startToNextChr (shd, strIdx, absIdx, stl, ltl, findChr) =
    (* we want to start iterating from next char after strIdx *)
    if strIdx - 1 < String.size shd then
      helpToNextChr (strIdx + 1, shd, absIdx + 1, stl, ltl, absIdx, findChr)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpToNextChr (0, stlhd, absIdx + 1, stltl, ltltl, absIdx, findChr)
      | (_, _) => (* tl is empty; just return absIdx *) absIdx

  fun helpTillNextChr
    (strPos, str, absIdx, stl, ltl, origIdx, findChr, lastNonLine, lastLine) =
    if strPos = String.size str then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) =>
          helpTillNextChr
            (0, shd, absIdx, stl, ltl, origIdx, findChr, lastNonLine, lastLine)
      | (_, _) => origIdx
    else
      let
        val chr = String.sub (str, strPos)
      in
        if chr = findChr then
          if lastLine = lastNonLine + 1 then
            (* graphical-chr -> \n
             * so return graphical-chr *)
            lastNonLine
          else
            Int.max (lastLine, lastNonLine)
        else
          let
            val lastLine = if chr = #"\n" then absIdx else lastLine
            val lastNonLine = if chr = #"\n" then lastNonLine else absIdx
          in
            helpTillNextChr
              ( strPos + 1
              , str
              , absIdx + 1
              , stl
              , ltl
              , origIdx
              , findChr
              , lastNonLine
              , lastLine
              )
          end
      end

  fun startTillNextChr (shd, strIdx, absIdx, stl, ltl, findChr) =
    (* we want to start iterating from next char after strIdx *)
    if strIdx + 1 < String.size shd then
      helpTillNextChr
        (strIdx + 1, shd, absIdx + 1, stl, ltl, absIdx, findChr, absIdx, absIdx)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpTillNextChr
            ( 0
            , stlhd
            , absIdx + 1
            , stltl
            , ltltl
            , absIdx
            , findChr
            , absIdx
            , absIdx
            )
      | (_, _) => (* tl is empty; just return absIdx *) absIdx

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
              fStart (shd, strIdx, cursorIdx, stl, ltl, chr)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let val strIdx = strIdx - String.size shd
                   in fStart (stlhd, strIdx, cursorIdx, stltl, ltltl, chr)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

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
      | (_, _) => origIdx
    else if String.sub (str, strPos) = findChr then
      absIdx
    else
      helpToPrevChr (strPos - 1, str, absIdx - 1, stl, ltl, origIdx, findChr)

  fun startToPrevChr (shd, strIdx, absIdx, stl, ltl, findChr) =
    (* we want to start iterating from Prev char after strIdx *)
    if strIdx > 0 then
      helpToPrevChr (strIdx - 1, shd, absIdx - 1, stl, ltl, absIdx, findChr)
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpToPrevChr
            ( String.size stlhd - 1
            , stlhd
            , absIdx - 1
            , stltl
            , ltltl
            , absIdx
            , findChr
            )
      | (_, _) => (* tl is empty; return 0 for lineGap start *) 0

  fun helpTillPrevChr
    ( strPos
    , str
    , absIdx
    , stl
    , ltl
    , origIdx
    , findChr
    , lastNonLine
    , lastLine
    , lastValid
    ) =
    if strPos < 0 then
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) =>
          helpTillPrevChr
            ( String.size shd - 1
            , shd
            , absIdx
            , stl
            , ltl
            , origIdx
            , findChr
            , lastNonLine
            , lastLine
            , lastValid
            )
      | (_, _) => origIdx
    else
      let
        val chr = String.sub (str, strPos)
      in
        if chr = findChr then
          if lastLine = lastNonLine then lastNonLine
          else if absIdx + 1 = lastLine then lastValid + 1
          else Int.min (lastLine, lastNonLine)
        else
          let
            val lastLine = if chr = #"\n" then absIdx else lastLine
            val lastNonLine = if chr = #"\n" then lastNonLine else absIdx

            (* There is a slightly tricky edge case 
             * which is the reason the lastValid variable.
             * Say we have a string "a\n\n\nbcd"
             * and we type "Ta" with the cursor at the end.
             * We want the cursor to go to the second line break
             * because (graphical-chr -> \n) should not be selectable.
             * However, with only lastLine and lastNonLine variables,
             * we only have information about the most recent \n 
             * and the most recent graphical-chr.
             * This means we don't have information about the case 
             * where a graphical-chr is followed by multiple '\n's.
             * The lastValid variable keeps track of this information
             * so we can use it to provide the expected behaviour.
             * *)
            val lastValid =
              if lastLine = lastNonLine + 1 then lastLine + 1
              else Int.min (lastLine, lastNonLine)
          in
            helpTillPrevChr
              ( strPos - 1
              , str
              , absIdx - 1
              , stl
              , ltl
              , origIdx
              , findChr
              , lastNonLine
              , lastLine
              , lastValid
              )
          end
      end

  fun startTillPrevChr (shd, strIdx, absIdx, stl, ltl, findChr) =
    (* we want to start iterating from Prev char after strIdx *)
    if strIdx > 0 then
      helpTillPrevChr
        ( strIdx - 1
        , shd
        , absIdx - 1
        , stl
        , ltl
        , absIdx
        , findChr
        , absIdx
        , absIdx
        , absIdx
        )
    else
      case (stl, ltl) of
        (stlhd :: stltl, ltlhd :: ltltl) =>
          helpTillPrevChr
            ( String.size stlhd - 1
            , stlhd
            , absIdx - 1
            , stltl
            , ltltl
            , absIdx
            , findChr
            , absIdx
            , absIdx
            , absIdx
            )
      | (_, _) => (* tl is empty; return 0 for lineGap start *) 0

  fun prevChr (lineGap: LineGap.t, cursorIdx, chr, fStart) =
    let
      val
        {rightStrings, rightLines, idx = bufferIdx, leftStrings, leftLines, ...} =
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
              fStart (shd, strIdx, cursorIdx, leftStrings, leftLines, chr)
            else
              (* strIdx is in tl *)
              (case (stl, ltl) of
                 (stlhd :: stltl, ltlhd :: ltltl) =>
                   let
                     val strIdx = strIdx - String.size shd
                     val leftStrings = shd :: leftStrings
                     val leftLines = lhd :: leftLines
                   in
                     fStart
                       (shd, strIdx, cursorIdx, leftStrings, leftLines, chr)
                   end
               | (_, _) => cursorIdx)
          end
      | (_, _) => cursorIdx
    end

  fun toPrevChr (lineGap, cursorIdx, chr) =
    prevChr (lineGap, cursorIdx, chr, startToPrevChr)

  fun tillPrevChr (lineGap, cursorIdx, chr) =
    prevChr (lineGap, cursorIdx, chr, startTillPrevChr)

  fun helpMatchPairNext
    (strPos, str, absIdx, stl, origIdx, openChr, openNum, closeChr, closeNum) =
    if strPos = String.size str then
      case stl of
        hd :: tl =>
          helpMatchPairNext
            (0, hd, absIdx, tl, origIdx, openChr, openNum, closeChr, closeNum)
      | [] => origIdx
    else
      let
        val chr = String.sub (str, strPos)
        val openNum = if chr = openChr then openNum + 1 else openNum
        val closeNum = if chr = closeChr then closeNum + 1 else closeNum
      in
        if openNum = closeNum then
          absIdx
        else
          helpMatchPairNext
            ( strPos + 1
            , str
            , absIdx + 1
            , stl
            , origIdx
            , openChr
            , openNum
            , closeChr
            , closeNum
            )
      end

  fun helpMatchPairPrev
    (strPos, str, absIdx, stl, origIdx, openChr, openNum, closeChr, closeNum) =
    if strPos < 0 then
      case stl of
        hd :: tl =>
          helpMatchPairPrev
            ( String.size hd - 1
            , hd
            , absIdx
            , tl
            , origIdx
            , openChr
            , openNum
            , closeChr
            , closeNum
            )
      | [] => origIdx
    else
      let
        val chr = String.sub (str, strPos)
        val openNum = if chr = openChr then openNum + 1 else openNum
        val closeNum = if chr = closeChr then closeNum + 1 else closeNum
      in
        if openNum = closeNum then
          absIdx
        else
          helpMatchPairPrev
            ( strPos - 1
            , str
            , absIdx - 1
            , stl
            , origIdx
            , openChr
            , openNum
            , closeChr
            , closeNum
            )
      end

  fun startMatchPair (strIdx, shd, leftStrings, rightStrings, cursorIdx) =
    case String.sub (shd, strIdx) of
      #"(" =>
        helpMatchPairNext
          ( strIdx + 1
          , shd
          , cursorIdx + 1
          , rightStrings
          , cursorIdx
          , #"("
          , 1
          , #")"
          , 0
          )
    | #")" =>
        helpMatchPairPrev
          ( strIdx - 1
          , shd
          , cursorIdx - 1
          , leftStrings
          , cursorIdx
          , #"("
          , 0
          , #")"
          , 1
          )
    | #"[" =>
        helpMatchPairNext
          ( strIdx + 1
          , shd
          , cursorIdx + 1
          , rightStrings
          , cursorIdx
          , #"["
          , 1
          , #"]"
          , 0
          )
    | #"]" =>
        helpMatchPairPrev
          ( strIdx - 1
          , shd
          , cursorIdx - 1
          , leftStrings
          , cursorIdx
          , #"["
          , 0
          , #"]"
          , 1
          )
    | #"{" =>
        helpMatchPairNext
          ( strIdx + 1
          , shd
          , cursorIdx + 1
          , rightStrings
          , cursorIdx
          , #"{"
          , 1
          , #"}"
          , 0
          )
    | #"}" =>
        helpMatchPairPrev
          ( strIdx - 1
          , shd
          , cursorIdx - 1
          , leftStrings
          , cursorIdx
          , #"{"
          , 0
          , #"}"
          , 1
          )
    | #"<" =>
        helpMatchPairNext
          ( strIdx + 1
          , shd
          , cursorIdx + 1
          , rightStrings
          , cursorIdx
          , #"<"
          , 1
          , #">"
          , 0
          )
    | #">" =>
        helpMatchPairPrev
          ( strIdx - 1
          , shd
          , cursorIdx - 1
          , leftStrings
          , cursorIdx
          , #"<"
          , 0
          , #">"
          , 1
          )
    | _ => cursorIdx

  fun matchPair (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, leftStrings, ...} = lineGap
    in
      case rightStrings of
        shd :: stl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              startMatchPair (strIdx, shd, leftStrings, stl, cursorIdx)
            else
              (* strIdx is in tl *)
              (case stl of
                 stlhd :: stltl =>
                   let
                     val strIdx = strIdx - String.size shd
                     val leftStrings = shd :: leftStrings
                   in
                     startMatchPair
                       (strIdx, stlhd, leftStrings, stltl, cursorIdx)
                   end
               | [] => cursorIdx)
          end
      | [] => cursorIdx
    end

  (* Prerequisite: move lineGap to reqLine *)
  fun getLineStartIdx (lineGap: LineGap.t, reqLine) =
    let
      val {rightStrings, rightLines, line = bufferLine, idx = bufferIdx, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          (* reqLine exists in lineGap, so retrieve it *)
          let
            val relativeLine = reqLine - bufferLine - 1
            val lineIdx = Vector.sub (lhd, relativeLine)
          in
            if lineIdx = String.size shd - 1 andalso List.null stl then
              (* if is end of buffer, return last idx in buffer; else, 
               * increment by 1 as we want to go to first char after line break *)
              bufferIdx + lineIdx
            else
              bufferIdx + lineIdx + 1
          end
      | (_, _) =>
          (* reqLine does not exist in lineGap, so just go to start of last line *)
          let
            val {leftStrings, leftLines, ...} = lineGap
          in
            (case (leftStrings, leftLines) of
               (lshd :: lstl, llhd :: lltl) =>
                 let
                   val result = startVi0
                     ( String.size lshd - 1
                     , lshd
                     , llhd
                     , bufferIdx - 1
                     , lstl
                     , lltl
                     )
                 in
                   if result = bufferIdx then bufferIdx - 1 else result
                 end
             | (_, _) => 0)
          end
    end

  (* Prerequisite: lineGap is moved to cursorIdx *)
  fun isCursorAtStartOfLine (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
    in
      case rightStrings of
        hd :: tl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size hd then
              (* chr is in hd *)
              String.sub (hd, strIdx) = #"\n"
            else
              (* chr is in tl *)
              (case tl of
                 tlhd :: _ =>
                   let val strIdx = strIdx - String.size hd
                   in String.sub (tlhd, strIdx) = #"\n"
                   end
               | [] => true)
          end
      | [] => true
    end

  (* Prerequisite: lineGap is moved to cursorIdx *)
  fun isPrevChrStartOfLine (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, leftStrings, ...} = lineGap
    in
      case rightStrings of
        hd :: tl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx > 0 then
              (* prev chr is in hd *)
              String.sub (hd, strIdx - 1) = #"\n"
            else
              (* prev chr if in leftStrings *)
              (case leftStrings of
                 lhd :: _ => String.sub (lhd, String.size lhd - 1) = #"\n"
               | [] =>
                   (* cursorIdx = 0 which means we are at start of file/line *)
                   true)
          end
      | [] => true
    end

  fun helpIsNextChrEndOfLine (strIdx, hd, tl) =
    if strIdx + 1 < String.size hd then
      (* next chr is in this string *)
      String.sub (hd, strIdx + 1) = #"\n"
    else
      (* next chr, if it exists, is in tl *)
      (case tl of
         tlhd :: _ => String.sub (tlhd, 0) = #"\n"
       | [] => true)

  (* Prerequisite: lineGap is moved to cursorIdx *)
  fun isNextChrEndOfLine (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
    in
      case rightStrings of
        hd :: tl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size hd then
              helpIsNextChrEndOfLine (strIdx, hd, tl)
            else
              (* strIdx is in tl *)
              (case tl of
                 tlhd :: tltl =>
                   helpIsNextChrEndOfLine (strIdx - String.size hd, tlhd, tltl)
               | [] =>
                   (* strIdx is at end of lineGap
                    * which also means at end of line *)
                   true)
          end
      | [] => true
    end

  (* Prerequisite: lineGap is moved to cursorIdx *)
  fun clipIdx (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
    in
      (* We are trying to check if cursorIdx is within the buffer. *)
      case rightStrings of
        _ :: _ :: _ =>
          (* if there is a string after the hd, 
           * we are definitely in a valid idx and should return it *)
          cursorIdx
      | [hd] =>
          let
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size hd - 1 then
              (* if we are before the last char in the string.
               * Unix file endings always have \n at the end
               * but we do not want cursor to ever go to end
               * as vi also does not go to the very end.
               * This is why we check strIdx is before the last char. 
               * *)
              cursorIdx
            else
              (* if end of buffer - 2 is greater than 0, then that;
               * else, 0 *)
              Int.max (bufferIdx + String.size hd - 2, 0)
          end
      | [] =>
          (* if end of buffer - 2 is greater than 0, then that;
           * else, 0 *)
          Int.max (bufferIdx - 2, 0)
    end
end
