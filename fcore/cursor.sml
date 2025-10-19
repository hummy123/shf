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
          if linePos = ~1 then
            (* no previous line in lhd *)
            helpVi0 (absIdx - strPos, stl, ltl)
          else
            let val lineIdx = Vector.sub (lhd, linePos)
            in absIdx - strPos + lineIdx + 1
            end
        end
      else
        helpVi0 (absIdx - strPos, stl, ltl)
    else
      helpVi0 (absIdx - strPos, stl, ltl)

  structure Vi0 =
    MakeIfCharFolderPrev
      (struct
         type env = unit
         val fStart = startVi0
         fun fStart (strPos, shd, lhd, absIdx, stl, ltl, _) =
           startVi0 (strPos, shd, lhd, absIdx, stl, ltl)
       end)

  fun vi0 (lineGap, cursorIdx) = Vi0.foldPrev (lineGap, cursorIdx, ())

  val viDlr = ViDlrDfa.next
  val viDlrForDelete = ViDlrDfa.nextForDelete

  val viL = ViLDfa.next
  val viH = ViHDfa.prev

  (* equivalent of vi's 'w' command *)
  val nextWord = ViWordDfa.startOfNextWord

  (* equivalent of vi's 'W' command *)
  val nextWORD = ViCapsWordDfa.startOfNextWORD

  (* equivalent of vi's 'b' command *)
  val prevWord = ViWordDfa.startOfCurrentWord
  val prevWordStrict = ViWordDfa.startOfCurrentWordStrict

  (* equivalent of vi's 'B' command *)
  val prevWORD = ViCapsWordDfa.startOfCurrentWORD
  val prevWORDStrict = ViCapsWordDfa.startOfCurrentWORDStrict

  (* equivalent of vi's 'ge' command *)
  val endOfPrevWord = ViWordDfa.endOfPrevWord

  (* equivalent of vi's 'gE' command *)
  val endOfPrevWORD = ViCapsWordDfa.endOfPrevWORD

  (* equivalent of vi's `e` command *)
  val endOfWord = ViWordDfa.endOfCurrentWord
  val endOfWordForDelete = ViWordDfa.endOfCurrentWordForDelete
  val endOfWordStrict = ViWordDfa.endOfCurrentWordStrict

  (* equivalent of vi's `E` command *)
  val endOfWORD = ViCapsWordDfa.endOfCurrentWORD
  val endOfWORDForDelete = ViCapsWordDfa.endOfCurrentWORDForDelete
  val endOfWORDStrict = ViCapsWordDfa.endOfCurrentWORDStrict

  (* Prerequisite: 
   * LineGap has been moved to start of line (provided with vi0). *)
  structure FirstNonSpaceChr =
    MakeIfCharFolderPrev
      (struct
         type env = unit

         fun helpFirstNonSpaceChr (strPos, str, absIdx, stl) =
           if strPos = String.size str then
             case stl of
               shd :: stl => helpFirstNonSpaceChr (0, shd, absIdx, stl)
             | [] => absIdx - 1
           else
             let
               val chr = String.sub (str, strPos)
             in
               if chr = #" " then
                 helpFirstNonSpaceChr (strPos + 1, str, absIdx + 1, stl)
               else
                 absIdx
             end

         fun fStart (strIdx, shd, _, absIdx, stl, _, _) =
           if strIdx < String.size shd then
             helpFirstNonSpaceChr (strIdx, shd, absIdx, stl)
           else
             case stl of
               stlhd :: stltl => helpFirstNonSpaceChr (0, stlhd, absIdx, stltl)
             | [] => (* tl is empty; just return absIdx *) absIdx
       end)

  fun firstNonSpaceChr (lineGap, cursorIdx) =
    FirstNonSpaceChr.foldPrev (lineGap, cursorIdx, ())

  structure ToNextChr =
    MakeIfCharFolderNext
      (struct
         type env = {findChr: char, count: int}

         fun helpToNextChr
           (strPos, str, absIdx, stl, lastFoundIdx, findChr, count) =
           if strPos = String.size str then
             case stl of
               str :: stl =>
                 helpToNextChr
                   (0, str, absIdx, stl, lastFoundIdx, findChr, count)
             | [] => lastFoundIdx
           else
             let
               val chr = String.sub (str, strPos)
             in
               if chr = findChr then
                 if count - 1 = 0 then
                   absIdx
                 else
                   helpToNextChr
                     ( strPos + 1
                     , str
                     , absIdx + 1
                     , stl
                     , absIdx
                     , findChr
                     , count - 1
                     )
               else
                 helpToNextChr
                   ( strPos + 1
                   , str
                   , absIdx + 1
                   , stl
                   , lastFoundIdx
                   , findChr
                   , count
                   )
             end

         fun fStart (strPos, str, _, absIdx, stl, _, {findChr, count}) =
           (* we want to start iterating from char after cursor *)
           helpToNextChr (strPos + 1, str, absIdx + 1, stl, ~1, findChr, count)
       end)

  val toNextChr = ToNextChr.foldNext

  structure ToPrevChr =
    MakeIfCharFolderPrev
      (struct
         type env = {findChr: char, count: int}

         fun helpToPrevChr
           (strPos, str, absIdx, stl, lastFoundIdx, findChr, count) =
           if strPos < 0 then
             case stl of
               shd :: stl =>
                 helpToPrevChr
                   ( String.size shd - 1
                   , shd
                   , absIdx
                   , stl
                   , lastFoundIdx
                   , findChr
                   , count
                   )
             | [] => lastFoundIdx
           else if String.sub (str, strPos) = findChr then
             if count - 1 = 0 then
               absIdx
             else
               helpToPrevChr
                 (strPos - 1, str, absIdx - 1, stl, absIdx, findChr, count - 1)
           else
             helpToPrevChr
               (strPos - 1, str, absIdx - 1, stl, lastFoundIdx, findChr, count)

         fun fStart (strIdx, shd, _, absIdx, stl, _, {findChr, count}) =
           helpToPrevChr (strIdx - 1, shd, absIdx - 1, stl, ~1, findChr, count)
       end)

  val toPrevChr = ToPrevChr.foldPrev

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
  fun isOnNewlineAfterChr (buffer, cursorIdx) =
    cursorIdx > 0 andalso not (isPrevChrStartOfLine (buffer, cursorIdx))
    andalso isCursorAtStartOfLine (buffer, cursorIdx)
end
