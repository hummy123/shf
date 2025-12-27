structure NormalMode =
struct
  (* parsing functions, deciding what to do while we are in normal mode *)

  open AppType
  open InputMsg

  fun switchToNormalSearchMode (app: app_type, caseSensitive) =
    NormalSearchFinish.onSearchChanged
      (app, "", PersistentVector.empty, 0, 0, caseSensitive, #buffer app)

  fun getNumLength (pos, str) =
    if pos = String.size str then
      pos
    else
      let val chr = String.sub (str, pos)
      in if Char.isDigit chr then getNumLength (pos + 1, str) else pos
      end

  fun appendChr (app: app_type, chr, str) =
    let
      val str = str ^ Char.toString chr
      val mode = NORMAL_MODE str
    in
      NormalModeWith.mode (app, mode, [])
    end

  local
    fun loop (app: app_type, cursorIdx, buffer, count, time) =
      if count = 0 then
        let
          open MailboxType

          val {cursorIdx = origCursorIdx, dfa, ...} = app
          val buffer = LineGap.goToStart buffer
        in
          NormalDelete.finishAfterDeletingBuffer
            (app, origCursorIdx, buffer, time, [])
        end
      else
        let
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val lineStart = Cursor.vi0 (buffer, cursorIdx)

          val buffer = LineGap.insert (lineStart, "  ", buffer)

          val buffer = LineGap.goToIdx (lineStart, buffer)
          val lineEnd = Cursor.viDlr (buffer, lineStart, 1)
          val buffer = LineGap.goToIdx (lineEnd, buffer)
          val nextLine = Cursor.viL (buffer, lineEnd, 1)

          val count = if lineEnd = nextLine then 0 else count - 1
        in
          loop (app, nextLine, buffer, count, time)
        end
  in
    fun indnetLine (app: app_type, count, time) =
      loop (app, #cursorIdx app, #buffer app, count, time)
  end

  local
    fun loop (cursorIdx, buffer, count) =
      if count = 0 then
        buffer
      else
        let
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val lineStart = Cursor.vi0 (buffer, cursorIdx)
          val firstNonSpaceChr = Cursor.firstNonSpaceChr (buffer, lineStart)

          (* delete from buffer *)
          val difference = firstNonSpaceChr - lineStart
          val deleteLength = Int.min (difference, 2)
          val buffer =
            if difference = 0 then
              (* can't dedent as there is no leading space *)
              buffer
            else
              LineGap.delete (lineStart, deleteLength, buffer)

          (* get next line to dedent *)
          val buffer = LineGap.goToIdx (lineStart, buffer)
          val lineEnd = Cursor.viDlr (buffer, lineStart, 1)
          val buffer = LineGap.goToIdx (lineEnd, buffer)
          val nextLine = Cursor.viL (buffer, lineEnd, 1)

          val count = if lineEnd = nextLine then 0 else count - 1
        in
          loop (nextLine, buffer, count)
        end
  in
    fun dedentLine (app: app_type, count, time) =
      let
        open MailboxType

        val {cursorIdx, buffer, dfa, ...} = app
        val buffer = LineGap.goToIdx (cursorIdx, buffer)

        val lineStart = Cursor.vi0 (buffer, cursorIdx)
        val firstNonSpaceChr = Cursor.firstNonSpaceChr (buffer, lineStart)

        (* calculate length to delete *)
        val difference = firstNonSpaceChr - lineStart
        val deleteLength = Int.min (difference, 2)

        (* delete once *)
        val buffer =
          if deleteLength = 0 then buffer
          else LineGap.delete (lineStart, deleteLength, buffer)

        (* Calculate nextLine and newCursorIdx.
         * The cursorIdx might be past the current line after we dedent.
         * If it is, we put the cursorIdx at the last char of the line. *)
        val buffer = LineGap.goToIdx (lineStart, buffer)
        val lineEnd = Cursor.viDlr (buffer, lineStart, 1)
        val buffer = LineGap.goToIdx (lineEnd, buffer)
        val nextLine = Cursor.viL (buffer, lineEnd, 1)
        val newCursorIdx = Int.min (lineEnd, cursorIdx)

        val buffer =
          if lineEnd = nextLine then
            (* at end of file, so we cannot dedent anymore *)
            buffer
          else
            (* dedent remaining lines specified by count *)
            loop (nextLine, buffer, count - 1)

        val buffer = LineGap.goToStart buffer
      in
        NormalDelete.finishAfterDeletingBuffer
          (app, newCursorIdx, buffer, time, [])
      end
  end

  fun parseGo (count, app, chrCmd) =
    case chrCmd of
      #"e" => MoveToEndOfPrevWord.move (app, count)
    | #"E" => MoveToEndOfPrevWORD.move (app, count)
    | #"g" => NormalMove.moveToStart app
    | _ => NormalFinish.clearMode app

  fun parseChr (app: app_type, count, chr, str, time) =
    case chr of
      #"h" => MoveViH.move (app, count)
    | #"j" => NormalMove.moveCursorDown (app, count)
    | #"k" => NormalMove.moveCursorUp (app, count)
    | #"l" => MoveViL.move (app, count)
    | #"w" => MoveToNextWord.move (app, count)
    | #"W" => MoveToNextWORD.move (app, count)
    | #"b" => MoveToPrevWord.move (app, count)
    | #"B" => MoveToPrevWORD.move (app, count)
    | #"e" => MoveToEndOfWord.move (app, count)
    | #"E" => MoveToEndOfWORD.move (app, count)
    | #"n" => NormalMove.moveToNextMatch (app, count)
    | #"N" => NormalMove.moveToPrevMatch (app, count)
    | #"z" => NormalFinish.centreToCursor app
    (* can only move to start or end of line once 
     * so hardcode count as 1 *)
    | #"0" =>
        (* 0 is a bit of a special case.
         * If 0 is pressed without any preceding characters,
         * then it should move cursor to the start of the line.
         * However, if a number was pressed previously before 0 was,
         * then this means user is entering a count.
         * In that case, we append 0 to the string. *)
        if String.size str > 0 then
          let
            val lastChr = String.sub (str, String.size str - 1)
          in
            if Char.isDigit lastChr then
              let
                val chr = Char.toString chr
                val str = str ^ chr
                val mode = NORMAL_MODE str
              in
                NormalModeWith.mode (app, mode, [])
              end
            else
              MoveToStartOfLine.move (app, 1)
          end
        else
          MoveToStartOfLine.move (app, 1)
    | #"$" => MoveToEndOfLine.move (app, 1)
    | #"^" => NormalMove.firstNonSpaceChr app
    | #"G" =>
        (* if str has a size larger than 0,
         * interpret as "go to line" command;
         * else, interpret as a command to move to end *)
        if String.size str = 0 then NormalMove.moveToEnd app
        else NormalMove.moveToLine (app, count)
    | #"%" => NormalMove.moveToMatchingPair app
    | #"D" => NormalDelete.deleteToEndOfLine (app, time)
    | #"x" => NormalDelete.removeChr (app, count, time)
    | #"J" => NormalDelete.removeLineBreaks (app, count, time)
    | #"/" => switchToNormalSearchMode (app, false)
    | #"?" => switchToNormalSearchMode (app, true)
    | #">" => indnetLine (app, count, time)
    | #"<" => dedentLine (app, count, time)
    (* multi-char commands which can be appended *)
    | #"t" => appendChr (app, chr, str)
    | #"T" => appendChr (app, chr, str)
    | #"y" => appendChr (app, chr, str)
    | #"d" => appendChr (app, chr, str)
    | #"f" => appendChr (app, chr, str)
    | #"F" => appendChr (app, chr, str)
    | #"g" => appendChr (app, chr, str)
    | #"c" => appendChr (app, chr, str)
    | _ =>
        (* user may be entering a cmd with more than one chr
         * such as "2dw" to delete two word
         * so add current chr to mode, and save it in the app state *)
        let
          val str = if Char.isDigit chr then str ^ Char.toString chr else ""
          val mode = NORMAL_MODE str
        in
          NormalModeWith.mode (app, mode, [])
        end

  structure ParseDelete =
  struct
    fun parseDeleteInside (app, chr, time) =
      case chr of
        #"w" => NormalDelete.deleteInsideWord (app, time)
      | #"W" => NormalDelete.deleteInsideWORD (app, time)
      | #"(" => NormalDelete.deleteInsideChrOpen (app, chr, time)
      | #"[" => NormalDelete.deleteInsideChrOpen (app, chr, time)
      | #"{" => NormalDelete.deleteInsideChrOpen (app, chr, time)
      | #"<" => NormalDelete.deleteInsideChrOpen (app, chr, time)
      | #")" => NormalDelete.deleteInsideChrClose (app, chr, time)
      | #"]" => NormalDelete.deleteInsideChrClose (app, chr, time)
      | #"}" => NormalDelete.deleteInsideChrClose (app, chr, time)
      | #">" => NormalDelete.deleteInsideChrClose (app, chr, time)
      | _ => NormalFinish.clearMode app

    fun parseDeleteAround (app, chr, time) =
      case chr of
        #"w" => NormalDelete.deleteAroundWord (app, time)
      | #"W" => NormalDelete.deleteAroundWORD (app, time)
      | #"(" => NormalDelete.deleteAroundChrOpen (app, chr, time)
      | #"[" => NormalDelete.deleteAroundChrOpen (app, chr, time)
      | #"{" => NormalDelete.deleteAroundChrOpen (app, chr, time)
      | #"<" => NormalDelete.deleteAroundChrOpen (app, chr, time)
      | #")" => NormalDelete.deleteAroundChrClose (app, chr, time)
      | #"]" => NormalDelete.deleteAroundChrClose (app, chr, time)
      | #"}" => NormalDelete.deleteAroundChrClose (app, chr, time)
      | #">" => NormalDelete.deleteAroundChrClose (app, chr, time)
      | _ => NormalFinish.clearMode app

    fun parseDeleteTerminal (str, count, app, chrCmd, time) =
      case chrCmd of
      (* terminal commands: require no input after *)
        #"h" => NormalDelete.deleteCharsLeft (app, count, time)
      | #"l" => NormalDelete.removeChr (app, count, time)
      (* vi's 'j' and 'k' commands move up or down a column
       * but 'dj' or 'dk' delete whole lines
       * so their implementation differs from
       * other cursor motions *)
      | #"j" => NormalDelete.deleteLineDown (app, count, time)
      | #"k" => NormalDelete.deleteLineBack (app, count, time)
      | #"w" => NormalDelete.deleteWord (app, count, time)
      | #"W" => NormalDelete.deleteByDfa (app, count, Cursor.nextWORD, time)
      | #"b" => NormalDelete.deleteByDfa (app, count, Cursor.prevWord, time)
      | #"B" => NormalDelete.deleteByDfa (app, count, Cursor.prevWORD, time)
      | #"e" =>
          NormalDelete.deleteByDfa (app, count, Cursor.endOfWordForDelete, time)
      | #"E" =>
          NormalDelete.deleteByDfa (app, count, Cursor.endOfWORDForDelete, time)
      | #"0" => NormalDelete.delete (app, 1, Cursor.vi0, time)
      | #"$" => NormalDelete.deleteToEndOfLine (app, time)
      | #"^" => NormalDelete.deleteToFirstNonSpaceChr (app, time)
      | #"d" => NormalDelete.deleteLine (app, count, time)
      | #"n" => NormalDelete.deleteToNextMatch (app, count, time)
      | #"N" => NormalDelete.deleteToPrevMatch (app, count, time)
      | #"%" => NormalDelete.deletePair (app, time)
      | #"G" => NormalDelete.deleteToEnd (app, time)
      (* non-terminal commands which require appending chr *)
      | #"t" => appendChr (app, chrCmd, str)
      | #"T" => appendChr (app, chrCmd, str)
      | #"f" => appendChr (app, chrCmd, str)
      | #"F" => appendChr (app, chrCmd, str)
      | #"g" => appendChr (app, chrCmd, str)
      | #"i" => appendChr (app, chrCmd, str)
      | #"a" => appendChr (app, chrCmd, str)
      (* invalid command: reset mode *)
      | _ => NormalFinish.clearMode app

    fun parseDeleteGo (app, count, chrCmd, time) =
      case chrCmd of
        #"e" => NormalDelete.deleteToEndOfPrevWord (app, count, time)
      | #"E" => NormalDelete.deleteToEndOfPrevWORD (app, count, time)
      | #"g" => NormalDelete.deleteToStart (app, time)
      | _ => NormalFinish.clearMode app

    fun parseDelete (strPos, str, count, app, chrCmd, time) =
      if strPos = String.size str - 1 then
        parseDeleteTerminal (str, count, app, chrCmd, time)
      else
        (* have to continue parsing string *)
        case String.sub (str, strPos + 1) of
          #"t" => NormalDelete.deleteTillNextChr (app, count, chrCmd, time)
        | #"T" => NormalDelete.deleteTillPrevChr (app, count, chrCmd, time)
        | #"f" => NormalDelete.deleteToNextChr (app, count, chrCmd, time)
        | #"F" => NormalDelete.deleteToPrevChr (app, count, chrCmd, time)
        | #"g" => parseDeleteGo (app, count, chrCmd, time)
        | #"i" => parseDeleteInside (app, chrCmd, time)
        | #"a" => parseDeleteAround (app, chrCmd, time)
        | _ => NormalFinish.clearMode app
  end

  structure ParseYankDelete =
  struct
    fun parseDeleteInside (app, chr, time) =
      case chr of
        #"w" => NormalYankDelete.deleteInsideWord (app, time)
      | #"W" => NormalYankDelete.deleteInsideWORD (app, time)
      | #"(" => NormalYankDelete.deleteInsideChrOpen (app, chr, time)
      | #"[" => NormalYankDelete.deleteInsideChrOpen (app, chr, time)
      | #"{" => NormalYankDelete.deleteInsideChrOpen (app, chr, time)
      | #"<" => NormalYankDelete.deleteInsideChrOpen (app, chr, time)
      | #")" => NormalYankDelete.deleteInsideChrClose (app, chr, time)
      | #"]" => NormalYankDelete.deleteInsideChrClose (app, chr, time)
      | #"}" => NormalYankDelete.deleteInsideChrClose (app, chr, time)
      | #">" => NormalYankDelete.deleteInsideChrClose (app, chr, time)
      | _ => NormalFinish.clearMode app

    fun parseDeleteAround (app, chr, time) =
      case chr of
        #"(" => NormalYankDelete.deleteAroundChrOpen (app, chr, time)
      | #"[" => NormalYankDelete.deleteAroundChrOpen (app, chr, time)
      | #"{" => NormalYankDelete.deleteAroundChrOpen (app, chr, time)
      | #"<" => NormalYankDelete.deleteAroundChrOpen (app, chr, time)
      | #")" => NormalYankDelete.deleteAroundChrClose (app, chr, time)
      | #"]" => NormalYankDelete.deleteAroundChrClose (app, chr, time)
      | #"}" => NormalYankDelete.deleteAroundChrClose (app, chr, time)
      | #">" => NormalYankDelete.deleteAroundChrClose (app, chr, time)
      | _ => NormalFinish.clearMode app

    fun parseDeleteTerminal (str, count, app, chrCmd, time) =
      case chrCmd of
      (* terminal commands: require no input after *)
        #"h" => NormalYankDelete.deleteCharsLeft (app, count, time)
      | #"l" => NormalYankDelete.removeChr (app, count, time)
      (* vi's 'j' and 'k' commands move up or down a column
       * but 'dj' or 'dk' delete whole lines
       * so their implementation differs from
       * other cursor motions *)
      | #"j" => NormalYankDelete.deleteLineDown (app, count, time)
      | #"k" => NormalYankDelete.deleteLineBack (app, count, time)
      | #"w" => NormalYankDelete.deleteWord (app, count, time)
      | #"W" => NormalYankDelete.deleteByDfa (app, count, Cursor.nextWORD, time)
      | #"b" => NormalYankDelete.deleteByDfa (app, count, Cursor.prevWord, time)
      | #"B" => NormalYankDelete.deleteByDfa (app, count, Cursor.prevWORD, time)
      | #"e" =>
          NormalYankDelete.deleteByDfa
            (app, count, Cursor.endOfWordForDelete, time)
      | #"E" =>
          NormalYankDelete.deleteByDfa
            (app, count, Cursor.endOfWORDForDelete, time)
      | #"0" => NormalYankDelete.delete (app, 1, Cursor.vi0, time)
      | #"$" => NormalYankDelete.deleteToEndOfLine (app, time)
      | #"^" => NormalYankDelete.deleteToFirstNonSpaceChr (app, time)
      | #"d" => NormalYankDelete.deleteLine (app, count, time)
      | #"n" => NormalYankDelete.deleteToNextMatch (app, count, time)
      | #"N" => NormalYankDelete.deleteToPrevMatch (app, count, time)
      | #"%" => NormalYankDelete.deletePair (app, time)
      | #"G" => NormalYankDelete.deleteToEnd (app, time)
      (* non-terminal commands which require appending chr *)
      | #"t" => appendChr (app, chrCmd, str)
      | #"T" => appendChr (app, chrCmd, str)
      | #"f" => appendChr (app, chrCmd, str)
      | #"F" => appendChr (app, chrCmd, str)
      | #"g" => appendChr (app, chrCmd, str)
      | #"i" => appendChr (app, chrCmd, str)
      | #"a" => appendChr (app, chrCmd, str)
      (* invalid command: reset mode *)
      | _ => NormalFinish.clearMode app

    fun parseDeleteGo (app, count, chrCmd, time) =
      case chrCmd of
        #"e" => NormalYankDelete.deleteToEndOfPrevWord (app, count, time)
      | #"E" => NormalYankDelete.deleteToEndOfPrevWORD (app, count, time)
      | #"g" => NormalYankDelete.deleteToStart (app, time)
      | _ => NormalFinish.clearMode app

    fun parseDelete (strPos, str, count, app, chrCmd, time) =
      if strPos = String.size str - 1 then
        parseDeleteTerminal (str, count, app, chrCmd, time)
      else
        (* have to continue parsing string *)
        case String.sub (str, strPos + 1) of
          #"t" => NormalYankDelete.deleteTillNextChr (app, count, chrCmd, time)
        | #"T" => NormalYankDelete.deleteTillPrevChr (app, count, chrCmd, time)
        | #"f" => NormalYankDelete.deleteToNextChr (app, count, chrCmd, time)
        | #"F" => NormalYankDelete.deleteToPrevChr (app, count, chrCmd, time)
        | #"g" => parseDeleteGo (app, count, chrCmd, time)
        | #"i" => parseDeleteInside (app, chrCmd, time)
        | #"a" => parseDeleteAround (app, chrCmd, time)
        | _ => NormalFinish.clearMode app
  end

  structure ParseYank =
  struct
    fun yankWhenMovingBack (app: app_type, fMove, count) =
      let
        open DrawMsg
        open MailboxType

        val {buffer, cursorIdx, ...} = app

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val low = fMove (buffer, cursorIdx, count)

        val length = cursorIdx - low
        val str = LineGap.substring (low, length, buffer)

        val msg = YANK str
        val mode = NORMAL_MODE ""
      in
        NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
      end

    fun yankWhenMovingForward (app: app_type, fMove, count) =
      let
        open DrawMsg
        open MailboxType

        val {buffer, cursorIdx, ...} = app

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val high = fMove (buffer, cursorIdx, count)

        val buffer = LineGap.goToIdx (high, buffer)
        val length = high - cursorIdx
        val str = LineGap.substring (cursorIdx, length, buffer)

        val msg = YANK str
        val mode = NORMAL_MODE ""
      in
        NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
      end

    fun parseYankTerminal (str, count, app, chrCmd, time) =
      case chrCmd of
      (* motions like yh / yj / yk / yl are not really needed.
       * Vim supports them, but I never use them.
       * I also don't need yx (yank a character and then remove it)
       * because I never do that. *)
        #"y" => NormalYank.yankLine (app, count)
      | #"0" => NormalYank.yankToStartOfLine app
      | #"w" => NormalYank.yankWhenMovingForward (app, Cursor.nextWord, count)
      | #"W" => NormalYank.yankWhenMovingForward (app, Cursor.nextWORD, count)
      | #"b" => NormalYank.yankWhenMovingBack (app, Cursor.prevWord, count)
      | #"B" => NormalYank.yankWhenMovingBack (app, Cursor.prevWORD, count)
      | #"e" =>
          NormalYank.yankWhenMovingForward
            (app, Cursor.endOfWordForDelete, count)
      | #"E" =>
          NormalYank.yankWhenMovingForward
            (app, Cursor.endOfWORDForDelete, count)
      | #"$" => NormalYank.yankWhenMovingForward (app, Cursor.viDlr, 1)
      | #"^" => NormalYank.yankToFirstNonSpaceChr app
      | #"G" => NormalYank.yankToEndOfText app
      | #"%" => NormalYank.yankToMatchingPair app
      | #"n" => NormalYank.yankToNextMatch (app, count)
      | #"N" => NormalYank.yankToPrevMatch (app, count)
      | #"x" => NormalYankDelete.removeChr (app, count, time)
      (* append non-terminal characters to string *)
      | #"d" =>
          let (* 'yd' motion, like 'ydw'; meant to be 'yank then delete' *)
          in appendChr (app, chrCmd, str)
          end
      | #"t" => appendChr (app, chrCmd, str)
      | #"T" => appendChr (app, chrCmd, str)
      | #"f" => appendChr (app, chrCmd, str)
      | #"F" => appendChr (app, chrCmd, str)
      | #"g" => appendChr (app, chrCmd, str)
      | #"i" => appendChr (app, chrCmd, str)
      | #"a" => appendChr (app, chrCmd, str)
      | _ => NormalFinish.clearMode app

    fun parseYankGo (count, app, chrCmd) =
      case chrCmd of
        #"e" =>
          NormalYank.yankWhenMovingBackPlusOne
            (app, Cursor.endOfPrevWord, count)
      | #"E" =>
          NormalYank.yankWhenMovingBackPlusOne
            (app, Cursor.endOfPrevWORD, count)
      | #"g" => NormalYank.yankToStart app
      | _ => NormalFinish.clearMode app

    fun parseYankInside (app, chr) =
      case chr of
        #"w" => NormalYank.yankInsideWord app
      | #"W" => NormalYank.yankInsideWORD app
      | #"(" => NormalYank.yankInsideChrOpen (app, chr)
      | #"[" => NormalYank.yankInsideChrOpen (app, chr)
      | #"{" => NormalYank.yankInsideChrOpen (app, chr)
      | #"<" => NormalYank.yankInsideChrOpen (app, chr)
      | #")" => NormalYank.yankInsideChrClose (app, chr)
      | #"]" => NormalYank.yankInsideChrClose (app, chr)
      | #"}" => NormalYank.yankInsideChrClose (app, chr)
      | #">" => NormalYank.yankInsideChrClose (app, chr)
      | _ => NormalFinish.clearMode app

    fun parseYankAround (app, chr) =
      case chr of
        #"(" => NormalYank.yankAroundChrOpen (app, chr)
      | #"[" => NormalYank.yankAroundChrOpen (app, chr)
      | #"{" => NormalYank.yankAroundChrOpen (app, chr)
      | #"<" => NormalYank.yankAroundChrOpen (app, chr)
      | #")" => NormalYank.yankAroundChrClose (app, chr)
      | #"]" => NormalYank.yankAroundChrClose (app, chr)
      | #"}" => NormalYank.yankAroundChrClose (app, chr)
      | #">" => NormalYank.yankAroundChrClose (app, chr)
      | _ => NormalFinish.clearMode app

    fun parseYank (strPos, str, count, app, chrCmd, time) =
      if strPos = String.size str - 1 then
        parseYankTerminal (str, count, app, chrCmd, time)
      else
        case String.sub (str, strPos + 1) of
          #"t" => NormalYank.yankTillNextChr (app, count, chrCmd)
        | #"T" => NormalYank.yankTillPrevChr (app, count, chrCmd)
        | #"f" => NormalYank.yankToNextChr (app, count, chrCmd)
        | #"F" => NormalYank.yankToPrevChr (app, count, chrCmd)
        | #"g" => parseYankGo (count, app, chrCmd)
        | #"i" => parseYankInside (app, chrCmd)
        | #"a" => parseYankAround (app, chrCmd)
        | #"d" =>
            ParseYankDelete.parseDelete
              (strPos + 1, str, count, app, chrCmd, time)
        | _ => NormalFinish.clearMode app
  end

  (* useful reference as list of non-terminal commands *)
  fun parseAfterCount (strPos, str, count, app, chrCmd, time) =
    (* we are trying to parse multi-char but non-terminal strings here.
     * For example, we don't want to parse 3w which is a terminal commmand
     * to go 3 words forwards
     * but we do want to parse 3d which is a non-terminal command
     * which can be made terminal by adding "w" or "e" at the end. 
     * *)
    case String.sub (str, strPos) of
      #"t" => NormalMove.tillNextChr (app, count, chrCmd)
    | #"T" => NormalMove.tillPrevChr (app, count, chrCmd)
    | #"y" => ParseYank.parseYank (strPos, str, count, app, chrCmd, time)
    | #"d" => ParseDelete.parseDelete (strPos, str, count, app, chrCmd, time)
    | #"f" => NormalMove.toNextChr (app, count, chrCmd)
    | #"F" => NormalMove.toPrevChr (app, count, chrCmd)
    | #"g" => (* go *) parseGo (count, app, chrCmd)
    | #"c" => (* change *) NormalFinish.clearMode app
    | _ =>
        (* isn't a non-terminal cmd
         * this case should never happen*)
        NormalFinish.clearMode app

  fun parseNormalModeCommand (app, str, chrCmd, time) =
    if String.size str = 0 then
      parseChr (app, 1, chrCmd, str, time)
    else if String.size str = 1 then
      case Int.fromString str of
        SOME count => parseChr (app, count, chrCmd, str, time)
      | NONE => parseAfterCount (0, str, 1, app, chrCmd, time)
    else
      let
        val numLength = getNumLength (0, str)
        val count = String.substring (str, 0, numLength)
        val count =
          case Int.fromString count of
            SOME x => x
          | NONE => 1
      in
        if numLength = String.size str then
          (* reached end of str; str only contained numbers *)
          parseChr (app, count, chrCmd, str, time)
        else
          (* continue parsing. *)
          parseAfterCount (numLength, str, count, app, chrCmd, time)
      end

  structure LeftArrow =
  struct
    fun parseLeftArrowCommand (strPos, str, count, app, time) =
      case String.sub (str, strPos) of
        #"y" =>
          if strPos + 1 = String.size str then
            (* terminal command, so simple yank *)
            raise Fail "left-arrow-yank unimplemnted"
          else
            (case String.sub (str, strPos + 1) of
               #"d" => NormalYankDelete.deleteCharsLeft (app, count, time)
             | _ => NormalFinish.clearMode app)
      | #"d" => NormalDelete.deleteCharsLeft (app, count, time)
      | _ => NormalFinish.clearMode app

    fun parse (app, str, time) =
      if String.size str = 0 then
        MoveViH.move (app, 1)
      else if String.size str = 1 then
        case Int.fromString str of
          SOME count => MoveViH.move (app, count)
        | NONE => parseLeftArrowCommand (0, str, 1, app, time)
      else
        let
          val numLength = getNumLength (0, str)
          val count = String.substring (str, 0, numLength)
          val count =
            case Int.fromString count of
              SOME x => x
            | NONE => 1
        in
          if numLength = String.size str then
            (* reached end of string; string only contained numbers *)
            MoveViH.move (app, count)
          else
            parseLeftArrowCommand (numLength, str, count, app, time)
        end
  end

  structure RightArrow =
  struct
    fun parseRightArrowCommand (strPos, str, count, app, time) =
      case String.sub (str, strPos) of
        #"y" =>
          if strPos + 1 = String.size str then
            raise Fail "right-arrow-yank unimplemnted"
          else
            (case String.sub (str, strPos + 1) of
               #"d" => NormalYankDelete.removeChr (app, count, time)
             | _ => NormalFinish.clearMode app)
      | #"d" => NormalDelete.removeChr (app, count, time)
      | _ => NormalFinish.clearMode app

    fun parse (app, str, time) =
      if String.size str = 0 then
        MoveViL.move (app, 1)
      else if String.size str = 1 then
        case Int.fromString str of
          SOME count => MoveViL.move (app, count)
        | NONE => parseRightArrowCommand (0, str, 1, app, time)
      else
        let
          val numLength = getNumLength (0, str)
          val count = String.substring (str, 0, numLength)
          val count =
            case Int.fromString count of
              SOME x => x
            | NONE => 1
        in
          if numLength = String.size str then
            (* reached end of string; string only contained numbers *)
            MoveViH.move (app, count)
          else
            parseRightArrowCommand (numLength, str, count, app, time)
        end
  end

  fun update (app, str, msg, time) =
    case msg of
      CHAR_EVENT chrCmd => parseNormalModeCommand (app, str, chrCmd, time)
    | KEY_ESC => NormalFinish.clearMode app
    | RESIZE_EVENT (width, height) =>
        NormalFinish.resizeText (app, width, height)

    | ARROW_RIGHT => RightArrow.parse (app, str, time)
    | ARROW_LEFT => LeftArrow.parse (app, str, time)
    | ARROW_UP => NormalFinish.clearMode app
    | ARROW_DOWN => NormalFinish.clearMode app

    | KEY_ENTER => NormalFinish.clearMode app
    | KEY_BACKSPACE => NormalFinish.clearMode app
end
