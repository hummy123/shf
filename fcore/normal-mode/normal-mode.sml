structure NormalMode =
struct
  (* parsing functions, deciding what to do while we are in normal mode *)

  open AppType
  open InputMsg

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
      AppWith.mode (app, mode, [])
    end

  fun parseMoveToChr (count, app, fMove, chrCmd) =
    NormalMove.moveToChr (app, count, fMove, chrCmd)

  fun parseGo (count, app, chrCmd) =
    case chrCmd of
      #"e" => MoveToEndOfPrevWord.move (app, count)
    | #"E" => MoveToEndOfPrevWORD.move (app, count)
    | #"g" => NormalMove.moveToStart app
    | _ => Finish.clearMode app

  fun parseChr (app: app_type, count, chr, str, time) =
    case chr of
      #"h" => MoveViH.move (app, count)
    | #"j" => MoveViJ.move (app, count)
    | #"k" => MoveViK.move (app, count)
    | #"l" => MoveViL.move (app, count)
    | #"w" => MoveToNextWord.move (app, count)
    | #"W" => MoveToNextWORD.move (app, count)
    | #"b" => MoveToPrevWord.move (app, count)
    | #"B" => MoveToPrevWORD.move (app, count)
    | #"e" => MoveToEndOfWord.move (app, count)
    | #"E" => MoveToEndOfWORD.move (app, count)
    | #"n" => NormalMove.moveToNextMatch (app, count)
    | #"N" => NormalMove.moveToPrevMatch (app, count)
    | #"z" => Finish.centreToCursor app
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
                AppWith.mode (app, mode, [])
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
        else NormalMove.moveToLine (app, count - 1)
    | #"%" => NormalMove.moveToMatchingPair app
    | #"x" => NormalDelete.removeChr (app, count, time)
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
          AppWith.mode (app, mode, [])
        end

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
    | _ => Finish.clearMode app

  fun parseDeleteAround (app, chr, time) =
    case chr of
      #"(" => NormalDelete.deleteInsideChrOpen (app, chr, time)
    | #"[" => NormalDelete.deleteInsideChrOpen (app, chr, time)
    | #"{" => NormalDelete.deleteInsideChrOpen (app, chr, time)
    | #"<" => NormalDelete.deleteInsideChrOpen (app, chr, time)
    | #")" => NormalDelete.deleteAroundChrClose (app, chr, time)
    | #"]" => NormalDelete.deleteAroundChrClose (app, chr, time)
    | #"}" => NormalDelete.deleteAroundChrClose (app, chr, time)
    | #">" => NormalDelete.deleteAroundChrClose (app, chr, time)
    | _ => Finish.clearMode app

  fun parseDeleteTerminal (str, count, app, chrCmd, time) =
    case chrCmd of
    (* terminal commands: require no input after *)
      #"h" => NormalDelete.delete (app, count, Cursor.viH, time)
    | #"l" => NormalDelete.delete (app, count, Cursor.viL, time)
    (* vi's 'j' and 'k' commands move up or down a column
     * but 'dj' or 'dk' delete whole lines
     * so their implementation differs from
     * other cursor motions *)
    | #"j" => NormalDelete.deleteLine (app, count + 1, time)
    | #"k" => NormalDelete.deleteLineBack (app, count, time)
    | #"w" => NormalDelete.deleteByDfa (app, count, Cursor.nextWord, time)
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
    (* non-terminal commands which require appending chr *)
    | #"t" => appendChr (app, chrCmd, str)
    | #"T" => appendChr (app, chrCmd, str)
    | #"f" => appendChr (app, chrCmd, str)
    | #"F" => appendChr (app, chrCmd, str)
    | #"g" => appendChr (app, chrCmd, str)
    | #"i" => appendChr (app, chrCmd, str)
    | #"a" => appendChr (app, chrCmd, str)
    (* invalid command: reset mode *)
    | _ => Finish.clearMode app

  fun parseDeleteGo (app, count, chrCmd, time) =
    case chrCmd of
      #"e" => NormalDelete.deleteByDfa (app, count, Cursor.endOfPrevWord, time)
    | #"E" => NormalDelete.deleteByDfa (app, count, Cursor.endOfPrevWORD, time)
    | #"g" => NormalDelete.deleteToStart (app, time)
    | _ => Finish.clearMode app

  fun parseDelete (strPos, str, count, app, chrCmd, time) =
    if strPos = String.size str - 1 then
      parseDeleteTerminal (str, count, app, chrCmd, time)
    else
      (* have to continue parsing string *)
      case String.sub (str, strPos + 1) of
        #"t" =>
          NormalDelete.deleteToChr
            (app, 1, Cursor.tillNextChr, op+, chrCmd, time)
      | #"T" =>
          NormalDelete.deleteToChr
            (app, 1, Cursor.tillPrevChr, op-, chrCmd, time)
      | #"f" =>
          NormalDelete.deleteToChr
            (app, count, Cursor.toNextChr, op+, chrCmd, time)
      | #"F" =>
          NormalDelete.deleteToChr
            (app, count, Cursor.toPrevChr, op-, chrCmd, time)
      | #"g" => parseDeleteGo (app, count, chrCmd, time)
      | #"i" => parseDeleteInside (app, chrCmd, time)
      | #"a" => parseDeleteAround (app, chrCmd, time)
      | _ => Finish.clearMode app

  (* useful reference as list of non-terminal commands *)
  fun parseAfterCount (strPos, str, count, app, chrCmd, time) =
    (* we are trying to parse multi-char but non-terminal strings here.
     * For example, we don't want to parse 3w which is a terminal commmand
     * to go 3 words forwards
     * but we do want to parse 3d which is a non-terminal command
     * which can be made terminal by adding "w" or "e" at the end. 
     * *)
    case String.sub (str, strPos) of
      #"t" =>
        (* to just before char, forward 
         * tillNextChr with count of 1 has same effect
         * as tillNextChr with any count above 1
         * so just hardcode 1 *)
        parseMoveToChr (1, app, Cursor.tillNextChr, chrCmd)
    | #"T" =>
        (* to just before chr, backward *)
        parseMoveToChr (1, app, Cursor.tillPrevChr, chrCmd)
    | #"y" => (* yank *) Finish.clearMode app
    | #"d" => (* delete *) parseDelete (strPos, str, count, app, chrCmd, time)
    | #"f" =>
        (* to chr, forward *)
        parseMoveToChr (count, app, Cursor.toNextChr, chrCmd)
    | #"F" =>
        (* to chr, backward *)
        parseMoveToChr (count, app, Cursor.toPrevChr, chrCmd)
    | #"g" => (* go *) parseGo (count, app, chrCmd)
    | #"c" => (* change *) Finish.clearMode app
    | _ =>
        (* isn't a non-terminal cmd
         * this case should never happen*)
        Finish.clearMode app

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

  fun update (app, str, msg, time) =
    case msg of
      CHAR_EVENT chrCmd => parseNormalModeCommand (app, str, chrCmd, time)
    | KEY_ESC => Finish.clearMode app
    | RESIZE_EVENT (width, height) => Finish.resizeText (app, width, height)
    | WITH_SEARCH_LIST searchList => Finish.withSearchList (app, searchList)
end
