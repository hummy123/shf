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

  fun parseChr (app: app_type, count, chr, str) =
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
    | #"x" => NormalDelete.removeChr (app, count)
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

  fun parseDeleteInside (app, chr) =
    case chr of
      #"w" => NormalDelete.deleteInsideWord app
    | #"W" => NormalDelete.deleteInsideWORD app
    | #"(" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #"[" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #"{" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #"<" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #")" => NormalDelete.deleteInsideChrClose (app, chr)
    | #"]" => NormalDelete.deleteInsideChrClose (app, chr)
    | #"}" => NormalDelete.deleteInsideChrClose (app, chr)
    | #">" => NormalDelete.deleteInsideChrClose (app, chr)
    | _ => Finish.clearMode app

  fun parseDeleteAround (app, chr) =
    case chr of
      #"(" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #"[" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #"{" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #"<" => NormalDelete.deleteInsideChrOpen (app, chr)
    | #")" => NormalDelete.deleteAroundChrClose (app, chr)
    | #"]" => NormalDelete.deleteAroundChrClose (app, chr)
    | #"}" => NormalDelete.deleteAroundChrClose (app, chr)
    | #">" => NormalDelete.deleteAroundChrClose (app, chr)
    | _ => Finish.clearMode app

  fun parseDeleteTerminal (str, count, app, chrCmd) =
    case chrCmd of
    (* terminal commands: require no input after *)
      #"h" => NormalDelete.delete (app, count, Cursor.viH)
    | #"l" => NormalDelete.delete (app, count, Cursor.viL)
    (* vi's 'j' and 'k' commands move up or down a column
     * but 'dj' or 'dk' delete whole lines
     * so their implementation differs from
     * other cursor motions *)
    | #"j" => NormalDelete.deleteLine (app, count + 1)
    | #"k" => NormalDelete.deleteLineBack (app, count)
    | #"w" => NormalDelete.deleteByDfa (app, count, Cursor.nextWord)
    | #"W" => NormalDelete.deleteByDfa (app, count, Cursor.nextWORD)
    | #"b" => NormalDelete.deleteByDfa (app, count, Cursor.prevWord)
    | #"B" => NormalDelete.deleteByDfa (app, count, Cursor.prevWORD)
    | #"e" => NormalDelete.deleteByDfa (app, count, Cursor.endOfWordForDelete)
    | #"E" => NormalDelete.deleteByDfa (app, count, Cursor.endOfWORDForDelete)
    | #"0" => NormalDelete.delete (app, 1, Cursor.vi0)
    | #"$" => NormalDelete.deleteToEndOfLine app
    | #"^" => NormalDelete.deleteToFirstNonSpaceChr app
    | #"d" => NormalDelete.deleteLine (app, count)
    | #"n" => NormalDelete.deleteToNextMatch (app, count)
    | #"N" => NormalDelete.deleteToPrevMatch (app, count)
    | #"%" => NormalDelete.deletePair app
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

  fun parseDeleteGo (app, count, chrCmd) =
    case chrCmd of
      #"e" => NormalDelete.deleteByDfa (app, count, Cursor.endOfPrevWord)
    | #"E" => NormalDelete.deleteByDfa (app, count, Cursor.endOfPrevWORD)
    | #"g" => NormalDelete.deleteToStart app
    | _ => Finish.clearMode app

  fun parseDelete (strPos, str, count, app, chrCmd) =
    if strPos = String.size str - 1 then
      parseDeleteTerminal (str, count, app, chrCmd)
    else
      (* have to continue parsing string *)
      case String.sub (str, strPos + 1) of
        #"t" =>
          NormalDelete.deleteToChr (app, 1, Cursor.tillNextChr, op+, chrCmd)
      | #"T" =>
          NormalDelete.deleteToChr (app, 1, Cursor.tillPrevChr, op-, chrCmd)
      | #"f" =>
          NormalDelete.deleteToChr (app, count, Cursor.toNextChr, op+, chrCmd)
      | #"F" =>
          NormalDelete.deleteToChr (app, count, Cursor.toPrevChr, op-, chrCmd)
      | #"g" => parseDeleteGo (app, count, chrCmd)
      | #"i" => parseDeleteInside (app, chrCmd)
      | #"a" => parseDeleteAround (app, chrCmd)
      | _ => Finish.clearMode app

  (* useful reference as list of non-terminal commands *)
  fun parseAfterCount (strPos, str, count, app, chrCmd) =
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
    | #"d" => (* delete *) parseDelete (strPos, str, count, app, chrCmd)
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

  fun parseNormalModeCommand (app, str, chrCmd) =
    if String.size str = 0 then
      parseChr (app, 1, chrCmd, str)
    else if String.size str = 1 then
      case Int.fromString str of
        SOME count => parseChr (app, count, chrCmd, str)
      | NONE => parseAfterCount (0, str, 1, app, chrCmd)
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
          parseChr (app, count, chrCmd, str)
        else
          (* continue parsing. *)
          parseAfterCount (numLength, str, count, app, chrCmd)
      end

  fun update (app, str, msg) =
    case msg of
      CHAR_EVENT chrCmd => parseNormalModeCommand (app, str, chrCmd)
    | KEY_ESC => Finish.clearMode app
    | RESIZE_EVENT (width, height) => Finish.resizeText (app, width, height)
    | WITH_SEARCH_LIST searchList => Finish.withSearchList (app, searchList)
end
