signature MAKE_DFA_LOOP =
sig
  val fStart: int * int * string * string list * Word8.word * int -> int
  val startState: Word8.word
end

functor MakeNextDfaLoop(M: MAKE_DFA_LOOP) =
struct
  fun next (lineGap: LineGap.t, cursorIdx, count) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
      (* convert absolute cursorIdx to idx relative to hd string *)
      val strIdx = cursorIdx - bufferIdx
    in
      case rightStrings of
        shd :: stl =>
          if strIdx < String.size shd then
            (* strIdx is in this string *)
            M.fStart (strIdx, cursorIdx, shd, stl, M.startState, count)
          else
            (* strIdx is in tl *)
            (case stl of
               stlhd :: stltl =>
                 let
                   val strIdx = strIdx - String.size shd
                 in
                   M.fStart
                     (strIdx, cursorIdx, stlhd, stltl, M.startState, count)
                 end
             | _ => cursorIdx)
      | [] => cursorIdx
    end
end

functor MakeNextDfaLoopPlus1(M: MAKE_DFA_LOOP) =
struct
  fun next (lineGap: LineGap.t, cursorIdx, count) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
      (* convert absolute cursorIdx to idx relative to hd string *)
      val strIdx = cursorIdx - bufferIdx + 1
      val absIdx = cursorIdx + 1
    in
      case rightStrings of
        shd :: stl =>
          if strIdx < String.size shd then
            (* strIdx is in this string *)
            M.fStart (strIdx, absIdx, shd, stl, M.startState, count)
          else
            (* strIdx is in tl *)
            (case stl of
               stlhd :: stltl =>
                 let val strIdx = strIdx - String.size shd
                 in M.fStart (strIdx, absIdx, stlhd, stltl, M.startState, count)
                 end
             | _ => cursorIdx)
      | [] => cursorIdx
    end
end

functor MakePrevDfaLoop(M: MAKE_DFA_LOOP) =
struct
  fun prev (lineGap: LineGap.t, cursorIdx, count) =
    let
      val {rightStrings, leftStrings, idx = bufferIdx, ...} = lineGap
      (* convert absolute cursorIdx to idx relative to hd string *)
      val strIdx = cursorIdx - bufferIdx
    in
      case rightStrings of
        shd :: stl =>
          if strIdx < String.size shd then
            (* strIdx is in this string *)
            M.fStart (strIdx, cursorIdx, shd, leftStrings, M.startState, count)
          else
            (* strIdx is in tl *)
            (case stl of
               stlhd :: stltl =>
                 let
                   val strIdx = strIdx - String.size shd
                   val leftStrings = shd :: leftStrings
                 in
                   M.fStart
                     ( strIdx
                     , cursorIdx
                     , stlhd
                     , leftStrings
                     , M.startState
                     , count
                     )
                 end
             | [] => cursorIdx)
      | [] => cursorIdx
    end
end

functor MakePrevDfaLoopMinus1(M: MAKE_DFA_LOOP) =
struct
  fun prev (lineGap: LineGap.t, cursorIdx, count) =
    let
      val {idx = bufferIdx, leftStrings, ...} = lineGap
      val strIdx = cursorIdx - bufferIdx - 1
      val absIdx = cursorIdx - 1
    in
      if strIdx < 0 then
        case leftStrings of
          lhd :: ltl =>
            M.fStart
              (String.size lhd - 1, absIdx, lhd, ltl, M.startState, count)
        | [] => 0
      else
        case #rightStrings lineGap of
          rhd :: _ =>
            M.fStart (strIdx, absIdx, rhd, leftStrings, M.startState, count)
        | [] => Int.max (0, cursorIdx - 2)
    end
end

signature MAKE_CHAR_FOLDER =
sig
  val startState: Word8.word
  val tables: Word8.word vector vector

  val isFinal: Word8.word -> bool
  val finish: int -> int
end

functor MakeCharFolderNext(Fn: MAKE_CHAR_FOLDER) =
struct
  fun nextState (currentState, currentChar) =
    let
      val currentState = Word8.toInt currentState
      val currentTable = Vector.sub (Fn.tables, currentState)
      val charIdx = Char.ord currentChar
    in
      Vector.sub (currentTable, charIdx)
    end

  fun foldNext (idx, absIdx, str, tl, currentState, counter) =
    if idx = String.size str then
      case tl of
        str :: tl => foldNext (0, absIdx, str, tl, currentState, counter)
      | [] => Int.max (absIdx - 2, 0)
    else
      let
        val chr = String.sub (str, idx)
        val newState = nextState (currentState, chr)
      in
        if Fn.isFinal newState then
          if counter - 1 = 0 then
            Fn.finish absIdx
          else
            (* new loop, so reset to start state and proceed *)
            foldNext (idx + 1, absIdx + 1, str, tl, Fn.startState, counter - 1)
        else
          foldNext (idx + 1, absIdx + 1, str, tl, newState, counter)
      end
end

functor MakeCharFolderPrev(Fn: MAKE_CHAR_FOLDER) =
struct
  fun nextState (currentState, currentChar) =
    let
      val currentState = Word8.toInt currentState
      val currentTable = Vector.sub (Fn.tables, currentState)
      val charIdx = Char.ord currentChar
    in
      Vector.sub (currentTable, charIdx)
    end

  fun foldPrev (idx, absIdx, str, tl, currentState, counter) =
    if idx < 0 then
      case tl of
        str :: tl =>
          foldPrev (String.size str - 1, absIdx, str, tl, currentState, counter)
      | [] => 0
    else
      let
        val chr = String.sub (str, idx)
        val newState = nextState (currentState, chr)
      in
        if Fn.isFinal newState then
          if counter - 1 = 0 then
            Fn.finish absIdx
          else
            foldPrev (idx - 1, absIdx - 1, str, tl, Fn.startState, counter - 1)
        else
          foldPrev (idx - 1, absIdx - 1, str, tl, newState, counter)
      end
end

signature MAKE_IF_CHAR_FOLDER =
sig
  val fStart: int * string * int vector * int * string list * int vector list
              -> int
end

functor MakeIfCharFolderPrev(Fn: MAKE_IF_CHAR_FOLDER) =
struct
  fun foldPrev (lineGap: LineGap.t, cursorIdx) =
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
              Fn.fStart (strIdx, strHd, lnHd, cursorIdx, leftStrings, leftLines)
            else
              (* strIdx must be in the strTl *)
              (case (strTl, lnTl) of
                 (nestStrHd :: _, nestLnHd :: _) =>
                   let
                     val strIdx = strIdx - String.size strHd
                   in
                     Fn.fStart
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
end
