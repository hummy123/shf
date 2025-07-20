signature MAKE_DFA_LOOP =
sig
  val fStart: int * int * string * string list * Word8.word * int -> int
  val startState: Word8.word
end

functor MakeNextDfaLoop(M: MAKE_DFA_LOOP) =
struct
  fun next (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
      (* convert absolute cursorIdx to idx relative to hd string *)
      val strIdx = cursorIdx - bufferIdx
    in
      case rightStrings of
        shd :: stl =>
          if strIdx < String.size shd then
            (* strIdx is in this string *)
            M.fStart (strIdx, cursorIdx, shd, stl, M.startState, 1)
          else
            (* strIdx is in tl *)
            (case stl of
               stlhd :: stltl =>
                 let val strIdx = strIdx - String.size shd
                 in M.fStart (strIdx, cursorIdx, stlhd, stltl, M.startState, 1)
                 end
             | _ => cursorIdx)
      | [] => cursorIdx
    end
end

functor MakePrevDfaLoop(M: MAKE_DFA_LOOP) =
struct
  fun prev (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, leftStrings, idx = bufferIdx, ...} = lineGap
      (* convert absolute cursorIdx to idx relative to hd string *)
      val strIdx = cursorIdx - bufferIdx
    in
      case rightStrings of
        shd :: stl =>
          if strIdx < String.size shd then
            (* strIdx is in this string *)
            M.fStart (strIdx, cursorIdx, shd, leftStrings, M.startState, 1)
          else
            (* strIdx is in tl *)
            (case stl of
               stlhd :: stltl =>
                 let
                   val strIdx = strIdx - String.size shd
                   val leftStrings = shd :: leftStrings
                 in
                   M.fStart
                     (strIdx, cursorIdx, stlhd, leftStrings, M.startState, 1)
                 end
             | [] => cursorIdx)
      | [] => cursorIdx
    end
end

functor MakePrevDfaLoopMinus1(M: MAKE_DFA_LOOP) =
struct
  fun prev (lineGap: LineGap.t, cursorIdx) =
    let
      val {idx = bufferIdx, leftStrings, ...} = lineGap
      val strIdx = cursorIdx - bufferIdx - 1
      val absIdx = cursorIdx - 1
    in
      if strIdx < 0 then
        case leftStrings of
          lhd :: ltl =>
            M.fStart (String.size lhd - 1, absIdx, lhd, ltl, M.startState, 1)
        | [] => 0
      else
        case #rightStrings lineGap of
          rhd :: _ =>
            M.fStart (strIdx, absIdx, rhd, leftStrings, M.startState, 1)
        | [] => Int.max (0, cursorIdx - 2)
    end
end
