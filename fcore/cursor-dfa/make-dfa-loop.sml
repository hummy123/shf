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
    in
      case rightStrings of
        shd :: stl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size shd then
              (* strIdx is in this string *)
              M.fStart (strIdx, cursorIdx, shd, stl, M.startState, 1)
            else
              (* strIdx is in tl *)
              case stl of
                stlhd :: stltl =>
                  M.fStart (strIdx, cursorIdx, stlhd, stltl, M.startState, 1)
              | _ => cursorIdx
          end
      | [] => cursorIdx
    end
end

functor MakePrevDfaLoop(M: MAKE_DFA_LOOP) =
struct
  fun prev (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, leftStrings, idx = bufferIdx, ...} = lineGap
    in
      case rightStrings of
        shd :: stl =>
          let
            (* convert absolute cursorIdx to idx relative to hd string *)
            val strIdx = cursorIdx - bufferIdx
          in
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
          end
      | [] => cursorIdx
    end
end
