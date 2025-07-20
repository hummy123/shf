signature MAKE_DFA_LOOP =
sig
  val fNext: int * int * string * string list * Word8.word * int -> int
  val fPrev: int * int * string * string list * Word8.word * int -> int
  val startState: Word8.word
end

functor MakeDfaLoop(M: MAKE_DFA_LOOP) =
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
              M.fNext (strIdx, cursorIdx, shd, stl, M.startState, 1)
            else
              (* strIdx is in tl *)
              case stl of
                stlhd :: stltl =>
                  M.fNext (strIdx, cursorIdx, stlhd, stltl, M.startState, 1)
              | _ => cursorIdx
          end
      | [] => cursorIdx
    end

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
              M.fPrev (strIdx, cursorIdx, shd, leftStrings, M.startState, 1)
            else
              (* strIdx is in tl *)
              (case stl of
                 stlhd :: stltl =>
                   let
                     val strIdx = strIdx - String.size shd
                     val leftStrings = shd :: leftStrings
                   in
                     M.fPrev
                       (strIdx, cursorIdx, stlhd, leftStrings, M.startState, 1)
                   end
               | [] => cursorIdx)
          end
      | [] => cursorIdx
    end
end
