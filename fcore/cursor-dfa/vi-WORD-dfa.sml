structure ViWORDDfa =
struct
  val startState = 0w0
  val startNonBlankState = 0w1
  val startSpaceState = 0w2
  val nonBlankAfterSpaceState = 0w4

  fun makeStart i =
    let
      val chr = Char.chr i
    in
      if Char.isSpace chr then
        startSpaceState
      else
        startNonBlankState
    end

  fun makeStartNonBlankState i =
    let
      val chr = Char.chr i
    in
      if Char.isSpace chr then
        startSpaceState
      else
        startNonBlankState
    end

  fun makeStartSpace i =
    let
      val chr = Char.chr i
    in
      if Char.isSpace chr then
        startSpaceState
      else
        nonBlankAfterSpaceState
    end

  val startTable = Vector.tabulate (255, makeStart)
  val startNonBlankTable = Vector.tabulate (255, makeStartNonBlankState)
  val startSpaceTable = Vector.tabulate (255, makeStartSpace)

  val tables = #[startTable, startNonBlankTable, startSpaceTable]

  fun next (currentState, currentChar) =
    let
      val currentState = Word8.toInt currentState
      val currentTable = Vector.sub (tables, currentState)
      val charIdx = Char.ord currentChar
    in
      Vector.sub (currentTable, charIdx)
    end

  fun loopNextWORD (idx, absIdx, str, tl, currentState, counter) =
    if idx = String.size str then
      case tl of
        str :: tl  =>
          loopNextWORD (0, absIdx, str, tl, currentState, counter)
      | [] =>
          Int.max (absIdx - 2, 0)
    else
      let
        val chr = String.sub (str, idx)
        val newState = next (currentState, chr)
      in
        if newState = nonBlankAfterSpaceState then 
          if counter = 0 then
            absIdx
          else
            (* new loop, so reset to start state and proceed *)
            loopNextWORD (idx + 1, absIdx + 1, str, tl, startState, counter - 1)
        else
          loopNextWORD (idx + 1, absIdx + 1, str, tl, newState, counter)
      end

  fun loopPrevWORD (idx, absIdx, str, tl, currentState, counter) =
    if idx < 0 then
      case tl of
        str :: tl =>
          loopPrevWORD (String.size str - 1, absIdx, str, tl, currentState, counter)
      | [] => 0
    else
      let
        val chr = String.sub (str, idx)
        val newState = next (currentState, chr)
      in
        if newState = nonBlankAfterSpaceState then
          if counter = 0 then
            absIdx
          else
            (* reset to start state and proceed *)
            loopPrevWORD (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
        else
          loopPrevWORD (idx - 1, absIdx - 1, str, tl, newState, counter)
      end

  fun nextWORD (lineGap: LineGap.t, cursorIdx) =
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
              loopNextWORD (strIdx, cursorIdx, shd, stl, startState, 0)
            else
              (* strIdx is in tl *)
              case stl of
                 stlhd :: stltl =>
                   loopNextWORD (strIdx, cursorIdx, stlhd, stltl, startState, 0)
               | _ => cursorIdx
          end
      | [] => cursorIdx
    end
end
