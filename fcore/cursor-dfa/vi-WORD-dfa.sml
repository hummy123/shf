structure ViWORDDfa =
struct
  val startState: Word8.word = 0w0
  val startNonBlankState: Word8.word = 0w1
  val startSpaceState: Word8.word = 0w2
  val nonBlankAfterSpaceState: Word8.word = 0w3
  val spaceAfterNonBlankState = 0w4

  fun makeStart i =
    let val chr = Char.chr i
    in if Char.isSpace chr then startSpaceState else startNonBlankState
    end

  fun makeStartNonBlankState i =
    let val chr = Char.chr i
    in if Char.isSpace chr then spaceAfterNonBlankState else startNonBlankState
    end

  fun makeStartSpace i =
    let val chr = Char.chr i
    in if Char.isSpace chr then startSpaceState else nonBlankAfterSpaceState
    end

  fun makeNonBlankAfterSpace i =
    let
      val chr = Char.chr i
    in
      if Char.isSpace chr then spaceAfterNonBlankState
      else nonBlankAfterSpaceState
    end

  val startTable = Vector.tabulate (255, makeStart)
  val startNonBlankTable = Vector.tabulate (255, makeStartNonBlankState)
  val startSpaceTable = Vector.tabulate (255, makeStartSpace)
  val nonBlankAfterSpaceTable = Vector.tabulate (255, makeNonBlankAfterSpace)
  val spaceAfterNonBlankTable = nonBlankAfterSpaceTable

  val tables =
   #[ startTable
    , startNonBlankTable
    , startSpaceTable
    , nonBlankAfterSpaceTable
    , spaceAfterNonBlankTable
    ]

  fun next (currentState, currentChar) =
    let
      val currentState = Word8.toInt currentState
      val currentTable = Vector.sub (tables, currentState)
      val charIdx = Char.ord currentChar
    in
      Vector.sub (currentTable, charIdx)
    end

  structure StartOfNextWORD =
    MakeNextDfaLoop
      (struct
         val startState = startState

         fun fStart (idx, absIdx, str, tl, currentState, counter) =
           if idx = String.size str then
             case tl of
               str :: tl => fStart (0, absIdx, str, tl, currentState, counter)
             | [] => Int.max (absIdx - 2, 0)
           else
             let
               val chr = String.sub (str, idx)
               val newState = next (currentState, chr)
             in
               if newState = nonBlankAfterSpaceState then
                 if counter - 1 = 0 then
                   absIdx
                 else
                   (* new loop, so reset to start state and proceed *)
                   fStart
                     (idx + 1, absIdx + 1, str, tl, startState, counter - 1)
               else
                 fStart (idx + 1, absIdx + 1, str, tl, newState, counter)
             end
       end)

  structure EndOfPrevWORD =
    MakePrevDfaLoop
      (struct
         val startState = startState

         fun fStart (idx, absIdx, str, tl, currentState, counter) =
           if idx < 0 then
             case tl of
               str :: tl =>
                 fStart
                   (String.size str - 1, absIdx, str, tl, currentState, counter)
             | [] => 0
           else
             let
               val chr = String.sub (str, idx)
               val newState = next (currentState, chr)
             in
               if newState = nonBlankAfterSpaceState then
                 if counter - 1 = 0 then
                   absIdx
                 else
                   (* reset to start state and proceed *)
                   fStart
                     (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
               else
                 fStart (idx - 1, absIdx - 1, str, tl, newState, counter)
             end
       end)

  structure StartOfCurrentWORD =
    MakePrevDfaLoop
      (struct
         val startState = startState

         fun fStart (idx, absIdx, str, tl, currentState, counter) =
           if idx < 0 then
             case tl of
               str :: tl =>
                 fStart
                   (String.size str - 1, absIdx, str, tl, currentState, counter)
             | [] => 0
           else
             let
               val chr = String.sub (str, idx)
               val newState = next (currentState, chr)
             in
               if newState = spaceAfterNonBlankState then
                 if counter - 1 = 0 then
                   absIdx + 1
                 else
                   fStart
                     (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
               else
                 fStart (idx - 1, absIdx - 1, str, tl, newState, counter)
             end
       end)

  val startOfNextWORD = StartOfNextWORD.next
  val endOfPrevWORD = EndOfPrevWORD.prev

  fun startOfCurrentWORD (lineGap: LineGap.t, cursorIdx) =
    let val lineGap = LineGap.goToIdx (cursorIdx - 1, lineGap)
    in StartOfCurrentWORD.prev (lineGap, cursorIdx - 1)
    end
end
