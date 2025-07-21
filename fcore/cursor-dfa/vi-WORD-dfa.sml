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

  fun startOfCurrentWORD (idx, absIdx, str, tl, currentState, counter) =
    if idx < 0 then
      case tl of
        str :: tl =>
          startOfCurrentWORD
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
            startOfCurrentWORD
              (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
        else
          startOfCurrentWORD (idx - 1, absIdx - 1, str, tl, newState, counter)
      end

  fun endOfCurrentWORD (idx, absIdx, str, tl, currentState, counter) =
    if idx = String.size str then
      case tl of
        str :: tl =>
          endOfCurrentWORD (0, absIdx, str, tl, currentState, counter)
      | [] => Int.max (0, absIdx - 2)
    else
      let
        val chr = String.sub (str, idx)
        val newState = next (currentState, chr)
      in
        if newState = spaceAfterNonBlankState then
          if counter - 1 = 0 then
            Int.max (0, absIdx - 1)
          else
            endOfCurrentWORD
              (idx + 1, absIdx + 1, str, tl, startState, counter - 1)
        else
          endOfCurrentWORD (idx + 1, absIdx + 1, str, tl, newState, counter)
      end

  structure StartOfCurrentWORD =
    MakePrevDfaLoopMinus1
      (struct val startState = startState val fStart = startOfCurrentWORD end)

  structure EndOfCurrentWORD =
    MakeNextDfaLoopPlus1
      (struct val startState = startState val fStart = endOfCurrentWORD end)

  structure StartOfNextWORDStrict =
    MakePrevDfaLoop
      (struct val startState = startState val fStart = startOfCurrentWORD end)

  structure EndOfCurrentWORDStrict =
    MakeNextDfaLoop
      (struct val startState = startState val fStart = endOfCurrentWORD end)

  (* W *)
  val startOfNextWORD = StartOfNextWORD.next
  (* gE *)
  val endOfPrevWORD = EndOfPrevWORD.prev
  (* B *)
  val startOfCurrentWORD = StartOfCurrentWORD.prev
  (* E *)
  val endOfCurrentWORD = EndOfCurrentWORD.next

  (* functions to strictly get the start and end of the current word. 
   * Problem: We want to support Vi motions like viW (selects a single word),
   * as well as ciW (change one WORD) and diW (delete one WORD).
   *
   * The 'startOfCurrentWORD' and 'endOfCurrentWORD' functions do this
   * (representing the vi 'B' and 'E' commands respectively),
   * except that 'B' goes to the previous WORD if the cursor is on the first
   * character of the current WORD, and 'E' goes to the next WORD if the cursor
   * is on the last character of the current WORD.
   *
   * What is meant by "strict" is that these below functions always stay
   * within the current WORD, not making the two exceptions mentioned above.
   *)
  val startOfCurrentWORDStrict = StartOfNextWORDStrict.prev
  val endOfCurrentWORDStrict = EndOfCurrentWORDStrict.next
end
