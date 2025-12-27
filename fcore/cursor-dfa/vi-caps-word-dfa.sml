structure ViCapsWordDfa =
struct
  val startState: Word8.word = 0w0
  val startNonBlankState: Word8.word = 0w1
  val startSpaceState: Word8.word = 0w2
  val nonBlankAfterSpaceState: Word8.word = 0w3
  val spaceAfterNonBlankState = 0w4
  val startNewline: Word8.word = 0w5
  val newlineToNewline: Word8.word = 0w6
  val chrToNewline: Word8.word = 0w07

  fun makeStart i =
    let
      val chr = Char.chr i
    in
      if chr = #"\n" then startNewline
      else if Char.isSpace chr then startSpaceState
      else startNonBlankState
    end

  fun makeStartNonBlankState i =
    let
      val chr = Char.chr i
    in
      if chr = #"\n" then chrToNewline
      else if Char.isSpace chr then spaceAfterNonBlankState
      else startNonBlankState
    end

  fun makeStartSpace i =
    let
      val chr = Char.chr i
    in
      if chr = #"\n" then chrToNewline
      else if Char.isSpace chr then startSpaceState
      else nonBlankAfterSpaceState
    end

  fun makeNonBlankAfterSpace i =
    let
      val chr = Char.chr i
    in
      if chr = #"\n" then chrToNewline
      else if Char.isSpace chr then spaceAfterNonBlankState
      else nonBlankAfterSpaceState
    end

  fun makeStartNewline i =
    let
      val chr = Char.chr i
    in
      if chr = #"\n" then newlineToNewline
      else if Char.isSpace chr then startSpaceState
      else nonBlankAfterSpaceState
    end

  val startTable = Vector.tabulate (255, makeStart)
  val startNonBlankTable = Vector.tabulate (255, makeStartNonBlankState)
  val startSpaceTable = Vector.tabulate (255, makeStartSpace)
  val nonBlankAfterSpaceTable = Vector.tabulate (255, makeNonBlankAfterSpace)
  val spaceAfterNonBlankTable = nonBlankAfterSpaceTable
  val newlineTable = Vector.tabulate (255, makeStartNewline)

  val tables =
   #[ startTable
    , startNonBlankTable
    , startSpaceTable
    , nonBlankAfterSpaceTable
    , spaceAfterNonBlankTable
    , newlineTable
    , newlineTable
    , newlineTable
    ]

  structure StartOfNextWORD =
    MakeNextDfaLoop
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderNext
             (struct
                val startState = startState
                val tables = tables

                fun finish x = x

                fun isFinal currentState =
                  currentState = nonBlankAfterSpaceState
                  orelse currentState = newlineToNewline
              end)

         val fStart = Folder.foldNext
       end)

  structure StartOfNextWORDForDelete =
    MakeNextDfaLoop
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderNext
             (struct
                val startState = startState
                val tables = tables

                fun finish x = x

                fun isFinal currentState =
                  currentState = nonBlankAfterSpaceState
                  orelse currentState = chrToNewline
              end)

         val fStart = Folder.foldNext
       end)

  structure EndOfPrevWORD =
    MakePrevDfaLoop
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderPrev
             (struct
                val startState = startState
                val tables = tables

                fun finish x = x

                fun isFinal currentState =
                  currentState = nonBlankAfterSpaceState
              end)

         val fStart = Folder.foldPrev
       end)

  structure StartOfCurrentWORDFolder =
    MakeCharFolderPrev
      (struct
         val startState = startState
         val tables = tables

         fun isFinal currentState =
           currentState = spaceAfterNonBlankState
           orelse currentState = chrToNewline
           orelse currentState = newlineToNewline

         fun finish idx = idx + 1
       end)

  structure StartOfCurrentWORD =
    MakePrevDfaLoopMinus1
      (struct
         val startState = startState
         val fStart = StartOfCurrentWORDFolder.foldPrev
       end)

  structure StartOfNextWORDStrict =
    MakePrevDfaLoop
      (struct
         val startState = startState
         val fStart = StartOfCurrentWORDFolder.foldPrev
       end)

  fun endOfCurrentWORDFolderIsFinal currentState =
    currentState = spaceAfterNonBlankState orelse currentState = chrToNewline

  structure EndOfCurrentWORDFolder =
    MakeCharFolderNext
      (struct
         val startState = startState
         val tables = tables

         val isFinal = endOfCurrentWORDFolderIsFinal

         fun finish idx =
           Int.max (0, idx - 1)
       end)

  structure EndOfCurrentWORD =
    MakeNextDfaLoopPlus1
      (struct
         val startState = startState
         val fStart = EndOfCurrentWORDFolder.foldNext
       end)

  structure EndOfCurrentWORDForDelete =
    MakeNextDfaLoopPlus1
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderNext
             (struct
                val startState = startState
                val tables = tables

                val isFinal = endOfCurrentWORDFolderIsFinal

                fun finish idx = idx
              end)

         val fStart = Folder.foldNext
       end)

  structure EndOfCurrentWORDStrict =
    MakeNextDfaLoop
      (struct
         val startState = startState
         val fStart = EndOfCurrentWORDFolder.foldNext
       end)

  (* W *)
  val startOfNextWORD = StartOfNextWORD.next
  val startOfNextWORDForDelete = StartOfNextWORDForDelete.next
  (* gE *)
  val endOfPrevWORD = EndOfPrevWORD.prev
  (* B *)
  val startOfCurrentWORD = StartOfCurrentWORD.prev
  (* E *)
  val endOfCurrentWORD = EndOfCurrentWORD.next
  val endOfCurrentWORDForDelete = EndOfCurrentWORDForDelete.next

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
