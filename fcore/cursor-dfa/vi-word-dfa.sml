structure ViWordDfa =
struct
  val startState: Word8.word = 0w0

  val startAlpha: Word8.word = 0w1
  val startSpace: Word8.word = 0w2
  val startPunct: Word8.word = 0w3

  val alphaToSpace: Word8.word = 0w4
  val punctToSpace: Word8.word = 0w5

  val spaceToAlpha: Word8.word = 0w6
  val spaceToPunct: Word8.word = 0w7

  val startNewline: Word8.word = 0w8
  val newlineToNewline: Word8.word = 0w9
  val chrToNewline: Word8.word = 0w10

  val newlineToAlpha: Word8.word = 0w11
  val newlineToPunct: Word8.word = 0w12

  val alphaToPunct: Word8.word = 0w13
  val punctToAlpha: Word8.word = 0w14

  fun makeStart i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then startAlpha
      else if chr = #"\n" then startNewline
      else if Char.isSpace chr then startSpace
      else startPunct
    end

  fun makeStartAlpha i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then startAlpha
      else if chr = #"\n" then chrToNewline
      else if Char.isSpace chr then alphaToSpace
      else alphaToPunct
    end

  fun makeStartSpace i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then spaceToAlpha
      else if chr = #"\n" then chrToNewline
      else if Char.isSpace chr then startSpace
      else spaceToPunct
    end

  fun makeStartPunct i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then punctToAlpha
      else if chr = #"\n" then chrToNewline
      else if Char.isSpace chr then punctToSpace
      else startPunct
    end

  fun makeStartNewline i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then newlineToAlpha
      else if chr = #"\n" then newlineToNewline
      else if Char.isSpace chr then startSpace
      else newlineToPunct
    end

  val startTable = Vector.tabulate (255, makeStart)

  val startAlphaTable = Vector.tabulate (255, makeStartAlpha)
  val startSpaceTable = Vector.tabulate (255, makeStartSpace)
  val startPunctTable = Vector.tabulate (255, makeStartPunct)

  val alphaToSpaceTable = startSpaceTable
  val punctToSpaceTable = startSpaceTable

  val spaceToAlphaTable = startAlphaTable
  val spaceToPunctTable = startPunctTable

  val newlineTable = Vector.tabulate (255, makeStartNewline)

  val tables =
   #[ startTable

    , startAlphaTable
    , startSpaceTable
    , startPunctTable

    , alphaToSpaceTable
    , punctToSpaceTable

    , spaceToAlphaTable
    , spaceToPunctTable

    , newlineTable
    , newlineTable
    , newlineTable

    , startAlphaTable
    , startPunctTable
    ]

  structure StartOfNextWord =
    MakeNextDfaLoop
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderNext
             (struct
                val startState = startState
                val tables = tables

                fun isFinal currentState =
                  currentState = alphaToPunct orelse currentState = punctToAlpha
                  orelse currentState = spaceToAlpha
                  orelse currentState = spaceToPunct
                  orelse currentState = newlineToNewline
                  orelse currentState = newlineToAlpha
                  orelse currentState = newlineToPunct

                fun finish x = x
              end)

         val fStart = Folder.foldNext
       end)

  structure EndOfPrevWord =
    MakePrevDfaLoop
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderPrev
             (struct
                val startState = startState
                val tables = tables

                fun isFinal currentState =
                  currentState = alphaToPunct orelse currentState = punctToAlpha
                  orelse currentState = spaceToAlpha
                  orelse currentState = spaceToPunct

                fun finish x = x
              end)

         val fStart = Folder.foldPrev
       end)

  structure StartOfCurrentWordFolder =
    MakeCharFolderPrev
      (struct
         val startState = startState
         val tables = tables

         fun isFinal currentState =
           currentState = alphaToSpace orelse currentState = punctToSpace
           orelse currentState = alphaToPunct orelse currentState = punctToAlpha
           orelse currentState = chrToNewline
           orelse currentState = newlineToNewline

         fun finish idx = idx + 1
       end)

  structure StartOfCurrentWord =
    MakePrevDfaLoopMinus1
      (struct
         val startState = startState
         val fStart = StartOfCurrentWordFolder.foldPrev
       end)

  structure StartOfCurrentWordStrict =
    MakePrevDfaLoop
      (struct
         val startState = startState
         val fStart = StartOfCurrentWordFolder.foldPrev
       end)

  fun isFinalForEndOfCurrentWord currentState =
    currentState = alphaToSpace orelse currentState = punctToSpace
    orelse currentState = alphaToPunct orelse currentState = punctToAlpha
    orelse currentState = chrToNewline

  structure EndOfCurrentWordFolder =
    MakeCharFolderNext
      (struct
         val startState = startState
         val tables = tables

         val isFinal = isFinalForEndOfCurrentWord
         fun finish x = x - 1
       end)

  structure EndOfCurrentWord =
    MakeNextDfaLoopPlus1
      (struct
         val startState = startState
         val fStart = EndOfCurrentWordFolder.foldNext
       end)

  structure EndOfCurrentWordStrict =
    MakeNextDfaLoop
      (struct
         val startState = startState
         val fStart = EndOfCurrentWordFolder.foldNext
       end)

  structure EndOfCurrentWordForDelete =
    MakeNextDfaLoopPlus1
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderNext
             (struct
                val startState = startState
                val tables = tables

                val isFinal = isFinalForEndOfCurrentWord
                fun finish x = x
              end)

         val fStart = Folder.foldNext
       end)

  (* w *)
  val startOfNextWord = StartOfNextWord.next
  (* ge *)
  val endOfPrevWord = EndOfPrevWord.prev
  (* b *)
  val startOfCurrentWord = StartOfCurrentWord.prev
  (* e *)
  val endOfCurrentWord = EndOfCurrentWord.next
  val endOfCurrentWordForDelete = EndOfCurrentWordForDelete.next

  (* the meaning of "Strict" and the utility of these two functions
   * is described in vi-WORD-dfa.sml *)
  val startOfCurrentWordStrict = StartOfCurrentWordStrict.prev
  val endOfCurrentWordStrict = EndOfCurrentWordStrict.next
end
