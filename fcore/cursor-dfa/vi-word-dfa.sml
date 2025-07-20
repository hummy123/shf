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

  val alphaToPunct: Word8.word = 0w8
  val punctToAlpha: Word8.word = 0w9

  fun makeStart i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then startAlpha
      else if Char.isSpace chr then startSpace
      else startPunct
    end

  fun makeStartAlpha i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then startAlpha
      else if Char.isSpace chr then alphaToSpace
      else alphaToPunct
    end

  fun makeStartSpace i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then spaceToAlpha
      else if Char.isSpace chr then startSpace
      else spaceToPunct
    end

  fun makeStartPunct i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then punctToAlpha
      else if Char.isSpace chr then punctToSpace
      else startPunct
    end

  val startTable = Vector.tabulate (255, makeStart)

  val startAlphaTable = Vector.tabulate (255, makeStartAlpha)
  val startSpaceTable = Vector.tabulate (255, makeStartSpace)
  val startPunctTable = Vector.tabulate (255, makeStartPunct)

  val alphaToSpaceTable = startSpaceTable
  val punctToSpaceTable = startSpaceTable

  val spaceToAlphaTable = startAlphaTable
  val spaceToPunctTable = startPunctTable

  val tables =
   #[ startTable

    , startAlphaTable
    , startSpaceTable
    , startPunctTable

    , alphaToSpaceTable
    , punctToSpaceTable

    , spaceToAlphaTable
    , spaceToPunctTable
    ]

  fun next (currentState, chr) =
    let
      val currentState = Word8.toInt currentState
      val currentTable = Vector.sub (tables, currentState)
      val charIdx = Char.ord chr
    in
      Vector.sub (currentTable, charIdx)
    end

  structure StartOfNextWord =
    MakeNextDfaLoop
      (struct
         val startState = startState

         fun fStart (idx, absIdx, str, tl, currentState, counter) =
           if idx = String.size str then
             case tl of
               str :: tl => fStart (0, absIdx, str, tl, currentState, counter)
             | [] => Int.max (0, absIdx - 2)
           else
             let
               val chr = String.sub (str, idx)
               val newState = next (currentState, chr)
             in
               if
                 newState = alphaToPunct orelse newState = punctToAlpha
                 orelse newState = spaceToAlpha orelse newState = spaceToPunct
               then
                 if counter - 1 = 0 then
                   absIdx
                 else
                   (* reset state *)
                   fStart
                     (idx + 1, absIdx + 1, str, tl, startState, counter - 1)
               else
                 fStart (idx + 1, absIdx + 1, str, tl, newState, counter)
             end
       end)

  structure EndOfPrevWord =
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
               if
                 newState = alphaToPunct orelse newState = punctToAlpha
                 orelse newState = spaceToAlpha orelse newState = spaceToPunct
               then
                 if counter - 1 = 0 then
                   absIdx
                 else
                   fStart
                     (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
               else
                 fStart (idx - 1, absIdx - 1, str, tl, newState, counter)
             end
       end)

  structure StartOfCurrentWord =
    MakePrevDfaLoopMinus1
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
                         handle _ => (print "156\n"; raise Empty)
               val newState =
                 next (currentState, chr)
                 handle _ =>
                   ( print ("158: " ^ Word8.toString currentState ^ "\n")
                   ; raise Empty
                   )
             in
               if
                 newState = alphaToSpace orelse newState = punctToSpace
                 orelse newState = alphaToPunct orelse newState = punctToAlpha
               then
                 if counter - 1 = 0 then
                   absIdx + 1
                 else
                   fStart
                     (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
               else
                 fStart (idx - 1, absIdx - 1, str, tl, newState, counter)
             end
       end)

  (* w *)
  val startOfNextWord = StartOfNextWord.next
  (* ge *)
  val endOfPrevWord = EndOfPrevWord.prev
  (* b *)
  val startOfCurrentWord = StartOfCurrentWord.prev
end
