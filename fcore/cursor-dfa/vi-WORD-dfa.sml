structure ViWORDDfa =
struct
  val startState: Word8.word = 0w0
  val startNonBlankState: Word8.word = 0w1
  val startSpaceState: Word8.word = 0w2
  val nonBlankAfterSpaceState: Word8.word = 0w4

  fun makeStart i =
    let val chr = Char.chr i
    in if Char.isSpace chr then startSpaceState else startNonBlankState
    end

  fun makeStartNonBlankState i =
    let val chr = Char.chr i
    in if Char.isSpace chr then startSpaceState else startNonBlankState
    end

  fun makeStartSpace i =
    let val chr = Char.chr i
    in if Char.isSpace chr then startSpaceState else nonBlankAfterSpaceState
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

  structure TraverseWORD =
    MakeDfaLoop
      (struct
         val startState = startState

         fun fNext (idx, absIdx, str, tl, currentState, counter) =
           if idx = String.size str then
             case tl of
               str :: tl => fNext (0, absIdx, str, tl, currentState, counter)
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
                   fNext (idx + 1, absIdx + 1, str, tl, startState, counter - 1)
               else
                 fNext (idx + 1, absIdx + 1, str, tl, newState, counter)
             end

         fun fPrev (idx, absIdx, str, tl, currentState, counter) =
           if idx < 0 then
             case tl of
               str :: tl =>
                 fPrev
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
                   fPrev (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
               else
                 fPrev (idx - 1, absIdx - 1, str, tl, newState, counter)
             end
       end)

  val next = TraverseWORD.next
  val prev = TraverseWORD.prev
end
