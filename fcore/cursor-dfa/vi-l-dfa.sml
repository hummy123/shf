structure ViLDfa =
struct
  val startState: Word8.word = 0w0
  val newlineState: Word8.word = 0w1
  val chrState: Word8.word = 0w2
  val newlineAfterCHrState: Word8.word = 0w3

  fun makeStart i =
    if Char.chr i = #"\n" then newlineState else chrState

  fun makeChr i =
    if Char.chr i = #"\n" then newlineAfterCHrState else chrState

  val startTable = Vector.tabulate (255, makeStart)
  val newlineTable = startTable
  val chrTable = Vector.tabulate (255, makeChr)
  val newlineAfterCHrTable = startTable

  val tables = #[startTable, newlineTable, chrTable, newlineAfterCHrTable]

  fun next (currentState, chr) =
    let val table = Vector.sub (tables, Word8.toInt currentState)
    in Vector.sub (table, Char.ord chr)
    end

  structure ViL =
    MakeNextDfaLoop
      (struct
         val startState = startState

         fun loop (idx, absIdx, str, tl, currentState, counter) =
           if idx = String.size str then
             case tl of
               str :: tl => loop (0, absIdx, str, tl, currentState, counter)
             | [] => absIdx
           else
             let
               val chr = String.sub (str, idx)
               val newState = next (currentState, chr)
             in
               if newState = newlineAfterCHrState then
                 loop (idx + 1, absIdx + 1, str, tl, newState, counter)
               else if counter - 1 = ~1 then
                 absIdx
               else
                 loop (idx + 1, absIdx + 1, str, tl, newState, counter - 1)
             end

         val fStart = loop
       end)

  val next = ViL.next
end
