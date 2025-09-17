structure ViLDfa =
struct
  val startState: Word8.word = 0w0
  val notNewlineState: Word8.word = 0w1
  val oneNewlineState: Word8.word = 0w2
  val twoNewlineState: Word8.word = 0w3

  fun makeStart i =
    if Char.chr i = #"\n" then oneNewlineState else notNewlineState

  fun makeOneNewline i =
    if Char.chr i = #"\n" then twoNewlineState else notNewlineState

  val startTable = Vector.tabulate (255, makeStart)
  val notNewlineTable = startTable
  val oneNewlineTable = Vector.tabulate (255, makeOneNewline)
  val twoNewLineTable = startTable

  val tables = #[startTable, notNewlineTable, oneNewlineTable, twoNewLineTable]

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
               if newState = twoNewlineState then
                 if counter - 1 = ~1 then
                   absIdx - 1
                 else
                   loop (idx + 1, absIdx + 1, str, tl, startState, counter - 1)
               else if newState = notNewlineState then
                 if counter - 1 = ~1 then
                   absIdx
                 else
                   loop (idx + 1, absIdx + 1, str, tl, startState, counter - 1)
               else
                 loop (idx + 1, absIdx + 1, str, tl, newState, counter)
             end

         val fStart = loop
       end)

  val next = ViL.next
end
