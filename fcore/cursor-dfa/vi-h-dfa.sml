structure ViHDfa =
struct
  val startState: Word8.word = 0w0
  val oneNewlineState: Word8.word = 0w1
  val twoNewlineState: Word8.word = 0w2
  val chrState: Word8.word = 0w3
  val chrBeforeNewlieState: Word8.word = 0w4

  fun makeStart i =
    if Char.chr i = #"\n" then oneNewlineState else chrState

  fun makeOneNewline i =
    if Char.chr i = #"\n" then twoNewlineState else chrBeforeNewlieState

  val startTable = Vector.tabulate (255, makeStart)
  val oneNewlineTable = Vector.tabulate (255, makeOneNewline)
  val twoNewlineTable = oneNewlineTable
  val chrTable = startTable
  val chrBeforeNewlieTable = startTable

  val tables =
   #[ startTable
    , oneNewlineTable
    , twoNewlineTable
    , chrTable
    , chrBeforeNewlieTable
    ]

  fun next (currentState, chr) =
    let val table = Vector.sub (tables, Word8.toInt currentState)
    in Vector.sub (table, Char.ord chr)
    end

  structure ViH =
    MakePrevDfaLoop
      (struct
         val startState = startState

         fun loop (idx, absIdx, str, tl, currentState, counter) =
           if idx < 0 then
             case tl of
               str :: tl =>
                 loop
                   (String.size str - 1, absIdx, str, tl, currentState, counter)
             | [] => 0
           else
             let
               val chr = String.sub (str, idx)
               val newState = next (currentState, chr)
             in
               if newState = chrBeforeNewlieState orelse newState = chrState then
                 if counter - 1 = ~1 then
                   absIdx
                 else
                   loop (idx - 1, absIdx - 1, str, tl, startState, counter - 1)
               else if newState = twoNewlineState then
                 if counter - 1 = ~1 then
                   absIdx + 1
                 else
                   loop
                     ( idx - 1
                     , absIdx - 1
                     , str
                     , tl
                     , oneNewlineState
                     , counter - 1
                     )
               else
                 loop (idx - 1, absIdx - 1, str, tl, newState, counter)
             end

         val fStart = loop
       end)

  val prev = ViH.prev
end
