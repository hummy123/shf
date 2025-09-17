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

  structure ViL =
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
                  currentState = notNewlineState
                  orelse currentState = oneNewlineState
              end)

         val fStart = Folder.foldNext
       end)

  val next = ViL.next
end
