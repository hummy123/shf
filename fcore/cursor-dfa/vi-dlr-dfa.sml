structure ViDlrDfa =
struct
  val startState: Word8.word = 0w0
  val newlineState: Word8.word = 0w1
  val notNewlineState = 0w2

  fun makeStart i =
    if Char.chr i = #"\n" then newlineState else notNewlineState

  val startTable = Vector.tabulate (255, makeStart)
  val newlineTable = startTable
  val notNewlineTable = startTable

  val tables = #[startTable, newlineTable, notNewlineTable]

  fun isFinal currentState = currentState = newlineState

  structure ViDlr =
    MakeNextDfaLoop
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderNext
             (struct
                val startState = startState
                val tables = tables

                fun finish x = x - 1
                val isFinal = isFinal
              end)

         fun fStart (idx, absIdx, str, tl, currentState, counter) =
           if String.sub (str, idx) = #"\n" then
             if counter = 1 then
               absIdx
             else
               Folder.foldNext
                 (idx + 1, absIdx + 1, str, tl, currentState, counter - 1)
           else
             Folder.foldNext (idx, absIdx, str, tl, currentState, counter)
       end)

  structure ViDlrForDelete =
    MakeNextDfaLoop
      (struct
         val startState = startState

         structure Folder =
           MakeCharFolderNext
             (struct
                val startState = startState
                val tables = tables

                fun finish x = x + 1
                val isFinal = isFinal
              end)

         val fStart = Folder.foldNext
       end)

  val next = ViDlr.next
  val nextForDelete = ViDlrForDelete.next
end
