signature MOVE =
sig
  val fMove: LineGap.t * int -> int
end

signature MAKE_MOVE =
sig
  val move: AppType.app_type * int -> AppType.app_type
end

functor MakeMove(Fn: MOVE): MAKE_MOVE =
struct
  fun helpMove (app: AppType.app_type, buffer, cursorIdx, count) =
    if count = 0 then
      Finish.buildTextAndClear (app, buffer, cursorIdx, #searchList app)
    else
      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val newCursorIdx = Fn.fMove (buffer, cursorIdx)
        val newCursorIdx = Cursor.clipIdx (buffer, newCursorIdx)
        val newCount =
          (* it's possible to loop a very high number like 5432131
           * which will take a long time because of the high number of loops
           * regardless of the data structure used.
           * If this happens, and the newCursorIdx is the same as the old one,
           * then skip to end of loop by going to base case. *)
          if cursorIdx = newCursorIdx then 0
          else count - 1
      in
        helpMove (app, buffer, newCursorIdx, newCount)
      end

  fun move (app: AppType.app_type, count) =
    let val {cursorIdx, buffer, ...} = app
    in helpMove (app, buffer, cursorIdx, count)
    end
end

structure MoveViH = MakeMove (struct val fMove = Cursor.viH end)
structure MoveViJ = MakeMove (struct val fMove = Cursor.viJ end)
structure MoveViK = MakeMove (struct val fMove = Cursor.viK end)
structure MoveViL = MakeMove (struct val fMove = Cursor.viL end)

structure MoveToNextWord = MakeMove (struct val fMove = Cursor.nextWord end)
structure MoveToNextWORD = MakeMove (struct val fMove = Cursor.nextWORD end)

structure MoveToEndOfWord = MakeMove (struct val fMove = Cursor.endOfWord end)
structure MoveToEndOfWORD = MakeMove (struct val fMove = Cursor.endOfWORD end)

structure MoveToPrevWord = MakeMove (struct val fMove = Cursor.prevWord end)
structure MoveToPrevWORD = MakeMove (struct val fMove = Cursor.prevWORD end)

structure MoveToEndOfPrevWord =
  MakeMove (struct val fMove = Cursor.endOfPrevWord end)
structure MoveToEndOfPrevWORD =
  MakeMove (struct val fMove = Cursor.endOfPrevWORD end)

structure MoveToStartOfLine = MakeMove (struct val fMove = Cursor.vi0 end)
structure MoveToEndOfLine = MakeMove (struct val fMove = Cursor.viDlr end)
