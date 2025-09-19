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
  fun finish (app: AppType.app_type, buffer, cursorIdx) =
    let
      val {searchList, bufferModifyTime, ...} = app
    in
      NormalFinish.buildTextAndClear
        (app, buffer, cursorIdx, searchList, [], bufferModifyTime)
    end

  fun helpMove (app: AppType.app_type, buffer, cursorIdx, count) =
    if count = 0 then
      finish (app, buffer, cursorIdx)
    else
      (* move LineGap to cursorIdx, which is necessary for finding newCursorIdx *)
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val textLength = #textLength buffer
        val newCursorIdx = Fn.fMove (buffer, cursorIdx)
      in
        if newCursorIdx >= textLength - 2 then
          let val newCursorIdx = Int.max (textLength - 2, 0)
          in finish (app, buffer, newCursorIdx)
          end
        else
          helpMove (app, buffer, newCursorIdx, count - 1)
      end

  fun move (app: AppType.app_type, count) =
    let val {cursorIdx, buffer, ...} = app
    in helpMove (app, buffer, cursorIdx, count)
    end
end

structure MoveToStartOfLine = MakeMove (struct val fMove = Cursor.vi0 end)

signature DFA_MOVE =
sig
  val fMove: LineGap.t * int * int -> int
end

signature MAKE_DFA_MOVE =
sig
  val move: AppType.app_type * int -> AppType.app_type
end

functor MakeDfaMove(Fn: DFA_MOVE): MAKE_DFA_MOVE =
struct
  fun move (app: AppType.app_type, count) : AppType.app_type =
    let
      val {buffer, cursorIdx, searchList, bufferModifyTime, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Fn.fMove (buffer, cursorIdx, count)

      val textLength = #textLength buffer
      val cursorIdx =
        if cursorIdx >= textLength - 2 then Int.max (textLength - 2, 0)
        else cursorIdx
    in
      NormalFinish.buildTextAndClear
        (app, buffer, cursorIdx, searchList, [], bufferModifyTime)
    end
end

structure MoveViH = MakeDfaMove (struct val fMove = Cursor.viH end)
structure MoveViL = MakeDfaMove (struct val fMove = Cursor.viL end)

structure MoveToNextWord = MakeDfaMove (struct val fMove = Cursor.nextWord end)
structure MoveToNextWORD = MakeDfaMove (struct val fMove = Cursor.nextWORD end)

structure MoveToEndOfWord =
  MakeDfaMove (struct val fMove = Cursor.endOfWord end)
structure MoveToEndOfWORD =
  MakeDfaMove (struct val fMove = Cursor.endOfWORD end)

structure MoveToPrevWord = MakeDfaMove (struct val fMove = Cursor.prevWord end)
structure MoveToPrevWORD = MakeDfaMove (struct val fMove = Cursor.prevWORD end)

structure MoveToEndOfPrevWord =
  MakeDfaMove (struct val fMove = Cursor.endOfPrevWord end)
structure MoveToEndOfPrevWORD =
  MakeDfaMove (struct val fMove = Cursor.endOfPrevWORD end)

structure MoveToEndOfLine = MakeDfaMove (struct val fMove = Cursor.viDlr end)
