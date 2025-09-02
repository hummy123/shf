structure NormalYank =
struct
  open AppType
  open DrawMsg
  open MailboxType

  fun yankLine (app: app_type, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.vi0 (buffer, cursorIdx)

      val buffer = LineGap.goToIdx (low, buffer)
      val high = Cursor.viDlrForDelete (buffer, low, count)

      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low
      val str = LineGap.substring (low, length, buffer)

      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankToStartOfLine (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.vi0 (buffer, cursorIdx)

      val length = cursorIdx - low
      val str = LineGap.substring (low, length, buffer)

      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankWhenMovingBack (app: app_type, fMove, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = fMove (buffer, cursorIdx, count)

      val length = cursorIdx - low
      val str = LineGap.substring (low, length, buffer)

      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankWhenMovingForward (app: app_type, fMove, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val high = fMove (buffer, cursorIdx, count)

      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - cursorIdx
      val str = LineGap.substring (cursorIdx, length, buffer)

      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankToFirstNonSpaceChr (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = Cursor.vi0 (buffer, cursorIdx)

      val buffer = LineGap.goToIdx (otherIdx, buffer)
      val otherIdx = Cursor.firstNonSpaceChr (buffer, otherIdx)
    in
      if cursorIdx > otherIdx then
        (* yanking backwards from cursorIdx *)
        let
          val () = print "272\n"
          val length = cursorIdx - otherIdx + 1
          val buffer = LineGap.goToIdx (otherIdx, buffer)

          val str = LineGap.substring (otherIdx, length, buffer)
          val msg = YANK str
          val mode = NORMAL_MODE ""
        in
          NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
        end
      else if cursorIdx < otherIdx then
        (* yanking forward from cursorIdx *)
        let
          val length = otherIdx - cursorIdx
          val str = LineGap.substring (cursorIdx, length, buffer)
          val msg = YANK str
          val mode = NORMAL_MODE ""
        in
          NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
        end
      else
        NormalFinish.clearMode app
    end

  fun yankToEndOfText (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToEnd buffer
      val {rightStrings, idx, ...} = buffer
      val finishIdx = Int.max (0, idx - 1)

      val length = finishIdx - cursorIdx
      val str = LineGap.substring (cursorIdx, length, buffer)

      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankToMatchingPair (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app
      val otherIdx = Cursor.matchPair (buffer, cursorIdx)
    in
      if cursorIdx = otherIdx then
        NormalFinish.clearMode app
      else
        let
          val low = Int.min (cursorIdx, otherIdx)
          val high = Int.max (cursorIdx, otherIdx)
          val length = high - low + 1

          val buffer = LineGap.goToIdx (high, buffer)
          val str = LineGap.substring (low, length, buffer)

          val msg = YANK str
          val mode = NORMAL_MODE ""
        in
          NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
        end
    end
end
