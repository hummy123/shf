structure NormalYank =
struct
  open AppType
  open DrawMsg
  open MailboxType

  fun yankLine (app: app_type, count) =
    let
      open DrawMsg
      open MailboxType

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
      open DrawMsg
      open MailboxType

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
      open DrawMsg
      open MailboxType

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
      open DrawMsg
      open MailboxType

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
end
