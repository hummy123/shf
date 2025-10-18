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

  fun yankWhenMovingBackPlusOne (app: app_type, fMove, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = fMove (buffer, cursorIdx, count)

      val length = (cursorIdx + 1) - low
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

  fun yankToNextMatch (app: app_type, count) =
    let
      val {cursorIdx, searchList, buffer, ...} = app
      val high = PersistentVector.nextMatch (cursorIdx, searchList, count)
    in
      if high = ~1 orelse high <= cursorIdx then
        NormalFinish.clearMode app
      else
        let
          val length = high - cursorIdx
          val buffer = LineGap.goToIdx (high, buffer)

          val str = LineGap.substring (cursorIdx, length, buffer)
          val msg = YANK str
          val mode = NORMAL_MODE ""
        in
          NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
        end
    end

  fun yankToPrevMatch (app: app_type, count) =
    let
      val {cursorIdx, searchList, buffer, ...} = app
      val low = PersistentVector.prevMatch (cursorIdx, searchList, count)
    in
      if low = ~1 orelse low >= cursorIdx then
        NormalFinish.clearMode app
      else
        let
          val length = cursorIdx - low

          val str = LineGap.substring (low, length, buffer)
          val msg = YANK str
          val mode = NORMAL_MODE ""
        in
          NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
        end
    end

  fun helpYankToChr
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, fInc, chr) =
    if count = 0 then
      let
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val length = high - low

        val buffer = LineGap.goToIdx (high, buffer)
        val str = LineGap.substring (low, length, buffer)
        val msg = YANK str
        val mode = NORMAL_MODE ""
      in
        NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
      end
    else
      let
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx, chr)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
        val newOtherIdx = fInc (newOtherIdx, 1)
      in
        helpYankToChr
          (app, buffer, cursorIdx, newOtherIdx, newCount, fMove, fInc, chr)
      end

  fun yankToNextChr (app: app_type, count, chr) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val newCursorIdx =
        Cursor.toNextChrNew (buffer, cursorIdx, {findChr = chr, count = count})
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        let
          val length = newCursorIdx - cursorIdx + 1
          val buffer = LineGap.goToIdx (newCursorIdx, buffer)
          val str = LineGap.substring (cursorIdx, length, buffer)
          val msg = YANK str
          val mode = NORMAL_MODE ""
        in
          NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
        end
    end

  fun yankToChr (app: app_type, count, fMove, fInc, chr) =
    helpYankToChr
      ( app
      , #buffer app
      , #cursorIdx app
      , #cursorIdx app
      , count
      , fMove
      , fInc
      , chr
      )

  fun yankToStart (app: app_type) =
    let
      val {cursorIdx, buffer, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val high = Cursor.viDlrForDelete (buffer, cursorIdx, 1)
      val buffer = LineGap.goToIdx (high, buffer)

      val str = LineGap.substring (0, high, buffer)
      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankInsideWord (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.prevWordStrict (buffer, cursorIdx, 1)
      val high = Cursor.endOfWordStrict (buffer, cursorIdx, 1)

      val high = high + 1
      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low

      val str = LineGap.substring (low, length, buffer)
      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      if str = "\n" then NormalFinish.clearMode app
      else NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankInsideWORD (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.prevWORDStrict (buffer, cursorIdx, 1)
      val high = Cursor.endOfWORDStrict (buffer, cursorIdx, 1)

      val high = high + 1
      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low

      val str = LineGap.substring (low, length, buffer)
      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      if str = "\n" then NormalFinish.clearMode app
      else NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun finishAfterYankInside (app: app_type, low, high, buffer) =
    let
      val length = high - low
      val str = LineGap.substring (low, length, buffer)
      val msg = YANK str
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, [DRAW msg])
    end

  fun yankInsideChrOpen (app: app_type, chr) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = cursorIdx + 1
      val buffer = LineGap.goToIdx (start, buffer)

      val low = Cursor.toPrevChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (low, buffer)
      val high = Cursor.matchPair (buffer, low)
      val buffer = LineGap.goToIdx (high, buffer)
      val low = low + 1
    in
      if low = high then NormalFinish.clearMode app
      else finishAfterYankInside (app, low, high, buffer)
    end

  fun yankInsideChrClose (app: app_type, chr) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = Int.max (cursorIdx - 1, 0)
      val buffer = LineGap.goToIdx (start, buffer)

      val high = Cursor.toNextChrNew (buffer, start, {findChr = chr, count = 1})
      val buffer = LineGap.goToIdx (high, buffer)
      val low = Cursor.matchPair (buffer, high) + 1
    in
      if low = high then NormalFinish.clearMode app
      else finishAfterYankInside (app, low, high, buffer)
    end

  fun yankAroundChrOpen (app: app_type, chr) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = cursorIdx + 1
      val buffer = LineGap.goToIdx (start, buffer)

      val low = Cursor.toPrevChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (low, buffer)
      val high = Cursor.matchPair (buffer, low) + 1
      val buffer = LineGap.goToIdx (high, buffer)
      val low = low
    in
      if low = high then NormalFinish.clearMode app
      else finishAfterYankInside (app, low, high, buffer)
    end

  fun yankAroundChrClose (app: app_type, chr) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = Int.max (cursorIdx - 1, 0)
      val buffer = LineGap.goToIdx (start, buffer)

      val high = Cursor.toNextChrNew (buffer, start, {findChr = chr, count = 1})
      val buffer = LineGap.goToIdx (high, buffer)
      val low = Cursor.matchPair (buffer, high)
      val high = high + 1
    in
      if low = high then NormalFinish.clearMode app
      else finishAfterYankInside (app, low, high, buffer)
    end
end
