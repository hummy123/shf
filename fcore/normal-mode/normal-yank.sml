structure NormalYank =
struct
  open AppType
  open DrawMsg
  open MailboxType

  fun finish (app, buffer, yankedString) =
    let
      val msgs = [DRAW (YANK yankedString)]
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.modeAndBuffer (app, buffer, mode, msgs)
    end

  fun yankLeft (app: app_type, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val min = Cursor.vi0 (buffer, cursorIdx)
      val low = Cursor.viH (buffer, cursorIdx, count)

      val low = Int.max (min, low)
      val length = cursorIdx - low
      val str = LineGap.substring (low, length, buffer)
    in
      finish (app, buffer, str)
    end

  fun yankRight (app: app_type, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val endOfLineIdx = Cursor.viDlr (buffer, cursorIdx, 1) + 1
      val high = Cursor.viL (buffer, cursorIdx, count)
      val high = Int.min (high, endOfLineIdx)
      val length = high - cursorIdx

      val buffer = LineGap.goToIdx (high, buffer)
      val str = LineGap.substring (cursorIdx, length, buffer)
    in
      finish (app, buffer, str)
    end

  fun yankLineUp (app: app_type, count) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val cursorLineNumber =
        if Cursor.isNextChrEndOfLine (buffer, cursorIdx) then
          LineGap.idxToLineNumber (cursorIdx + 1, buffer)
        else
          LineGap.idxToLineNumber (cursorIdx, buffer)
      val newCursorLineNumber = Int.max (cursorLineNumber - count, 0)
    in
      if cursorLineNumber = 0 then
        NormalFinish.clearMode app
      else if newCursorLineNumber = 0 then
        let
          val endOfLine = Cursor.viDlr (buffer, cursorIdx, 1)
          val buffer = LineGap.goToIdx (endOfLine, buffer)

          val endOfLine =
            if Cursor.isCursorAtStartOfLine (buffer, endOfLine) then
              endOfLine + 1
            else
              endOfLine + 2

          val buffer = LineGap.goToIdx (endOfLine, buffer)
          val str = LineGap.substring (0, endOfLine, buffer)
        in
          finish (app, buffer, str)
        end
      else
        let
          val endOfLine = Cursor.viDlr (buffer, cursorIdx, 1)
          val buffer = LineGap.goToIdx (endOfLine, buffer)
          val endsOnNewline = Cursor.isCursorAtStartOfLine (buffer, endOfLine)

          val endOfLine = if endsOnNewline then endOfLine else endOfLine + 1

          val newCursorLineNumber =
            if endsOnNewline andalso endOfLine = #textLength buffer - 1 then
              newCursorLineNumber - 1
            else
              newCursorLineNumber
          val buffer = LineGap.goToLine (newCursorLineNumber, buffer)

          val lineIdx = LineGap.lineNumberToIdx (newCursorLineNumber, buffer)
          val length = endOfLine - lineIdx

          val buffer = LineGap.goToIdx (endOfLine, buffer)
          val str = LineGap.substring (lineIdx + 1, length, buffer)
        in
          finish (app, buffer, str)
        end
    end

  fun yankLineDown (app: app_type, count) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val startIdx = Cursor.vi0 (buffer, cursorIdx)
      val buffer = LineGap.goToIdx (startIdx, buffer)

      val startLine =
        if Cursor.isCursorAtStartOfLine (buffer, startIdx) then
          LineGap.idxToLineNumber (startIdx, buffer)
        else
          LineGap.idxToLineNumber (startIdx + 1, buffer)
      val endLine = startLine + count + 1

      val buffer = LineGap.goToLine (endLine, buffer)
      val endLineIdx = LineGap.lineNumberToIdx (endLine, buffer)
      val buffer = LineGap.goToIdx (endLineIdx - 1, buffer)

      (* get "real" endLine by not considering newline after non-newline *)
      val endLine =
        if Cursor.isOnNewlineAfterChr (buffer, endLineIdx - 1) then
          LineGap.idxToLineNumber (endLineIdx - 1, buffer)
        else
          LineGap.idxToLineNumber (endLineIdx, buffer)
    in
      if endLineIdx = #textLength buffer andalso endLine = startLine then
        (* cursor is already on last line so don't yank *)
        NormalFinish.clearMode app
      else
        let
          val endLineIdx = endLineIdx + 1
          val length = endLineIdx - startIdx

          (* perform the actual yank *)
          val buffer = LineGap.goToIdx (endLineIdx, buffer)
          val str = LineGap.substring (startIdx, length, buffer)
        in
          finish (app, buffer, str)
        end
    end

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
    in
      finish (app, buffer, str)
    end

  fun yankToStartOfLine (app: app_type) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.vi0 (buffer, cursorIdx)

      val length = cursorIdx - low
      val str = LineGap.substring (low, length, buffer)
    in
      finish (app, buffer, str)
    end

  fun yankWhenMovingBack (app: app_type, fMove, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = fMove (buffer, cursorIdx, count)

      val length = cursorIdx - low
      val str = LineGap.substring (low, length, buffer)
    in
      finish (app, buffer, str)
    end

  fun yankWhenMovingBackPlusOne (app: app_type, fMove, count) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = fMove (buffer, cursorIdx, count)

      val length = (cursorIdx + 1) - low
      val str = LineGap.substring (low, length, buffer)
    in
      finish (app, buffer, str)
    end

  fun yankWhenMovingForward (app: app_type, fMove, count) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val high = fMove (buffer, cursorIdx, count)
      val high = if high = #textLength buffer then high - 1 else high

      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - cursorIdx
      val str = LineGap.substring (cursorIdx, length, buffer)
    in
      finish (app, buffer, str)
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
        in
          finish (app, buffer, str)
        end
      else if cursorIdx < otherIdx then
        (* yanking forward from cursorIdx *)
        let
          val length = otherIdx - cursorIdx
          val str = LineGap.substring (cursorIdx, length, buffer)
        in
          finish (app, buffer, str)
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
    in
      finish (app, buffer, str)
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
        in
          finish (app, buffer, str)
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
        in
          finish (app, buffer, str)
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
        in
          finish (app, buffer, str)
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
      in
        finish (app, buffer, str)
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
        Cursor.toNextChr (buffer, cursorIdx, {findChr = chr, count = count})
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        let
          val length = newCursorIdx - cursorIdx + 1
          val buffer = LineGap.goToIdx (newCursorIdx, buffer)
          val str = LineGap.substring (cursorIdx, length, buffer)
        in
          finish (app, buffer, str)
        end
    end

  fun yankTillNextChr (app: app_type, count, chr) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val newCursorIdx =
        Cursor.toNextChr (buffer, cursorIdx, {findChr = chr, count = count})
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        let
          val length = newCursorIdx - cursorIdx
          val buffer = LineGap.goToIdx (newCursorIdx, buffer)
          val str = LineGap.substring (cursorIdx, length, buffer)
        in
          finish (app, buffer, str)
        end
    end

  fun yankToPrevChr (app: app_type, count, chr) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val newCursorIdx =
        Cursor.toPrevChr (buffer, cursorIdx, {findChr = chr, count = count})
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        let
          val length = cursorIdx - newCursorIdx
          val str = LineGap.substring (newCursorIdx, length, buffer)
        in
          finish (app, buffer, str)
        end
    end

  fun yankTillPrevChr (app: app_type, count, chr) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val newCursorIdx =
        Cursor.toPrevChr (buffer, cursorIdx, {findChr = chr, count = count})
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        let
          val newCursorIdx = newCursorIdx + 1
          val length = cursorIdx - newCursorIdx
          val str = LineGap.substring (newCursorIdx, length, buffer)
        in
          finish (app, buffer, str)
        end
    end

  fun yankToStart (app: app_type) =
    let
      val {cursorIdx, buffer, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val high = Cursor.viDlrForDelete (buffer, cursorIdx, 1)
      val buffer = LineGap.goToIdx (high, buffer)
      val str = LineGap.substring (0, high, buffer)
    in
      finish (app, buffer, str)
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
    in
      if str = "\n" then NormalFinish.clearMode app
      else finish (app, buffer, str)
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
    in
      if str = "\n" then NormalFinish.clearMode app
      else finish (app, buffer, str)
    end

  fun finishAfterYankInside (app: app_type, low, high, buffer) =
    let
      val length = high - low
      val str = LineGap.substring (low, length, buffer)
    in
      finish (app, buffer, str)
    end

  fun yankInsideChrOpen (app: app_type, chr) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = cursorIdx + 1
      val buffer = LineGap.goToIdx (start, buffer)

      val low = Cursor.toPrevChr (buffer, start, {findChr = chr, count = 1})
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

      val high = Cursor.toNextChr (buffer, start, {findChr = chr, count = 1})
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

      val low = Cursor.toPrevChr (buffer, start, {findChr = chr, count = 1})
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

      val high = Cursor.toNextChr (buffer, start, {findChr = chr, count = 1})
      val buffer = LineGap.goToIdx (high, buffer)
      val low = Cursor.matchPair (buffer, high)
      val high = high + 1
    in
      if low = high then NormalFinish.clearMode app
      else finishAfterYankInside (app, low, high, buffer)
    end
end
