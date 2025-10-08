signature MAKE_NORMAL_DELETE =
sig
  val initMsgs: int * int * LineGap.t -> MailboxType.t list
end

functor MakeNormalDelete(Fn: MAKE_NORMAL_DELETE) =
struct
  open AppType
  open DrawMsg
  open MailboxType

  fun finishAfterDeletingBuffer (app: app_type, low, buffer, time, msgs) =
    let
      val buffer = LineGap.goToIdx (low, buffer)
      val buffer = LineGap.goToStart buffer
      val msgs = SEARCH (buffer, #dfa app, time) :: msgs

      val buffer = LineGap.goToIdx (low - 1111, buffer)
      val (buffer, searchList) =
        SearchList.buildRange (buffer, low + 1111, #dfa app)

      val buffer = LineGap.goToIdx (low, buffer)
    in
      NormalFinish.buildTextAndClear (app, buffer, low, searchList, msgs, time)
    end

  fun deleteAndFinish (app: app_type, low, length, buffer, time) =
    let
      val buffer = LineGap.goToIdx (low + length, buffer)
      val initialMsg = Fn.initMsgs (low, length, buffer)
      val buffer = LineGap.delete (low, length, buffer)
    in
      finishAfterDeletingBuffer (app, low, buffer, time, initialMsg)
    end

  fun moveCursorAfterDeletingLines (app, buffer, time, initialMsg, startIdx) =
    let
      val newEndIdx = #textLength buffer - 1
    in
      if newEndIdx < 0 then
        (* deleted whole file; add newline to the end *)
        let val buffer = LineGap.append ("\n", buffer)
        in finishAfterDeletingBuffer (app, 0, buffer, time, initialMsg)
        end
      else if newEndIdx = 0 then
        (* there is only one char left in the file *)
        finishAfterDeletingBuffer (app, 0, buffer, time, initialMsg)
      else if startIdx >= newEndIdx then
        (* deleted the last part of the file such that the cursor's idx
         * now refers to an index that no longer exists.
         * Have to move cursor to the last line of the file. *)
        let
          val buffer = LineGap.goToIdx (newEndIdx, buffer)
        in
          if Cursor.isOnNewlineAfterChr (buffer, newEndIdx) then
            let
              val buffer = LineGap.goToIdx (newEndIdx - 1, buffer)
              val newCursorIdx = Cursor.vi0 (buffer, newEndIdx - 1)
            in
              finishAfterDeletingBuffer
                (app, newCursorIdx, buffer, time, initialMsg)
            end
          else
            let
              val newCursorIdx = Cursor.vi0 (buffer, newEndIdx)
            in
              finishAfterDeletingBuffer
                (app, newCursorIdx, buffer, time, initialMsg)
            end
        end
      else
        finishAfterDeletingBuffer (app, startIdx, buffer, time, initialMsg)
    end


  (* equivalent of vi's 'x' command **)
  fun removeChr (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        NormalFinish.clearMode app
      else
        let
          val lineStart = Cursor.vi0 (buffer, cursorIdx)
          val lineEnd = Cursor.viDlrForDelete (buffer, cursorIdx, 1)
          val buffer = LineGap.goToIdx (lineEnd, buffer)

          (* if specified length (cursorIdx + count) extends 
           * beyond current line's length,
          *  then clip the length, to ensure we don't delete the newline *)
          val high = cursorIdx + count
          val high =
            if Cursor.isOnNewlineAfterChr (buffer, lineEnd) then lineEnd
            else Int.min (lineEnd - 1, high)
          val length = high - cursorIdx

          val initialMsg = Fn.initMsgs (cursorIdx, length, buffer)
          val buffer = LineGap.delete (cursorIdx, length, buffer)

          (* figure out where to place cursor *)
          val buffer = LineGap.goToIdx (lineStart, buffer)
          val lineEndAfterDelete = Cursor.viDlr (buffer, lineStart, 1)

          val cursorIdx = Int.min (lineEndAfterDelete, cursorIdx)
        in
          if cursorIdx >= #textLength buffer - 1 then
            (* special case: when buffer does not end with newline
             * and we are deleting from the last line *)
            let
              val cursorIdx = #textLength buffer - 1
              val buffer = LineGap.goToIdx (cursorIdx, buffer)
              val cursorIdx =
                if Cursor.isOnNewlineAfterChr (buffer, cursorIdx) then
                  cursorIdx - 1
                else
                  cursorIdx
            in
              finishAfterDeletingBuffer
                (app, cursorIdx, buffer, time, initialMsg)
            end
          else
            finishAfterDeletingBuffer (app, cursorIdx, buffer, time, initialMsg)
        end
    end

  (* Note: The below implementation of removing line breaks with the 'J' 
   * command slightly differs from the implementation in Vim.
   * In Vim, the J, 1J, and 2J commands all have the same effect.
   * 3J, 4J, 5J, etc. all have different effects because of the different counts
   * though.
   * Our implementation has a different effect for each count. 
   * 1J delettes 1 line break, 2J deletes 2, and so on. *)
  fun helpRemoveLineBreaks (app, buffer, cursorIdx, count, time) =
    if count = 0 then
      (* we don't use Fn.initMsgs in this function.
       * Removing line breaks is a discrete action which doesn't operate
       * as a range the way that motions like 'dw' or 'diw'.
       * Instead, a single character is deleted at different places. 
       * So it doesn't make any sense to use Fn.initMsgs 
       * which expects a range. *)
      finishAfterDeletingBuffer (app, cursorIdx, buffer, time, [])
    else
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
      in
        if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
          if cursorIdx >= #textLength buffer - 2 then
            finishAfterDeletingBuffer (app, cursorIdx, buffer, time, [])
          else
            (* if the cursor is at a linebreak, delete the linebreak
             * and don't insert a space. *)
            let val buffer = LineGap.delete (cursorIdx, 1, buffer)
            in helpRemoveLineBreaks (app, buffer, cursorIdx, count - 1, time)
            end
        else
          let
            val newCursorIdx = Cursor.toNextChr (buffer, cursorIdx, #"\n")
          in
            if newCursorIdx >= #textLength buffer - 2 then
              finishAfterDeletingBuffer (app, cursorIdx, buffer, time, [])
            else
              let
                val buffer = LineGap.delete (newCursorIdx, 1, buffer)
                val buffer = LineGap.insert (newCursorIdx, " ", buffer)
              in
                helpRemoveLineBreaks
                  (app, buffer, newCursorIdx, count - 1, time)
              end
          end
      end

  fun removeLineBreaks (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        if cursorIdx >= #textLength buffer - 2 then
          NormalFinish.clearMode app
        else
          let val buffer = LineGap.delete (cursorIdx, 1, buffer)
          in helpRemoveLineBreaks (app, buffer, cursorIdx, count - 1, time)
          end
      else
        let
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val newCursorIdx = Cursor.toNextChr (buffer, cursorIdx, #"\n")
          val buffer = LineGap.goToIdx (newCursorIdx, buffer)
        in
          if
            cursorIdx = newCursorIdx
            orelse newCursorIdx >= #textLength buffer - 2
          then
            (* no change, either because no #"\n" was found
             * or because the next #"\n" found is the last character in the
             * file.
             * We don't delete the last #"\n" in the file
             * because Unix convention is that text files always
             * end with newlines. *)
            NormalFinish.clearMode app
          else
            let
              val buffer = LineGap.delete (newCursorIdx, 1, buffer)
              val buffer = LineGap.insert (newCursorIdx, " ", buffer)
            in
              helpRemoveLineBreaks (app, buffer, newCursorIdx, count - 1, time)
            end
        end
    end

  fun helpDelete
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, time) =
    (* As a small optimisation to reduce allocations, 
    * we accumulate otherIdx by calling fMove with it and the buffer
    * on each loop.
    * Then, at the end of the loop, we perform the actual deletion.
    * This is faster than performing the actual deletion on every loop
    * because we only delete once, and avoid allocating intermediary buffers.
    * The behaviour between the two is equivalent. *)
    if count = 0 then
      let
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val high =
          if high >= #textLength buffer - 2 then
            Int.max (#textLength buffer - 2, 0)
          else
            high
        val length = high - low

        val buffer = LineGap.goToIdx (high, buffer)
        val initialMsg = Fn.initMsgs (low, length, buffer)

        val buffer = LineGap.delete (low, length, buffer)
        val buffer = LineGap.goToStart buffer
        val initialMsg = SEARCH (buffer, #dfa app, time) :: initialMsg

        val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
        val (buffer, searchList) =
          SearchList.buildRange (buffer, cursorIdx + 1111, #dfa app)

        (* If we have deleted from the buffer so that cursorIdx
        * is no longer a valid idx,
        * clip cursorIdx to the end. *)
        val buffer = LineGap.goToIdx (low, buffer)
        val cursorIdx =
          if low >= #textLength buffer then
            Int.max (#textLength buffer - 2, low)
          else
            low
      in
        NormalFinish.buildTextAndClear
          (app, buffer, cursorIdx, searchList, initialMsg, time)
      end
    else
      let
        (* get otherIdx, where cursor will want to go after motion. *)
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
      in
        helpDelete (app, buffer, cursorIdx, newOtherIdx, newCount, fMove, time)
      end

  fun delete (app: app_type, count, fMove, time) =
    let val {buffer, cursorIdx, ...} = app
    in helpDelete (app, buffer, cursorIdx, cursorIdx, count, fMove, time)
    end

  fun finishDeleteByDfa (app, low, high, buffer, time) =
    let
      val buffer = LineGap.goToIdx (high, buffer)
      val high =
        (* by default, we have a newline at the end of the buffer.
         * However, we also support the case where there is no newline
         * at the end of the buffer.
         * The first case is supported by default,
         * but we have to increment the textLength if the buffer
         * does not end with a newline, to make sure that
         * we delete the last character. *)
        if
          high = #textLength buffer - 1
          andalso not (Cursor.isCursorAtStartOfLine (buffer, high))
        then #textLength buffer
        else high

      val length = high - low
      val initialMsg = Fn.initMsgs (low, length, buffer)
      val buffer = LineGap.delete (low, length, buffer)

      (* if we deleted all text in the buffer, 
       * then make sure that we append a newline at the end
       * so that the buffer contains at least one character *)
      val buffer =
        if #textLength buffer = 0 then LineGap.append ("\n", buffer) else buffer
    in
      if low >= #textLength buffer - 1 andalso #textLength buffer > 0 then
        (* edge case:
         * when we delete from the cursor's position to the end of a file,
         * we have to move the cursor to be on the last char
         * because the original cursor's position is no longer valid. *)
        let
          val newCursorIdx = Int.max (#textLength buffer - 1, 0)
          val buffer = LineGap.goToIdx (newCursorIdx, buffer)
          val newCursorIdx =
            if Cursor.isOnNewlineAfterChr (buffer, newCursorIdx) then
              newCursorIdx - 1
            else
              newCursorIdx
        in
          finishAfterDeletingBuffer
            (app, newCursorIdx, buffer, time, initialMsg)
        end
      else
        let
          val buffer = LineGap.goToIdx (low, buffer)
          val newCursorIdx =
            if Cursor.isOnNewlineAfterChr (buffer, low) then low - 1 else low
        in
          finishAfterDeletingBuffer
            (app, newCursorIdx, buffer, time, initialMsg)
        end
    end

  fun deleteByDfa (app as {buffer, ...}: app_type, count, fMove, time) =
    if #textLength buffer = 1 then
      NormalFinish.clearMode app
    else
      let
        val {buffer, cursorIdx, ...} = app

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val otherIdx = fMove (buffer, cursorIdx, count)
      in
        if otherIdx > cursorIdx then
          if otherIdx >= #textLength buffer then
            (* prevent us from deleting last newline
             * to help us preserve unix-style line endings *)
            let
              (* if we're on the first character/column in a line,
               * we would like to delete the preceding newline too *)
              val cursorIdx =
                if Cursor.isPrevChrStartOfLine (buffer, cursorIdx) then
                  cursorIdx - 1
                else
                  cursorIdx
              val high = #textLength buffer - 1
            in
              finishDeleteByDfa (app, cursorIdx, high, buffer, time)
            end
          else
            finishDeleteByDfa (app, cursorIdx, otherIdx, buffer, time)
        else if otherIdx < cursorIdx then
          finishDeleteByDfa (app, otherIdx, cursorIdx, buffer, time)
        else
          NormalFinish.clearMode app
      end

  fun deleteCharsLeft (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val startOfLine = Cursor.vi0 (buffer, cursorIdx)
      val low = Cursor.viH (buffer, cursorIdx, count)
      val low = if low < startOfLine then startOfLine else low

      val length = cursorIdx - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun deleteToEndOfPrevWord (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.endOfPrevWord (buffer, cursorIdx, count)

      val length = (cursorIdx + 1) - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun deleteToEndOfPrevWORD (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.endOfPrevWORD (buffer, cursorIdx, count)

      val length = (cursorIdx + 1) - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun deleteToEndOfLine (app: app_type, time) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        (* if we are on \n, we don't want to delete or do anything
        * so reset the mode *)
        NormalFinish.clearMode app
      else
        let
          val lineStart = Cursor.vi0 (buffer, cursorIdx)
          val high = Cursor.viDlrForDelete (buffer, cursorIdx, 1)
          val length = high - cursorIdx

          val buffer = LineGap.goToIdx (high, buffer)
          val initialMsg = Fn.initMsgs (cursorIdx, length, buffer)
          val buffer = LineGap.delete (cursorIdx, length, buffer)

          (* calculate new cursorIdx.
           * Because we deleted the cursor that this line is on,
           * we need to set the cursorIdx to the place
           * that is considered to be the "end of line"
           * after having performed the deletion. *)
          val buffer = LineGap.goToIdx (lineStart, buffer)
          val cursorIdx = Cursor.viDlr (buffer, lineStart, 1)
        in
          finishAfterDeletingBuffer (app, cursorIdx, buffer, time, initialMsg)
        end
    end

  fun deleteLine (app: app_type, count, time) =
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
      val endLine = startLine + count

      val buffer = LineGap.goToLine (endLine, buffer)
      val endLineIdx = LineGap.lineNumberToIdx (endLine, buffer)
      val buffer = LineGap.goToIdx (endLineIdx, buffer)
      val endLineIdx =
        if Cursor.isCursorAtStartOfLine (buffer, endLineIdx) then endLineIdx + 1
        else endLineIdx

      val length = endLineIdx - startIdx
      val initialMsg = Fn.initMsgs (startIdx, length, buffer)
      val buffer = LineGap.delete (startIdx, length, buffer)
    in
      (* just need to reposition the cursor *)
      moveCursorAfterDeletingLines (app, buffer, time, initialMsg, startIdx)
    end

  fun deleteLineDown (app: app_type, count, time) =
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
        (* cursor is already on last line so not deleting *)
        NormalModeWith.bufferMsgsAndMode (app, buffer, [], NORMAL_MODE "")
      else
        let
          val buffer = LineGap.goToIdx (endLineIdx, buffer)

          (* right now, endLineIdx may be on a newline.
           * If it is, we want to delete that newline too,
           * and in that case, we increment by 1 to do so. 
           * However, we don't want to delete the last newline in the file
           * so we don't increment in that case. 
           * Edge case: if the startIdx also begins after a newline
           * then it is okay for us to delete the newline at the end of the file
           * because there will already be a newline at the end of the file
           * after the deletion. *)
          val endsOnNewline = Cursor.isCursorAtStartOfLine (buffer, endLineIdx)

          val buffer = LineGap.goToIdx (startIdx, buffer)
          val startsAfterNewline =
            startIdx > 0 andalso Cursor.isPrevChrStartOfLine (buffer, startIdx)

          val endLineIdx =
            if endsOnNewline then
              if endLineIdx = #textLength buffer - 1 then
                if startsAfterNewline then endLineIdx + 1 else endLineIdx
              else
                endLineIdx + 1
            else
              endLineIdx

          val length = endLineIdx - startIdx

          (* perform the actual deletion *)
          val buffer = LineGap.goToIdx (endLineIdx, buffer)
          val initialMsg = Fn.initMsgs (startIdx, length, buffer)
          val buffer = LineGap.delete (startIdx, length, buffer)
        in
          moveCursorAfterDeletingLines (app, buffer, time, initialMsg, startIdx)
        end
    end

  fun finishDeleteLineBack (app, buffer, lineIdx, length, endOfLine, time) =
    if endOfLine >= #textLength buffer - 2 then
      (* deleting from last line *)
      let
        (* go to first column of previous line *)
        val buffer = LineGap.goToIdx (endOfLine, buffer)
        val initialMsg = Fn.initMsgs (lineIdx, length, buffer)
        val buffer = LineGap.delete (lineIdx, length, buffer)

        val buffer =
          if #textLength buffer = 0 then LineGap.append ("\n", buffer)
          else buffer

        (* since we deleted from the last line,
         * we want to place the cursor at the first column
         * of the now-last line in the buffer. *)
        val newCursorIdx = Int.max (#textLength buffer - 1, 0)
        val buffer = LineGap.goToIdx (newCursorIdx, buffer)
        val newCursorIdx =
          if Cursor.isOnNewlineAfterChr (buffer, newCursorIdx) then
            newCursorIdx - 1
          else
            newCursorIdx
        val buffer = LineGap.goToIdx (newCursorIdx, buffer)
        val newCursorIdx = Cursor.vi0 (buffer, newCursorIdx)
      in
        finishAfterDeletingBuffer (app, newCursorIdx, buffer, time, initialMsg)
      end
    else
      let
        (* make sure the cursorIdx will be at the first column
         * of current line, after deleting from buffer. *)
        val buffer = LineGap.goToIdx (lineIdx, buffer)
        val newCursorIdx =
          if Cursor.isOnNewlineAfterChr (buffer, lineIdx) then lineIdx + 1
          else lineIdx

        val buffer = LineGap.goToIdx (endOfLine, buffer)
        val initialMsg = Fn.initMsgs (lineIdx, length, buffer)
        val buffer = LineGap.delete (lineIdx, length, buffer)

        val buffer =
          if #textLength buffer = 0 then LineGap.append ("\n", buffer)
          else buffer
      in
        finishAfterDeletingBuffer (app, newCursorIdx, buffer, time, initialMsg)
      end

  fun deleteLineBack (app: app_type, count, time) =
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
        (* deleting from current line to start of file *)
        let
          val endOfLine = Cursor.viDlr (buffer, cursorIdx, 1)

          (* edge case: if cursor is on a newline (if endOfLine = cursorIdx)
           * then we only want to delete 1 character at this line, 
           * which is the newline the cursor is at.
           * Otherwise, we want to delete 2 chars by default. *)
          val buffer = LineGap.goToIdx (endOfLine, buffer)
          val endOfLine =
            if Cursor.isCursorAtStartOfLine (buffer, endOfLine) then
              endOfLine + 1
            else
              endOfLine + 2
        in
          finishDeleteLineBack (app, buffer, 0, endOfLine, endOfLine, time)
        end
      else
        let
          val endOfLine = Cursor.viDlr (buffer, cursorIdx, 1)
          val buffer = LineGap.goToIdx (endOfLine, buffer)
          val endsOnNewline = Cursor.isCursorAtStartOfLine (buffer, endOfLine)

          (* edge case: if cursor is on a newline,
           * then we don't want to delete the newline
           * as we are already deleting the newline 
           * at the start of this range *)
          val endOfLine = if endsOnNewline then endOfLine else endOfLine + 1

          val newCursorLineNumber =
            if endsOnNewline andalso endOfLine = #textLength buffer - 1 then
              newCursorLineNumber - 1
            else
              newCursorLineNumber
          val buffer = LineGap.goToLine (newCursorLineNumber, buffer)

          val lineIdx = LineGap.lineNumberToIdx (newCursorLineNumber, buffer)
          val length = endOfLine - lineIdx
        in
          finishDeleteLineBack (app, buffer, lineIdx, length, endOfLine, time)
        end
    end

  fun deleteToFirstNonSpaceChr (app: app_type, time) =
    let
      val {buffer, cursorIdx, windowWidth, windowHeight, startLine, ...} = app

      (* move LineGap and buffer to start of line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = Cursor.vi0 (buffer, cursorIdx)

      (* move cursorIdx to first character on line *)
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = Cursor.firstNonSpaceChr (buffer, otherIdx)

      val low = Int.min (cursorIdx, otherIdx)
      val high = Int.max (cursorIdx, otherIdx)
      val length = high - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun helpDeleteToChr
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, fInc, chr, time) =
    if count = 0 then
      let
        val low = Int.min (cursorIdx, otherIdx)
        val high = Int.max (cursorIdx, otherIdx)
        val length = high - low
      in
        deleteAndFinish (app, low, length, buffer, time)
      end
    else
      let
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx, chr)
        val newCount = if newOtherIdx = otherIdx then 0 else count - 1
        val newOtherIdx = fInc (newOtherIdx, 1)
      in
        helpDeleteToChr
          ( app
          , buffer
          , cursorIdx
          , newOtherIdx
          , newCount
          , fMove
          , fInc
          , chr
          , time
          )
      end

  fun deleteToChr (app: app_type, count, fMove, fInc, chr, time) =
    helpDeleteToChr
      ( app
      , #buffer app
      , #cursorIdx app
      , #cursorIdx app
      , count
      , fMove
      , fInc
      , chr
      , time
      )

  fun deleteToStart (app: app_type, time) : AppType.app_type =
    let
      val {cursorIdx, buffer, windowWidth, windowHeight, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viDlrForDelete (buffer, cursorIdx, 1)

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val initialMsg = Fn.initMsgs (0, cursorIdx, buffer)

      val buffer = LineGap.delete (0, cursorIdx, buffer)
      val buffer = LineGap.goToStart buffer
      val initialMsg = SEARCH (buffer, dfa, time) :: initialMsg

      val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
      val (buffer, searchList) =
        SearchList.buildRange (buffer, cursorIdx + 1111, dfa)

      val cursorIdx = 0
      val startLine = 0
      val visualScrollColumn = 0
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val drawMsg = NormalModeTextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , searchList
        , visualScrollColumn
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DRAW_TEXT drawMsg

      val msgs = DRAW drawMsg :: initialMsg
      val mode = NORMAL_MODE ""
    in
      NormalModeWith.bufferAndCursorIdx
        ( app
        , buffer
        , cursorIdx
        , mode
        , startLine
        , searchList
        , msgs
        , time
        , visualScrollColumn
        )
    end

  fun helpDeleteToMatch (app: app_type, low, high, time) =
    let
      val {buffer, dfa, ...} = app
      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low
      val initialMsg = Fn.initMsgs (low, length, buffer)

      val buffer = LineGap.delete (low, length, buffer)
      val buffer = LineGap.goToStart buffer
      val initialMsg = SEARCH (buffer, dfa, time) :: initialMsg

      val buffer = LineGap.goToIdx (low - 1111, buffer)
      val (buffer, searchList) = SearchList.buildRange (buffer, low + 1111, dfa)

      val buffer = LineGap.goToIdx (low, buffer)
    in
      NormalFinish.buildTextAndClear
        (app, buffer, low, searchList, initialMsg, time)
    end

  fun deleteToNextMatch (app: app_type, count, time) =
    let
      val {cursorIdx, searchList, ...} = app
      val newCursorIdx =
        PersistentVector.nextMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 orelse newCursorIdx <= cursorIdx then
        NormalFinish.clearMode app
      else
        helpDeleteToMatch (app, cursorIdx, newCursorIdx, time)
    end

  fun deleteToPrevMatch (app: app_type, count, time) =
    let
      val {cursorIdx, searchList, ...} = app
      val newCursorIdx =
        PersistentVector.prevMatch (cursorIdx, searchList, count)
    in
      if newCursorIdx = ~1 orelse newCursorIdx >= cursorIdx then
        NormalFinish.clearMode app
      else
        helpDeleteToMatch (app, newCursorIdx, cursorIdx, time)
    end

  (* check if we are trying to delete from an empty buffer
   * or a buffer which consists of only one character which is \n *)
  fun canDeleteInsideOrAround (buffer, low, length) =
    not (length = 1 andalso LineGap.substring (low, 1, buffer) = "\n")

  fun deleteInsideWord (app: app_type, time) =
    let
      val {buffer, cursorIdx, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val low = Cursor.prevWordStrict (buffer, cursorIdx, 1)
      val high = Cursor.endOfWordStrict (buffer, cursorIdx, 1) + 1

      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low
    in
      if canDeleteInsideOrAround (buffer, low, length) then
        let
          val buffer = LineGap.goToIdx (high, buffer)
          val length = high - low
          val initialMsg = Fn.initMsgs (low, length, buffer)

          val buffer = LineGap.delete (low, length, buffer)

          val buffer = LineGap.goToStart buffer
          val initialMsg = SEARCH (buffer, dfa, time) :: initialMsg

          val buffer = LineGap.goToIdx (low - 1111, buffer)
          val (buffer, searchList) =
            SearchList.buildRange (buffer, low + 1111, dfa)

          val buffer = LineGap.goToIdx (low, buffer)
        in
          NormalFinish.buildTextAndClear
            (app, buffer, low, searchList, initialMsg, time)
        end
      else
        app
    end

  fun deleteInsideWORD (app: app_type, time) =
    let
      val {buffer, cursorIdx, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val low = Cursor.prevWORDStrict (buffer, cursorIdx, 1)
      val high = Cursor.endOfWORDStrict (buffer, cursorIdx, 1) + 1

      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low
    in
      if canDeleteInsideOrAround (buffer, low, length) then
        let
          val initialMsg = Fn.initMsgs (low, length, buffer)
          val buffer = LineGap.delete (low, length, buffer)

          val buffer = LineGap.goToStart buffer
          val initialMsg = SEARCH (buffer, dfa, time) :: initialMsg

          val buffer = LineGap.goToIdx (low - 1111, buffer)
          val (buffer, searchList) =
            SearchList.buildRange (buffer, low + 1111, dfa)

          val buffer = LineGap.goToIdx (low, buffer)
        in
          NormalFinish.buildTextAndClear
            (app, buffer, low, searchList, initialMsg, time)
        end
      else
        app
    end

  fun finishAfterDeleteInside (app: app_type, origLow, high, time) =
    if origLow = high then
      NormalFinish.clearMode app
    else
      let
        val {cursorIdx, buffer, dfa, ...} = app
        val low = origLow + 1
        val length = high - low

        val buffer = LineGap.goToIdx (high, buffer)
        val initialMsg = Fn.initMsgs (low, length, buffer)

        val buffer = LineGap.delete (low, length, buffer)
        val buffer = LineGap.goToStart buffer
        val initialMsg = SEARCH (buffer, dfa, time) :: initialMsg

        val buffer = LineGap.goToIdx (low - 1111, buffer)
        val (buffer, searchList) =
          SearchList.buildRange (buffer, low + 1111, dfa)

        val buffer = LineGap.goToIdx (origLow, buffer)
      in
        NormalFinish.buildTextAndClear
          (app, buffer, origLow, searchList, initialMsg, time)
      end

  fun deleteInsideChrOpen (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = cursorIdx + 1
      val buffer = LineGap.goToIdx (start, buffer)

      val origLow = Cursor.toPrevChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (origLow, buffer)
      val high = Cursor.matchPair (buffer, origLow)
    in
      finishAfterDeleteInside (app, origLow, high, time)
    end

  fun deleteInsideChrClose (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = Int.max (cursorIdx - 1, 0)
      val buffer = LineGap.goToIdx (start, buffer)

      val high = Cursor.toNextChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (high, buffer)
      val origLow = Cursor.matchPair (buffer, high)
    in
      finishAfterDeleteInside (app, origLow, high, time)
    end

  fun finishDeleteAroundChr (app, low, high, buffer, time) =
    let
      val length = high - low + 1
      val buffer = LineGap.goToIdx (high, buffer)
      val initialMsg = Fn.initMsgs (low, length, buffer)

      val buffer = LineGap.delete (low, length, buffer)
      val buffer = LineGap.goToIdx (low, buffer)
      val low =
        if Cursor.isCursorAtStartOfLine (buffer, low) then Int.max (low - 1, 0)
        else low
    in
      finishAfterDeletingBuffer (app, low, buffer, time, initialMsg)
    end

  fun deleteAroundChrOpen (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = cursorIdx + 1
      val buffer = LineGap.goToIdx (start, buffer)

      val low = Cursor.toPrevChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (low, buffer)
      val high = Cursor.matchPair (buffer, low)
    in
      if low = high then NormalFinish.clearMode app
      else finishDeleteAroundChr (app, low, high, buffer, time)
    end

  fun deleteAroundChrClose (app: app_type, chr, time) =
    let
      val {cursorIdx, buffer, ...} = app

      val start = Int.max (cursorIdx - 1, 0)
      val buffer = LineGap.goToIdx (start, buffer)

      val high = Cursor.toNextChr (buffer, start, chr)
      val buffer = LineGap.goToIdx (high, buffer)
      val low = Cursor.matchPair (buffer, high)
    in
      if low = high then NormalFinish.clearMode app
      else finishDeleteAroundChr (app, low, high, buffer, time)
    end

  fun deletePair (app: app_type, time) =
    let
      val {cursorIdx, buffer, dfa, ...} = app
      val otherIdx = Cursor.matchPair (buffer, cursorIdx)
    in
      if otherIdx = cursorIdx then
        NormalFinish.clearMode app
      else
        let
          val low = Int.min (cursorIdx, otherIdx)
          val high = Int.max (cursorIdx, otherIdx)
          val length = high - low + 1

          val buffer = LineGap.goToIdx (high, buffer)
          val initialMsg = Fn.initMsgs (low, length, buffer)

          val buffer = LineGap.delete (low, length, buffer)
          val buffer = LineGap.goToIdx (low, buffer)

          val low =
            if Cursor.isCursorAtStartOfLine (buffer, low) then
              (* we may have deleted the last character of this current line,
               * and if we did, we have to move the cursor back by 1 *)
              Int.max (low - 1, 0)
            else
              low

          val buffer = LineGap.goToStart buffer
          val initialMsg = SEARCH (buffer, dfa, time) :: initialMsg

          val buffer = LineGap.goToIdx (low - 1111, buffer)
          val (buffer, searchList) =
            SearchList.buildRange (buffer, low + 1111, dfa)

          val buffer = LineGap.goToIdx (low, buffer)
        in
          NormalFinish.buildTextAndClear
            (app, buffer, low, searchList, initialMsg, time)
        end
    end
end
