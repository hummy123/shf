signature MAKE_NORMAL_DELETE =
sig
  val initMsgs: int * int * LineGap.t -> MailboxType.t list
end

functor MakeNormalDelete(Fn: MAKE_NORMAL_DELETE) =
struct
  open AppType
  open DrawMsg
  open MailboxType

  fun finishAfterDeletingBuffer (app, low, buffer, searchList, time, msgs) =
    let val buffer = LineGap.goToIdx (low, buffer)
    in NormalFinish.buildTextAndClear (app, buffer, low, searchList, msgs, time)
    end

  fun deleteAndFinish (app: app_type, low, length, buffer, time) =
    let
      val {searchList, dfa, ...} = app

      val buffer = LineGap.goToIdx (low + length, buffer)
      val msgs = Fn.initMsgs (low, length, buffer)
      val (buffer, searchList) = SearchList.deleteBufferAndSearchList
        (low, length, buffer, searchList, dfa)

      val low =
        if low >= #textLength buffer - 1 then
          Int.max (#textLength buffer - 1, 0)
        else
          low

      val buffer = LineGap.goToIdx (low, buffer)

      val low =
        if Cursor.isOnNewlineAfterChr (buffer, low) then low - 1 else low

      val buffer =
        if #textLength buffer = 0 then LineGap.fromString "\n" else buffer
      val buffer = LineGap.goToIdx (low, buffer)
    in
      NormalFinish.buildTextAndClear (app, buffer, low, searchList, msgs, time)
    end

  fun moveCursorAfterDeletingLines
    (app, buffer, time, initialMsg, startIdx, searchList) =
    let
      val newEndIdx = #textLength buffer - 1
    in
      if newEndIdx < 0 then
        (* deleted whole file; add newline to the end *)
        let
          val buffer = LineGap.append ("\n", buffer)
        in
          finishAfterDeletingBuffer
            (app, 0, buffer, searchList, time, initialMsg)
        end
      else if newEndIdx = 0 then
        (* there is only one char left in the file *)
        finishAfterDeletingBuffer (app, 0, buffer, searchList, time, initialMsg)
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
                (app, newCursorIdx, buffer, searchList, time, initialMsg)
            end
          else
            let
              val newCursorIdx = Cursor.vi0 (buffer, newEndIdx)
            in
              finishAfterDeletingBuffer
                (app, newCursorIdx, buffer, searchList, time, initialMsg)
            end
        end
      else
        finishAfterDeletingBuffer
          (app, startIdx, buffer, searchList, time, initialMsg)
    end

  (* equivalent of vi's 'x' command **)
  fun removeChr (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, searchList, dfa, ...} = app
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
          val (buffer, searchList) = SearchList.deleteBufferAndSearchList
            (cursorIdx, length, buffer, searchList, dfa)

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
                (app, cursorIdx, buffer, searchList, time, initialMsg)
            end
          else
            finishAfterDeletingBuffer
              (app, cursorIdx, buffer, searchList, time, initialMsg)
        end
    end

  (* Note: The below implementation of removing line breaks with the 'J' 
   * command slightly differs from the implementation in Vim.
   * In Vim, the J, 1J, and 2J commands all have the same effect.
   * 3J, 4J, 5J, etc. all have different effects because of the different counts
   * though.
   * Our implementation has a different effect for each count. 
   * 1J delettes 1 line break, 2J deletes 2, and so on. *)
  fun helpRemoveLineBreaks
    (app, buffer, cursorIdx, count, time, searchList, dfa) =
    if count = 0 then
      (* we don't use Fn.initMsgs in this function.
       * Removing line breaks is a discrete action which doesn't operate
       * as a range the way that motions like 'dw' or 'diw'.
       * Instead, a single character is deleted at different places. 
       * So it doesn't make any sense to use Fn.initMsgs 
       * which expects a range. *)
      finishAfterDeletingBuffer (app, cursorIdx, buffer, searchList, time, [])
    else
      let
        val buffer = LineGap.goToIdx (cursorIdx, buffer)
      in
        if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
          if cursorIdx >= #textLength buffer - 2 then
            finishAfterDeletingBuffer
              (app, cursorIdx, buffer, searchList, time, [])
          else
            (* if the cursor is at a linebreak, delete the linebreak
             * and don't insert a space. *)
            let
              val (buffer, searchList) = SearchList.deleteBufferAndSearchList
                (cursorIdx, 1, buffer, searchList, dfa)
            in
              helpRemoveLineBreaks
                (app, buffer, cursorIdx, count - 1, time, searchList, dfa)
            end
        else
          let
            val newCursorIdx =
              Cursor.toNextChr (buffer, cursorIdx, {findChr = #"\n", count = 1})
          in
            if newCursorIdx >= #textLength buffer - 2 then
              finishAfterDeletingBuffer
                (app, cursorIdx, buffer, searchList, time, [])
            else
              let
                (* todo: have not implemented search-list-rebuilding 
                 * for insertions yet *)
                val (buffer, searchList) = SearchList.deleteBufferAndSearchList
                  (newCursorIdx, 1, buffer, searchList, dfa)
                val buffer = LineGap.insert (newCursorIdx, " ", buffer)
              in
                helpRemoveLineBreaks
                  (app, buffer, newCursorIdx, count - 1, time, searchList, dfa)
              end
          end
      end

  fun removeLineBreaks (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, searchList, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        if cursorIdx >= #textLength buffer - 2 then
          NormalFinish.clearMode app
        else
          let
            val (buffer, searchList) = SearchList.deleteBufferAndSearchList
              (cursorIdx, 1, buffer, searchList, dfa)
          in
            helpRemoveLineBreaks
              (app, buffer, cursorIdx, count - 1, time, searchList, dfa)
          end
      else
        let
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val newCursorIdx =
            Cursor.toNextChr (buffer, cursorIdx, {findChr = #"\n", count = 1})
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
              (* todo: have not implemented search-list-rebuilding 
               * for insertions yet *)
              val (buffer, searchList) = SearchList.deleteBufferAndSearchList
                (newCursorIdx, 1, buffer, searchList, dfa)
              val buffer = LineGap.insert (newCursorIdx, " ", buffer)
            in
              helpRemoveLineBreaks
                (app, buffer, newCursorIdx, count - 1, time, searchList, dfa)
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
        val {searchList, dfa, ...} = app

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

        val (buffer, searchList) = SearchList.deleteBufferAndSearchList
          (low, length, buffer, searchList, dfa)

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

  fun finishDeleteByDfa (app: app_type, low, high, buffer, time) =
    let
      val {searchList, dfa, ...} = app

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

      val (buffer, searchList) = SearchList.deleteBufferAndSearchList
        (low, length, buffer, searchList, dfa)

      (* if we deleted all text in the buffer, 
       * then make sure that we append a newline at the end
       * so that the buffer contains at least one character.
       * todo: incrementally update searchList based on insertion *)
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
            (app, newCursorIdx, buffer, searchList, time, initialMsg)
        end
      else
        let
          val buffer = LineGap.goToIdx (low, buffer)
          val newCursorIdx =
            if Cursor.isOnNewlineAfterChr (buffer, low) then low - 1 else low
        in
          finishAfterDeletingBuffer
            (app, newCursorIdx, buffer, searchList, time, initialMsg)
        end
    end

  fun deleteWord (app as {buffer, ...}: app_type, count, time) =
    if #textLength buffer = 1 then
      NormalFinish.clearMode app
    else
      let
        val {buffer, cursorIdx, ...} = app

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val high = Cursor.nextWordForDelete (buffer, cursorIdx, count)
        val length = high - cursorIdx
      in
        deleteAndFinish (app, cursorIdx, length, buffer, time)
      end

  fun deleteWORD (app as {buffer, ...}: app_type, count, time) =
    if #textLength buffer = 1 then
      NormalFinish.clearMode app
    else
      let
        val {buffer, cursorIdx, ...} = app

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val high = Cursor.nextWORDForDelete (buffer, cursorIdx, count)
        val length = high - cursorIdx
      in
        deleteAndFinish (app, cursorIdx, length, buffer, time)
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
    if #cursorIdx app = 0 then
      NormalFinish.clearMode app
    else
      let
        val {buffer, cursorIdx, searchList, dfa, ...} = app

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val low = Cursor.endOfPrevWord (buffer, cursorIdx, count)

        val length = (cursorIdx + 1) - low

        val buffer = LineGap.goToIdx (cursorIdx + 1, buffer)
        val initialMsg = Fn.initMsgs (low, length, buffer)
        val (buffer, searchList) = SearchList.deleteBufferAndSearchList
          (low, length, buffer, searchList, dfa)

        val buffer = LineGap.goToIdx (low, buffer)
        val newCursorIdx =
          if Cursor.isOnNewlineAfterChr (buffer, low) then Int.max (low - 1, 0)
          else low
      in
        finishAfterDeletingBuffer
          (app, newCursorIdx, buffer, searchList, time, initialMsg)
      end

  fun deleteToEndOfPrevWORD (app: app_type, count, time) =
    if #cursorIdx app = 0 then
      NormalFinish.clearMode app
    else
      let
        val {buffer, cursorIdx, searchList, dfa, ...} = app

        val buffer = LineGap.goToIdx (cursorIdx, buffer)
        val low = Cursor.endOfPrevWORD (buffer, cursorIdx, count)

        val length = (cursorIdx + 1) - low

        val buffer = LineGap.goToIdx (cursorIdx + 1, buffer)
        val initialMsg = Fn.initMsgs (low, length, buffer)
        val (buffer, searchList) = SearchList.deleteBufferAndSearchList
          (low, length, buffer, searchList, dfa)

        val buffer = LineGap.goToIdx (low, buffer)
        val newCursorIdx =
          if Cursor.isOnNewlineAfterChr (buffer, low) then Int.max (low - 1, 0)
          else low
      in
        finishAfterDeletingBuffer
          (app, newCursorIdx, buffer, searchList, time, initialMsg)
      end

  fun deleteToEndOfLine (app: app_type, time) =
    let
      val {buffer, cursorIdx, searchList, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
    in
      if Cursor.isCursorAtStartOfLine (buffer, cursorIdx) then
        (* if we are on \n, we don't want to delete or do anything
        * so reset the mode *)
        NormalFinish.clearMode app
      else
        let
          val lineStart = Cursor.vi0 (buffer, cursorIdx)
          val high = Cursor.viDlr (buffer, cursorIdx, 1) + 1
          val length = high - cursorIdx

          val buffer = LineGap.goToIdx (high, buffer)
          val initialMsg = Fn.initMsgs (cursorIdx, length, buffer)
          val (buffer, searchList) = SearchList.deleteBufferAndSearchList
            (cursorIdx, length, buffer, searchList, dfa)

          (* calculate new cursorIdx.
           * Because we deleted the cursor that this line is on,
           * we need to set the cursorIdx to the place
           * that is considered to be the "end of line"
           * after having performed the deletion. *)
          val buffer = LineGap.goToIdx (lineStart, buffer)
          val cursorIdx = Cursor.viDlr (buffer, lineStart, 1)
          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val cursorIdx =
            if Cursor.isOnNewlineAfterChr (buffer, cursorIdx) then cursorIdx - 1
            else cursorIdx
        in
          finishAfterDeletingBuffer
            (app, cursorIdx, buffer, searchList, time, initialMsg)
        end
    end

  fun deleteLine (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, searchList, dfa, ...} = app
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
      val (buffer, searchList) = SearchList.deleteBufferAndSearchList
        (startIdx, length, buffer, searchList, dfa)
    in
      (* just need to reposition the cursor *)
      moveCursorAfterDeletingLines
        (app, buffer, time, initialMsg, startIdx, searchList)
    end

  fun deleteLineDown (app: app_type, count, time) =
    let
      val {buffer, cursorIdx, searchList, dfa, ...} = app
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

          (* endLineIdx is on a newline because it was retrieved 
           * by calling the `LineGap.lineNumberToIdx` function, 
           * which always returns the idx of a line break.
           * Since that is the case, we want to delete that newline too,
           * and we increment by 1 to do so. 
           * However, we don't want to delete the last newline in the file
           * so we don't increment in that case. 
           * Edge case: if the startIdx also begins after a newline
           * then it is okay for us to delete the newline at the end of the file
           * because there will already be a newline at the end of the file
           * after the deletion. *)
          val buffer = LineGap.goToIdx (startIdx, buffer)
          val startsAfterNewline =
            startIdx > 0 andalso Cursor.isPrevChrStartOfLine (buffer, startIdx)

          val endLineIdx =
            if endLineIdx = #textLength buffer - 1 then
              if startsAfterNewline then endLineIdx + 1 else endLineIdx
            else
              endLineIdx + 1

          val length = endLineIdx - startIdx

          (* perform the actual deletion *)
          val buffer = LineGap.goToIdx (endLineIdx, buffer)
          val initialMsg = Fn.initMsgs (startIdx, length, buffer)
          val (buffer, searchList) = SearchList.deleteBufferAndSearchList
            (startIdx, length, buffer, searchList, dfa)
        in
          moveCursorAfterDeletingLines
            (app, buffer, time, initialMsg, startIdx, searchList)
        end
    end

  fun finishDeleteLineUp (app, buffer, lineIdx, length, endOfLine, time) =
    if endOfLine >= #textLength buffer - 2 then
      (* deleting from last line *)
      let
        val {searchList, dfa, ...} = app

        (* go to first column of previous line *)
        val buffer = LineGap.goToIdx (endOfLine, buffer)
        val initialMsg = Fn.initMsgs (lineIdx, length, buffer)
        val (buffer, searchList) = SearchList.deleteBufferAndSearchList
          (lineIdx, length, buffer, searchList, dfa)

        val buffer =
          (* todo: incrementally rebuild searchList if we are appending *)
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
        finishAfterDeletingBuffer
          (app, newCursorIdx, buffer, searchList, time, initialMsg)
      end
    else
      let
        val {searchList, dfa, ...} = app

        (* make sure the cursorIdx will be at the first column
         * of current line, after deleting from buffer. *)
        val buffer = LineGap.goToIdx (lineIdx, buffer)
        val newCursorIdx =
          if Cursor.isOnNewlineAfterChr (buffer, lineIdx) then lineIdx + 1
          else lineIdx

        val buffer = LineGap.goToIdx (endOfLine, buffer)
        val initialMsg = Fn.initMsgs (lineIdx, length, buffer)
        val (buffer, searchList) = SearchList.deleteBufferAndSearchList
          (lineIdx, length, buffer, searchList, dfa)

        val buffer =
          (* todo: incrementally rebuild searchList if we are appending *)
          if #textLength buffer = 0 then LineGap.append ("\n", buffer)
          else buffer
      in
        finishAfterDeletingBuffer
          (app, newCursorIdx, buffer, searchList, time, initialMsg)
      end

  fun deleteLineUp (app: app_type, count, time) =
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
          finishDeleteLineUp (app, buffer, 0, endOfLine, endOfLine, time)
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
          finishDeleteLineUp (app, buffer, lineIdx, length, endOfLine, time)
        end
    end

  fun deleteToFirstNonSpaceChr (app: app_type, time) =
    let
      val {buffer, cursorIdx, ...} = app

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

  fun finishDeleteToChr (app, buffer, cursorIdx, otherIdx, time) =
    let
      val low = Int.min (cursorIdx, otherIdx)
      val high = Int.max (cursorIdx, otherIdx)
      val length = high - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun helpDeleteToChr
    (app: app_type, buffer, cursorIdx, otherIdx, count, fMove, fInc, chr, time) =
    if count = 0 then
      finishDeleteToChr (app, buffer, cursorIdx, otherIdx, time)
    else
      let
        val buffer = LineGap.goToIdx (otherIdx, buffer)
        val newOtherIdx = fMove (buffer, otherIdx, chr)
      in
        if otherIdx = newOtherIdx then
          finishDeleteToChr (app, buffer, cursorIdx, otherIdx, time)
        else
          helpDeleteToChr
            ( app
            , buffer
            , cursorIdx
            , fInc (newOtherIdx, 1)
            , count - 1
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

  fun deleteToNextChr (app: app_type, count, chr, time) =
    let
      val {buffer, cursorIdx, searchList, dfa, ...} = app
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
          val initialMsg = Fn.initMsgs (cursorIdx, length, buffer)
          val (buffer, searchList) = SearchList.deleteBufferAndSearchList
            (cursorIdx, length, buffer, searchList, dfa)

          val buffer =
            (* todo: rebuild searchList if 
             * we are creating new buffer from string *)
            if #textLength buffer = 0 then LineGap.fromString "\n"
            else buffer

          val buffer = LineGap.goToIdx (cursorIdx, buffer)
          val cursorIdx =
            if Cursor.isOnNewlineAfterChr (buffer, cursorIdx) then cursorIdx - 1
            else cursorIdx
        in
          finishAfterDeletingBuffer
            (app, cursorIdx, buffer, searchList, time, initialMsg)
        end
    end

  fun deleteTillNextChr (app: app_type, count, chr, time) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val newCursorIdx =
        Cursor.toNextChr (buffer, cursorIdx, {findChr = chr, count = count})
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        let val length = newCursorIdx - cursorIdx
        in deleteAndFinish (app, cursorIdx, length, buffer, time)
        end
    end

  fun deleteToPrevChr (app: app_type, count, chr, time) =
    let
      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val newCursorIdx =
        Cursor.toPrevChr (buffer, cursorIdx, {findChr = chr, count = count})
    in
      if newCursorIdx = ~1 then
        NormalFinish.clearMode app
      else
        let val length = cursorIdx - newCursorIdx
        in deleteAndFinish (app, newCursorIdx, length, buffer, time)
        end
    end

  fun deleteTillPrevChr (app: app_type, count, chr, time) =
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
          val low = newCursorIdx + 1
          val length = cursorIdx - newCursorIdx - 1
        in
          deleteAndFinish (app, low, length, buffer, time)
        end
    end

  fun deleteToStart (app: app_type, time) : AppType.app_type =
    let
      val {cursorIdx, buffer, windowWidth, windowHeight, dfa, searchList, ...} =
        app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorIdx = Cursor.viDlrForDelete (buffer, cursorIdx, 1)

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val initialMsg = Fn.initMsgs (0, cursorIdx, buffer)

      val (buffer, searchList) = SearchList.deleteBufferAndSearchList
        (0, cursorIdx, buffer, searchList, dfa)

      val buffer =
        (* todo: adjust searchList if we call SearchList.fromString *)
        if #textLength buffer = 0 then LineGap.fromString "\n"
        else buffer

      val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
      val (buffer, searchList) = SearchList.build (buffer, dfa)

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

  fun deleteToEnd (app: app_type, time) =
    let
      val {buffer, cursorIdx, searchList, dfa, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val startOfLineIdx = Cursor.vi0 (buffer, cursorIdx)
      val length = #textLength buffer - startOfLineIdx

      val buffer = LineGap.goToIdx (#textLength buffer, buffer)
      val initialMsg = Fn.initMsgs (startOfLineIdx, length, buffer)

      val (buffer, searchList) = SearchList.deleteBufferAndSearchList
        (startOfLineIdx, length, buffer, searchList, dfa)

      val buffer =
        (* todo: if we are creating a new buffer from string,
         * then make sure we update the searchList for it too *)
        if #textLength buffer = 0 then LineGap.fromString "\n"
        else buffer

      val newLineEndIdx = Int.max (startOfLineIdx - 1, 0)
      val buffer = LineGap.goToIdx (newLineEndIdx, buffer)
      val newLineEndIdx =
        if Cursor.isOnNewlineAfterChr (buffer, newLineEndIdx) then
          Int.max (newLineEndIdx - 1, 0)
        else
          newLineEndIdx

      val buffer = LineGap.goToIdx (newLineEndIdx, buffer)
      val newLineStartIdx = Cursor.vi0 (buffer, newLineEndIdx)
    in
      finishAfterDeletingBuffer
        (app, newLineStartIdx, buffer, searchList, time, initialMsg)
    end

  fun helpDeleteToMatch (app: app_type, low, high, time) =
    let
      val {buffer, dfa, searchList, ...} = app
      val buffer = LineGap.goToIdx (high, buffer)
      val length = high - low
      val initialMsg = Fn.initMsgs (low, length, buffer)

      val (buffer, searchList) = SearchList.deleteBufferAndSearchList
        (low, length, buffer, searchList, dfa)

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

  fun deleteInsideWord (app: app_type, time) =
    let
      val {buffer, cursorIdx, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val chr = LineGap.sub (cursorIdx, buffer)
    in
      if chr = #"\n" then
        NormalFinish.clearMode app
      else if Char.isAlphaNum chr orelse chr = #"_" then
        let
          val low = Cursor.firstContiguousAlpha (buffer, cursorIdx)
          val high = Cursor.lastContiguousAlpha (buffer, cursorIdx) + 1
          val length = high - low
        in
          deleteAndFinish (app, low, length, buffer, time)
        end
      else if Char.isSpace chr then
        let
          val low = Cursor.firstContiguousSpace (buffer, cursorIdx)
          val high = Cursor.lastContiguousSpace (buffer, cursorIdx) + 1
          val length = high - low
        in
          deleteAndFinish (app, low, length, buffer, time)
        end
      else
        (* char is punct *)
        let
          val low = Cursor.firstContiguousPunct (buffer, cursorIdx)
          val high = Cursor.lastContiguousPunct (buffer, cursorIdx) + 1
          val length = high - low
        in
          deleteAndFinish (app, low, length, buffer, time)
        end
    end

  fun deleteInsideWORD (app: app_type, time) =
    let
      val {buffer, cursorIdx, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val chr = LineGap.sub (cursorIdx, buffer)
    in
      if chr = #"\n" then
        NormalFinish.clearMode app
      else if Char.isSpace chr then
        let
          val low = Cursor.firstContiguousSpace (buffer, cursorIdx)
          val high = Cursor.lastContiguousSpace (buffer, cursorIdx) + 1
          val length = high - low
        in
          deleteAndFinish (app, low, length, buffer, time)
        end
      else
        let
          val low = Cursor.firstContiguousNonSpace (buffer, cursorIdx)
          val high = Cursor.lastContiguousNonSpace (buffer, cursorIdx) + 1
          val length = high - low
        in
          deleteAndFinish (app, low, length, buffer, time)
        end
    end

  fun deleteAroundWord (app: app_type, time) =
    let
      val {buffer, cursorIdx, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)

      val low = Cursor.aroundWordPrev (buffer, cursorIdx)
      val high = Cursor.aroundWordNext (buffer, cursorIdx) + 1

      val length = high - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun deleteAroundWORD (app: app_type, time) =
    let
      val {buffer, cursorIdx, dfa, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val low = Cursor.aroundWORDPrev (buffer, cursorIdx)
      val high = Cursor.aroundWORDNext (buffer, cursorIdx) + 1
      val length = high - low
    in
      deleteAndFinish (app, low, length, buffer, time)
    end

  fun finishDeletingInsidePair (app, buffer, cursorIdx, otherIdx, dfa, time) =
    let
      val low = Int.min (cursorIdx, otherIdx)
      val high = Int.max (cursorIdx, otherIdx)
    in
      if high = low + 1 then
        NormalFinish.clearMode app
      else
        let
          val deleteLow = low + 1
          val length = high - deleteLow

          val buffer = LineGap.goToIdx (high, buffer)
          val initialMsg = Fn.initMsgs (deleteLow, length, buffer)

          val (buffer, searchList) = SearchList.deleteBufferAndSearchList
            (deleteLow, length, buffer, #searchList app, dfa)

          val buffer = LineGap.goToIdx (low, buffer)
        in
          NormalFinish.buildTextAndClear
            (app, buffer, low, searchList, initialMsg, time)
        end
    end

  fun deleteInsidePair (app: app_type, openChr, closeChr, time) =
    let
      val {buffer, cursorIdx, dfa, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorChr = LineGap.sub (cursorIdx, buffer)
    in
      if cursorChr = openChr orelse cursorChr = closeChr then
        (* cursor is at pair, so match and delete *)
        let
          val otherIdx = Cursor.matchPair (buffer, cursorIdx)
        in
          if otherIdx = ~1 then
            NormalFinish.clearMode app
          else
            finishDeletingInsidePair
              (app, buffer, cursorIdx, otherIdx, dfa, time)
        end
      else
        (* check to see if we are inside pair *)
        let
          val prevIdx =
            Cursor.toOpenChrPrev
              (buffer, cursorIdx, {openChr = openChr, closeChr = closeChr})
        in
          if prevIdx = ~1 then
            (* no openChr before cursor, so check after cursor *)
            let
              val nextIdx =
                Cursor.toNextChr
                  (buffer, cursorIdx, {findChr = openChr, count = 1})
            in
              if nextIdx = ~1 then
                NormalFinish.clearMode app
              else
                let
                  val buffer = LineGap.goToIdx (nextIdx, buffer)
                  val matchIdx = Cursor.matchPair (buffer, nextIdx)
                in
                  if matchIdx = ~1 then
                    NormalFinish.clearMode app
                  else
                    finishDeletingInsidePair
                      (app, buffer, nextIdx, matchIdx, dfa, time)
                end
            end
          else
            (* there is an openChr before cursor, so match it,
             * and if there is a match, then delete *)
            let
              val buffer = LineGap.goToIdx (prevIdx, buffer)
              val otherIdx = Cursor.matchPair (buffer, prevIdx)
            in
              if otherIdx = ~1 then
                NormalFinish.clearMode app
              else
                finishDeletingInsidePair
                  (app, buffer, prevIdx, otherIdx, dfa, time)
            end
        end
    end

  fun finishDeleteAroundPair (app, buffer, chr1Idx, chr2Idx, time) =
    let
      val {searchList, dfa, ...} = app

      val low = Int.min (chr1Idx, chr2Idx)
      val high = Int.max (chr1Idx, chr2Idx) + 1
      val length = high - low

      val buffer = LineGap.goToIdx (high, buffer)
      val initialMsg = Fn.initMsgs (low, length, buffer)

      val (buffer, searchList) = SearchList.deleteBufferAndSearchList
        (low, length, buffer, searchList, dfa)

      val buffer = LineGap.goToIdx (low, buffer)
      val low =
        if Cursor.isOnNewlineAfterChr (buffer, low) then low - 1 else low
      val buffer = LineGap.goToIdx (low, buffer)
    in
      finishAfterDeletingBuffer (app, low, buffer, searchList, time, initialMsg)
    end

  fun deleteAroundPair (app: app_type, openChr, closeChr, time) =
    let
      val {buffer, cursorIdx, ...} = app

      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val cursorChr = LineGap.sub (cursorIdx, buffer)
    in
      if cursorChr = closeChr orelse cursorChr = openChr then
        let val pairIdx = Cursor.matchPair (buffer, cursorIdx)
        in finishDeleteAroundPair (app, buffer, cursorIdx, pairIdx, time)
        end
      else
        let
          val prevIdx =
            Cursor.toOpenChrPrev
              (buffer, cursorIdx, {openChr = openChr, closeChr = closeChr})
        in
          if prevIdx = ~1 then
            (* we are not in a pair, so try to find openChr after cursor *)
            let
              val nextIdx =
                Cursor.toNextChr
                  (buffer, cursorIdx, {findChr = openChr, count = 1})
            in
              if nextIdx = ~1 then
                NormalFinish.clearMode app
              else
                let
                  val buffer = LineGap.goToIdx (nextIdx, buffer)
                  val matchIdx = Cursor.matchPair (buffer, nextIdx)
                in
                  if matchIdx = ~1 then
                    NormalFinish.clearMode app
                  else
                    finishDeleteAroundPair
                      (app, buffer, nextIdx, matchIdx, time)
                end
            end
          else
            (* we are on a pair-character, so check if pair
             * is balanced and delete if it is *)
            let
              val buffer = LineGap.goToIdx (prevIdx, buffer)
              val matchIdx = Cursor.matchPair (buffer, prevIdx)
            in
              if matchIdx = ~1 then NormalFinish.clearMode app
              else finishDeleteAroundPair (app, buffer, prevIdx, matchIdx, time)
            end
        end
    end

  fun deletePair (app: app_type, time) =
    let
      val {cursorIdx, buffer, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx, buffer)
      val otherIdx = Cursor.nextPairChr (buffer, cursorIdx)
    in
      if otherIdx = ~1 then
        NormalFinish.clearMode app
      else
        let
          val buffer = LineGap.goToIdx (otherIdx, buffer)
          val otherIdx = Cursor.matchPair (buffer, otherIdx)
        in
          if otherIdx = ~1 then NormalFinish.clearMode app
          else finishDeleteAroundPair (app, buffer, cursorIdx, otherIdx, time)
        end
    end
end
