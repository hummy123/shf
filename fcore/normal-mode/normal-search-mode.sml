structure NormalSearchMode =
struct
  open AppType
  open InputMsg
  open MailboxType

  fun buildTempSearchList (searchString, buffer, cursorIdx) =
    let val unescapedString = EscapeString.unescape searchString
    in SearchList.buildRange (buffer, unescapedString, cursorIdx + 1111)
    end

  fun addChr (app: app_type, searchString, searchCursorIdx, chr) =
    let
      val {cursorIdx, buffer, ...} = app

      val c = String.implode [chr]
      val searchString =
        if searchCursorIdx = String.size searchString then
          searchString ^ c
        else
          let
            val sub1 = Substring.extract (searchString, 0, SOME searchCursorIdx)
            val sub2 = Substring.full c
            val sub3 = Substring.extract (searchString, searchCursorIdx, NONE)
          in
            Substring.concat [sub1, sub2, sub3]
          end
      val searchCursorIdx = searchCursorIdx + 1

      val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
      val tempSearchList = buildTempSearchList (searchString, buffer, cursorIdx)
    in
      NormalSearchFinish.onSearchChanged
        (app, searchString, tempSearchList, searchCursorIdx, buffer)
    end

  (* return to normal mode, keeping the same searchString and searchList
   * from before entering this mode. *)
  fun exitToNormalMode (app: app_type) =
    let
      val {buffer, cursorIdx, searchList, bufferModifyTime, ...} = app
    in
      NormalFinish.buildTextAndClear
        (app, buffer, cursorIdx, searchList, [], bufferModifyTime)
    end

  (* save search string and tempSearchList and return to normal mode *)
  fun saveSearch (app: app_type, searchString, tempSearchList, time) =
    let
      val
        { buffer
        , cursorIdx
        , windowWidth
        , windowHeight
        , startLine
        , visualScrollColumn
        , ...
        } = app

      val buffer = LineGap.goToStart buffer
      val searchString = EscapeString.unescape searchString
      val initialMsg = [SEARCH (buffer, searchString, time)]

      (* move LineGap to first line displayed on screen *)
      val buffer = LineGap.goToLine (startLine, buffer)

      (* move buffer to new startLine as required by TextBuilder.build *)
      val buffer = LineGap.goToLine (startLine, buffer)

      val drawMsg = NormalModeTextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , tempSearchList
        , searchString
        , visualScrollColumn
        )
      val drawMsg = Vector.concat drawMsg
      val drawMsg = DrawMsg.DRAW_TEXT drawMsg
      val msgs = DRAW drawMsg :: initialMsg

      val mode = NORMAL_MODE ""
    in
      NormalSearchModeWith.returnToNormalMode
        (app, buffer, searchString, tempSearchList, startLine, mode, msgs)
    end

  fun backspace (app: app_type, searchString, tempSearchList, searchCursorIdx) =
    if searchCursorIdx = 0 then
      app
    else
      let
        val searchString =
          if searchCursorIdx = String.size searchString then
            String.substring (searchString, 0, String.size searchString - 1)
          else
            let
              val sub1 = Substring.extract
                (searchString, 0, SOME (searchCursorIdx - 1))
              val sub2 = Substring.extract (searchString, searchCursorIdx, SOME
                (String.size searchString - searchCursorIdx))
            in
              Substring.concat [sub1, sub2]
            end
        val searchCursorIdx = searchCursorIdx - 1

        val {cursorIdx, buffer, ...} = app
        val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
        val tempSearchList =
          buildTempSearchList (searchString, buffer, cursorIdx)
      in
        NormalSearchFinish.onSearchChanged
          (app, searchString, tempSearchList, searchCursorIdx, buffer)
      end

  fun moveLeft (app, searchString, tempSearchList, searchCursorIdx) =
    if searchCursorIdx = 0 then
      app
    else
      let
        val searchCursorIdx = searchCursorIdx - 1
      in
        NormalSearchFinish.onSearchChanged
          (app, searchString, tempSearchList, searchCursorIdx, #buffer app)
      end

  fun moveRight (app, searchString, tempSearchList, searchCursorIdx) =
    if searchCursorIdx = String.size searchString then
      app
    else
      let
        val searchCursorIdx = searchCursorIdx + 1
      in
        NormalSearchFinish.onSearchChanged
          (app, searchString, tempSearchList, searchCursorIdx, #buffer app)
      end

  fun update (app, {searchString, tempSearchList, searchCursorIdx}, msg, time) =
    case msg of
      CHAR_EVENT chr => addChr (app, searchString, searchCursorIdx, chr)
    | KEY_BACKSPACE =>
        backspace (app, searchString, tempSearchList, searchCursorIdx)
    | KEY_ESC => exitToNormalMode app
    | KEY_ENTER => saveSearch (app, searchString, tempSearchList, time)
    | ARROW_LEFT =>
        moveLeft (app, searchString, tempSearchList, searchCursorIdx)
    | ARROW_RIGHT =>
        moveRight (app, searchString, tempSearchList, searchCursorIdx)
    | WITH_SEARCH_LIST (searchList, time) =>
        NormalFinish.withSearchList (app, searchList, time)
    | RESIZE_EVENT (width, height) =>
        NormalSearchFinish.resize
          (app, width, height, searchCursorIdx, tempSearchList)

    (* In Vim's search mode, the up and down arrows can be used 
     * to scroll through the search history. 
     * I don't find this feature too useful as it is often easier to type
     * the whole search string again, so I'm leaving it unimplemented 
     * until/unless I find that I wish this functionality was there
     * while using the program. *)
    | ARROW_UP => app
    | ARROW_DOWN => app
end
