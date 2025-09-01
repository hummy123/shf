structure NormalSearchMode =
struct
  open AppType
  open InputMsg
  open MailboxType

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
      val tempSearchList =
        SearchList.buildRange (buffer, searchString, cursorIdx + 1111)
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
  fun saveSearch (app: app_type, searchString, tempSearchList) =
    let
      val {buffer, cursorIdx, windowWidth, windowHeight, startLine, ...} = app
      val buffer = LineGap.goToStart buffer
      val initialMsg = [SEARCH (buffer, searchString)]

      (* move LineGap to first line displayed on screen *)
      val buffer = LineGap.goToLine (startLine, buffer)

      (* get new startLine which may move screen depending on cursor movements *)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

      (* move buffer to new startLine as required by TextBuilder.build *)
      val buffer = LineGap.goToLine (startLine, buffer)

      val msgs = TextBuilder.build
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , windowHeight
        , tempSearchList
        , searchString
        , initialMsg
        )

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
          SearchList.buildRange (buffer, searchString, cursorIdx + 1111)
      in
        NormalSearchFinish.onSearchChanged
          (app, searchString, tempSearchList, searchCursorIdx, buffer)
      end

  fun update (app, {searchString, tempSearchList, searchCursorIdx}, msg, time) =
    case msg of
      CHAR_EVENT chr => addChr (app, searchString, searchCursorIdx, chr)
    | KEY_BACKSPACE =>
        backspace (app, searchString, tempSearchList, searchCursorIdx)
    | KEY_ESC => exitToNormalMode app
    | KEY_ENTER => saveSearch (app, searchString, tempSearchList)
    | RESIZE_EVENT (width, height) => app
    | WITH_SEARCH_LIST searchList => app
end
