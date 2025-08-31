structure NormalSearchMode =
struct
  open AppType
  open InputMsg
  open MailboxType

  (* todo: redraw based on results of tempSearchList *)
  fun addChr (app: app_type, searchString, chr) =
    let
      val c = String.implode [chr]
      val searchString = searchString ^ c

      val {buffer, cursorIdx, ...} = app
      val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
      val tempSearchList =
        SearchList.buildRange (buffer, searchString, cursorIdx + 1111)

      val mode =
        NORMAL_SEARCH_MODE
          {searchString = searchString, tempSearchList = tempSearchList}
    in
      NormalModeWith.mode (app, mode, [])
    end

  (* save search string and tempSearchList and return to normal mode *)
  fun finishSearch (app: app_type, searchString, tempSearchList) = 
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
        ( app
        , buffer
        , searchString
        , tempSearchList
        , startLine
        , mode
        , msgs
        )
    end

  fun update (app, {searchString, tempSearchList}, msg, time) =
    case msg of
      CHAR_EVENT chr => addChr (app, searchString, chr)
    | KEY_ESC => NormalFinish.clearMode app
    | KEY_ENTER => finishSearch (app, searchString, tempSearchList)
    | RESIZE_EVENT (width, height) => app
    | WITH_SEARCH_LIST searchList => app
end
