structure NormalSearchMode =
struct
  open AppType
  open InputMsg
  open MailboxType

  fun onSearchChanged (app: app_type, searchString, tempSearchList, buffer) =
    let
      open DrawMsg

      val {buffer, cursorIdx, startLine, windowWidth, windowHeight, ...} = app
      val mode =
        NORMAL_SEARCH_MODE
          {searchString = searchString, tempSearchList = tempSearchList}

      val floatWindowWidth = Real32.fromInt windowWidth
      val floatWindowHeight = Real32.fromInt windowHeight

      val searchStringPosY = windowHeight - TextConstants.ySpace - 5

      val initialTextAcc = TextBuilder.buildLineToList
        ( searchString
        , 5
        , searchStringPosY
        , windowWidth
        , floatWindowWidth
        , floatWindowHeight
        )

      val buffer = LineGap.goToLine (startLine, buffer)
      val startLine = TextWindow.getStartLine
        (buffer, startLine, cursorIdx, windowWidth, windowHeight)

      val remainingWindowHeight = windowHeight - (TextConstants.ySpace * 2)

      val msgs = TextBuilder.buildWithExisting
        ( startLine
        , cursorIdx
        , buffer
        , windowWidth
        , remainingWindowHeight
        , floatWindowWidth
        , floatWindowHeight
        , tempSearchList
        , searchString
        , []
        , initialTextAcc
        , []
        )
    in
      NormalSearchModeWith.changeTempSearchString
        (app, buffer, startLine, mode, msgs)
    end

  fun addChr (app: app_type, searchString, chr) =
    let
      val {cursorIdx, buffer, ...} = app

      val c = String.implode [chr]
      val searchString = searchString ^ c

      val buffer = LineGap.goToIdx (cursorIdx - 1111, buffer)
      val tempSearchList =
        SearchList.buildRange (buffer, searchString, cursorIdx + 1111)
    in
      onSearchChanged (app, searchString, tempSearchList, buffer)
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

  fun update (app, {searchString, tempSearchList}, msg, time) =
    case msg of
      CHAR_EVENT chr => addChr (app, searchString, chr)
    | KEY_ESC => exitToNormalMode app
    | KEY_ENTER => saveSearch (app, searchString, tempSearchList)
    | RESIZE_EVENT (width, height) => app
    | WITH_SEARCH_LIST searchList => app
end
