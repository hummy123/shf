structure NormalSearchMode =
struct
  open AppType
  open InputMsg

  (* todo: redraw based on results of tempSearchList *)
  fun parseChr (app: app_type, searchString, chr) =
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
      AppWith.mode (app, mode, [])
    end

  (* todo: switch to normal mode, save tempSearchList and searchString,
   * and redraw *)
  fun finishSearch (app: app_type, searchString, tempSearchList) = app

  fun update (app, {searchString, tempSearchList}, msg, time) =
    case msg of
      CHAR_EVENT chr => parseChr (app, searchString, chr)
    | KEY_ESC => Finish.clearMode app
    | KEY_ENTER => finishSearch (app, searchString, tempSearchList)
    | RESIZE_EVENT (width, height) => app
    | WITH_SEARCH_LIST searchList => app
end
