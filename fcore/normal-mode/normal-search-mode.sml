structure NormalSearchMode =
struct
  open AppType
  open InputMsg

  fun update (app, searchString, msg, time) =
    case msg of
      CHAR_EVENT chrCmd => app
    | KEY_ESC => Finish.clearMode app
    | RESIZE_EVENT (width, height) => app
    | WITH_SEARCH_LIST searchList => app
end
