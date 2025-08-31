structure AppUpdate =
struct
  open AppType

  fun update (app: app_type, msg, time) =
    case #mode app of
      NORMAL_MODE modeData => NormalMode.update (app, modeData, msg, time)
    | NORMAL_SEARCH_MODE modeData =>
        NormalSearchMode.update (app, modeData, msg, time)
end
