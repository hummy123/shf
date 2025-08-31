structure AppUpdate =
struct
  open AppType

  fun update (app: app_type, msg, time) =
    case #mode app of
      NORMAL_MODE str => NormalMode.update (app, str, msg, time)
    | NORMAL_SEARCH_MODE str => NormalSearchMode.update (app, str, msg, time)
end
