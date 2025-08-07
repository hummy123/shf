structure AppUpdate =
struct
  open AppType

  fun update (app, msg) =
    case #mode app of NORMAL_MODE str => NormalMode.update (app, str, msg)
end
