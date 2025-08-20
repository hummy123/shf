structure TestUtils =
struct
  fun init buffer =
    AppType.init (buffer, 0, 0, Time.now ())

  fun update (app, cmd) =
    AppUpdate.update (app, cmd, Time.now ())
end
