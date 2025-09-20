structure TestUtils =
struct
  fun withUnixLineEndings str =
    if String.size str > 0 andalso String.sub (str, String.size str - 1) = #"\n" then
      str
    else
      str ^ "\n"

  fun init bufferString =
    let
      val bufferString = withUnixLineEndings bufferString
      val buffer = LineGap.fromString bufferString
    in
      AppType.init (buffer, 0, 0, Time.now ())
    end

  fun update (app, cmd) =
    AppUpdate.update (app, cmd, Time.now ())
end
