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

  fun updateMany (app, str) =
    let
      fun loop (pos, app) =
        if pos = String.size str then
          app
        else
          let
            val chr = String.sub (str, pos)
            val chr = InputMsg.CHAR_EVENT chr
            val app = update (app, chr)
          in
            loop (pos + 1, app)
          end
    in
      loop (0, app)
    end
end
