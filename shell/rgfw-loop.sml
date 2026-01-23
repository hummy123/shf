structure RgfwLoop =
struct
  fun loop (window, app) =
    if Rgfw.shouldCloseWindow window then
      Rgfw.closeWindow window
    else
      let
        val _ = Gles3.clearColor (0.89, 0.89, 0.89, 1.0)
        val _ = Gles3.clear ()
        val () = Rgfw.swapBuffers window
      in
        loop (window, app)
      end

  local
    fun loop (io, acc, lastCharWasNewline) =
      case TextIO.inputLine io of
        SOME str =>
          let
            val endsWithNewline =
              String.size str > 0
              andalso String.sub (str, String.size str - 1) = #"\n"
          in
            loop (io, LineGap.append (str, acc), endsWithNewline)
          end
      | NONE =>
          if lastCharWasNewline then
            LineGap.goToStart acc
          else
            let val acc = LineGap.append ("\n", acc)
            in LineGap.goToStart acc
            end
  in
    fun ioToLineGap (io, acc) = loop (io, acc, false)
  end

  fun main () =
    let
      val window = Rgfw.createWindow ("shf", 0, 0, 1920, 1080)
      val () = Gles3.enableDepthTest ()

      (* load file intol gap buffer and create initial app *)
      val io = TextIO.openIn "temp.txt"
      val lineGap = ioToLineGap (io, LineGap.empty)
      val _ = TextIO.closeIn io
      val app = AppType.init (lineGap, 1920, 1080, Time.now ())
    in
      loop (window, app)
    end
end

val _ = RgfwLoop.main ()
