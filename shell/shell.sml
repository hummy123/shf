structure Shell =
struct
  open InputMsg

  fun frameBufferSizeCallback (width, height) =
    InputMailbox.append (RESIZE_EVENT (width, height))

  fun charCallback word =
    let
      val word = Word32.toInt word
      val chr = Char.chr word
    in
      InputMailbox.append (CHAR_EVENT chr)
    end

  fun keyCallback (key, scancode, action, mods) =
    let
      open Input
    in
      if key = KEY_ESC andalso action = PRESS andalso mods = 0 then
        InputMailbox.append (InputMsg.KEY_ESC)
      else if key = KEY_ENTER andalso action = PRESS andalso mods = 0 then
        InputMailbox.append (InputMsg.KEY_ENTER)
      else if key = KEY_BACKSPACE andalso action <> RELEASE andalso mods = 0 then
        InputMailbox.append (InputMsg.KEY_BACKSPACE)
      else if key = KEY_ARROW_LEFT andalso action <> RELEASE andalso mods = 0 then
        InputMailbox.append (InputMsg.ARROW_LEFT)
      else if key = KEY_ARROW_RIGHT andalso action <> RELEASE andalso mods = 0 then
        InputMailbox.append (InputMsg.ARROW_RIGHT)
      else if key = KEY_ARROW_UP andalso action <> RELEASE andalso mods = 0 then
        InputMailbox.append (InputMsg.ARROW_UP)
      else if key = KEY_ARROW_DOWN andalso action <> RELEASE andalso mods = 0 then
        InputMailbox.append (InputMsg.ARROW_DOWN)
      else
        ()
    end

  fun registerCallbacks window =
    let
      val () = Input.exportFramebufferSizeCallback frameBufferSizeCallback
      val () = Input.setFramebufferSizeCallback window

      val () = Input.exportCharCallback charCallback
      val () = Input.setCharCallback window

      val () = Input.exportKeyCallback keyCallback
      val () = Input.setKeyCallback window
    in
      ()
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
      (* Set up GLFW. *)
      val _ = Glfw.init ()
      val _ = Glfw.windowHint (Glfw.CONTEXT_VERSION_MAJOR (), 3)
      val _ = Glfw.windowHint (Glfw.DEPRECATED (), Glfw.FALSE ())
      val window = Glfw.createWindow (1920, 1080, "shf")
      val _ = Glfw.makeContextCurrent window
      val _ = Gles3.loadGlad ()

      (* load file intol gap buffer and create initial app *)
      val io = TextIO.openIn "temp.txt"
      val lineGap = ioToLineGap (io, LineGap.empty)
      val _ = TextIO.closeIn io
      val app = AppType.init (lineGap, 1920, 1080, Time.now ())

      val () = registerCallbacks window
    in
      GlDraw.loop (app, window)
    end
end

val () = Shell.main ()
