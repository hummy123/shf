structure GlfwLoop =
struct
  open DrawMsg

  fun yank (window, str) =
    let
      (* print when text is yanked
       * because GLFW currently has a bug on Wayland
       * when setting the clipboard string *)
      val msg = "|" ^ String.toCString str ^ "|\n"
      val () = print msg
      val () = Glfw.setClipboardString (window, str)
    in
      ()
    end

  fun consumeEvent (drawState, window, msg) =
    let
      val {textVertexBuffer, textProgram, textDrawLength = _, ...} = drawState
    in
      case msg of
        DRAW_TEXT textVec => GlDraw.uploadText (drawState, textVec)
      | YANK str => (yank (window, str); drawState)
    end

  fun consumeEventsLoop (pos, msgVec, drawState, window) =
    if pos = Vector.length msgVec then
      drawState
    else
      let
        val msg = Vector.sub (msgVec, pos)
        val drawState = consumeEvent (drawState, window, msg)
      in
        consumeEventsLoop (pos + 1, msgVec, drawState, window)
      end

  fun consumeEvents (drawState, window) =
    consumeEventsLoop (0, DrawMailbox.getMessagesAndClear (), drawState, window)

  fun updateLoop (pos, msgVec, app) =
    if pos = Vector.length msgVec then
      app
    else
      let
        val msg = Vector.sub (msgVec, pos)
        val app = Updater.update (app, msg)
      in
        updateLoop (pos + 1, msgVec, app)
      end

  fun update app =
    updateLoop (0, InputMailbox.getMessagesAndClear (), app)

  fun helpLoop (app, drawState, window, gamepad) =
    case Glfw.windowShouldClose window of
      false =>
        let
          val drawState = consumeEvents (drawState, window)

          val _ = Gles3.clearColor (0.89, 0.89, 0.89, 1.0)
          val _ = Gles3.clear ()

          (* one update reacting to gamepad events *)
          val (gamepad, actions) = GlfwGamepad.query gamepad
          val app = updateLoop (0, Vector.fromList actions, app)

          (* one update reacting to keyboard events *)
          val app = update app
          val _ = GlDraw.draw drawState

          val _ = Glfw.swapBuffers window
          val _ = Glfw.waitEvents ()
        in
          helpLoop (app, drawState, window, gamepad)
        end
    | true => Glfw.terminate ()

  fun loop (app, window) =
    let
      val drawState = GlDraw.create ()

      val gamepad: GlfwGamepad.gamepad_state =
        { mode = GlfwGamepad.PENDING
        , shiftChr = false
        , trianglePressed = false
        , circlePressed = false
        , crossPressed = false
        , squarePressed = false
        }
    in
      helpLoop (app, drawState, window, gamepad)
    end

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
      val _ = Glfw.loadGlad ()
      val _ = Gles3.enableDepthTest ()

      (* load file intol gap buffer and create initial app *)
      val io = TextIO.openIn "temp.txt"
      val lineGap = ioToLineGap (io, LineGap.empty)
      val _ = TextIO.closeIn io
      val app = AppType.init (lineGap, 1920, 1080, Time.now ())

      val () = registerCallbacks window
    in
      loop (app, window)
    end
end

val _ = GlfwLoop.main ()
