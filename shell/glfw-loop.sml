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
end
