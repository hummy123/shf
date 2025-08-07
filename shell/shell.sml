structure Shell =
struct
  open CML
  open InputMsg

  (* create mailboxes for CML communication *)
  val inputMailbox = Mailbox.mailbox ()
  val drawMailbox = Mailbox.mailbox ()
  val searchMailbox = Mailbox.mailbox ()

  fun frameBufferSizeCallback (width, height) =
    Mailbox.send (inputMailbox, RESIZE_EVENT (width, height))

  fun charCallback word =
    let
      val word = Word32.toInt word
      val chr = Char.chr word
    in
      Mailbox.send (inputMailbox, CHAR_EVENT chr)
    end

  fun keyCallback (key, scancode, action, mods) =
    let
      open Input
    in
      if key = KEY_ESC andalso action = PRESS andalso mods = 0 then
        Mailbox.send (inputMailbox, InputMsg.KEY_ESC)
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

  fun ioToLineGap (io, acc) =
    case TextIO.inputLine io of
      SOME str => ioToLineGap (io, LineGap.append (str, acc))
    | NONE => LineGap.goToStart acc

  fun main () =
    let
      (* Set up GLFW. *)
      val _ = Glfw.init ()
      val _ = Glfw.windowHint (Glfw.CONTEXT_VERSION_MAJOR (), 3)
      val _ = Glfw.windowHint (Glfw.DEPRECATED (), Glfw.FALSE ())
      val _ = Glfw.windowHint (Glfw.SAMPLES (), 4)
      val window = Glfw.createWindow (1920, 1080, "shf")
      val _ = Glfw.makeContextCurrent window
      val _ = Gles3.loadGlad ()

      (* load file intol gap buffer and create initial app *)
      val io = TextIO.openIn "temp.txt"
      val lineGap = ioToLineGap (io, LineGap.empty)
      val _ = TextIO.closeIn io
      val app = AppType.init (lineGap, 1920, 1080)

      (* todo: remove temp line below which tests search list *)
      val app =
        let
          val buffer = #buffer app
          val buffer = LineGap.goToEnd buffer
          val searchString = "val "
          val searchList = SearchList.build (buffer, searchString)
          val buffer = LineGap.goToStart buffer
        in
          AppWith.searchList (app, searchList, buffer, searchString)
        end

      val () = registerCallbacks window

      val _ = CML.spawn (fn () => GlDraw.loop (drawMailbox, window))
      val _ = CML.spawn (fn () =>
        UpdateThread.loop (app, inputMailbox, drawMailbox, searchMailbox))
      val _ = CML.spawn (fn () =>
        SearchThread.loop (searchMailbox, inputMailbox))
    in
      ()
    end
end

val _ = RunCML.doit (Shell.main, NONE)
