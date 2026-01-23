structure RgfwLoop =
struct
  fun yank string =
    Rgfw.writeClipboard (string, String.size string)

  fun consumeEvent (drawState, window, msg) =
    let
      open DrawMsg

      val {textVertexBuffer, textProgram, textDrawLength = _, ...} = drawState
    in
      case msg of
        DRAW_TEXT textVec => GlDraw.uploadText (drawState, textVec)
      | YANK str => (yank str; drawState)
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

  fun loop (window, app, drawState) =
    if Rgfw.shouldCloseWindow window then
      Rgfw.closeWindow window
    else
      let
        val _ = Gles3.clearColor (0.89, 0.89, 0.89, 1.0)
        val _ = Gles3.clear ()

        val app = Updater.update app

        val () = GlDraw.draw drawState
        val () = Rgfw.swapBuffers window
      in
        loop (window, app, drawState)
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
      val drawState = GlDraw.create ()
    in
      loop (window, app, drawState)
    end
end

val _ = RgfwLoop.main ()
