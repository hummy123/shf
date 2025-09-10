structure GlDraw =
struct
  open CML
  open DrawMsg
  (* The name doesn't make it clear, but this structure
   * couples GLFW and OpenGL.
   * I'm not sure if I will use native windowing systems
   * or other graphics APIs at a later stage,
   * but the current priority is GLFW + OpenGL. 
   * *)
  type t =
    { textVertexBuffer: Word32.word
    , textProgram: Word32.word
    , textDrawLength: int

    , cursorVertexBuffer: Word32.word
    , cursorProgram: Word32.word
    , cursorDrawLength: int

    , bgVertexBuffer: Word32.word
    , bgProgram: Word32.word
    , bgDrawLength: int

    , window: MLton.Pointer.t
    }

  fun createShader (shaderType, shaderString) =
    let
      val shader = Gles3.createShader shaderType
      val _ = Gles3.shaderSource (shader, shaderString)
      val _ = Gles3.compileShader shader
    in
      shader
    end

  fun createProgram (vertexShader, fragmentShader) =
    let
      val program = Gles3.createProgram ()
      val _ = Gles3.attachShader (program, vertexShader)
      val _ = Gles3.attachShader (program, fragmentShader)
      val _ = Gles3.linkProgram program
    in
      program
    end

  fun create window =
    let
      (* create vertex buffer, program, etc. for text. *)
      val textVertexBuffer = Gles3.createBuffer ()
      val xyrgbVertexShader = createShader
        (Gles3.VERTEX_SHADER, GlShaders.xyrgbVertexShaderString)

      val rgbFragmentShader = createShader
        (Gles3.FRAGMENT_SHADER, GlShaders.rgbFragmentShaderString)

      val textProgram = createProgram (xyrgbVertexShader, rgbFragmentShader)

      (* create cursor buffer, program, etc. *)
      val cursorVertexBuffer = Gles3.createBuffer ()
      val cursorProgram = createProgram (xyrgbVertexShader, rgbFragmentShader)

      (* create background buffer and program *)
      val bgVertexBuffer = Gles3.createBuffer ()
      val bgProgram = createProgram (xyrgbVertexShader, rgbFragmentShader)

      (* clean up shaders which are no longer needed once progran is linked. *)
      val _ = Gles3.deleteShader xyrgbVertexShader
      val _ = Gles3.deleteShader rgbFragmentShader
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = 0

      , cursorVertexBuffer = cursorVertexBuffer
      , cursorProgram = cursorProgram
      , cursorDrawLength = 0

      , bgVertexBuffer = bgVertexBuffer
      , bgProgram = bgProgram
      , bgDrawLength = 0

      , window = window
      }
    end

  fun uploadText (shellState: t, vec) =
    let
      val
        { textVertexBuffer
        , textProgram
        , textDrawLength = _
        , cursorVertexBuffer
        , cursorProgram
        , cursorDrawLength
        , bgVertexBuffer
        , bgProgram
        , bgDrawLength
        , window
        } = shellState

      val _ = Gles3.bindBuffer textVertexBuffer
      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newTextDrawLength = Vector.length vec div 5
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = newTextDrawLength
      , cursorVertexBuffer = cursorVertexBuffer
      , cursorProgram = cursorProgram
      , cursorDrawLength = cursorDrawLength
      , bgVertexBuffer = bgVertexBuffer
      , bgProgram = bgProgram
      , bgDrawLength = bgDrawLength
      , window = window
      }
    end

  fun uploadCursor (shellState: t, vec) =
    let
      val
        { textVertexBuffer
        , textProgram
        , textDrawLength
        , cursorVertexBuffer
        , cursorProgram
        , cursorDrawLength = _
        , bgVertexBuffer
        , bgProgram
        , bgDrawLength
        , window
        } = shellState

      val _ = Gles3.bindBuffer cursorVertexBuffer
      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newCursorDrawLength = Vector.length vec div 5
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = textDrawLength
      , cursorVertexBuffer = cursorVertexBuffer
      , cursorProgram = cursorProgram
      , cursorDrawLength = newCursorDrawLength
      , bgVertexBuffer = bgVertexBuffer
      , bgProgram = bgProgram
      , bgDrawLength = bgDrawLength
      , window = window
      }
    end

  fun uploadBg (shellState: t, vec) =
    let
      val
        { textVertexBuffer
        , textProgram
        , textDrawLength
        , cursorVertexBuffer
        , cursorProgram
        , cursorDrawLength
        , bgVertexBuffer
        , bgProgram
        , bgDrawLength = _
        , window
        } = shellState

      val _ = Gles3.bindBuffer bgVertexBuffer
      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newBgDrawLength = Vector.length vec div 5
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = textDrawLength
      , cursorVertexBuffer = cursorVertexBuffer
      , cursorProgram = cursorProgram
      , cursorDrawLength = cursorDrawLength
      , bgVertexBuffer = bgVertexBuffer
      , bgProgram = bgProgram
      , bgDrawLength = newBgDrawLength
      , window = window
      }
    end

  fun drawXyrgb (vertexBuffer, program, drawLength) =
    if drawLength > 0 then
      let
        val _ = Gles3.bindBuffer vertexBuffer
        (* enable xy component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (0, 2, 5, 0)
        val _ = Gles3.enableVertexAttribArray 0
        (* enable rgb component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (1, 3, 5, 8)
        val _ = Gles3.enableVertexAttribArray 1

        val _ = Gles3.useProgram program
        val _ = Gles3.drawArrays (Gles3.TRIANGLES, 0, drawLength)
      in
        ()
      end
    else
      ()

  fun draw (drawObject: t) =
    let
      val
        { textVertexBuffer
        , textDrawLength
        , textProgram
        , cursorVertexBuffer
        , cursorDrawLength
        , cursorProgram
        , bgVertexBuffer
        , bgProgram
        , bgDrawLength
        , ...
        } = drawObject

      val _ = drawXyrgb (bgVertexBuffer, bgProgram, bgDrawLength)
      val _ = drawXyrgb (cursorVertexBuffer, cursorProgram, cursorDrawLength)
      val _ = drawXyrgb (textVertexBuffer, textProgram, textDrawLength)
    in
      ()
    end

  fun yank (shellState: t, str) =
    let
      (* print when text is yanked *)
      val msg = "|" ^ String.toCString str ^ "|\n"
      val () = print msg
      val () = Glfw.setClipboardString (#window shellState, str)
    in
      shellState
    end

  fun consumeDrawEvent (shellState, msg) =
    let
      val {textVertexBuffer, textProgram, window, textDrawLength = _, ...} =
        shellState
    in
      case msg of
        REDRAW_TEXT textVec => uploadText (shellState, textVec)
      | REDRAW_CURSOR cursorVec => uploadCursor (shellState, cursorVec)
      | REDRAW_BG bgVec => uploadBg (shellState, bgVec)
      | YANK str => yank (shellState, str)
    end

  local
    fun loop (pos, msgVec, shellState) =
      if pos = Vector.length msgVec then
        shellState
      else
        let
          val msg = Vector.sub (msgVec, pos)
          val shellState = consumeDrawEvent (shellState, msg)
        in
          loop (pos + 1, msgVec, shellState)
        end
  in
    fun consumeDrawEvents shellState =
      loop (0, DrawMailbox.getMessagesAndClear (), shellState)
  end

  local
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
  in
    fun update app =
      updateLoop (0, InputMailbox.getMessagesAndClear (), app)
  end

  fun helpLoop (app, shellState as {window, ...}: t) =
    case Glfw.windowShouldClose window of
      false =>
        let
          val shellState = consumeDrawEvents shellState

          val _ = Gles3.clearColor (0.087, 0.095, 0.13, 1.0)
          val _ = Gles3.clear ()

          val app = update app
          val _ = draw shellState

          val _ = Glfw.swapBuffers window
          val _ = Glfw.waitEvents ()
        in
          helpLoop (app, shellState)
        end
    | true => Glfw.terminate ()

  fun loop (app, window) =
    let val shellState = create window
    in helpLoop (app, shellState)
    end
end
