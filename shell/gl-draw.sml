structure GlDraw =
struct
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
      val xyzRgbVertexShader = createShader
        (Gles3.VERTEX_SHADER, GlShaders.xyzRgbVertexShaderString)

      val rgbFragmentShader = createShader
        (Gles3.FRAGMENT_SHADER, GlShaders.rgbFragmentShaderString)

      val textProgram = createProgram (xyzRgbVertexShader, rgbFragmentShader)

      (* clean up shaders which are no longer needed once progran is linked. *)
      val _ = Gles3.deleteShader xyzRgbVertexShader
      val _ = Gles3.deleteShader rgbFragmentShader

      (* because we only have a single vertex buffer, 
       * we only need to bind and set attributes once. *)
      val _ = Gles3.bindBuffer textVertexBuffer

      (* enable xyz component from uploaded array *)
      val _ = Gles3.vertexAttribPointer (0, 3, 6, 0)
      val _ = Gles3.enableVertexAttribArray 0
      (* enable rgb component from uploaded array *)
      val _ = Gles3.vertexAttribPointer (1, 3, 6, 12)
      val _ = Gles3.enableVertexAttribArray 1

      val _ = Gles3.useProgram textProgram
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = 0
      , window = window
      }
    end

  fun uploadText (shellState: t, vec) =
    let
      val {textVertexBuffer, textProgram, textDrawLength = _, window} =
        shellState

      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newTextDrawLength = Vector.length vec div 6
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = newTextDrawLength
      , window = window
      }
    end

  fun draw (drawObject: t) =
    let
      val {textVertexBuffer, textDrawLength, textProgram, window = _} =
        drawObject
    in
      Gles3.drawArrays (Gles3.TRIANGLES, 0, textDrawLength)
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
        DRAW_TEXT textVec => uploadText (shellState, textVec)
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

          val _ = Gles3.clearColor (0.89, 0.89, 0.89, 1.0)
          val _ = Gles3.clear ()

          val () = GlfwGamepad.query ()
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
