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
    , drawMailbox: DrawMsg.t Mailbox.mbox
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

  fun create (drawMailbox, window) =
    let
      (* create vertex buffer, program, etc. for text. *)
      val textVertexBuffer = Gles3.createBuffer ()
      val xyrgbVertexShader = createShader
        (Gles3.VERTEX_SHADER, GlShaders.xyrgbVertexShaderString)

      val rgbFragmentShader = createShader
        (Gles3.FRAGMENT_SHADER, GlShaders.rgbFragmentShaderString)

      val textProgram = createProgram (xyrgbVertexShader, rgbFragmentShader)

      (* clean up shaders which are no longer needed once progran is linked. *)
      val _ = Gles3.deleteShader xyrgbVertexShader
      val _ = Gles3.deleteShader rgbFragmentShader
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = 0
      , drawMailbox = drawMailbox
      , window = window
      }
    end

  fun uploadText (shellState: t, vec) =
    let
      val
        {textVertexBuffer, textProgram, window, drawMailbox, textDrawLength = _} =
        shellState

      val _ = Gles3.bindBuffer textVertexBuffer
      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newTextDrawLength = Vector.length vec div 5
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , drawMailbox = drawMailbox
      , window = window
      , textDrawLength = newTextDrawLength
      }
    end

  fun drawText ({textVertexBuffer, textProgram, textDrawLength, ...}: t) =
    if textDrawLength > 0 then
      let
        val _ = Gles3.bindBuffer textVertexBuffer
        (* enable xy component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (0, 2, 5, 0)
        val _ = Gles3.enableVertexAttribArray 0
        (* enable rgb component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (1, 3, 5, 8)
        val _ = Gles3.enableVertexAttribArray 1

        val _ = Gles3.useProgram textProgram
        val _ = Gles3.drawArrays (Gles3.TRIANGLES, 0, textDrawLength)
      in
        ()
      end
    else
      ()

  fun draw (drawObject: t) =
    let val _ = drawText drawObject
    in ()
    end


  fun consumeDrawEvent (shellState, msg) =
    let
      val
        {textVertexBuffer, textProgram, window, drawMailbox, textDrawLength = _} =
        shellState
    in
      case msg of REDRAW_TEXT textVec => uploadText (shellState, textVec)
    end

  fun consumeDrawEvents (shellState as {drawMailbox, ...}: t) =
    case Mailbox.recvPoll drawMailbox of
      NONE => shellState
    | SOME msg =>
        let val shellState = consumeDrawEvent (shellState, msg)
        in consumeDrawEvents shellState
        end

  fun helpLoop (shellState as {window, ...}: t) =
    case Glfw.windowShouldClose window of
      false =>
        let
          val shellState = consumeDrawEvents shellState

          val _ = Gles3.clearColor (1.0, 1.0, 1.0, 1.0)
          val _ = Gles3.clear ()

          val _ = draw shellState

          val _ = Glfw.swapBuffers window
          val _ = Glfw.waitEvents ()
        in
          helpLoop shellState
        end
    | true => Glfw.terminate ()

  fun loop (drawMailbox, window) =
    let val shellState = create (drawMailbox, window)
    in helpLoop shellState
    end
end
