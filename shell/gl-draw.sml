structure GlDraw =
struct
  open DrawMsg

  type t =
    { textVertexBuffer: Word32.word
    , textProgram: Word32.word
    , textDrawLength: int
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

  fun create () =
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
      }
    end

  fun uploadText (drawState: t, vec) =
    let
      val {textVertexBuffer, textProgram, textDrawLength = _} = drawState

      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newTextDrawLength = Vector.length vec div 6
    in
      { textVertexBuffer = textVertexBuffer
      , textProgram = textProgram
      , textDrawLength = newTextDrawLength
      }
    end

  fun draw (drawObject: t) =
    let val {textVertexBuffer, textDrawLength, textProgram} = drawObject
    in Gles3.drawArrays (Gles3.TRIANGLES, 0, textDrawLength)
    end
end
