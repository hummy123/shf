structure GlDraw =
struct
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
      val shader = Gles3.createShader Gles3.VERTEX_SHADER
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
      (* Set up GLFW. *)
      val _ = Glfw.init ()
      val _ = Glfw.windowHint (Glfw.CONTEXT_VERSION_MAJOR (), 3)
      val _ = Glfw.windowHint (Glfw.DEPRECATED (), Glfw.FALSE ())
      val _ = Glfw.windowHint (Glfw.SAMPLES (), 4)
      val window = Glfw.createWindow (1600, 900, "shf")
      val _ = Glfw.makeContextCurrent window
      val _ = Gles3.loadGlad ()

    (* todo: create vertex buffer, program, etc. for text. *)
    in
      {window = window}
    end
end
