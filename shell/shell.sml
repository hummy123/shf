structure Shell =
struct
  open CML

  fun loop window =
    case Glfw.windowShouldClose window of
      false =>
        let
          val _ = Gles3.clearColor (0.1, 0.1, 0.1, 0.1)
          val _ = Gles3.clear ()

          val _ = Glfw.swapBuffers window
          val _ = Glfw.waitEvents ()
        in
          loop window
        end
    | true => Glfw.terminate ()

  fun main () =
    let
      (* Set up GLFW. *)
      val _ = Glfw.init ()
      val _ = Glfw.windowHint (Glfw.CONTEXT_VERSION_MAJOR (), 3)
      val _ = Glfw.windowHint (Glfw.DEPRECATED (), Glfw.FALSE ())
      val _ = Glfw.windowHint (Glfw.SAMPLES (), 4)
      val window = Glfw.createWindow (1600, 900, "shf")
      val _ = Glfw.makeContextCurrent window
      val _ = Gles3.loadGlad ()
    in
      loop window
    end
end

val _ = RunCML.doit (Shell.main, NONE)
