structure Shell =
struct
  open CML

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

      val _ = GlDraw.loop window
    in
      ()
    end
end

val _ = RunCML.doit (Shell.main, NONE)
