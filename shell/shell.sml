structure Shell =
struct
  open CML

  fun ioToString (io, acc) =
    case TextIO.inputLine io of
      SOME str => ioToString (io, acc ^ str)
    | NONE => acc

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

      (* upload text vector *)
      val io = TextIO.openIn "fcore/buffer.sml"
      val str = ioToString (io, "")
      val lineGap = LineGap.fromString str
      val _ = TextIO.closeIn io

      val textVec = Buffer.startBuildTextLineGap (0, lineGap, 1920, 1080)
      val shellState = GlDraw.create window
      val shellState = GlDraw.uploadText (shellState, textVec)

      val _ = GlDraw.helpLoop shellState
    in
      ()
    end
end

val _ = RunCML.doit (Shell.main, NONE)
