structure Shell =
struct
  open CML

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

      (* upload text vector *)
      val io = TextIO.openIn "fcore/buffer.sml"
      val lineGap = ioToLineGap (io, LineGap.empty)
      val _ = TextIO.closeIn io

      val (textVec, _) = Buffer.startBuildTextLineGap (0, lineGap, 1920, 1080)
      val shellState = GlDraw.create window
      val shellState = GlDraw.uploadText (shellState, textVec)

      val _ = GlDraw.helpLoop shellState
    in
      ()
    end
end

val _ = RunCML.doit (Shell.main, NONE)
