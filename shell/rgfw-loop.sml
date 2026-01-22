structure RgfwLoop =
struct
  fun loop window =
    if Rgfw.shouldCloseWindow window then
      Rgfw.closeWindow window
    else
      let 
        val () = Rgfw.swapBuffers window
      in 
        loop window
      end

  fun main () =
    let
      val window = Rgfw.createWindow ("shf", 0, 0, 1920, 1080)
    in
      loop window
    end
end

val _ = RgfwLoop.main ()
