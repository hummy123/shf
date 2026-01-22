structure RgfwLoop =
struct
  fun loop (counter, window) =
    if Rgfw.shouldCloseWindow window then
      Rgfw.closeWindow window
    else
      let val () = print ("counter = " ^ Int.toString counter ^ "\n")
      in loop (counter + 1, window)
      end

  fun main () =
    let
      val () = print "15\n"
      val window = Rgfw.createWindow ("shf", 0, 0, 1920, 1080)
      val () = print "17\n"
    in
      loop (0, window)
    end
end

val _ = RgfwLoop.main ()
