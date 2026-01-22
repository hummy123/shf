structure Rgfw =
struct
  type window = MLton.Pointer.t

  (* RGFW functions. *)
  val createWindow = 
    _import "createWindow" public : string * int * int * int * int -> window;
  val closeWindow =
    _import "closeWindow" public : window -> unit;
  val shouldCloseWindow =
    _import "shouldCloseWindow" public : window -> bool;
  val swapBuffers =
    _import "swapBuffers" public : window -> unit;
end
