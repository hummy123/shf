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
  val pollEvents =
    _import "pollEvents" public reentrant : unit -> unit;

  val writeClipboard =
    _import "writeClipboard" public : string * int -> unit;

  val exportEscapeCallback =
    _export "mltonEscape" public : (unit -> unit) -> unit;
  val setKeyCallback =
    _import "setKeyCallback" public : unit -> unit;
end
