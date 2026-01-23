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
  val enableVsync =
    _import "enableVsync" public : window -> unit;
  val pollEvents =
    _import "pollEvents" public reentrant : unit -> unit;

  val writeClipboard =
    _import "writeClipboard" public : string * int -> unit;

  val exportEscapeCallback =
    _export "mltonEscape" public : (unit -> unit) -> unit;
  val exportBackspaceCallback =
    _export "mltonBackspace" public : (unit -> unit) -> unit;
  val exportEnterCallback =
    _export "mltonEnter" public : (unit -> unit) -> unit;
  val exportCharCallback =
    _export "mltonChar" public : (char -> unit) -> unit;
  val setKeyCallback =
    _import "setKeyCallback" public : unit -> unit;
    
  val exportResizeCallback =
    _export "mltonResize" public : (int * int -> unit) -> unit;
  val setResizeCallback =
    _import "setResizeCallback" public : unit -> unit;
end
