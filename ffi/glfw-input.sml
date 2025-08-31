structure Input =
struct
  type window = MLton.Pointer.t

  (* Constants. *)
  val (PRESS, _) =
    _symbol "PRESS" public : ( unit -> int ) * ( int -> unit );
  val PRESS = PRESS ()

  val (REPEAT, _) =
    _symbol "REPEAT" public : ( unit -> int ) * ( int -> unit );
  val REPEAT = REPEAT ()

  val (RELEASE, _) =
    _symbol "RELEASE" public : ( unit -> int ) * ( int -> unit );
  val RELEASE = RELEASE ()

  val exportFramebufferSizeCallback =
    _export "mltonFramebufferSizeCallback" public : (int * int -> unit) -> unit;
  val setFramebufferSizeCallback =
    _import "setFramebufferSizeCallback" public : window -> unit;

  val exportCharCallback =
    _export "mltonCharCallback" public : (Word32.word -> unit) -> unit;
  val setCharCallback =
    _import "setCharCallback" public : window -> unit;

  val exportKeyCallback =
    _export "mltonKeyCallback" public : (int * int * int * int -> unit) -> unit;
  val setKeyCallback =
    _import "setKeyCallback" public : window -> unit;

  val (KEY_ESC, _) =
    _symbol "KEY_ESC" public : ( unit -> int ) * ( int -> unit );
  val KEY_ESC = KEY_ESC ()
  val (KEY_ENTER, _) =
    _symbol "KEY_ENTER" public : ( unit -> int ) * ( int -> unit );
  val KEY_ENTER = KEY_ENTER ()
end
