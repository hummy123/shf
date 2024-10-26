signature APP_TYPE =
sig
  datatype mode = 
    NORMAL_MODE of string

  type app_type =
    { mode: mode
    , buffer: LineGap.t
    , windowWidth: int
    , windowHeight: int
    (* line to start drawing from *)
    , startLine: int
    (* absolute index of movable cursor *)
    , cursorIdx: int
    }

  val init: LineGap.t * int * int -> app_type
end

structure AppType :> APP_TYPE =
struct
  datatype mode =
    NORMAL_MODE of string

  type app_type =
    { mode: mode
    , buffer: LineGap.t
    , windowWidth: int
    , windowHeight: int
    (* line to start drawing from *)
    , startLine: int
    (* absolute index of movable cursor *)
    , cursorIdx: int
    }

  fun init (buffer, windowWidth, windowHeight) : app_type =
    { mode = NORMAL_MODE ""
    , buffer = buffer
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    , startLine = 0
    , cursorIdx = 0
    }
end
