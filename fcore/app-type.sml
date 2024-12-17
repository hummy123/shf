signature APP_TYPE =
sig
  datatype mode = NORMAL_MODE of string

  type app_type =
    { mode: mode
    , buffer: LineGap.t
    , searchList: SearchList.t
    , searchString: string
    , windowWidth: int
    , windowHeight: int
    (* line to start drawing from *)
    , startLine: int
    (* absolute index of movable cursor *)
    , cursorIdx: int
    (* msgs to send after an update.
     * The list of messages is reset on each invocation of AppUpdate.update. *)
    , msgs: MailboxType.t list

    }

  val init: LineGap.t * int * int -> app_type
end

structure AppType :> APP_TYPE =
struct
  datatype mode = NORMAL_MODE of string

  type app_type =
    { mode: mode
    , buffer: LineGap.t
    , searchList: SearchList.t
    , searchString: string
    , windowWidth: int
    , windowHeight: int
    (* line to start drawing from *)
    , startLine: int
    (* absolute index of movable cursor *)
    , cursorIdx: int
    (* msgs to send after an update.
     * The list of messages is reset on each invocation of AppUpdate.update. *)
    , msgs: MailboxType.t list
    }

  fun init (buffer, windowWidth, windowHeight) : app_type =
    { mode = NORMAL_MODE ""
    , buffer = buffer
    , searchList = SearchList.empty
    , searchString = ""
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    , startLine = 0
    , cursorIdx = 0
    , msgs = []
    }
end
