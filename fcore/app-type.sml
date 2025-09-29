structure AppType =
struct
  datatype mode =
    NORMAL_MODE of string
  | NORMAL_SEARCH_MODE of
      { searchString: string
      , tempSearchList: PersistentVector.t
      , searchCursorIdx: int
      , searchScrollColumn: int
      }

  type app_type =
    { mode: mode
    , buffer: LineGap.t
    , bufferModifyTime: Time.time
    , searchList: PersistentVector.t
    , searchString: string
    , windowWidth: int
    , windowHeight: int
    (* line to start drawing from *)
    , startLine: int
    (* absolute index of movable cursor *)
    , cursorIdx: int
    (* column to start drawing text at for horizontal scrolling. *)
    , visualScrollColumn: int
    (* msgs to send after an update.
     * The list of messages is reset on each invocation of AppUpdate.update. *)
    , msgs: MailboxType.t list
    }

  fun init (buffer, windowWidth, windowHeight, time) : app_type =
    { mode = NORMAL_MODE ""
    , buffer = buffer
    , bufferModifyTime = time
    , searchList = SearchList.empty
    , searchString = ""
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    , startLine = 0
    , cursorIdx = 0
    , visualScrollColumn = 0
    , msgs = []
    }
end
