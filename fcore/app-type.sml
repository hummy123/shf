structure AppType =
struct
  type app_type =
    { buffer: LineGap.t
    , windowWidth: int
    , windowHeight: int
    (* line to start drawing from *)
    , startLine: int
    (* absolute index of movable cursor *)
    , cursorIdx: int
    (* when moving cursor up or down a line, 
     * move to this column if possible *)
    , preferredColumn: int
    }

  fun init (buffer, windowWidth, windowHeight) : app_type =
    { buffer = buffer
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    , startLine = 0
    , cursorIdx = 0
    , preferredColumn = 0
    }
end
