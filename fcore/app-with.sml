structure AppWith =
struct
  open AppType

  (* this function exists only for testing *)
  fun idx (app, newIdx) =
    let
      val
        { startLine
        , buffer
        , bufferModifyTime
        , searchList
        , searchString
        , mode
        , windowWidth
        , windowHeight
        , msgs
        , cursorIdx = _
        } = app
    in
      { startLine = startLine
      , buffer = buffer
      , bufferModifyTime = bufferModifyTime
      , searchList = searchList
      , searchString = searchString
      , mode = mode
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , msgs = msgs
      , cursorIdx = newIdx
      }
    end
end
