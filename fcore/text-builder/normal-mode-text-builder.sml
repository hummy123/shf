structure NormalModeTextBuilder =
struct
  structure Utils = TextBuilderUtils

  (* Prerequisite to all functions in this structure: 
   * - Move buffer to startLine before calling any function. *)

  fun startBuild
    ( startLine
    , cursorPos
    , buffer: LineGap.t
    , windowWidth
    , windowHeight
    , floatWindowWidth
    , floatWindowHeight
    , searchList
    , visualScrollColumn
    , acc
    ) =
    let
      val
        { rightStrings
        , rightLines
        , line = curLine
        , idx = curIdx
        , textLength
        , ...
        } = buffer

      val env = Utils.initEnv
        ( 0
        , 0
        , windowWidth
        , windowHeight
        , floatWindowWidth
        , floatWindowHeight
        , searchList
        , visualScrollColumn
        , startLine
        )
      val {startX, startY, ...} = env
    in
      if textLength = 1 then
        (* empty string, so there is nothing we can draw 
         * except a cursor at the line start.
         * An empty string is usually thought of to have a length of 0 
         * and this is true, but we always have a \n at the end of the buffer
         * to respect Unix-style file endings, which we always uphold. 
         * So, for us, an empty string has a length of 1. *)
        [Utils.makeCursor (startX, startY, env)]
      else
        case (rightStrings, rightLines) of
          (shd :: stl, lhd :: ltl) =>
            let
              (* get relative index of line to start building from *)
              val strPos =
                Utils.getRelativeLineStartFromRightHead
                  (startLine, curLine, lhd)
              (* get absolute idx of line *)
              val absIdx = curIdx + strPos

              val searchPos = BinSearch.equalOrMore (absIdx, searchList)
              val searchPos =
                if searchPos = ~1 then Vector.length searchList else searchPos
            in
              TextBuilderWithHighlight.build
                ( strPos
                , shd
                , stl
                , lhd
                , ltl
                , startX
                , startY
                , 0
                , startLine
                , absIdx
                , cursorPos
                , env
                , acc
                , searchPos
                )
            end
        | (_, _) => acc
    end

  fun buildWithExisting
    ( startLine
    , cursorPos
    , buffer: LineGap.t
    , windowWidth
    , windowHeight
    , searchList: SearchList.t
    , visualScrollColumn
    , acc
    ) =
    startBuild
      ( startLine
      , cursorPos
      , buffer
      , windowWidth
      , windowHeight
      , Real32.fromInt windowWidth
      , Real32.fromInt windowHeight
      , searchList
      , visualScrollColumn
      , []
      )

  fun build
    ( startLine
    , cursorPos
    , buffer: LineGap.t
    , windowWidth
    , windowHeight
    , searchList: SearchList.t
    , visualScrollColumn
    ) =
    startBuild
      ( startLine
      , cursorPos
      , buffer
      , windowWidth
      , windowHeight
      , Real32.fromInt windowWidth
      , Real32.fromInt windowHeight
      , searchList
      , visualScrollColumn
      , []
      )
end
