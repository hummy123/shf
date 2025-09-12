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
    , searchString
    , visualScrollColumn
    , acc
    ) =
    let
      val {rightStrings, rightLines, line = curLine, idx = curIdx, ...} = buffer
    in
      case (rightStrings, rightLines) of
        (shd :: stl, lhd :: ltl) =>
          let
            (* get relative index of line to start building from *)
            val strPos =
              Utils.getRelativeLineStartFromRightHead (startLine, curLine, lhd)
              + 1
            (* get absolute idx of line *)
            val absIdx = curIdx + strPos
            val searchPos = BinSearch.equalOrMore (absIdx, searchList)

            val env = Utils.initEnv
              ( windowWidth
              , windowHeight
              , floatWindowWidth
              , floatWindowHeight
              , searchList
              , String.size searchString
              , visualScrollColumn
              , startLine
              )
            val {startX, startY, ...} = env
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
    , searchString
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
      , searchString
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
    , searchString
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
      , searchString
      , visualScrollColumn
      , []
      )
end
