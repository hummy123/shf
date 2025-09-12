structure TextBuilder =
struct
  structure TC = TextConstants
  structure Utils = TextBuilderUtils

  local
    fun loop
      (pos, str, posX, posY, endX, acc, floatWindowWidth, floatWindowHeight) =
      if pos = String.size str then
        acc
      else if posX + TC.xSpace >= endX then
        acc
      else
        let
          val chr = String.sub (str, pos)
          val r: Real32.real = 0.67
          val g: Real32.real = 0.51
          val b: Real32.real = 0.83
          val chr = makeChr
            (chr, posX, posY, floatWindowWidth, floatWindowHeight, r, g, b)
          val acc = chr :: acc
          val nextPosX = posX + TC.xSpace
        in
          loop
            ( pos + 1
            , str
            , nextPosX
            , posY
            , endX
            , acc
            , floatWindowWidth
            , floatWindowHeight
            )
        end
  in
    (* builds a single text line from a string.
     * Used for getting Real32.real vector representing search input. 
     * Todo: Add | cursor to show position of search-string-cursor. *)
    fun buildLineToList
      (str, startX, startY, endX, floatWindowWidth, floatWindowHeight) =
      let
        val r: Real32.real = 0.67
        val g: Real32.real = 0.51
        val b: Real32.real = 0.83
        val acc = makeChr
          (#"/", startX, startY, floatWindowWidth, floatWindowHeight, r, g, b)
        val posX = startX + TC.xSpace
      in
        loop
          ( 0
          , str
          , posX
          , startY
          , endX
          , [acc]
          , floatWindowWidth
          , floatWindowHeight
          )
      end
  end

  (* todo: add startX and startY parameters, 
   * so we can control where on the * screen the text starts from. 
   * Not worth doing until we have/in preparation of tiling functionality *)
  fun buildWithExisting
    ( startLine
    , cursorPos
    , lineGap: LineGap.t
    , windowWidth
    , windowHeight
    , floatWindowWidth
    , floatWindowHeight
    , searchList: SearchList.t
    , searchString
    , msgs
    , textAcc
    , bgAcc
    ) =
    let
      val {rightStrings, rightLines, line = curLine, idx = curIdx, ...} =
        lineGap
    in
      case (rightStrings, rightLines) of
        (rStrHd :: rStrTl, rLnHd :: _) =>
          let
            (* get relative index of line to start building from *)
            val startIdx = helpGetLineStartIdx (startLine, curLine, rLnHd)
            (* get absolute idx of line *)
            val absIdx = curIdx + startIdx

            val env = initEnv
              ( windowWidth
              , windowHeight
              , floatWindowWidth
              , floatWindowHeight
              , msgs
              , searchList
              , String.size searchString
              )
            val {startX, startY, ...} = env

            val cursorAcc = Vector.fromList []
            val searchPos = BinSearch.equalOrMore (absIdx, searchList)
          in
            buildTextStringSearch
              ( startIdx
              , rStrHd
              , textAcc
              , startX
              , startY
              , rStrTl
              , absIdx
              , cursorPos
              , cursorAcc
              , bgAcc
              , env
              , searchPos
              )
          end
      | (_, _) =>
          (* requested line goes beyond the buffer,
           * so just return empty list as there is nothig
           * else we can do. *)
          []
    end

  fun build
    ( startLine
    , cursorPos
    , lineGap: LineGap.t
    , windowWidth
    , windowHeight
    , searchList: SearchList.t
    , searchString
    , msgs
    ) =
    buildWithExisting
      ( startLine
      , cursorPos
      , lineGap : LineGap.t
      , windowWidth
      , windowHeight
      , Real32.fromInt windowWidth
      , Real32.fromInt windowHeight
      , searchList : SearchList.t
      , searchString
      , msgs
      , []
      , []
      )
end
