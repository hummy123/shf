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

  (* gets line start idx, relative to right hd *)
  fun helpGetLineStartIdx (startLine, curLine, rLnHd) =
    if startLine > curLine then
      let val lnPos = startLine - curLine - 1
      in Vector.sub (rLnHd, lnPos) + 1
      end
    else
      0

  (* gets line start idx, absolute *)
  fun helpGetLineAbsIdx (curIdx, startLine, curLine, rLnHd) =
    let
      val startIdx =
        if startLine > curLine then
          let val lnPos = startLine - curLine - 1
          in Vector.sub (rLnHd, lnPos) + 1
          end
        else
          0
    in
      curIdx + startIdx
    end

  fun getLineAbsIdx (startLine, lineGap: LineGap.t) =
    let
      val {rightLines, line = curLine, idx = curIdx, ...} = lineGap
    in
      case rightLines of
        rLnHd :: _ => helpGetLineAbsIdx (curIdx, startLine, curLine, rLnHd)
      | [] => (* should never happen *) 0
    end

  fun initEnv
    ( windowWidth
    , windowHeight
    , floatWindowWidth
    , floatWindowHeight
    , searchList
    , searchLen
    ) =
    if TC.textLineWidth > windowWidth then
      { w = windowWidth
      , h = windowHeight
      , startX = 5
      , startY = 5
      , z = 0.01
      , fw = floatWindowWidth
      , fh = floatWindowHeight
      , r = 0.67
      , g = 0.51
      , b = 0.83
      , hr = 0.211
      , hg = 0.219
      , hb = 0.25
      , msgs = msgs
      , searchList = searchList
      , searchLen = searchLen
      }
    else
      let
        val startX = (windowWidth - TC.textLineWidth) div 2
        val finishWidth = startX + TC.textLineWidth
      in
        { w = finishWidth
        , h = windowHeight
        , startX = startX
        , startY = 5
        , z = 0.01
        , fw = floatWindowWidth
        , fh = floatWindowHeight
        , r = 0.67
        , g = 0.51
        , b = 0.83
        , hr = 0.211
        , hg = 0.219
        , hb = 0.25
        , msgs = msgs
        , searchList = searchList
        , searchLen = searchLen
        }
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
