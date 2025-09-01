signature TEXT_BUILDER =
sig
  (* Prerequisites: LineGap is moved to requested line first. *)
  val getLineAbsIdx: int * LineGap.t -> int

  (* Prerequisites: LineGap is moved to requested line first. *)
  val build:
    int
    * int
    * LineGap.t
    * int
    * int
    * SearchList.t
    * string
    * MailboxType.t list
    -> MailboxType.t list
end

structure TextBuilder :> TEXT_BUILDER =
struct
  open TextConstants
  structure TC = TextConstants

  type env_data =
    { r: Real32.real
    , g: Real32.real
    , b: Real32.real

    (* hr/hg/hb = highlight red/green/blue *)
    , hr: Real32.real
    , hg: Real32.real
    , hb: Real32.real

    , startX: int
    , startY: int

    (* w = width, h = height.
     * These do no necessarily correspond to the whole window's width and height.
     * For example, in some cases we want to horizontally centre text on the screen.
     * In that case, "w" means "end width/pixel, which causes a line break". *)
    , w: int
    , h: int

    (* fw/fh = float window width and float window height *)
    , fw: Real32.real
    , fh: Real32.real
    , msgs: MailboxType.t list
    , searchList: int vector
    , searchLen: int
    }

  fun accToDrawMsg (textAcc, cursorAcc, bgAcc, env: env_data) =
    let
      open MailboxType
      open DrawMsg

      val msgs = #msgs env

      val textAcc = Vector.concat textAcc
      val bgAcc = Vector.concat bgAcc

      val textMsg = REDRAW_TEXT textAcc
      val cursorMsg = REDRAW_CURSOR cursorAcc
      val bgMsg = REDRAW_BG bgAcc
    in
      DRAW bgMsg :: DRAW textMsg :: DRAW cursorMsg :: msgs
    end

  (* builds text from a string with char-wrap.
   * char-wrap is a similar concept to word-wrap, 
   * but it breaks on character in the middle of a word.
   *
   * Will likely want multiple versions of these two mutually recursive
   * functions for each selection and cursor type:
   * cursor over an individual character, 
   * range selection where multiple characters are selected, etc.
   *
   * Todo: 
   * - Possibly add visual horizontal indentation when char-wrap occurs
   *   on an indented line *)
  (* same as buildTextStringAfterCursor, except this keeps track of absolute
   * index and cursor pos too *)

  fun makeRect (posX, posY, fw, fh, r, g, b) =
    Rect.lerp
      (Real32.fromInt (posX - 1), Real32.fromInt posY, scale, fw, fh, r, g, b)

  fun makeChr (chr, posX, posY, windowWidth, windowHeight, r, g, b) =
    CozetteAscii.make
      ( chr
      , Real32.fromInt posX
      , Real32.fromInt posY
      , scale
      , windowWidth
      , windowHeight
      , r
      , g
      , b
      )

  fun buildTextString
    ( pos
    , str
    , acc
    , posX
    , posY
    , tl
    , absIdx
    , cursorPos
    , cursorAcc
    , bgAcc
    , env: env_data
    ) =
    if pos < String.size str then
      case String.sub (str, pos) of
        #" " =>
          (* if inside cursor, then create cursorAcc;
           * else, just skip as usual *)
          if absIdx <> cursorPos then
            (* not in cursur *)
            buildTextString
              ( pos + 1
              , str
              , acc
              , posX + xSpace
              , posY
              , tl
              , absIdx + 1
              , cursorPos
              , cursorAcc
              , bgAcc
              , env
              )
          else
            (* in cursor *)
            let
              val {r, g, b, fw, fh, ...} = env

              val cursorAcc = makeRect (posX, posY, fw, fh, r, g, b)
            in
              buildTextString
                ( pos + 1
                , str
                , acc
                , posX + xSpace
                , posY
                , tl
                , absIdx + 1
                , cursorPos
                , cursorAcc
                , bgAcc
                , env
                )
            end
      | #"\n" =>
          if posY + ySpace < #h env then
            if absIdx <> cursorPos then
              (* not in cursor position, so iterate like normal *)
              buildTextString
                ( pos + 1
                , str
                , acc
                , #startX env
                , posY + ySpace
                , tl
                , absIdx + 1
                , cursorPos
                , cursorAcc
                , bgAcc
                , env
                )
            else
              (* in cursor position, so build cursorAcc *)
              let
                val {r, g, b, fw, fh, ...} = env

                val cursorAcc = makeRect (posX, posY, fw, fh, r, g, b)
              in
                buildTextString
                  ( pos + 1
                  , str
                  , acc
                  , #startX env
                  , posY + ySpace
                  , tl
                  , absIdx + 1
                  , cursorPos
                  , cursorAcc
                  , bgAcc
                  , env
                  )
              end
          else
            accToDrawMsg (acc, cursorAcc, bgAcc, env)
      | chr =>
          let in
            if absIdx <> cursorPos then
              (* not equal to cursor *)
              if posX + xSpace < #w env then
                let
                  val {r, g, b, fw, fh, ...} = env

                  val chrVec = makeChr (chr, posX, posY, fw, fh, r, g, b)
                  val acc = chrVec :: acc
                in
                  buildTextString
                    ( pos + 1
                    , str
                    , acc
                    , posX + xSpace
                    , posY
                    , tl
                    , absIdx + 1
                    , cursorPos
                    , cursorAcc
                    , bgAcc
                    , env
                    )
                end
              else if posY + ySpace < #h env then
                let
                  val {r, g, b, fw, fh, ...} = env

                  val chrVec = makeChr
                    (chr, #startX env, posY + ySpace, fw, fh, r, g, b)
                  val acc = chrVec :: acc
                in
                  buildTextString
                    ( pos + 1
                    , str
                    , acc
                    , #startX env + xSpace
                    , posY + ySpace
                    , tl
                    , absIdx + 1
                    , cursorPos
                    , cursorAcc
                    , bgAcc
                    , env
                    )
                end
              else
                accToDrawMsg (acc, cursorAcc, bgAcc, env)
            else
              (* equal to cursor *)
              let
                val {fw, fh, r, g, b, hr, hg, hb, ...} = env

                val cursorAcc = makeRect (posX, posY, fw, fh, r, g, b)
              in
                if posX + xSpace < #w env then
                  let
                    val chrVec = makeChr (chr, posX, posY, fw, fh, hr, hg, hb)
                    val acc = chrVec :: acc
                  in
                    (* can start building after cursor now, 
                     * since cursor was built *)
                    buildTextString
                      ( pos + 1
                      , str
                      , acc
                      , posX + xSpace
                      , posY
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      )
                  end
                else if posY + ySpace < #h env then
                  let
                    val chrVec = makeChr
                      (chr, #startX env, posY + ySpace, fw, fh, hr, hg, hb)
                    val acc = chrVec :: acc
                  in
                    (* can start building after cursor now, 
                     * since cursor was built *)
                    buildTextString
                      ( pos + 1
                      , str
                      , acc
                      , #startX env + xSpace
                      , posY + ySpace
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      )
                  end
                else
                  accToDrawMsg (acc, cursorAcc, bgAcc, env)
              end
          end
    else
      (* change to searching in string's tl *)
      case tl of
        hd :: tl =>
          buildTextString
            ( 0
            , hd
            , acc
            , posX
            , posY
            , tl
            , absIdx
            , cursorPos
            , cursorAcc
            , bgAcc
            , env
            )
      | [] => accToDrawMsg (acc, cursorAcc, bgAcc, env)

  fun isInSearchRange (absIdx, searchPos, searchList, searchLen) =
    let val searchIdx = Vector.sub (searchList, searchPos)
    in absIdx >= searchIdx andalso absIdx < searchIdx + searchLen
    end

  fun isAfterSearchRange (absIdx, searchPos, searchList, searchLen) =
    let val searchIdx = Vector.sub (searchList, searchPos)
    in absIdx >= searchIdx + searchLen
    end

  fun advanceSearchPos (absIdx, searchPos, searchList, searchLen) =
    if isAfterSearchRange (absIdx, searchPos, searchList, searchLen) then
      searchPos + 1
    else
      searchPos

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
     * Used for getting Real32.real vector representing search input. *)
    fun buildLineToList
      (str, startX, startY, endX, floatWindowWidth, floatWindowHeight) =
      loop
        (0, str, startX, startY, endX, [], floatWindowWidth, floatWindowHeight)
  end

  fun buildTextStringSearch
    ( pos
    , str
    , acc
    , posX
    , posY
    , tl
    , absIdx
    , cursorPos
    , cursorAcc
    , bgAcc
    , env: env_data
    , searchPos
    ) =
    if searchPos = Vector.length (#searchList env) then
      (* exhausted search list so call normal build function *)
      buildTextString
        ( pos
        , str
        , acc
        , posX
        , posY
        , tl
        , absIdx
        , cursorPos
        , cursorAcc
        , bgAcc
        , env
        )
    else
      let
        val searchPos =
          advanceSearchPos (absIdx, searchPos, #searchList env, #searchLen env)
      in
        if searchPos = Vector.length (#searchList env) then
          (* exhausted search list so call normal build function *)
          buildTextString
            ( pos
            , str
            , acc
            , posX
            , posY
            , tl
            , absIdx
            , cursorPos
            , cursorAcc
            , bgAcc
            , env
            )

        else if pos < String.size str then
          case String.sub (str, pos) of
            #" " =>
              (* if inside cursor, then create cursorAcc;
               * else, just skip as usual *)
              if absIdx <> cursorPos then
                (* not in cursur *)
                if
                  isInSearchRange
                    (absIdx, searchPos, #searchList env, #searchLen env)
                then
                  (* draw *)
                  let
                    (* todo: temp colours *)
                    val r: Real32.real = 0.3
                    val g: Real32.real = 0.1
                    val b: Real32.real = 0.1
                    val {fw, fh, ...} = env

                    val space = makeRect (posX, posY, fw, fh, r, g, b)
                    val bgAcc = space :: bgAcc
                  in
                    buildTextStringSearch
                      ( pos + 1
                      , str
                      , acc
                      , posX + xSpace
                      , posY
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      , searchPos
                      )
                  end
                else
                  buildTextStringSearch
                    ( pos + 1
                    , str
                    , acc
                    , posX + xSpace
                    , posY
                    , tl
                    , absIdx + 1
                    , cursorPos
                    , cursorAcc
                    , bgAcc
                    , env
                    , searchPos
                    )
              else
                (* in cursor *)
                let
                  val {fw, fh, r, g, b, ...} = env

                  val cursorAcc = makeRect (posX, posY, fw, fh, r, g, b)
                in
                  buildTextStringSearch
                    ( pos + 1
                    , str
                    , acc
                    , posX + xSpace
                    , posY
                    , tl
                    , absIdx + 1
                    , cursorPos
                    , cursorAcc
                    , bgAcc
                    , env
                    , searchPos
                    )
                end
          | #"\n" =>
              if posY + ySpace < #h env then
                if absIdx <> cursorPos then
                  (* not in cursor position, so iterate like normal *)
                  buildTextStringSearch
                    ( pos + 1
                    , str
                    , acc
                    , #startX env
                    , posY + ySpace
                    , tl
                    , absIdx + 1
                    , cursorPos
                    , cursorAcc
                    , bgAcc
                    , env
                    , searchPos
                    )
                else
                  (* in cursor position, so build cursorAcc *)
                  let
                    val {fw, fh, r, g, b, ...} = env

                    val cursorAcc = makeRect (posX, posY, fw, fh, r, g, b)
                  in
                    buildTextStringSearch
                      ( pos + 1
                      , str
                      , acc
                      , #startX env
                      , posY + ySpace
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      , searchPos
                      )
                  end
              else
                accToDrawMsg (acc, cursorAcc, bgAcc, env)
          | chr =>
              let
                val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
              in
                if absIdx <> cursorPos then
                  (* not equal to cursor *)
                  if posX + xSpace < #w env then
                    if
                      isInSearchRange
                        (absIdx, searchPos, #searchList env, #searchLen env)
                    then
                      let
                        val {fw, fh, ...} = env

                        (* todo: temp colours *)
                        val r: Real32.real = 0.7
                        val g: Real32.real = 0.7
                        val b: Real32.real = 0.7

                        (* build char vec *)
                        val chrVec = makeChr (chr, posX, posY, fw, fh, r, g, b)
                        val acc = chrVec :: acc

                        (* build cursor (behind text) vec *)
                        val r: Real32.real = 0.3
                        val g: Real32.real = 0.1
                        val b: Real32.real = 0.1

                        val space = makeRect (posX, posY, fw, fh, r, g, b)
                        val bgAcc = space :: bgAcc
                      in
                        buildTextStringSearch
                          ( pos + 1
                          , str
                          , acc
                          , posX + xSpace
                          , posY
                          , tl
                          , absIdx + 1
                          , cursorPos
                          , cursorAcc
                          , bgAcc
                          , env
                          , searchPos
                          )
                      end
                    else
                      let
                        val {fw, fh, r, g, b, ...} = env

                        val chrVec = makeChr (chr, posX, posY, fw, fh, r, g, b)
                        val acc = chrVec :: acc
                      in
                        buildTextStringSearch
                          ( pos + 1
                          , str
                          , acc
                          , posX + xSpace
                          , posY
                          , tl
                          , absIdx + 1
                          , cursorPos
                          , cursorAcc
                          , bgAcc
                          , env
                          , searchPos
                          )
                      end
                  else if posY + ySpace < #h env then
                    let
                      val {fw, fh, r, g, b, ...} = env

                      val chrVec = makeChr
                        (chr, #startX env, posY + ySpace, fw, fh, r, g, b)
                      val acc = chrVec :: acc
                    in
                      buildTextStringSearch
                        ( pos + 1
                        , str
                        , acc
                        , #startX env + xSpace
                        , posY + ySpace
                        , tl
                        , absIdx + 1
                        , cursorPos
                        , cursorAcc
                        , bgAcc
                        , env
                        , searchPos
                        )
                    end
                  else
                    accToDrawMsg (acc, cursorAcc, bgAcc, env)
                else
                  (* equal to cursor *)
                  let
                    val {fw, fh, r, g, b, hr, hg, hb, ...} = env
                    val cursorAcc = makeRect (posX, posY, fw, fh, r, g, b)
                  in
                    if posX + xSpace < #w env then
                      let
                        val chrVec = makeChr
                          (chr, posX, posY, fw, fh, hr, hg, hb)
                        val acc = chrVec :: acc
                      in
                        (* can start building after cursor now, 
                         * since cursor was built *)
                        buildTextStringSearch
                          ( pos + 1
                          , str
                          , acc
                          , posX + xSpace
                          , posY
                          , tl
                          , absIdx + 1
                          , cursorPos
                          , cursorAcc
                          , bgAcc
                          , env
                          , searchPos
                          )
                      end
                    else if posY + ySpace < #h env then
                      let
                        val chrVec = makeChr
                          (chr, #startX env, posY + ySpace, fw, fh, hr, hg, hb)
                        val acc = chrVec :: acc
                      in
                        (* can start building after cursor now, 
                         * since cursor was built *)
                        buildTextStringSearch
                          ( pos + 1
                          , str
                          , acc
                          , #startX env + xSpace
                          , posY + ySpace
                          , tl
                          , absIdx + 1
                          , cursorPos
                          , cursorAcc
                          , bgAcc
                          , env
                          , searchPos
                          )
                      end
                    else
                      accToDrawMsg (acc, cursorAcc, bgAcc, env)
                  end
              end
        else
          (* change to searching in string's tl *)
          case tl of
            hd :: tl =>
              buildTextStringSearch
                ( 0
                , hd
                , acc
                , posX
                , posY
                , tl
                , absIdx
                , cursorPos
                , cursorAcc
                , bgAcc
                , env
                , searchPos
                )
          | [] => accToDrawMsg (acc, cursorAcc, bgAcc, env)
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
    , msgs
    , searchList
    , searchLen
    ) =
    if TC.textLineWidth > windowWidth then
      { w = windowWidth
      , h = windowHeight
      , startX = 5
      , startY = 5
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
