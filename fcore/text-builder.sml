signature TEXT_BUILDER =
sig
  (* Prerequisites: LineGap is moved to requested line first. *)
  val getLineAbsIdx: int * LineGap.t -> int

  (* Prerequisites: LineGap is moved to requested line first. *)
  val build: int * int * LineGap.t * int * int * SearchList.t * string
             -> MailboxType.t list
end

structure TextBuilder :> TEXT_BUILDER =
struct
  open TextConstants

  fun accToDrawMsg (textAcc, cursorAcc, bgAcc) =
    let
      open MailboxType
      open DrawMsg

      val textAcc = Vector.concat textAcc
      val bgAcc = Vector.concat bgAcc

      val textMsg = REDRAW_TEXT textAcc
      val cursorMsg = REDRAW_CURSOR cursorAcc
      val bgMsg = REDRAW_BG bgAcc
    in
      [DRAW bgMsg, DRAW textMsg, DRAW cursorMsg]
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
  type env_data =
    { r: Real32.real
    , g: Real32.real
    , b: Real32.real

    (* hr/hg/hb = highlight red/green/blue *)
    , hr: Real32.real
    , hg: Real32.real
    , hb: Real32.real

    , w: int
    , h: int

    (* fw/fh = float window width and float window height *)
    , fw: Real32.real
    , fh: Real32.real
    }

  fun buildTextString
    ( pos
    , str
    , acc
    , posX
    , posY
    , startX
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
              , startX
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

              val cursorAcc = Rect.lerp
                (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
            in
              buildTextString
                ( pos + 1
                , str
                , acc
                , posX + xSpace
                , posY
                , startX
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
                , startX
                , posY + ySpace
                , startX
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

                val cursorAcc = Rect.lerp
                  (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
              in
                buildTextString
                  ( pos + 1
                  , str
                  , acc
                  , startX
                  , posY + ySpace
                  , startX
                  , tl
                  , absIdx + 1
                  , cursorPos
                  , cursorAcc
                  , bgAcc
                  , env
                  )
              end
          else
            accToDrawMsg (acc, cursorAcc, bgAcc)
      | chr =>
          let
            val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
          in
            if absIdx <> cursorPos then
              (* not equal to cursor *)
              if posX + xSpace < #w env then
                let
                  val {r, g, b, fw, fh, ...} = env

                  val chrVec = chrFun
                    (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
                  val acc = chrVec :: acc
                in
                  buildTextString
                    ( pos + 1
                    , str
                    , acc
                    , posX + xSpace
                    , posY
                    , startX
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

                  val chrVec = chrFun
                    (startX, posY + ySpace, fontSize, fontSize, fw, fh, r, g, b)
                  val acc = chrVec :: acc
                in
                  buildTextString
                    ( pos + 1
                    , str
                    , acc
                    , startX + xSpace
                    , posY + ySpace
                    , startX
                    , tl
                    , absIdx + 1
                    , cursorPos
                    , cursorAcc
                    , bgAcc
                    , env
                    )
                end
              else
                accToDrawMsg (acc, cursorAcc, bgAcc)
            else
              (* equal to cursor *)
              let
                val {fw, fh, r, g, b, hr, hg, hb, ...} = env

                val cursorAcc = Rect.lerp
                  (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
              in
                if posX + xSpace < #w env then
                  let
                    val chrVec = chrFun
                      (posX, posY, fontSize, fontSize, fw, fh, hr, hg, hb)
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
                      , startX
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
                    val chrVec = chrFun
                      ( startX
                      , posY + ySpace
                      , fontSize
                      , fontSize
                      , fw
                      , fh
                      , hr
                      , hg
                      , hb
                      )
                    val acc = chrVec :: acc
                  in
                    (* can start building after cursor now, 
                     * since cursor was built *)
                    buildTextString
                      ( pos + 1
                      , str
                      , acc
                      , startX + xSpace
                      , posY + ySpace
                      , startX
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      )
                  end
                else
                  accToDrawMsg (acc, cursorAcc, bgAcc)
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
            , startX
            , tl
            , absIdx
            , cursorPos
            , cursorAcc
            , bgAcc
            , env
            )
      | [] => accToDrawMsg (acc, cursorAcc, bgAcc)

  fun isInSearchRange (absIdx, searchPos, searchHd, searchLen) =
    let val searchIdx = Vector.sub (searchHd, searchPos)
    in absIdx >= searchIdx andalso absIdx < searchIdx + searchLen
    end

  fun isAfterSearchRange (absIdx, searchPos, searchHd, searchLen) =
    let val searchIdx = Vector.sub (searchHd, searchPos)
    in absIdx >= searchIdx + searchLen
    end

  fun buildTextStringSearch
    ( pos
    , str
    , acc
    , posX
    , posY
    , startX
    , tl
    , absIdx
    , cursorPos
    , cursorAcc
    , bgAcc
    , env: env_data
    , searchHd
    , searchPos
    , searchLen
    ) =
    if searchPos = Vector.length searchHd then
          (* exhausted search list so call normal build function *)
          buildTextString
            ( pos
            , str
            , acc
            , posX
            , posY
            , startX
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
            if isInSearchRange (absIdx, searchPos, searchHd, searchLen) then
              (* draw *)
              let
                (* todo: temp colours *)
                val r: Real32.real = 0.3
                val g: Real32.real = 0.1
                val b: Real32.real = 0.1
                val {fw, fh, ...} = env

                val space = Rect.lerp
                  (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
                val bgAcc = space :: bgAcc
              in
                buildTextStringSearch
                  ( pos + 1
                  , str
                  , acc
                  , posX + xSpace
                  , posY
                  , startX
                  , tl
                  , absIdx + 1
                  , cursorPos
                  , cursorAcc
                  , bgAcc
                  , env
                  , searchHd
                  , searchPos
                  , searchLen
                  )
              end
            else
              buildTextStringSearch
                ( pos + 1
                , str
                , acc
                , posX + xSpace
                , posY
                , startX
                , tl
                , absIdx + 1
                , cursorPos
                , cursorAcc
                , bgAcc
                , env
                , searchHd
                , searchPos
                , searchLen
                )
          else
            (* in cursor *)
            let
              val {fw, fh, r, g, b, ...} = env

              val cursorAcc = Rect.lerp
                (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
            in
              buildTextStringSearch
                ( pos + 1
                , str
                , acc
                , posX + xSpace
                , posY
                , startX
                , tl
                , absIdx + 1
                , cursorPos
                , cursorAcc
                , bgAcc
                , env
                , searchHd
                , searchPos
                , searchLen
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
                , startX
                , posY + ySpace
                , startX
                , tl
                , absIdx + 1
                , cursorPos
                , cursorAcc
                , bgAcc
                , env
                , searchHd
                , searchPos
                , searchLen
                )
            else
              (* in cursor position, so build cursorAcc *)
              let
                val {fw, fh, r, g, b, ...} = env

                val cursorAcc = Rect.lerp
                  (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
              in
                buildTextStringSearch
                  ( pos + 1
                  , str
                  , acc
                  , startX
                  , posY + ySpace
                  , startX
                  , tl
                  , absIdx + 1
                  , cursorPos
                  , cursorAcc
                  , bgAcc
                  , env
                  , searchHd
                  , searchPos
                  , searchLen
                  )
              end
          else
            accToDrawMsg (acc, cursorAcc, bgAcc)
      | chr =>
          let
            val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
          in
            if absIdx <> cursorPos then
              (* not equal to cursor *)
              if posX + xSpace < #w env then
                if isInSearchRange (absIdx, searchPos, searchHd, searchLen) then
                  let
                    val {fw, fh, ...} = env

                    (* todo: temp colours *)
                    val r: Real32.real = 0.7
                    val g: Real32.real = 0.7
                    val b: Real32.real = 0.7

                    (* build char vec *)
                    val chrVec = chrFun
                      (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
                    val acc = chrVec :: acc

                    (* build cursor (behind text) vec *)
                    val r: Real32.real = 0.3
                    val g: Real32.real = 0.1
                    val b: Real32.real = 0.1

                    val space = Rect.lerp
                      (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
                    val bgAcc = space :: bgAcc
                  in
                    buildTextStringSearch
                      ( pos + 1
                      , str
                      , acc
                      , posX + xSpace
                      , posY
                      , startX
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      , searchHd
                      , searchPos
                      , searchLen
                      )
                  end
                else
                  let
                    val {fw, fh, r, g, b, ...} = env

                    val chrVec = chrFun
                      (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
                    val acc = chrVec :: acc
                    val searchPos =
                      if
                        isAfterSearchRange
                          (absIdx, searchPos, searchHd, searchLen)
                      then searchPos + 1
                      else searchPos
                  in
                    buildTextStringSearch
                      ( pos + 1
                      , str
                      , acc
                      , posX + xSpace
                      , posY
                      , startX
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      , searchHd
                      , searchPos
                      , searchLen
                      )
                  end
              else if posY + ySpace < #h env then
                let
                  val {fw, fh, r, g, b, ...} = env

                  val chrVec = chrFun
                    (startX, posY + ySpace, fontSize, fontSize, fw, fh, r, g, b)
                  val acc = chrVec :: acc
                in
                  buildTextStringSearch
                    ( pos + 1
                    , str
                    , acc
                    , startX + xSpace
                    , posY + ySpace
                    , startX
                    , tl
                    , absIdx + 1
                    , cursorPos
                    , cursorAcc
                    , bgAcc
                    , env
                    , searchHd
                    , searchPos
                    , searchLen
                    )
                end
              else
                accToDrawMsg (acc, cursorAcc, bgAcc)
            else
              (* equal to cursor *)
              let
                val {fw, fh, r, g, b, hr, hg, hb, ...} = env
                val cursorAcc = Rect.lerp
                  (posX, posY, fontSize, fontSize, fw, fh, r, g, b)
              in
                if posX + xSpace < #w env then
                  let
                    val chrVec = chrFun
                      (posX, posY, fontSize, fontSize, fw, fh, hr, hg, hb)
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
                      , startX
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      , searchHd
                      , searchPos
                      , searchLen
                      )
                  end
                else if posY + ySpace < #h env then
                  let
                    val chrVec = chrFun
                      ( startX
                      , posY + ySpace
                      , fontSize
                      , fontSize
                      , fw
                      , fh
                      , hr
                      , hg
                      , hb
                      )
                    val acc = chrVec :: acc
                  in
                    (* can start building after cursor now, 
                     * since cursor was built *)
                    buildTextStringSearch
                      ( pos + 1
                      , str
                      , acc
                      , startX + xSpace
                      , posY + ySpace
                      , startX
                      , tl
                      , absIdx + 1
                      , cursorPos
                      , cursorAcc
                      , bgAcc
                      , env
                      , searchHd
                      , searchPos
                      , searchLen
                      )
                  end
                else
                  accToDrawMsg (acc, cursorAcc, bgAcc)
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
            , startX
            , tl
            , absIdx
            , cursorPos
            , cursorAcc
            , bgAcc
            , env
            , searchHd
            , searchPos
            , searchLen
            )
      | [] => accToDrawMsg (acc, cursorAcc, bgAcc)

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

  fun build
    ( startLine
    , cursorPos
    , lineGap: LineGap.t
    , windowWidth
    , windowHeight
    , searchList: SearchList.t
    , searchString
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

            val env =
              { w = windowWidth
              , h = windowHeight
              , fw = Real32.fromInt windowWidth
              , fh = Real32.fromInt windowHeight
              , r = 0.67
              , g = 0.51
              , b = 0.83
              , hr = 0.211
              , hg = 0.219
              , hb = 0.25
              }

            val cursorAcc = Vector.fromList []
            val searchPos = BinSearch.equalOrMore (absIdx, searchList)
          in
            if searchPos < Vector.length searchList then
                   buildTextStringSearch
                     ( startIdx
                     , rStrHd
                     , []
                     , 5
                     , 5
                     , 5
                     , rStrTl
                     , absIdx
                     , cursorPos
                     , cursorAcc
                     , []
                     , env
                     , searchList
                     , searchPos
                     , String.size searchString
                     )
            else
                 buildTextString
                   ( startIdx
                   , rStrHd
                   , []
                   , 5
                   , 5
                   , 5
                   , rStrTl
                   , absIdx
                   , cursorPos
                   , cursorAcc
                   , []
                   , env
                   )
          end
      | (_, _) =>
          (* requested line goes beyond the buffer,
           * so just return empty list as there is nothig
           * else we can do. *)
          []
    end
end
