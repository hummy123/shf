signature TEXT_BUILDER =
sig
  (* Prerequisite: LineGap is moved to requested line first. *)
  val build: int * int * LineGap.t * int * int
             -> MailboxType.t list
end

structure TextBuilder :> TEXT_BUILDER =
struct
  val xSpace = 13
  val xSpace3 = xSpace * 3
  val ySpace = 25
  val fontSize = 30.0

  fun accToDrawMsg (textAcc, cursorAcc) =
    let
      open MailboxType
      open DrawMsg

      val textAcc = Vector.concat textAcc
      val cursorAcc = Vector.concat cursorAcc

      val textMsg = REDRAW_TEXT textAcc
      val cursorMsg = REDRAW_CURSOR cursorAcc
    in
      [DRAW textMsg, DRAW cursorMsg]
    end

  fun buildCursor (posX, posY, fWindowWidth, fWindowHeight, r, g, b) =
    let
      val left = posX + 9
      val left = Real32.fromInt left
      val right = left + 12.0

      val top = Real32.fromInt posY
      val bottom = top + fontSize + 2.0

      val halfHeight = fWindowHeight / 2.0
      val top = (~(top - halfHeight)) / halfHeight
      val bottom = (~(bottom - halfHeight)) / halfHeight

      val halfWidth = fWindowWidth / 2.0
      val left = (left - halfWidth) / halfWidth
      val right = (right - halfWidth) / halfWidth

      val vec =
       #[ left, top, r, g, b
        , right, top, r, g, b
        , left, bottom, r, g, b

        , left, bottom, r, g, b
        , right, bottom, r, g, b
        , right, top, r, g, b
        ]
    in
      [vec]
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
  fun buildTextStringAfterCursor
    ( pos, str, acc, posX, posY, startX
    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
    , r, g, b, tl, cursorAcc
    ) =
    if pos < String.size str then
      case String.sub (str, pos) of
        #" " =>
          (* if space, then proceed forward one char 
           * without adding to acc *)
          buildTextStringAfterCursor
           ( pos + 1, str, acc, posX + xSpace, posY, startX
           , windowWidth, windowHeight, fWindowWidth, fWindowHeight
           , r, g, b, tl, cursorAcc
           )
      | #"\t" =>
          (* if tab, proceed forward one char,
           * and jump visually forwards three chars *)
          buildTextStringAfterCursor
           ( pos + 1, str, acc, posX + xSpace3, posY, startX
           , windowWidth, windowHeight, fWindowWidth, fWindowHeight
           , r, g, b, tl, cursorAcc
           )
      | #"\n" =>
          (* if \n, move down vertically, and move to start horizontally
           * assuming we have not exceeded the window's height.
           * If we have exceeded the window's height, just return acc. *)
          if posY + ySpace < windowHeight then
            buildTextStringAfterCursor
              ( pos + 1, str, acc, startX, posY + ySpace, startX
              , windowWidth, windowHeight, fWindowWidth, fWindowHeight
              , r, g, b, tl, cursorAcc
              )
          else
            (* return if there is no more vertical space after line break *)
            accToDrawMsg (acc, cursorAcc)
      | #"\r" =>
          (* same as \n, except we also check if we are in a \r\n pair,
           * and proceed two characters forward if so *)
          if posY + ySpace < windowHeight then
            if
              pos < String.size str - 1
              andalso String.sub (str, pos + 1) = #"\n"
            then 
              buildTextStringAfterCursor
                ( pos + 2, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, cursorAcc
                )
            else 
              buildTextStringAfterCursor
                ( pos + 1, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, cursorAcc
                )
          else
            (* return if there is no more vertical space after line break *)
            accToDrawMsg (acc, cursorAcc)
      | chr =>
          (* for any other character, add it to acc if there is space,
           * and proceed forward one character in the string *)
          let
            val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
          in
            if posX + xSpace < windowWidth then
              (* if there is horizontal space, place char on the right *)
              let
                val chrVec = chrFun
                  (posX, posY, fontSize, fontSize, fWindowWidth, fWindowHeight, r, g, b)
                val acc = chrVec :: acc
              in
                buildTextStringAfterCursor
                  ( pos + 1, str, acc, posX + xSpace, posY, startX
                  , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                  , r, g, b, tl, cursorAcc
                  )
              end
            else if posY + ySpace < windowHeight then
              (* if there is vertical space, place char down below at startX *)
              let
                val chrVec = chrFun
                  ( startX, posY + ySpace, fontSize, fontSize
                  , fWindowWidth, fWindowHeight
                  , r, g, b
                  )
                val acc = chrVec :: acc
              in
                buildTextStringAfterCursor
                  ( pos + 1, str, acc, startX + xSpace, posY + ySpace, startX
                  , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                  , r, g, b, tl, cursorAcc
                  )
              end
            else
              (* return if no space horizontally or vertically *)
              accToDrawMsg (acc, cursorAcc)
          end
    else
      (* if we reached the end of the string, 
       * call function to build next string *)
      continueBuildTextLineGapAfterCursor
        ( tl, acc, posX, posY, startX
        , windowWidth, windowHeight, fWindowWidth, fWindowHeight
        , r, g, b, cursorAcc
        )

  and continueBuildTextLineGapAfterCursor
    ( strList, acc, posX, posY, startX
    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
    , r, g, b, cursorAcc
    ) =
    case strList of
      hd :: tl =>
        buildTextStringAfterCursor
          ( 0, hd, acc, posX, posY, startX
          , windowWidth, windowHeight, fWindowWidth, fWindowHeight
          , r, g, b, tl, cursorAcc
          )
    | [] => accToDrawMsg (acc, cursorAcc)

  (* same as buildTextStringAfterCursor, except this keeps track of absolute
   * index and cursor pos too *)
  fun buildTextStringBeforeCursor
    ( pos, str, acc, posX, posY, startX
    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
    , r, g, b, tl, absIdx, cursorPos, hr, hg, hb
    ) =
    if pos < String.size str then
      case String.sub (str, pos) of
        #" " =>
          buildTextStringBeforeCursor
           ( pos + 1, str, acc, posX + xSpace, posY, startX
           , windowWidth, windowHeight, fWindowWidth, fWindowHeight
           , r, g, b, tl, absIdx + 1, cursorPos, hr, hg, hb
           )
      | #"\t" =>
          buildTextStringBeforeCursor
           ( pos + 1, str, acc, posX + xSpace3, posY, startX
           , windowWidth, windowHeight, fWindowWidth, fWindowHeight
           , r, g, b, tl, absIdx + 1, cursorPos, hr, hg, hb
           )
      | #"\n" =>
          if posY + ySpace < windowHeight then
            buildTextStringBeforeCursor
              ( pos + 1, str, acc, startX, posY + ySpace, startX
              , windowWidth, windowHeight, fWindowWidth, fWindowHeight
              , r, g, b, tl, absIdx + 1, cursorPos, hr, hg, hb
              )
          else
            accToDrawMsg (acc, [])
      | #"\r" =>
          if posY + ySpace < windowHeight then
            if
              pos < String.size str - 1
              andalso String.sub (str, pos + 1) = #"\n"
            then 
              buildTextStringBeforeCursor
                ( pos + 2, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, absIdx + 2, cursorPos, hr, hg, hb
                )
            else 
              buildTextStringBeforeCursor
                ( pos + 1, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, absIdx + 1, cursorPos, hr, hg, hb
                )
          else
            accToDrawMsg (acc, [])
      | chr =>
          let
            val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
          in
            if posX + xSpace < windowWidth then
              let
                val chrVec = chrFun
                  (posX, posY, fontSize, fontSize, fWindowWidth, fWindowHeight, r, g, b)
                val acc = chrVec :: acc
              in
                buildTextStringBeforeCursor
                  ( pos + 1, str, acc, posX + xSpace, posY, startX
                  , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                  , r, g, b, tl, absIdx + 1, cursorPos, hr, hg, hb
                  )
              end
            else if posY + ySpace < windowHeight then
              let
                val chrVec = chrFun
                  ( startX, posY + ySpace, fontSize, fontSize
                  , fWindowWidth, fWindowHeight
                  , r, g, b
                  )
                val acc = chrVec :: acc
              in
                buildTextStringBeforeCursor
                  ( pos + 1, str, acc, startX + xSpace, posY + ySpace, startX
                  , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                  , r, g, b, tl, absIdx + 1, cursorPos, hr, hg, hb
                  )
              end
            else
              accToDrawMsg (acc, [])
          end
    else
      continueBuildTextLineGapBeforeCursor
        ( tl, acc, posX, posY, startX
        , windowWidth, windowHeight, fWindowWidth, fWindowHeight
        , r, g, b, absIdx, cursorPos, hr, hg, hb
        )

  and buildTextStringWithinCursor
    ( pos, str, acc, posX, posY, startX
    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
    , r, g, b, tl, absIdx, cursorPos, cursorAcc, hr, hg, hb
    ) =
    if pos < String.size str then
      case String.sub (str, pos) of
        #" " =>
          (* if inside cursor, then create cursorAcc;
           * else, just skip as usual *)
          if absIdx <> cursorPos then
            (* not in cursur *)
            buildTextStringWithinCursor
             ( pos + 1, str, acc, posX + xSpace, posY, startX
             , windowWidth, windowHeight, fWindowWidth, fWindowHeight
             , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
             )
          else
            (* in cursor *)
            let
              val cursorAcc = buildCursor (posX, posY, fWindowWidth, fWindowHeight, r, g ,b)
            in
              buildTextStringAfterCursor
                ( pos + 1, str, acc, posX + xSpace, posY, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, cursorAcc
                )
            end
      | #"\t" =>
          (* todo: draw cursor if cursor is on tab
           * but this is not a priority right now *)
          buildTextStringWithinCursor
            ( pos + 1, str, acc, posX + xSpace3, posY, startX
            , windowWidth, windowHeight, fWindowWidth, fWindowHeight
            , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
            )
      | #"\n" =>
          if posY + ySpace < windowHeight then
            if absIdx <> cursorPos then
              (* not in cursor position, so iterate like normal *)
              buildTextStringWithinCursor
                ( pos + 1, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                )
            else
              (* in cursor position, so build cursorAcc 
               * and call AfterCursor function *)
               if pos = String.size str - 1 andalso tl = [] then
                 (* if we are at end of lineGap, we want to build cursorAcc
                  * at different coordinates than usual *)
                 let
                   val cursorAcc = 
                     buildCursor (startX, posY + ySpace, fWindowWidth, fWindowHeight, r, g, b)
                 in
                   accToDrawMsg (acc, cursorAcc)
                 end
               else
                 let
                   val cursorAcc = buildCursor (posX, posY, fWindowWidth, fWindowHeight, r, g ,b)
                 in
                   buildTextStringAfterCursor
                     ( pos + 1, str, acc, startX, posY + ySpace, startX
                     , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                     , r, g, b, tl, cursorAcc
                     )
                 end
          else
            accToDrawMsg (acc, cursorAcc)
      | #"\r" =>
          if posY + ySpace < windowHeight then
            if
              pos < String.size str - 1
              andalso String.sub (str, pos + 1) = #"\n"
            then 
              buildTextStringWithinCursor
                ( pos + 2, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, absIdx + 2, cursorPos, cursorAcc, hr, hg, hb
                )
            else 
              buildTextStringWithinCursor
                ( pos + 1, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                )
          else
            accToDrawMsg (acc, cursorAcc)
      | chr =>
          let
            val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
          in
            if absIdx <> cursorPos then
              (* not equal to cursor *)
              if posX + xSpace < windowWidth then
                let
                  val chrVec = chrFun
                    (posX, posY, fontSize, fontSize, fWindowWidth, fWindowHeight, r, g, b)
                  val acc = chrVec :: acc
                in
                  buildTextStringWithinCursor
                    ( pos + 1, str, acc, posX + xSpace, posY, startX
                    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                    , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                    )
                end
              else if posY + ySpace < windowHeight then
                let
                  val chrVec = chrFun
                    ( startX, posY + ySpace, fontSize, fontSize
                    , fWindowWidth, fWindowHeight
                    , r, g, b
                    )
                  val acc = chrVec :: acc
                in
                  buildTextStringWithinCursor
                    ( pos + 1, str, acc, startX + xSpace, posY + ySpace, startX
                    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                    , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                    )
                end
              else
                accToDrawMsg (acc, cursorAcc)
            else
              (* equal to cursor *)
              let
                val cursorAcc = buildCursor (posX, posY, fWindowWidth, fWindowHeight, r, g ,b)
              in
                if posX + xSpace < windowWidth then
                  let
                    val chrVec = chrFun
                      ( posX, posY, fontSize, fontSize 
                      , fWindowWidth, fWindowHeight
                      , hr, hg, hb
                      )
                    val acc = chrVec :: acc
                  in
                    (* can start building after cursor now, 
                     * since cursor was built *)
                    buildTextStringAfterCursor
                      ( pos + 1, str, acc, posX + xSpace, posY, startX
                      , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                      , r, g, b, tl, cursorAcc
                      )
                  end
                else if posY + ySpace < windowHeight then
                  let
                    val chrVec = chrFun
                      ( startX, posY + ySpace, fontSize, fontSize
                      , fWindowWidth, fWindowHeight
                      , hr, hg, hb
                      )
                    val acc = chrVec :: acc
                  in
                    (* can start building after cursor now, 
                     * since cursor was built *)
                    buildTextStringAfterCursor
                      ( pos + 1, str, acc, startX + xSpace, posY + ySpace, startX
                      , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                      , r, g, b, tl, cursorAcc
                      )
                  end
                else
                  accToDrawMsg (acc, cursorAcc)
              end
          end

    else
      (* we have built cursor now, so can call after-cursor function
       * to build rest of text *)
      continueBuildTextLineGapAfterCursor
        ( tl, acc, posX, posY, startX
        , windowWidth, windowHeight, fWindowWidth, fWindowHeight
        , r, g, b, cursorAcc
        )

  and continueBuildTextLineGapBeforeCursor
    ( strList, acc, posX, posY, startX
    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
    , r, g, b, absIdx, cursorPos, hr, hg, hb
    ) =
    case strList of
      hd :: tl =>
      if cursorPos >= absIdx + cursorPos then
        (* if end of string is before cursor *)
        buildTextStringBeforeCursor
          ( 0, hd, acc, posX, posY, startX
          , windowWidth, windowHeight, fWindowWidth, fWindowHeight
          , r, g, b, tl, absIdx, cursorPos, hr, hg, hb
          )
      else
        (* if within cursor *)
        buildTextStringWithinCursor
          ( 0, hd, acc, posX, posY, startX
          , windowWidth, windowHeight, fWindowWidth, fWindowHeight
          , r, g, b, tl, absIdx, cursorPos, [], hr, hg, hb
          )
    | [] => accToDrawMsg (acc, [])

  fun build
    (startLine, cursorPos, lineGap: LineGap.t, windowWidth, windowHeight) =
    let
      val lineGap = LineGap.goToLine (startLine, lineGap)
      val {rightStrings, rightLines, line = curLine, idx = curIdx, ...} = lineGap
    in
      case (rightStrings, rightLines) of
        (rStrHd :: rStrTl, rLnHd :: _) =>
          let
            (* get index of line to start building from *)
            val startIdx =
              if startLine > curLine then
                let
                  val lnPos = startLine - curLine - 1
                  val startIdx = Vector.sub (rLnHd, lnPos)
                in
                  if
                    String.sub (rStrHd, startIdx) = #"\r"
                    andalso startIdx < String.size rStrHd - 1
                    andalso String.sub (rStrHd, startIdx + 1) = #"\n"
                  then 
                    (* handle \r\n pair *) 
                    startIdx + 2
                  else startIdx + 1
                end
              else
                0

              val absIdx = curIdx + startIdx

          in
            if cursorPos < curIdx + String.size rStrHd then
              (* if cursor is within string *)
              buildTextStringWithinCursor
                ( startIdx, rStrHd, []
                , 5, 5, 5
                , windowWidth, windowHeight
                , Real32.fromInt windowWidth, Real32.fromInt windowHeight
                , 0.67, 0.51, 0.83
                , rStrTl, absIdx, cursorPos, []
                , 0.211, 0.219, 0.25
                )
            else
              (* if cursor is after string *)
              buildTextStringBeforeCursor
                ( startIdx, rStrHd, []
                , 5, 5, 5
                , windowWidth, windowHeight
                , Real32.fromInt windowWidth, Real32.fromInt windowHeight
                , 0.67, 0.51, 0.83
                , rStrTl, absIdx, cursorPos
                , 0.211, 0.219, 0.25
                )
          end
      | (_, _) =>
          (* requested line goes beyond the buffer,
           * so just return empty list as there is nothig
           * else we can do. *)
          []
    end
end
