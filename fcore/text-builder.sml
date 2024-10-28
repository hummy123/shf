signature TEXT_BUILDER =
sig
  (* Prerequisite: LineGap is moved to requested line first. *)
  val build: int * int * LineGap.t * int * int
             -> MailboxType.t list
end

structure TextBuilder :> TEXT_BUILDER =
struct
  open TextConstants

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
  (* same as buildTextStringAfterCursor, except this keeps track of absolute
   * index and cursor pos too *)

  fun buildTextString
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
            buildTextString
             ( pos + 1, str, acc, posX + xSpace, posY, startX
             , windowWidth, windowHeight, fWindowWidth, fWindowHeight
             , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
             )
          else
            (* in cursor *)
            let
              val cursorAcc = buildCursor (posX, posY, fWindowWidth, fWindowHeight, r, g ,b)
            in
              buildTextString
                ( pos + 1, str, acc, posX + xSpace, posY, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                )
            end
      | #"\n" =>
          if posY + ySpace < windowHeight then
            if absIdx <> cursorPos then
              (* not in cursor position, so iterate like normal *)
              buildTextString
                ( pos + 1, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                )
            else
              (* in cursor position, so build cursorAcc *)
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
                   buildTextString
                     ( pos + 1, str, acc, startX, posY + ySpace, startX
                     , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                     , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                     )
                 end
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
                  buildTextString
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
                  buildTextString
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
                    buildTextString
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
                      , hr, hg, hb
                      )
                    val acc = chrVec :: acc
                  in
                    (* can start building after cursor now, 
                     * since cursor was built *)
                    buildTextString
                      ( pos + 1, str, acc, startX + xSpace, posY + ySpace, startX
                      , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                      , r, g, b, tl, absIdx + 1, cursorPos, cursorAcc, hr, hg, hb
                      )
                  end
                else
                  accToDrawMsg (acc, cursorAcc)
              end
          end
    else
      (* we have built cursor now, so can call after-cursor function
       * to build rest of text *)
       case tl of
         hd :: tl =>
           buildTextString
             ( 0, hd, acc, posX, posY, startX
             , windowWidth, windowHeight, fWindowWidth, fWindowHeight
             , r, g, b, tl, absIdx, cursorPos, cursorAcc, hr, hg, hb
             )
       | [] =>
           accToDrawMsg (acc, cursorAcc)

  fun build
    (startLine, cursorPos, lineGap: LineGap.t, windowWidth, windowHeight) =
    let
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
            buildTextString
              ( startIdx, rStrHd, []
              , 5, 5, 5
              , windowWidth, windowHeight
              , Real32.fromInt windowWidth, Real32.fromInt windowHeight
              , 0.67, 0.51, 0.83
              , rStrTl, absIdx, cursorPos, []
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
