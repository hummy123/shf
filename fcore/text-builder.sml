signature TEXT_BUILDER =
sig
  (* Prerequisite: LineGap is moved to requested line first. *)
  val build: int * LineGap.t * int * int
             -> Real32.real vector
end

structure TextBuilder :> TEXT_BUILDER =
struct
  val xSpace = 13
  val xSpace3 = xSpace * 3
  val ySpace = 25
  val fontSize = 30.0

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
    , r, g, b, tl
    ) =
    if pos < String.size str then
      case String.sub (str, pos) of
        #" " =>
          (* if space, then proceed forward one char 
           * without adding to acc *)
          buildTextStringAfterCursor
           ( pos + 1, str, acc, posX + xSpace, posY, startX
           , windowWidth, windowHeight, fWindowWidth, fWindowHeight
           , r, g, b, tl
           )
      | #"\t" =>
          (* if tab, proceed forward one char,
           * and jump visually forwards three chars *)
          buildTextStringAfterCursor
           ( pos + 1, str, acc, posX + xSpace3, posY, startX
           , windowWidth, windowHeight, fWindowWidth, fWindowHeight
           , r, g, b, tl
           )
      | #"\n" =>
          (* if \n, move down vertically, and move to start horizontally
           * assuming we have not exceeded the window's height.
           * If we have exceeded the window's height, just return acc. *)
          if posY + ySpace < windowHeight then
            buildTextStringAfterCursor
              ( pos + 1, str, acc, startX, posY + ySpace, startX
              , windowWidth, windowHeight, fWindowWidth, fWindowHeight
              , r, g, b, tl
              )
          else
            (* return if there is no more vertical space after line break *)
            acc
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
                , r, g, b, tl
                )
            else 
              buildTextStringAfterCursor
                ( pos + 1, str, acc, startX, posY + ySpace, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b, tl
                )
          else
            (* return if there is no more vertical space after line break *)
            acc
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
                  , r, g, b, tl
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
                  , r, g, b, tl
                  )
              end
            else
              (* return if no space horizontally or vertically *)
              acc
          end
    else
      (* if we reached the end of the string, 
       * call function to build next string *)
      continueBuildTextLineGapAfterCursor
        ( tl, acc, posX, posY, startX
        , windowWidth, windowHeight, fWindowWidth, fWindowHeight
        , r, g, b
        )

  and continueBuildTextLineGapAfterCursor
    ( strList, acc, posX, posY, startX
    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
    , r, g, b
    ) =
    case strList of
      hd :: tl =>
        buildTextStringAfterCursor
          ( 0, hd, acc, posX, posY, startX
          , windowWidth, windowHeight, fWindowWidth, fWindowHeight
          , r, g, b, tl
          )
    | [] => acc

  fun build
    (startLine, lineGap: LineGap.t, windowWidth, windowHeight) =
    let
      val lineGap = LineGap.goToLine (startLine, lineGap)
      val {rightStrings, rightLines, line = curLine, ...} = lineGap

      val acc =
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
            in
              buildTextStringAfterCursor
                ( startIdx, rStrHd, []
                , 5, 5, 5
                , windowWidth, windowHeight
                , Real32.fromInt windowWidth, Real32.fromInt windowHeight
                , 0.67, 0.51, 0.83
                , rStrTl
                )
            end
        | (_, _) =>
            (* requested line goes beyond the buffer,
             * so just return empty list as there is nothig
             * else we can do. *)
            []
    in
      Vector.concat acc
    end
end
