structure Buffer =
struct
  (* builds text from a string with char-wrap.
   * char-wrap is a similar concept to word-wrap, 
   * but it breaks on character in the middle of a word.
   * Todo: 
   * - Let caller know which termination case was reached.
   *   Did we reach the end of the string or did we exhaust the screen space?
   * - Possibly add visual horizontal indentation when char-wrap occurs
   *   on an indented line *)
  fun buildTextString
    ( pos, str, acc, posX, posY, startX
    , windowWidth, windowHeight, fWindowWidth, fWindowHeight
    , r, g, b
    ) =
    if pos < String.size str then
      case String.sub (str, pos) of
        #" " =>
          (* if space, then proceed forward one char 
           * without adding to acc *)
          buildTextString
            ( pos + 1, str, acc, posX + 25, posY, startX
            , windowWidth, windowHeight, fWindowWidth, fWindowHeight
            , r, g, b
            )
      | #"\t" =>
          (* if tab, proceed forward one char,
           * and jump visually forwards three chars *)
          buildTextString
            ( pos + 1, str, acc, posX + 75, posY, startX
            , windowWidth, windowHeight, fWindowWidth, fWindowHeight
            , r, g, b
            )
      | #"\n" =>
          (* if \n, move down vertically, and move to start horizontally
           * assuming we have not exceeded the window's height.
           * If we have exceeded the window's height, just return acc. *)
          if posY + 25 < windowHeight then
            buildTextString
              ( pos + 1, str, acc, startX, posY + 25, startX
              , windowWidth, windowHeight, fWindowWidth, fWindowHeight
              , r, g, b
              )
          else
            acc
      | #"\r" =>
          (* same as \n, except we also check if we are in a \r\n pair,
           * and proceed two characters forward if so *)
          if posY + 25 < windowHeight then
            if
              pos < String.size str - 1
              andalso String.sub (str, pos + 1) = #"\n"
            then
              buildTextString
                ( pos + 2, str, acc, startX, posY + 25, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b
                )
            else
              buildTextString
                ( pos + 1, str, acc, startX, posY + 25, startX
                , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                , r, g, b
                )
          else
            acc
      | chr =>
          (* for any other character, add it to acc if there is space,
           * and proceed forward one character in the string *)
          let
            val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
          in
            if posX + 25 < windowWidth then
              (* if there is horizontal space, place char on the right *)
              let
                val chrVec = chrFun
                  (posX, posY, 25.0, 25.0, fWindowWidth, fWindowHeight, r, g, b)
                val acc = chrVec :: acc
              in
                buildTextString
                  ( pos + 1, str, acc, posX + 25, posY, startX
                  , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                  , r, g, b
                  )
              end
            else if posY + 25 < windowHeight then
              (* if there is vertical space, place char down below at startX *)
              let
                val chrVec = chrFun
                  ( startX, posY + 25, 25.0, 25.0
                  , fWindowWidth, fWindowHeight
                  , r, g, b
                  )
                val acc = chrVec :: acc
              in
                buildTextString
                  ( pos + 1, str, acc, startX + 25, posY + 25, startX
                  , windowWidth, windowHeight, fWindowWidth, fWindowHeight
                  , r, g, b
                  )
              end
            else
              (* if no space horizontally or vertically, just return *)
              acc
          end
    else
      acc
end
