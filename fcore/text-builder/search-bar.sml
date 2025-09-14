structure SearchBar =
struct
  structure TC = TextConstants
  structure Utils = TextBuilderUtils

  fun loop
    (pos, str, posX, posY, endX, acc, floatWindowWidth, floatWindowHeight) =
    if pos = String.size str then
      acc
    else if posX >= endX then
      acc
    else
      let
        val chr = String.sub (str, pos)
        val r: Real32.real = 0.67
        val g: Real32.real = 0.51
        val b: Real32.real = 0.83
        val fPosX = Real32.fromInt posX
        val fPosY = Real32.fromInt posY
        val z: Real32.real = 0.1

        val chr = CozetteAscii.make
          ( chr
          , fPosX
          , fPosY
          , z
          , TC.scale
          , floatWindowWidth
          , floatWindowHeight
          , r
          , g
          , b
          )

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

  (* builds a single text line from a string.
   * Used for getting Real32.real vector representing search input. 
   * Todo: add scrolling, so that text scrolls horizontally when greater than width. *)
  fun build
    ( str
    , startX
    , startY
    , endX
    , floatWindowWidth
    , floatWindowHeight
    , searchCursorIdx
    , searchScrollColumn
    ) =
    let
      val r: Real32.real = 0.67
      val g: Real32.real = 0.51
      val b: Real32.real = 0.83
      val z: Real32.real = 0.1

      val width = endX - startX
      val (startX, endX) =
        if TC.textLineWidth > width then
          (startX, endX)
        else
          let
            val startX = (width - TC.textLineWidth) div 2
            val endX = startX + TC.textLineWidth
          in
            (startX, endX)
          end

      val fPosX = Real32.fromInt startX
      val fPosY = Real32.fromInt startY

      val slash = CozetteAscii.make
        ( #"/"
        , fPosX
        , fPosY
        , z
        , TC.scale
        , floatWindowWidth
        , floatWindowHeight
        , r
        , g
        , b
        )

      val cursor =
        let
          val xpos = (searchCursorIdx + 1) - searchScrollColumn
          val xpos = TextConstants.xSpace * xpos + startX
          val xpos = Int.min (endX, xpos)
          val x = Real32.fromInt xpos
        in
          PipeCursor.lerp
            ( x
            , fPosY
            , 0.01
            , TextConstants.scale
            , floatWindowWidth
            , floatWindowHeight
            , r
            , g
            , b
            )
        end

      val posX = startX + TC.xSpace
    in
      loop
        ( searchScrollColumn
        , str
        , posX
        , startY
        , endX
        , [cursor, slash]
        , floatWindowWidth
        , floatWindowHeight
        )
    end
end
