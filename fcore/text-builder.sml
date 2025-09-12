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
        val fPosX = Real32.fromInt startX
        val fPosY = Real32.fromInt startY
        val z: Real32.real = 0.1

        val chr = CozetteAscii.make
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

        val posX = startX + TC.xSpace
      in
        loop
          ( 0
          , str
          , posX
          , startY
          , endX
          , [chr]
          , floatWindowWidth
          , floatWindowHeight
          )
      end
  end
end
