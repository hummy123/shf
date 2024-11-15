structure Rect =
struct
  fun lerp (startX, startY, drawWidth, drawHeight, windowWidth, windowHeight, r, g, b) : Real32.real vector =
    let
       val startX = Real32.fromInt startX
       val startY = Real32.fromInt startY
       val endY = windowHeight - startY
       val startY = windowHeight - (startY + drawHeight)
       val endX = startX + drawWidth
       val windowHeight = windowHeight / 2.0
       val windowWidth = windowWidth / 2.0
    in
       #[      (((startX * (1.0 - 0.733333349228)) + (endX * 0.733333349228)) / windowWidth) - 1.0,
      (((startY * (1.0 - 1.0)) + (endY * 1.0)) / windowHeight) - 1.0, r, g, b,
      (((startX * (1.0 - 0.733333349228)) + (endX * 0.733333349228)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.0)) + (endY * 0.0)) / windowHeight) - 1.0, r, g, b,
      (((startX * (1.0 - 0.266666650772)) + (endX * 0.266666650772)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.0)) + (endY * 0.0)) / windowHeight) - 1.0, r, g, b,
      (((startX * (1.0 - 0.266666650772)) + (endX * 0.266666650772)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.0)) + (endY * 0.0)) / windowHeight) - 1.0, r, g, b,
      (((startX * (1.0 - 0.266666650772)) + (endX * 0.266666650772)) / windowWidth) - 1.0,
      (((startY * (1.0 - 1.0)) + (endY * 1.0)) / windowHeight) - 1.0, r, g, b,
      (((startX * (1.0 - 0.733333349228)) + (endX * 0.733333349228)) / windowWidth) - 1.0,
      (((startY * (1.0 - 1.0)) + (endY * 1.0)) / windowHeight) - 1.0, r, g, b
    ]
  end
end
