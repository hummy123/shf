structure AppType =
struct
  type app_type =
    {buffer: LineGap.t, windowWidth: int, windowHeight: int}

  fun init (buffer, windowWidth, windowHeight): app_type =
    {buffer = buffer, windowWidth = windowWidth, windowHeight = windowHeight}
end
