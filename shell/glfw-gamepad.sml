structure GlfwGamepad =
struct
  datatype joystick_dir = UP | LEFT | DOWN | RIGHT | CENTRE | L2 | R2

  fun isInDeadZone (x, y) =
    x < 0.1 andalso x > ~0.1 andalso y < 0.1 andalso y > ~0.1

  (* todo: also query L2 and R2, and handle them *)
  fun axisToDir (x, y) =
    if isInDeadZone (x, y) then CENTRE
    else if abs x > abs y then if x > 0.0 then RIGHT else LEFT
    else if y > 0.0 then DOWN
    else UP

  fun getGamepadState () =
    Input.getGamepadState 0 <> 0 orelse Input.getGamepadState 1 <> 0
    orelse Input.getGamepadState 2 <> 0 orelse Input.getGamepadState 3 <> 0
    orelse Input.getGamepadState 4 <> 0 orelse Input.getGamepadState 5 <> 0
    orelse Input.getGamepadState 6 <> 0 orelse Input.getGamepadState 7 <> 0
    orelse Input.getGamepadState 8 <> 0 orelse Input.getGamepadState 9 <> 0
    orelse Input.getGamepadState 10 <> 0 orelse Input.getGamepadState 11 <> 0
    orelse Input.getGamepadState 12 <> 0 orelse Input.getGamepadState 13 <> 0
    orelse Input.getGamepadState 14 <> 0 orelse Input.getGamepadState 15 <> 0

  local
    val state =
      { crossPressed = ref false
      , circlePressed = ref false
      , squarePressed = ref false
      , trianglePressed = ref false
      , r1Pressed = ref false
      , l1Pressed = ref false
      }

    open InputMsg

    fun handleTrianglePressed (x, y) =
      if !(#trianglePressed state) then
        ()
      else
        let
          val () = #trianglePressed state := true
          val chr =
            case axisToDir (x, y) of
              CENTRE => #"a"
            | UP => #"e"
            | RIGHT => #"i"
            | DOWN => #"m"
            | LEFT => #"q"
            | L2 => #"u"
            | R2 => #"y"
        in
          InputMailbox.append (CHAR_EVENT chr)
        end

    fun handleCirclePressed (x, y) =
      if !(#circlePressed state) then
        ()
      else
        let
          val () = #circlePressed state := true
          val chr =
            case axisToDir (x, y) of
              CENTRE => #"b"
            | UP => #"f"
            | RIGHT => #"j"
            | DOWN => #"n"
            | LEFT => #"r"
            | L2 => #"v"
            | R2 => #"z"
        in
          InputMailbox.append (CHAR_EVENT chr)
        end

    fun handleCrossPressed (x, y) =
      if !(#crossPressed state) then
        ()
      else
        let
          val () = #crossPressed state := true
        in
          case axisToDir (x, y) of
            CENTRE => InputMailbox.append (CHAR_EVENT #"c")
          | UP => InputMailbox.append (CHAR_EVENT #"g")
          | RIGHT => InputMailbox.append (CHAR_EVENT #"k")
          | DOWN => InputMailbox.append (CHAR_EVENT #"o")
          | LEFT => InputMailbox.append (CHAR_EVENT #"s")
          | L2 => InputMailbox.append (CHAR_EVENT #"w")
          | R2 =>
              (* todo: either shift or enter *)
              raise Fail "glfw-gamepad.sml: 77\n"
        end

    fun handleSquarePressed (x, y) =
      if !(#squarePressed state) then
        ()
      else
        let
          val () = #squarePressed state := true
        in
          case axisToDir (x, y) of
            CENTRE => InputMailbox.append (CHAR_EVENT #"d")
          | UP => InputMailbox.append (CHAR_EVENT #"h")
          | RIGHT => InputMailbox.append (CHAR_EVENT #"l")
          | DOWN => InputMailbox.append (CHAR_EVENT #"p")
          | LEFT => InputMailbox.append (CHAR_EVENT #"t")
          | L2 => InputMailbox.append (CHAR_EVENT #"x")
          | R2 =>
              (* todo: either shift or enter *)
              raise Fail "glfw-gamepad.sml: 87\n"
        end
  in
    fun handleIfJoystickIsPresent () =
      let
        open InputMsg

        (* query possible events for information *)
        val xAxis = Input.getLeftJoystickXAxisState ()
        val yAxis = Input.getLeftJoystickYAxisState ()

        val crossPressed = Input.isCrossButtonPressed ()
        val circlePressed = Input.isCircleButtonPressed ()
        val squarePressed = Input.isSquareButtonPressed ()
        val trianglePressed = Input.isTriangleButtonPressed ()

        val r1Pressed = Input.isR1ButtonPressed ()
        val l1Pressed = Input.isL1ButtonPressed ()

        (* handle button events *)
        val () =
          if crossPressed = 0 then #crossPressed state := false
          else if !(#crossPressed state) then ()
          else handleCrossPressed (xAxis, yAxis)

        val () =
          if circlePressed = 0 then #circlePressed state := false
          else if !(#circlePressed state) then ()
          else handleCirclePressed (xAxis, yAxis)

        val () =
          if squarePressed = 0 then #squarePressed state := false
          else if !(#squarePressed state) then ()
          else handleSquarePressed (xAxis, yAxis)

        val () =
          if trianglePressed = 0 then #trianglePressed state := false
          else if !(#trianglePressed state) then ()
          else handleTrianglePressed (xAxis, yAxis)

        val () =
          if r1Pressed = 0 then
            #r1Pressed state := false
          else if !(#r1Pressed state) then
            ()
          else
            let val () = InputMailbox.append (CHAR_EVENT #" ")
            in #r1Pressed state := true
            end

        val () =
          if l1Pressed = 0 then
            #l1Pressed state := false
          else if !(#l1Pressed state) then
            ()
          else
            let val () = InputMailbox.append KEY_BACKSPACE
            in #l1Pressed state := false
            end
      in
        ()
      end
  end

  fun query () =
    if getGamepadState () then handleIfJoystickIsPresent ()
    else (* nothing to do if no gamepad is present *) ()
end
