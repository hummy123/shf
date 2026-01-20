structure GlfwGamepad =
struct
  datatype joystick_dir = UP | LEFT | DOWN | RIGHT | CENTRE | L2 | R2

  fun isInDeadZone (x, y) =
    x < 0.1 andalso x > ~0.1 andalso y < 0.1 andalso y > ~0.1

  (* todo: query L2 and R2, and handle them *)
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

  local open InputMsg
  in
    fun handleTrianglePressed (x, y) =
      let
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
      let
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

    fun handleSquarePressed (x, y) =
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

  fun handleIfJoystickIsPresent () =
    let
      open InputMsg

      val xAxis = Input.getLeftJoystickXAxisState ()
      val yAxis = Input.getLeftJoystickYAxisState ()

      val crossPrressed = Input.isCrossButtonPressed ()
      val circlePressed = Input.isCircleButtonPressed ()
      val squarePressed = Input.isSquareButtonPressed ()
      val trianglePressed = Input.isTriangleButtonPressed ()

      val r1Pressed = Input.isR1ButtonPressed ()
      val l1Pressed = Input.isL1ButtonPressed ()
    in
      if crossPrressed <> 0 then
        (* pressing two buttons at the same time is a no-op *)
        if
          circlePressed <> 0 orelse squarePressed <> 0
          orelse trianglePressed <> 0 orelse r1Pressed <> 0
          orelse l1Pressed <> 0
        then ()
        else handleCrossPressed (xAxis, yAxis)
      else if circlePressed <> 0 then
        if
          squarePressed <> 0 orelse trianglePressed <> 0 orelse r1Pressed <> 0
          orelse l1Pressed <> 0
        then ()
        else handleCirclePressed (xAxis, yAxis)
      else
        ()
    end

  fun query () =
    if getGamepadState () then handleIfJoystickIsPresent ()
    else (* nothing to do if no gamepad is present *) ()
end
