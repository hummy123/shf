structure GlfwGamepad =
struct
  datatype joystick_dir = UP | LEFT | DOWN | RIGHT | CENTRE | L2 | R2

  fun isInDeadZone (x, y) =
    x < 0.1 andalso x > ~0.1 andalso y < 0.1 andalso y > ~0.1

  fun axisToDir (x, y, l2, r2) =
    if isInDeadZone (x, y) then
      (* analogue is in dead zone, so only query L2 and R2 *)
      if r2 < 0.3 andalso l2 < 0.3 then CENTRE
      else if abs r2 > abs l2 then R2
      else L2
    else
      let
        val ax = abs x
        val ay = abs y
        val al2 = abs l2
        val ar2 = abs r2
      in
        if ax > ay andalso ax > al2 andalso ax > ar2 then
          if x > 0.0 then RIGHT else LEFT
        else if ay > ax andalso ay > al2 andalso ay > ar2 then
          if y > 0.0 then DOWN else UP
        else if al2 > ax andalso al2 > ay andalso al2 > ar2 then
          L2
        else
          R2
      end

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
      , shiftChr = ref false
      }

    fun appendChr chr =
      (InputMailbox.append (InputMsg.CHAR_EVENT chr); #shiftChr state := false)

    fun handleTrianglePressed (x, y, l2, r2) =
      if !(#trianglePressed state) then
        ()
      else
        let
          val () = #trianglePressed state := true
          val chr =
            if !(#shiftChr state) then
              case axisToDir (x, y, l2, r2) of
                CENTRE => #"A"
              | UP => #"E"
              | RIGHT => #"I"
              | DOWN => #"M"
              | LEFT => #"Q"
              | L2 => #"U"
              | R2 => #"Y"
            else
              case axisToDir (x, y, l2, r2) of
                CENTRE => #"a"
              | UP => #"e"
              | RIGHT => #"i"
              | DOWN => #"m"
              | LEFT => #"q"
              | L2 => #"u"
              | R2 => #"y"
        in
          appendChr chr
        end

    fun handleCirclePressed (x, y, l2, r2) =
      if !(#circlePressed state) then
        ()
      else
        let
          val () = #circlePressed state := true
          val chr =
            if !(#shiftChr state) then
              case axisToDir (x, y, l2, r2) of
                CENTRE => #"B"
              | UP => #"F"
              | RIGHT => #"J"
              | DOWN => #"N"
              | LEFT => #"R"
              | L2 => #"V"
              | R2 => #"Z"
            else
              case axisToDir (x, y, l2, r2) of
                CENTRE => #"b"
              | UP => #"f"
              | RIGHT => #"j"
              | DOWN => #"n"
              | LEFT => #"r"
              | L2 => #"v"
              | R2 => #"z"
        in
          appendChr chr
        end

    fun handleCrossPressed (x, y, l2, r2) =
      if !(#crossPressed state) then
        ()
      else
        let
          val () = #crossPressed state := true
        in
          if !(#shiftChr state) then
            case axisToDir (x, y, l2, r2) of
              CENTRE => appendChr #"U"
            | UP => appendChr #"U"
            | RIGHT => appendChr #"U"
            | DOWN => appendChr #"U"
            | LEFT => appendChr #"U"
            | L2 => appendChr #"U"
            | R2 => InputMailbox.append InputMsg.KEY_ENTER
          else
            case axisToDir (x, y, l2, r2) of
              CENTRE => appendChr #"c"
            | UP => appendChr #"g"
            | RIGHT => appendChr #"k"
            | DOWN => appendChr #"o"
            | LEFT => appendChr #"s"
            | L2 => appendChr #"w"
            | R2 => InputMailbox.append InputMsg.KEY_ENTER
        end

    fun handleSquarePressed (x, y, l2, r2) =
      if !(#squarePressed state) then
        ()
      else
        let
          val () = #squarePressed state := true
        in
          if !(#shiftChr state) then
            case axisToDir (x, y, l2, r2) of
              CENTRE => appendChr #"U"
            | UP => appendChr #"U"
            | RIGHT => appendChr #"U"
            | DOWN => appendChr #"U"
            | LEFT => appendChr #"U"
            | L2 => appendChr #"U"
            | R2 => #shiftChr state := true
          else
            case axisToDir (x, y, l2, r2) of
              CENTRE => appendChr #"d"
            | UP => appendChr #"h"
            | RIGHT => appendChr #"l"
            | DOWN => appendChr #"p"
            | LEFT => appendChr #"t"
            | L2 => appendChr #"x"
            | R2 => #shiftChr state := true
        end
  in
    fun handleIfJoystickIsPresent () =
      let
        open InputMsg

        (* query possible events for information *)
        val xAxis = Input.getLeftJoystickXAxisState ()
        val yAxis = Input.getLeftJoystickYAxisState ()

        val r2 = (Input.getR2State () + 1.0) / 2.0
        val l2 = (Input.getL2State () + 1.0) / 2.0

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
          else handleCrossPressed (xAxis, yAxis, l2, r2)

        val () =
          if circlePressed = 0 then #circlePressed state := false
          else if !(#circlePressed state) then ()
          else handleCirclePressed (xAxis, yAxis, l2, r2)

        val () =
          if squarePressed = 0 then #squarePressed state := false
          else handleSquarePressed (xAxis, yAxis, l2, r2)

        val () =
          if trianglePressed = 0 then #trianglePressed state := false
          else if !(#trianglePressed state) then ()
          else handleTrianglePressed (xAxis, yAxis, l2, r2)

        val () =
          if r1Pressed = 0 then
            #r1Pressed state := false
          else if !(#r1Pressed state) then
            ()
          else
            let val () = InputMailbox.append (InputMsg.CHAR_EVENT #" ")
            in #r1Pressed state := true
            end

        val () =
          if l1Pressed = 0 then
            #l1Pressed state := false
          else if !(#l1Pressed state) then
            ()
          else
            let val () = InputMailbox.append InputMsg.KEY_BACKSPACE
            in #l1Pressed state := true
            end

        val () =
          (* if l2 and r2 are both pressed, then send escape event *)
          if l2 > 0.3 andalso r2 > 0.3 then
            InputMailbox.append InputMsg.KEY_ESC
          else
            ()
      in
        ()
      end
  end

  fun query () =
    if getGamepadState () then handleIfJoystickIsPresent ()
    else (* nothing to do if no gamepad is present *) ()
end
