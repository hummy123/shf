structure GlfwGamepad =
struct
  datatype mode =
    PENDING
  | TRIANGLE
  | TRIANGLE_CIRCLE
  | CIRCLE
  | CIRCLE_CROSS
  | CROSS
  | CROSS_SQUARE
  | SQUARE
  (* maybe SQUARE_TRIANGLE for numbers and symbols? *)
  | SQUARE_TRIANGLE

  structure IM = InputMsg

  type gamepad_state =
    { mode: mode
    , shiftChr: bool
    , trianglePressed: bool
    , circlePressed: bool
    , crossPressed: bool
    , squarePressed: bool
    }

  fun setToPendingAndUnshifted (gamepadState: gamepad_state) =
    let
      val
        { mode = _
        , shiftChr = _
        , trianglePressed
        , circlePressed
        , crossPressed
        , squarePressed
        } = gamepadState
    in
      { mode = PENDING
      , shiftChr = false
      , trianglePressed = false
      , circlePressed = false
      , crossPressed = false
      , squarePressed = false
      }
    end

  fun onPendingMode
    ( state: gamepad_state
    , trianglePressed
    , circlePressed
    , crossPressed
    , squarePressed
    , actions: IM.t list
    ) =
    if
      trianglePressed orelse circlePressed orelse crossPressed
      orelse squarePressed
    then
      (* some button is being pressed, 
       * so we record that in the returned state,
       * in addition to whatever buttons were previously pressed *)
      let
        val trianglePressed = #trianglePressed state orelse trianglePressed
        val circlePressed = #circlePressed state orelse circlePressed
        val crossPressed = #crossPressed state orelse crossPressed
        val squarePressed = #squarePressed state orelse squarePressed

        val newState =
          { mode = #mode state
          , shiftChr = #shiftChr state
          , trianglePressed = trianglePressed
          , circlePressed = circlePressed
          , crossPressed = crossPressed
          , squarePressed = squarePressed
          }
      in
        (newState, actions)
      end
    else
      (* nothing is currently pressed, 
       * so we check if there is a valid mode indicated by the state
       * and change the mode if so *)
      let
        val
          { trianglePressed
          , circlePressed
          , crossPressed
          , squarePressed
          , shiftChr
          , mode = _
          } = state
        val newMode =
          if trianglePressed andalso not (crossPressed orelse squarePressed) then
            if not circlePressed then TRIANGLE else TRIANGLE_CIRCLE
          else if circlePressed andalso not squarePressed then
            if not crossPressed then CIRCLE else CIRCLE_CROSS
          else if crossPressed then
            if not squarePressed then CROSS else CROSS_SQUARE
          else if squarePressed then
            if not trianglePressed then SQUARE else SQUARE_TRIANGLE
          else
            (* some buttons are being pressed, 
             * but not a valid combination to switch
             * to another mode, so we are still on PENDING *)
            PENDING

        val newState =
          { mode = newMode
          , shiftChr = shiftChr
          , trianglePressed = false
          , circlePressed = false
          , crossPressed = false
          , squarePressed = false
          }
      in
        (newState, actions)
      end

  fun onTriangleMode
    ( gamepadState
    , trianglePressed
    , circlePressed
    , crossPressed
    , squarePressed
    , actions
    ) =
    if trianglePressed then
      let
        val newState = setToPendingAndUnshifted gamepadState
        val actions = IM.CHAR_EVENT #"a" :: actions
      in
        (newState, actions)
      end
    else if circlePressed then
      let
        val newState = setToPendingAndUnshifted gamepadState
        val actions = IM.CHAR_EVENT #"b" :: actions
      in
        (newState, actions)
      end
    else if crossPressed then
      let
        val newState = setToPendingAndUnshifted gamepadState
        val actions = IM.CHAR_EVENT #"c" :: actions
      in
        (newState, actions)
      end
    else if squarePressed then
      let
        val newState = setToPendingAndUnshifted gamepadState
        val actions = IM.CHAR_EVENT #"d" :: actions
      in
        (newState, actions)
      end
    else
      (gamepadState, actions)

  fun handleButtons
    ( gamepadState
    , trianglePressed
    , circlePressed
    , crossPressed
    , squarePressed
    , l1Pressed
    , r1Pressed
    ) =
    let
      val actions = if l1Pressed then [IM.KEY_BACKSPACE] else []
      val actions =
        if r1Pressed then (IM.CHAR_EVENT #" ") :: actions else actions
    in
      case #mode gamepadState of
        PENDING =>
          onPendingMode
            ( gamepadState
            , trianglePressed
            , circlePressed
            , crossPressed
            , squarePressed
            , actions
            )
      | TRIANGLE =>
          onTriangleMode
            ( gamepadState
            , trianglePressed
            , circlePressed
            , crossPressed
            , squarePressed
            , actions
            )
    end

  (* impure functions below *)
  fun getGamepadState () =
    Input.getGamepadState 0 <> 0 orelse Input.getGamepadState 1 <> 0
    orelse Input.getGamepadState 2 <> 0 orelse Input.getGamepadState 3 <> 0
    orelse Input.getGamepadState 4 <> 0 orelse Input.getGamepadState 5 <> 0
    orelse Input.getGamepadState 6 <> 0 orelse Input.getGamepadState 7 <> 0
    orelse Input.getGamepadState 8 <> 0 orelse Input.getGamepadState 9 <> 0
    orelse Input.getGamepadState 10 <> 0 orelse Input.getGamepadState 11 <> 0
    orelse Input.getGamepadState 12 <> 0 orelse Input.getGamepadState 13 <> 0
    orelse Input.getGamepadState 14 <> 0 orelse Input.getGamepadState 15 <> 0

  fun query gamepadState =
    if getGamepadState () then
      let
        val trianglePressed = Input.isTriangleButtonPressed () <> 0
        val circlePressed = Input.isCircleButtonPressed () <> 0
        val crossPressed = Input.isCrossButtonPressed () <> 0
        val squarePressed = Input.isSquareButtonPressed () <> 0
        val l1Pressed = Input.isL1ButtonPressed () <> 0
        val r1Pressed = Input.isL1ButtonPressed () <> 0
      in
        handleButtons
          ( gamepadState
          , trianglePressed
          , circlePressed
          , crossPressed
          , squarePressed
          , l1Pressed
          , r1Pressed
          )
      end
    else
      (* nothing to do, so return same state *)
      (gamepadState, [])
end
