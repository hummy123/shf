structure TextBuilderUtils =
struct
  structure TC = TextConstants

  type env_data =
    { charR: Real32.real
    , charG: Real32.real
    , charB: Real32.real

    , cursorR: Real32.real
    , cursorG: Real32.real
    , cursorB: Real32.real

    , highlightR: Real32.real
    , highlightG: Real32.real
    , highlightB: Real32.real

    (* different colours for char when cursor is on char *)
    , cursorOnCharR: Real32.real
    , cursorOnCharG: Real32.real
    , cursorOnCharB: Real32.real

    , highlightOnCharR: Real32.real
    , highlightOnCharG: Real32.real
    , highlightOnCharB: Real32.real

    , charZ: Real32.real
    , cursorZ: Real32.real
    , highlightZ: Real32.real

    , startX: int
    , startY: int

    , scrollColumnStart: int
    , scrollColumnEnd: int
    , lastLineNumber: int

    (* fw/fh = float window width and float window height *)
    , fw: Real32.real
    , fh: Real32.real

    , searchList: PersistentVector.t
    }

  fun initEnv
    ( startX
    , startY
    , endX
    , endY
    , floatWindowWidth
    , floatWindowHeight
    , searchList
    , visualScrollColumn
    , startLine
    ) : env_data =
    let
      val width = endX - startX
      val lastLineNumber =
        let
          val height = endY - startY
          val howManyLines = height div TC.ySpace
        in
          startLine + howManyLines
        end
    in
      if TC.textLineWidth > width then
        { charR = 0.01
        , charG = 0.01
        , charB = 0.01

        , highlightR = 0.76
        , highlightG = 0.73
        , highlightB = 0.71

        , cursorR = 0.65
        , cursorG = 0.01
        , cursorB = 0.01

        , highlightOnCharR = 0.0
        , highlightOnCharG = 0.0
        , highlightOnCharB = 0.0

        , cursorOnCharR = 1.0
        , cursorOnCharG = 1.0
        , cursorOnCharB = 1.0

        , charZ = 0.01
        , cursorZ = 0.03
        , highlightZ = 0.05

        , startX = startX
        , startY = startX

        , scrollColumnStart = visualScrollColumn
        , scrollColumnEnd = width div TC.xSpace + visualScrollColumn
        , lastLineNumber = lastLineNumber

        , fw = floatWindowWidth
        , fh = floatWindowHeight

        , searchList = searchList
        }
      else
        let
          val startX = (width - TC.textLineWidth) div 2
        in
          { charR = 0.01
          , charG = 0.01
          , charB = 0.01

          , highlightR = 0.76
          , highlightG = 0.73
          , highlightB = 0.71

          , cursorR = 0.65
          , cursorG = 0.01
          , cursorB = 0.01

          , highlightOnCharR = 0.0
          , highlightOnCharG = 0.0
          , highlightOnCharB = 0.0

          , cursorOnCharR = 1.0
          , cursorOnCharG = 1.0
          , cursorOnCharB = 1.0

          , charZ = 0.01
          , cursorZ = 0.03
          , highlightZ = 0.05

          , startX = startX
          , startY = startY

          , scrollColumnStart = visualScrollColumn
          , scrollColumnEnd = visualScrollColumn + TC.textLineCount
          , lastLineNumber = lastLineNumber

          , fw = floatWindowWidth
          , fh = floatWindowHeight

          , searchList = searchList
          }
        end
    end

  (* different functions to make vectors of different things we want to draw. *)
  fun makeCursor (posX, posY, env: env_data) =
    Rect.lerp
      ( Real32.fromInt (posX - 2)
      , Real32.fromInt posY
      , #cursorZ env
      , TC.scale
      , #fw env
      , #fh env
      , #cursorR env
      , #cursorG env
      , #cursorB env
      )

  fun makeHighlight (posX, posY, env: env_data) =
    Rect.lerp
      ( Real32.fromInt (posX - 2)
      , Real32.fromInt posY
      , #highlightZ env
      , TC.scale
      , #fw env
      , #fh env
      , #highlightR env
      , #highlightG env
      , #highlightB env
      )

  fun makeChr (chr, posX, posY, env: env_data) =
    CozetteAscii.make
      ( chr
      , Real32.fromInt posX
      , Real32.fromInt posY
      , #charZ env
      , TC.scale
      , #fw env
      , #fh env
      , #charR env
      , #charG env
      , #charB env
      )

  fun makeCursorOnChr (chr, posX, posY, env: env_data) =
    CozetteAscii.make
      ( chr
      , Real32.fromInt posX
      , Real32.fromInt posY
      , #charZ env
      , TC.scale
      , #fw env
      , #fh env
      , #cursorOnCharR env
      , #cursorOnCharG env
      , #cursorOnCharB env
      )

  fun makeHighlightChr (chr, posX, posY, env: env_data) =
    CozetteAscii.make
      ( chr
      , Real32.fromInt posX
      , Real32.fromInt posY
      , #charZ env
      , TC.scale
      , #fw env
      , #fh env
      , #highlightOnCharR env
      , #highlightOnCharG env
      , #highlightOnCharB env
      )

  (* gets line start idx, relative to right hd *)
  fun getRelativeLineStartFromRightHead (startLine, curLine, rLnHd) =
    if startLine > curLine then
      let val lnPos = startLine - curLine - 1
      in Vector.sub (rLnHd, lnPos) + 1
      end
    else
      0

  (* gets line start idx, absolute *)
  fun getAbsoluteLineStartFromRightHead (curIdx, startLine, curLine, rLnHd) =
    let
      val startIdx =
        if startLine > curLine then
          let val lnPos = startLine - curLine - 1
          in Vector.sub (rLnHd, lnPos) + 1
          end
        else
          0
    in
      curIdx + startIdx
    end

  fun getLineAbsIdxFromBuffer (startLine, buffer: LineGap.t) =
    let
      val {rightLines, line = curLine, idx = curIdx, ...} = buffer
    in
      case rightLines of
        rLnHd :: _ =>
          getAbsoluteLineStartFromRightHead (curIdx, startLine, curLine, rLnHd)
      | [] =>
          raise Fail
            "text-builder-utils.sml 268:\ 
            \should never call function when at end of buffer"
    end
end
