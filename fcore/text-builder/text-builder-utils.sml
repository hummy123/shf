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

    , searchList: int vector
    , searchLen: int
    }

  fun initEnv
    ( endX
    , endY
    , floatWindowWidth
    , floatWindowHeight
    , searchList
    , searchLen
    , visualScrollColumn
    , startLine
    ) : env_data =
    if TC.textLineWidth > endX then
      { charR = 0.67
      , charG = 0.51
      , charB = 0.83

      , highlightR = 0.211
      , highlightG = 0.219
      , highlightB = 0.25

      , cursorR = 1.0
      , cursorG = 1.0
      , cursorB = 1.0

      , highlightOnCharR = 0.0
      , highlightOnCharG = 0.0
      , highlightOnCharB = 0.0

      , cursorOnCharR = 0.67
      , cursorOnCharG = 0.51
      , cursorOnCharB = 0.83

      , charZ = 0.01
      , cursorZ = 0.05
      , highlightZ = 0.03

      , startX = 5
      , startY = 5

      , scrollColumnStart = visualScrollColumn
      , scrollColumnEnd = let val width = endX - 5
                          in width div TC.xSpace + visualScrollColumn
                          end

      , lastLineNumber =
          let
            val height = endY - 5
            val howManyLines = height div TC.ySpace
          in
            startLine + howManyLines
          end

      , fw = floatWindowWidth
      , fh = floatWindowHeight

      , searchList = searchList
      , searchLen = searchLen
      }
    else
      let
        val startX = (endX - TC.textLineWidth) div 2
        val finishWidth = startX + TC.textLineWidth
      in
        { charR = 0.67
        , charG = 0.51
        , charB = 0.83

        , highlightR = 0.211
        , highlightG = 0.219
        , highlightB = 0.25

        , cursorR = 1.0
        , cursorG = 1.0
        , cursorB = 1.0

        , highlightOnCharR = 0.0
        , highlightOnCharG = 0.0
        , highlightOnCharB = 0.0

        , cursorOnCharR = 0.67
        , cursorOnCharG = 0.51
        , cursorOnCharB = 0.83

        , charZ = 0.01
        , cursorZ = 0.05
        , highlightZ = 0.03

        , startX = startX
        , startY = 5

        , scrollColumnStart = visualScrollColumn
        , scrollColumnEnd = TC.textLineCount

        , lastLineNumber =
            let
              val height = endY - 5
              val howManyLines = height div TC.ySpace
            in
              startLine + howManyLines
            end

        , fw = floatWindowWidth
        , fh = floatWindowHeight

        , searchList = searchList
        , searchLen = searchLen
        }
      end

  (* different functions to make vectors of different things we want to draw. *)
  fun makeCursor (posX, posY, env: env_data) =
    Rect.lerp
      ( Real32.fromInt (posX - 1)
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
      ( Real32.fromInt (posX - 1)
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

  fun isInSearchRange
    (absIdx, searchPos, {searchList, searchLen, ...}: env_data) =
    let val searchIdx = Vector.sub (searchList, searchPos)
    in absIdx >= searchIdx andalso absIdx < searchIdx + searchLen
    end

  fun isAfterSearchRange
    (absIdx, searchPos, {searchList, searchLen, ...}: env_data) =
    let val searchIdx = Vector.sub (searchList, searchPos)
    in absIdx >= searchIdx + searchLen
    end

  fun advanceSearchPos (absIdx, searchPos, env) =
    if isAfterSearchRange (absIdx, searchPos, env) then searchPos + 1
    else searchPos

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
