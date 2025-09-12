structure TextBuilderUtils =
struct
  structure TC = TextConstants

  type env_data =
    { charR: Real32.real
    , charG: Real32.real
    , charB: Real32.real

    (* different colours for char when cursor is on char *)
    , cursorOnCharR: Real32.real
    , cursorOnCharG: Real32.real
    , cursorOnCharB: Real32.real

    , cursorR: Real32.real
    , cursorG: Real32.real
    , cursorB: Real32.real

    , highlightR: Real32.real
    , highlightG: Real32.real
    , highlightB: Real32.real

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
end
