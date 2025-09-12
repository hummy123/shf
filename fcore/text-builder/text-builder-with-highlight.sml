structure TextBuilderWithHighlight =
struct
  structure TC = TextConstants
  structure Utils = TextBuilderUtils

  fun goToFirstLineAfter
    (stl, ltl, posY, lineNumber, absIdx, cursorIdx, env, acc, searchPos) =
    case (stl, ltl) of
      (shd :: stl, lhd :: ltl) =>
        if Vector.length lhd > 0 then
          let
            val lineOffset = Vector.sub (lhd, 0)
            val strPos = lineOffset + 1
            val absIdx = absIdx + strPos
            val posY = posY + TC.ySpace
            val lineNumber = lineNumber + 1
          in
            build
              ( strPos
              , shd
              , stl
              , lhd
              , ltl
              , #startX env
              , posY
              , 0
              , lineNumber
              , absIdx
              , cursorIdx
              , env
              , acc
              , searchPos
              )
          end
        else
          (* keep looping until we find a linebreak *)
          goToFirstLineAfter
            ( stl
            , ltl
            , posY
            , lineNumber
            , absIdx + String.size shd
            , cursorIdx
            , env
            , acc
            , searchPos
            )
    | (_, _) => acc

  and skipToColumnStart
    ( pos
    , str
    , stl
    , line
    , ltl
    , posY
    , lineNumber
    , absIdx
    , cursorIdx
    , env
    , acc
    , searchPos
    ) =
    if Vector.length line = 0 then
      let
        (* get index of buffer after this string *)
        val absIdx = absIdx - pos
        val absIdx = absIdx + String.size str
      in
        goToFirstLineAfter
          (stl, ltl, posY, lineNumber, absIdx, cursorIdx, env, acc, searchPos)
      end
    else
      (* bin search lines *)
      let
        val searchPos = BinSearch.equalOrMore (pos + 1, #searchList env)
      in
        if searchPos = Vector.length line then
          (* next line is not in this node *)
          let
            val absIdx = absIdx - pos
            val absIdx = absIdx + String.size str
          in
            goToFirstLineAfter
              ( stl
              , ltl
              , posY
              , lineNumber
              , absIdx
              , cursorIdx
              , env
              , acc
              , searchPos
              )
          end
        else
          let
            val lineOffset = Vector.sub (line, searchPos)
            val newStrPos = lineOffset + 1
            val absIdx = absIdx - pos + newStrPos
            val posY = posY + TC.ySpace
            val lineNumber = lineNumber + 1
          in
            build
              ( newStrPos
              , str
              , stl
              , line
              , ltl
              , #startX env
              , posY
              , 0
              , lineNumber
              , absIdx
              , cursorIdx
              , env
              , acc
              , searchPos
              )
          end
      end

  and build
    ( pos
    , str
    , stl
    , line
    , ltl
    , posX
    , posY
    , column
    , lineNumber
    , absIdx
    , cursorIdx
    , env: Utils.env_data
    , acc
    , searchPos
    ) =
    if searchPos = Vector.length (#searchList env) then
      (* exhausted search list; call normal text-builder function *)
      TextBuilderWithCursor.build
        ( pos
        , str
        , stl
        , line
        , ltl
        , posX
        , posY
        , column
        , lineNumber
        , absIdx
        , cursorIdx
        , env
        , acc
        )
    else if pos = String.size str then
      case (stl, ltl) of
        (str :: stl, line :: ltl) =>
          build
            ( 0
            , str
            , stl
            , line
            , ltl
            , posX
            , posY
            , column
            , lineNumber
            , absIdx
            , cursorIdx
            , env
            , acc
            , searchPos
            )
      | (_, _) => acc
    else if column < #scrollColumnStart env then
      skipToColumnStart
        ( pos
        , str
        , stl
        , line
        , ltl
        , posY
        , lineNumber
        , absIdx
        , cursorIdx
        , env
        , acc
        , searchPos
        )
    else
      let
        val searchPos = Utils.advanceSearchPos (absIdx, searchPos, env)
      in
        if searchPos = Vector.length (#searchList env) then
          (* another check to see if we exhausted the searchList *)
          TextBuilderWithCursor.build
            ( pos
            , str
            , stl
            , line
            , ltl
            , posX
            , posY
            , column
            , lineNumber
            , absIdx
            , cursorIdx
            , env
            , acc
            )
        else
          case String.sub (str, pos) of
            #" " =>
              let
                val acc =
                  if absIdx = cursorIdx then
                    Utils.makeCursor (posX, posY, env) :: acc
                  else
                    acc
                val acc =
                  if Utils.isInSearchRange (absIdx, searchPos, env) then
                    Utils.makeHighlight (posX, posY, env) :: acc
                  else
                    acc
              in
                build
                  ( pos + 1
                  , str
                  , stl
                  , line
                  , ltl
                  , posX + TC.xSpace
                  , posY
                  , column + 1
                  , lineNumber
                  , absIdx + 1
                  , cursorIdx
                  , env
                  , acc
                  , searchPos
                  )
              end
          | #"\n" =>
              let
                val acc =
                  if absIdx = cursorIdx then
                    Utils.makeCursor (posX, posY, env) :: acc
                  else
                    acc
              in
                build
                  ( pos + 1
                  , str
                  , stl
                  , line
                  , ltl
                  , #startX env
                  , posY + TC.ySpace
                  , 0
                  , lineNumber + 1
                  , absIdx + 1
                  , cursorIdx
                  , env
                  , acc
                  , searchPos
                  )
              end
          | chr =>
              let
                val acc =
                  if absIdx = cursorIdx then
                    Utils.makeCursor (posX, posY, env)
                    :: Utils.makeCursorOnChr (chr, posX, posY, env) :: acc
                  else if Utils.isInSearchRange (absIdx, searchPos, env) then
                    Utils.makeHighlightChr (chr, posX, posY, env)
                    :: Utils.makeHighlight (posX, posY, env) :: acc
                  else
                    acc
              in
                build
                  ( pos + 1
                  , str
                  , stl
                  , line
                  , ltl
                  , posX + TC.xSpace
                  , posY
                  , column + 1
                  , lineNumber
                  , absIdx + 1
                  , cursorIdx
                  , env
                  , acc
                  , searchPos
                  )
              end
      end
end
