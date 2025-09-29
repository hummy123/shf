structure TextBuilderWithHighlight =
struct
  structure TC = TextConstants
  structure Utils = TextBuilderUtils

  fun isSecondLastChr (pos, str, tl) =
    case tl of
      [] => pos = String.size str - 2
    | _ => false

  fun goToFirstLineAfter
    (stl, ltl, posY, lineNumber, absIdx, cursorIdx, env, acc) =
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
            )
    | (_, _) => acc

  and skipToNextLine
    (pos, str, stl, line, ltl, posY, lineNumber, absIdx, cursorIdx, env, acc) =
    if Vector.length line = 0 then
      let
        (* get index of buffer after this string *)
        val absIdx = absIdx - pos
        val absIdx = absIdx + String.size str
      in
        goToFirstLineAfter
          (stl, ltl, posY, lineNumber, absIdx, cursorIdx, env, acc)
      end
    else
      (* bin search lines *)
      let
        val linePos = BinSearch.equalOrMore (pos + 1, line)
      in
        if linePos = ~1 then
          (* next line is not in this node *)
          let
            val absIdx = absIdx - pos
            val absIdx = absIdx + String.size str
          in
            goToFirstLineAfter
              (stl, ltl, posY, lineNumber, absIdx, cursorIdx, env, acc)
          end
        else
          let
            val lineOffset = Vector.sub (line, linePos)
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
    ) =
    if pos = String.size str then
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
            )
      | (_, _) => acc
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
              if PersistentVector.isInRange (absIdx, #searchList env) then
                Utils.makeHighlight (posX, posY, env) :: acc
              else
                acc
            val posX =
              if column < #scrollColumnStart env then #startX env
              else posX + TC.xSpace
          in
            build
              ( pos + 1
              , str
              , stl
              , line
              , ltl
              , posX
              , posY
              , column + 1
              , lineNumber
              , absIdx + 1
              , cursorIdx
              , env
              , acc
              )
          end
      | #"\n" =>
          if lineNumber + 1 > #lastLineNumber env then
            acc
          else
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
                )
            end
      | chr =>
          if column < #scrollColumnStart env then
            build
              ( pos + 1
              , str
              , stl
              , line
              , ltl
              , #startX env
              , posY
              , column + 1
              , lineNumber
              , absIdx + 1
              , cursorIdx
              , env
              , acc
              )
          else if column > #scrollColumnEnd env then
            skipToNextLine
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
              )
          else
            let
              val acc =
                if absIdx = cursorIdx then
                  Utils.makeCursorOnChr (chr, posX, posY, env)
                  :: Utils.makeCursor (posX, posY, env) :: acc
                else if PersistentVector.isInRange (absIdx, #searchList env) then
                  Utils.makeHighlightChr (chr, posX, posY, env)
                  :: Utils.makeHighlight (posX, posY, env) :: acc
                else
                  Utils.makeChr (chr, posX, posY, env) :: acc
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
                )
            end
end
