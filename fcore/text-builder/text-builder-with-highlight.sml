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
            skipToFirstVisibleColumn
              ( strPos
              , shd
              , stl
              , lhd
              , ltl
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

  and skipToNextLine
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
        val linePos = BinSearch.equalOrMore (pos + 1, line)
      in
        if linePos = ~1 then
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
            val lineOffset = Vector.sub (line, linePos)
            val newStrPos = lineOffset + 1
            val absIdx = absIdx - pos + newStrPos
            val posY = posY + TC.ySpace
            val lineNumber = lineNumber + 1
          in
            skipToFirstVisibleColumn
              ( newStrPos
              , str
              , stl
              , line
              , ltl
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

  and skipToFirstVisibleColumn
    ( pos
    , str
    , stl
    , line
    , ltl
    , posY
    , column
    , lineNumber
    , absIdx
    , cursorIdx
    , env: Utils.env_data
    , acc
    , searchPos
    ) =
    if column = #scrollColumnStart env then
      (* return to build function *)
      build
        ( pos
        , str
        , stl
        , line
        , ltl
        , #startX env
        , posY
        , column
        , lineNumber
        , absIdx
        , cursorIdx
        , env
        , acc
        , searchPos
        )
    else if pos = String.size str then
      (* go to next node *)
      case (stl, ltl) of
        (shd :: stl, lhd :: ltl) =>
          skipToFirstVisibleColumn
            ( 0
            , shd
            , stl
            , lhd
            , ltl
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
    else
      case String.sub (str, pos) of
        #"\n" =>
          let
            (* increment line lineNumber and posY, 
             * and then call skipToFirstVisibleColumn recursively.
             * The recursive call check this condition:
             * Is the new column 0 the same as the column the scroll starts at?
             * If it is, then we call build, or else we continue skipping 
             * until we reach the start column. *)
            val posY = posY + TC.ySpace
            val lineNumber = lineNumber + 1
          in
            skipToFirstVisibleColumn
              ( pos + 1
              , str
              , stl
              , line
              , ltl
              , posY
              , 0
              , lineNumber
              , absIdx + 1
              , cursorIdx
              , env
              , acc
              , searchPos
              )
          end
      | chr =>
          skipToFirstVisibleColumn
            ( pos + 1
            , str
            , stl
            , line
            , ltl
            , posY
            , column + 1
            , lineNumber
            , absIdx + 1
            , cursorIdx
            , env
            , acc
            , searchPos
            )

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
      skipToFirstVisibleColumn
        ( pos
        , str
        , stl
        , line
        , ltl
        , posY
        , column
        , lineNumber
        , absIdx
        , cursorIdx
        , env
        , acc
        , searchPos
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
                    Utils.makeCursorOnChr (chr, posX, posY, env)
                    :: Utils.makeCursor (posX, posY, env) :: acc
                  else if Utils.isInSearchRange (absIdx, searchPos, env) then
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
                  , searchPos
                  )
              end
      end
end
