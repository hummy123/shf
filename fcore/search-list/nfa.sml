structure Nfa =
struct
  datatype regex =
    CHAR_LITERAL of {char: char, position: int}
  | CONCAT of {l: regex, r: regex, leftMaxState: int, rightMaxState: int}
  | ALTERNATION of {l: regex, r: regex, leftMaxState: int, rightMaxState: int}
  | ZERO_OR_ONE of regex
  | ZERO_OR_MORE of regex
  | ONE_OR_MORE of regex
  | GROUP of regex
  | WILDCARD of int

  structure Set =
  struct
    datatype set = BRANCH of set * int * bool * set | LEAF

    fun insertIfNew (newKey, tree) =
      case tree of
        BRANCH (l, curKey, isMarked, r) =>
          if newKey > curKey then
            let val r = insertIfNew (newKey, r)
            in BRANCH (l, curKey, isMarked, r)
            end
          else if newKey < curKey then
            let val l = insertIfNew (newKey, l)
            in BRANCH (l, curKey, isMarked, r)
            end
          else
            tree
      | LEAF => BRANCH (LEAF, newKey, false, LEAF)

    fun setMarked (findKey, tree) =
      case tree of
        BRANCH (l, curKey, isMarked, r) =>
          if findKey > curKey then
            let val r = setMarked (findKey, r)
            in BRANCH (l, curKey, isMarked, r)
            end
          else if findKey < curKey then
            let val l = setMarked (findKey, l)
            in BRANCH (l, curKey, isMarked, r)
            end
          else
            BRANCH (l, curKey, true, r)
      | LEAF => (* this case should not occur *) LEAF
  end

  structure ParseNfa =
  struct
    (* parsing through precedence climbing algorithm. *)

    datatype action =
      TRY_NEXT_NODE_WITHOUT_CONSUMING_CHR

    val postfixLevel = 1
    val concatLevel = 2
    val altLevel = 3

    local
      fun loop (pos, str, openParens, closeParens) =
        if pos = String.size str then
          NONE
        else
          case String.sub (str, pos) of
            #"(" => loop (pos + 1, str, openParens + 1, closeParens)
          | #")" =>
              if closeParens + 1 = openParens then SOME pos
              else loop (pos + 1, str, openParens, closeParens + 1)
          | _ => loop (pos + 1, str, openParens, closeParens)
    in
      fun getRightParenIdx (pos, str) = loop (pos, str, 1, 0)
    end

    fun computeAtom (pos, str, stateNum) =
      if pos = String.size str then
        NONE
      else
        case String.sub (str, pos) of
          #"(" =>
            (case getRightParenIdx (pos + 1, str) of
               SOME groupEndIdx =>
                 let
                   val substr = String.substring
                     (str, pos + 1, groupEndIdx - pos - 1)
                 in
                   case parse (substr, stateNum) of
                     SOME (rhs, stateNum) =>
                       SOME (groupEndIdx + 1, rhs, stateNum)
                   | NONE => NONE
                 end
             | NONE => NONE)
        | #")" => NONE
        | #"?" => NONE
        | #"*" => NONE
        | #"+" => NONE
        | #"." => SOME (pos + 1, WILDCARD (stateNum + 1), stateNum + 1)
        | chr =>
            let val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
            in SOME (pos + 1, chr, stateNum + 1)
            end

    and climb (pos, str, lhs, level, stateNum) : (int * regex * int) option =
      if pos = String.size str then
        SOME (pos, lhs, stateNum)
      else
        case String.sub (str, pos) of
          #"|" =>
            if level < altLevel then
              SOME (pos, lhs, stateNum)
            else if pos + 1 < String.size str then
              let
                val chr = String.sub (str, pos + 1)
                val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
              in
                case climb (pos + 2, str, chr, altLevel, stateNum + 1) of
                  SOME (pos, rhs, rightStateNum) =>
                    let
                      val result = ALTERNATION
                        { l = lhs
                        , r = rhs
                        , leftMaxState = stateNum
                        , rightMaxState = rightStateNum
                        }
                    in
                      SOME (pos, result, rightStateNum)
                    end
                | NONE => NONE
              end
            else
              NONE
        | #"?" =>
            if level < postfixLevel then
              SOME (pos, lhs, stateNum)
            else
              let val lhs = ZERO_OR_ONE lhs
              in climb (pos + 1, str, lhs, postfixLevel, stateNum)
              end
        | #"*" =>
            if level < postfixLevel then
              SOME (pos, lhs, stateNum)
            else
              let val lhs = ZERO_OR_MORE lhs
              in climb (pos + 1, str, lhs, postfixLevel, stateNum)
              end
        | #"+" =>
            if level < postfixLevel then
              SOME (pos, lhs, stateNum)
            else
              let val lhs = ONE_OR_MORE lhs
              in climb (pos + 1, str, lhs, postfixLevel, stateNum)
              end
        | chr =>
            if level < concatLevel then
              SOME (pos, lhs, stateNum)
            else
              case computeAtom (pos, str, stateNum) of
                SOME (nextPos, curAtom, atomStateNum) =>
                  (case climb (nextPos, str, curAtom, concatLevel, atomStateNum) of
                     SOME (pos, rhs, rightStateNum) =>
                       let
                         val result = CONCAT
                           { l = lhs
                           , r = rhs
                           , leftMaxState = stateNum
                           , rightMaxState = rightStateNum
                           }
                       in
                         SOME (pos, result, rightStateNum)
                       end
                   | NONE => NONE)
              | NONE => NONE

    and loop (pos, str, ast, stateNum) =
      if pos = String.size str then
        SOME (ast, stateNum)
      else
        case climb (pos, str, ast, altLevel, stateNum) of
          SOME (pos, ast, stateNum) => loop (pos, str, ast, stateNum)
        | NONE => NONE

    and parse (str, stateNum) =
      if String.size str > 0 then
        case computeAtom (0, str, stateNum) of
          SOME (nextPos, lhs, stateNum) => loop (nextPos, str, lhs, stateNum)
        | NONE => NONE
      else
        NONE
  end

  structure ToDfa =
  struct
    fun isNullable tree =
      case tree of
        CHAR_LITERAL _ => false
      | WILDCARD _ => false

      | CONCAT {l, r, ...} => isNullable l andalso isNullable r
      | ALTERNATION {l, r, ...} => isNullable l orelse isNullable r

      | ZERO_OR_ONE _ => true
      | ZERO_OR_MORE _ => true

      | ONE_OR_MORE regex => isNullable regex
      | GROUP regex => isNullable regex

    fun firstpos (tree, acc) =
      case tree of
        CHAR_LITERAL {position, ...} => position :: acc
      | WILDCARD i => i :: acc

      | CONCAT {l, r, ...} =>
          if isNullable l then
            let val acc = firstpos (l, acc)
            in firstpos (r, acc)
            end
          else
            firstpos (l, acc)
      | ALTERNATION {l, r, ...} =>
          let val acc = firstpos (l, acc)
          in firstpos (r, acc)
          end

      | ZERO_OR_ONE regex => firstpos (regex, acc)
      | ZERO_OR_MORE regex => firstpos (regex, acc)
      | ONE_OR_MORE regex => firstpos (regex, acc)
      | GROUP regex => firstpos (regex, acc)

    fun lastpos (tree, acc) =
      case tree of
        CHAR_LITERAL {position, ...} => position :: acc
      | WILDCARD i => i :: acc

      | CONCAT {l, r, ...} =>
          if isNullable r then
            let val acc = lastpos (l, acc)
            in lastpos (r, acc)
            end
          else
            lastpos (l, acc)
      | ALTERNATION {l, r, ...} =>
          let val acc = lastpos (l, acc)
          in lastpos (r, acc)
          end

      | ZERO_OR_ONE regex => lastpos (regex, acc)
      | ZERO_OR_MORE regex => lastpos (regex, acc)
      | ONE_OR_MORE regex => lastpos (regex, acc)
      | GROUP regex => lastpos (regex, acc)

    fun followpos tree =
      case tree of
        CONCAT {r, ...} => firstpos (r, [])
      | ZERO_OR_MORE r => firstpos (r, [])
      | ZERO_OR_ONE r => firstpos (r, [])
      | ONE_OR_MORE r => firstpos (r, [])
      | _ => []
  end

  fun parse str =
    case ParseNfa.parse (str, 0) of
      SOME (ast, _) => SOME ast
    | NONE => NONE

  fun firstpos regex = ToDfa.firstpos (regex, [])

  fun lastpos regex = ToDfa.lastpos (regex, [])
end
