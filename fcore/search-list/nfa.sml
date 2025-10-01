structure Nfa =
struct
  datatype regex =
    CHAR_LITERAL of {char: char, position: int}
  | CONCAT of regex * regex
  | ALTERNATION of regex * regex
  | ZERO_OR_ONE of regex
  | ZERO_OR_MORE of regex
  | ONE_OR_MORE of regex
  | GROUP of regex
  | WILDCARD of int

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
                  SOME (pos, rhs, stateNum) =>
                    let val result = ALTERNATION (lhs, rhs)
                    in SOME (pos, result, stateNum)
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
                SOME (nextPos, curAtom, stateNum) =>
                  (case climb (nextPos, str, curAtom, concatLevel, stateNum + 1) of
                     SOME (pos, rhs, stateNum) =>
                       let val result = CONCAT (lhs, rhs)
                       in SOME (pos, result, stateNum)
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

      | CONCAT (r1, r2) => isNullable r1 andalso isNullable r2
      | ALTERNATION (r1, r2) => isNullable r1 orelse isNullable r2

      | ZERO_OR_ONE _ => true
      | ZERO_OR_MORE _ => true

      | ONE_OR_MORE regex => isNullable regex
      | GROUP regex => isNullable regex

    fun firstpos (tree, acc) =
      case tree of
        CHAR_LITERAL {position, ...} => position :: acc
      | WILDCARD i => i :: acc

      | CONCAT (r1, r2) =>
          if isNullable r1 then
            let val acc = firstpos (r1, acc)
            in firstpos (r2, acc)
            end
          else
            firstpos (r1, acc)
      | ALTERNATION (r1, r2) =>
          let val acc = firstpos (r1, acc)
          in firstpos (r2, acc)
          end

      | ZERO_OR_ONE regex => firstpos (regex, acc)
      | ZERO_OR_MORE regex => firstpos (regex, acc)
      | ONE_OR_MORE regex => firstpos (regex, acc)
      | GROUP regex => firstpos (regex, acc)

    fun lastpos (tree, acc) =
      case tree of
        CHAR_LITERAL {position, ...} => position :: acc
      | WILDCARD i => i :: acc

      | CONCAT (r1, r2) =>
          if isNullable r2 then
            let val acc = lastpos (r1, acc)
            in lastpos (r2, acc)
            end
          else
            lastpos (r2, acc)
      | ALTERNATION (r1, r2) =>
          let val acc = lastpos (r1, acc)
          in lastpos (r2, acc)
          end

      | ZERO_OR_ONE regex => lastpos (regex, acc)
      | ZERO_OR_MORE regex => lastpos (regex, acc)
      | ONE_OR_MORE regex => lastpos (regex, acc)
      | GROUP regex => lastpos (regex, acc)
  end

  fun parse str =
    case ParseNfa.parse (str, 0) of
      SOME (ast, _) => SOME ast
    | NONE => NONE

  fun firstpos regex = ToDfa.firstpos (regex, [])
  fun lastpos regex = ToDfa.lastpos (regex, [])
end
