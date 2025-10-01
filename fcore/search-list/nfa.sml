structure Nfa =
struct
  datatype regex =
    CHAR_LITERAL of {char: char, position: int}
  | CONCAT of regex list
  | ALTERNATION of regex list
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

    val groupLevel = 1
    val postfixLevel = 2
    val concatLevel = 3
    val altLevel = 4

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

    fun climb (pos, str, lhs, level, stateNum) : (int * regex * int) option =
      if pos = String.size str then
        SOME (pos, lhs, stateNum)
      else
        case String.sub (str, pos) of
          #")" => SOME (pos + 1, lhs, stateNum)
        | #"(" =>
            if level < groupLevel then
              SOME (pos, lhs, stateNum)
            else
              (case getRightParenIdx (pos + 1, str) of
                 SOME groupEndIdx =>
                   let
                     val substr = String.substring
                       (str, pos + 1, groupEndIdx - pos - 1)
                   in
                     (case parse (substr, stateNum) of
                        SOME (rhs, stateNum) =>
                          let
                            val rhs = GROUP rhs
                            val result = CONCAT [lhs, rhs]
                          in
                            climb
                              ( groupEndIdx + 1
                              , str
                              , result
                              , groupLevel
                              , stateNum
                              )
                          end
                      | NONE => NONE)
                   end
               | NONE => NONE)
        | #"|" =>
            if level < altLevel then
              SOME (pos, lhs, stateNum)
            else if pos + 1 < String.size str then
              let
                val chr = String.sub (str, pos + 1)
                val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
              in
                case climb (pos + 2, str, chr, altLevel, stateNum + 1) of
                  SOME (pos, rhs, stateNum) =>
                    let
                      val result =
                        case rhs of
                          ALTERNATION lst => ALTERNATION (lhs :: lst)
                        | _ => ALTERNATION [lhs, rhs]
                    in
                      SOME (pos, result, stateNum)
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
              let
                val currentState =
                  if chr = #"." then WILDCARD (stateNum + 1)
                  else CHAR_LITERAL {char = chr, position = stateNum + 1}
              in
                case
                  climb (pos + 1, str, currentState, concatLevel, stateNum + 1)
                of
                  SOME (pos, rhs, stateNum) =>
                    let
                      val result =
                        case rhs of
                          CONCAT lst => CONCAT (lhs :: lst)
                        | _ => CONCAT [lhs, rhs]
                    in
                      SOME (pos, result, stateNum)
                    end
                | NONE => NONE
              end

    and loop (pos, str, ast, stateNum) =
      if pos = String.size str then
        SOME (ast, stateNum)
      else
        case climb (pos, str, ast, altLevel, stateNum) of
          SOME (pos, ast, stateNum) => loop (pos, str, ast, stateNum)
        | NONE => NONE

    and parse (str, stateNum) =
      if String.size str > 0 then
        (* todo: we currently assume that the first char is always a CHAR_LITERAL
        * but we should actually check what character the chr is
        * before deciding it represents one variant or another *)
        let
          val chr = String.sub (str, 0)
          val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
        in
          loop (1, str, chr, stateNum + 1)
        end
      else
        NONE
  end

  structure ToDfa =
  struct
    fun isNullable tree =
      case tree of
        CHAR_LITERAL _        => false
      | WILDCARD              => false

      | CONCAT []             => true
      | CONCAT concatList     => isConcatNullable concatList

      | ALTERNATION []        => true
      | ALTERNATION altList   => isAlternationNullable (altList, true)

      | ZERO_OR_ONE _         => true
      | ZERO_OR_MORE _        => true

      | ONE_OR_MORE regex     => isNullable regex
      | GROUP regex           => isNullable regex

    (* if just one node is nullable, then concat node is nullable too *)
    and isConcatNullable lst =
      case lst of
        hd :: tl  => isNullable hd orelse isAlternationNullable (tl, false)
      | []        => false

    (* if all nodes are nullable, then alt node is also nullable *)
    and isAlternationNullable (lst, areAllNullableSoFar) =
      case lst of
        hd :: tl =>
          let
            val isCurrentNullable = isNullable hd andalso areAllNullableSoFar
          in
            isAlternationNullable (tl, isCurrentNullable)
          end
      | [] => areAllNullableSoFar
  end

  fun parse str =
    case ParseNfa.parse (str, 0) of
      SOME (ast, _) => SOME ast
    | NONE          => NONE
end
