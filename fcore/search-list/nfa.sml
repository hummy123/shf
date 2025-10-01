structure Nfa =
struct
  datatype regex =
    CHAR_LITERAL of char
  | CONCAT of regex list
  | ALTERNATION of regex list
  | ZERO_OR_ONE of regex
  | ZERO_OR_MORE of regex
  | ONE_OR_MORE of regex
  | GROUP of regex
  | WILDCARD

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

    fun climb (pos, str, lhs, level) : (int * regex) option =
      if pos = String.size str then
        SOME (pos, lhs)
      else
        case String.sub (str, pos) of
          #")" => SOME (pos + 1, lhs)
        | #"(" =>
            if level < groupLevel then
              SOME (pos, lhs)
            else
              (case getRightParenIdx (pos + 1, str) of
                 SOME groupEndIdx =>
                   let
                     val substr = String.substring
                       (str, pos + 1, groupEndIdx - pos - 1)
                   in
                     (case parse substr of
                        SOME rhs =>
                          let
                            val rhs = GROUP rhs
                            val result = CONCAT [lhs, rhs]
                          in
                            climb (groupEndIdx + 1, str, result, groupLevel)
                          end
                      | NONE => NONE)
                   end
               | NONE => NONE)
        | #"|" =>
            if level < altLevel then
              SOME (pos, lhs)
            else if pos + 1 < String.size str then
              let
                val chr = String.sub (str, pos + 1)
                val chr = CHAR_LITERAL chr
              in
                case climb (pos + 2, str, chr, altLevel) of
                  SOME (pos, rhs) =>
                    let
                      val result =
                        case rhs of
                          ALTERNATION lst => ALTERNATION (lhs :: lst)
                        | _ => ALTERNATION [lhs, rhs]
                    in
                      SOME (pos, result)
                    end
                | NONE => NONE
              end
            else
              NONE
        | #"?" =>
            if level < postfixLevel then
              SOME (pos, lhs)
            else
              let val lhs = ZERO_OR_ONE lhs
              in climb (pos + 1, str, lhs, postfixLevel)
              end
        | #"*" =>
            if level < postfixLevel then
              SOME (pos, lhs)
            else
              let val lhs = ZERO_OR_MORE lhs
              in climb (pos + 1, str, lhs, postfixLevel)
              end
        | #"+" =>
            if level < postfixLevel then
              SOME (pos, lhs)
            else
              let val lhs = ONE_OR_MORE lhs
              in climb (pos + 1, str, lhs, postfixLevel)
              end
        | chr =>
            if level < concatLevel then
              SOME (pos, lhs)
            else
              let
                val currentState =
                  if chr = #"." then WILDCARD else CHAR_LITERAL chr
              in
                case climb (pos + 1, str, currentState, concatLevel) of
                  SOME (pos, rhs) =>
                    let
                      val result =
                        case rhs of
                          CONCAT lst => CONCAT (lhs :: lst)
                        | _ => CONCAT [lhs, rhs]
                    in
                      SOME (pos, result)
                    end
                | NONE => NONE
              end

    and loop (pos, str, ast) =
      if pos = String.size str then
        SOME ast
      else
        case climb (pos, str, ast, altLevel) of
          SOME (pos, ast) => loop (pos, str, ast)
        | NONE => NONE

    and parse str =
      if String.size str > 0 then
        (* todo: we currently assume that the first char is always a CHAR_LITERAL
        * but we should actually check what character the chr is
        * before deciding it represents one variant or another *)
        let
          val chr = String.sub (str, 0)
          val chr = CHAR_LITERAL chr
        in
          loop (1, str, chr)
        end
      else
        NONE
  end

  val parse = ParseNfa.parse
end
