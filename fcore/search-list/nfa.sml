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

  val groupLevel = 1
  val postfixLevel = 2
  val concatLevel = 3
  val altLevel = 4

  local
    fun loop (pos, str, openParens, closeParens) =
      if pos = String.size str then
        pos
      else
        case String.sub (str, pos) of
          #"(" => loop (pos + 1, str, openParens + 1, closeParens)
        | #")" =>
            if closeParens + 1 = openParens then pos
            else loop (pos + 1, str, openParens, closeParens + 1)
        | _ => loop (pos + 1, str, openParens, closeParens)
  in
    fun getRightParenIdx (pos, str) = loop (pos, str, 1, 0)
  end

  fun helpClimb (pos, str, lhs, level) =
    if pos = String.size str then
      (pos, lhs)
    else
      case String.sub (str, pos) of
        #")" => (pos + 1, lhs)
      | #"(" =>
          if level < groupLevel then
            (pos, lhs)
          else
            let
              val groupEndIdx = getRightParenIdx (pos + 1, str)
              val substr = String.substring
                (str, pos + 1, groupEndIdx - pos - 1)
              val rhs = climb substr
              val rhs = GROUP rhs
              val result = CONCAT [lhs, rhs]
            in
              helpClimb (groupEndIdx + 1, str, result, groupLevel)
            end
      | #"|" =>
          if level < altLevel then
            (pos, lhs)
          else
            let
              val chr = String.sub (str, pos + 1)
              val chr = CHAR_LITERAL chr
              val (pos, rhs) = helpClimb (pos + 2, str, chr, altLevel)
              val result =
                case rhs of
                  ALTERNATION lst => ALTERNATION (lhs :: lst)
                | _ => ALTERNATION [lhs, rhs]
            in
              (pos, result)
            end
      | #"?" =>
          if level < postfixLevel then
            (pos, lhs)
          else
            let val lhs = ZERO_OR_ONE lhs
            in helpClimb (pos + 1, str, lhs, postfixLevel)
            end
      | #"*" =>
          if level < postfixLevel then
            (pos, lhs)
          else
            let val lhs = ZERO_OR_MORE lhs
            in helpClimb (pos + 1, str, lhs, postfixLevel)
            end
      | #"+" =>
          if level < postfixLevel then
            (pos, lhs)
          else
            let val lhs = ONE_OR_MORE lhs
            in helpClimb (pos + 1, str, lhs, postfixLevel)
            end
      | chr =>
          if level < concatLevel then
            (pos, lhs)
          else
            let
              val chr = CHAR_LITERAL chr
              val (pos, rhs) = helpClimb (pos + 1, str, chr, concatLevel)
              val result =
                case rhs of
                  CONCAT lst => CONCAT (lhs :: lst)
                | _ => CONCAT [lhs, rhs]
            in
              (pos, result)
            end

  and loop (pos, str, ast) =
    if pos = String.size str then
      ast
    else
      let val (pos, ast) = helpClimb (pos, str, ast, altLevel)
      in loop (pos, str, ast)
      end

  and climb str =
    let
      val chr = String.sub (str, 0)
      val chr = CHAR_LITERAL chr
    in
      loop (1, str, chr)
    end
end
