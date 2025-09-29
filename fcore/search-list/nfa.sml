structure Nfa =
struct
  datatype state = VALID of int | INVALID | UNTESTED

  datatype regex =
    CHAR_LITERAL of char * state
  | CONCAT of (regex * state) list * state
  | ALTERNATION of (regex * state) list * state
  | ZERO_OR_ONE of regex * state
  | ZERO_OR_MORE of regex * state
  | ONE_OR_MORE of regex * state
  | GROUP of regex * state

  val groupLevel = 1
  val postfixLevel = 2
  val concatLevel = 3
  val altLevel = 4

  local
    fun loop (tl, maxValid) =
      case tl of
        (_, VALID curValid) :: tl => loop (tl, Int.max (maxValid, curValid))
      | (_, UNTESTED) :: _ => UNTESTED
      | (_, INVALID) :: _ =>
          raise Fail
            "nfa.sml 24: \
            \should not have INVALID state in acc"
      | [] => VALID maxValid
  in
    fun getAlternationState acc =
      case acc of
        (_, VALID maxValid) :: tl => loop (tl, maxValid)
      | (_, UNTESTED) :: _ => UNTESTED
      | (_, INVALID) :: _ =>
          raise Fail
            "nfa.sml 26: \
            \should not have INVALID state in acc"
      | [] => UNTESTED
  end

  local
    fun rebuildConcat (lst, chr, idx) =
      case lst of
        [(hd, _)] =>
          let
            val (hd: regex, state: state) = rebuild (hd, chr, idx)
            val result = [(hd, state)]
            val concat = CONCAT (result, state)
          in
            (concat, state)
          end
      | (hd, _) :: tl =>
          let
            val (hd, state) = rebuild (hd, chr, idx)
          in
            case state of
              UNTESTED =>
                let val concat = CONCAT ((hd, state) :: tl, UNTESTED)
                in (concat, UNTESTED)
                end
            | INVALID =>
                let val concat = CONCAT ([], INVALID)
                in (concat, INVALID)
                end
            | VALID _ =>
                let val concat = CONCAT (tl, UNTESTED)
                in (concat, UNTESTED)
                end
          end
      | [] =>
          (* should never occur *)
          raise Fail
            "nfa.sml, rebuildConcat 45: \
            \should never try to rebuild empty concat list"

    and rebuildAlternation (lst, chr, idx, acc) =
      case lst of
        [(hd, _)] =>
          let
            val (hd, state) = rebuild (hd, chr, idx)
            val acc =
              case state of
                VALID _ => (hd, state) :: acc
              | UNTESTED => (hd, state) :: acc
              | INVALID => acc
            val state = getAlternationState acc
          in
            (ALTERNATION (acc, state), state)
          end
      | (hd, _) :: tl =>
          let
            val (hd, state) = rebuild (hd, chr, idx)
            val acc =
              case state of
                VALID _ => (hd, state) :: acc
              | UNTESTED => (hd, state) :: acc
              | INVALID => acc
          in
            rebuildAlternation (tl, chr, idx, acc)
          end
      | [] => (ALTERNATION ([], INVALID), INVALID)

    and rebuild (nfa, chr, idx) =
      case nfa of
        CHAR_LITERAL (lit, UNTESTED) =>
          if chr = lit then (CHAR_LITERAL (lit, VALID idx), VALID idx)
          else (CHAR_LITERAL (lit, INVALID), INVALID)
      | CHAR_LITERAL (lit, state) => (nfa, state)

      | CONCAT (lst, UNTESTED) => rebuildConcat (lst, chr, idx)
      | CONCAT (_, state) => (nfa, state)

      | ALTERNATION (lst, UNTESTED) => rebuildAlternation (lst, chr, idx, [])
      | ALTERNATION (_, state) => (nfa, state)

      | _ => raise Fail "nfa.sml 69: not char literal or concat or alternation"

    fun loop (pos, str, nfa, origNfa, startPos) =
      if pos = String.size str then
        false
      else
        let
          val chr = String.sub (str, pos)
          val (nfa, state) = rebuild (nfa, chr, pos)
        in
          case state of
            VALID _ => true
          | INVALID => loop (startPos + 1, str, origNfa, origNfa, startPos + 1)
          | UNTESTED => loop (pos + 1, str, nfa, origNfa, startPos)
        end
  in
    fun hasAnyMatch (str, nfa) =
      loop (0, str, nfa, nfa, 0)
  end

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
              val rhs = GROUP (rhs, UNTESTED)
              val result = CONCAT ([(lhs, UNTESTED), (rhs, UNTESTED)], UNTESTED)
            in
              helpClimb (groupEndIdx + 1, str, result, groupLevel)
            end
      | #"|" =>
          if level < altLevel then
            (pos, lhs)
          else
            let
              val chr = String.sub (str, pos + 1)
              val chr = CHAR_LITERAL (chr, UNTESTED)
              val (pos, rhs) = helpClimb (pos + 2, str, chr, altLevel)
              val result =
                case rhs of
                  ALTERNATION (lst, state) =>
                    ALTERNATION ((lhs, UNTESTED) :: lst, UNTESTED)
                | _ =>
                    ALTERNATION ([(lhs, UNTESTED), (rhs, UNTESTED)], UNTESTED)
            in
              (pos, result)
            end
      | #"?" =>
          if level < postfixLevel then
            (pos, lhs)
          else
            let val lhs = ZERO_OR_ONE (lhs, UNTESTED)
            in helpClimb (pos + 1, str, lhs, postfixLevel)
            end
      | #"*" =>
          if level < postfixLevel then
            (pos, lhs)
          else
            let val lhs = ZERO_OR_MORE (lhs, UNTESTED)
            in helpClimb (pos + 1, str, lhs, postfixLevel)
            end
      | #"+" =>
          if level < postfixLevel then
            (pos, lhs)
          else
            let val lhs = ONE_OR_MORE (lhs, UNTESTED)
            in helpClimb (pos + 1, str, lhs, postfixLevel)
            end
      | chr =>
          if level < concatLevel then
            (pos, lhs)
          else
            let
              val chr = CHAR_LITERAL (chr, UNTESTED)
              val (pos, rhs) = helpClimb (pos + 1, str, chr, concatLevel)
              val result =
                case rhs of
                  CONCAT (lst, _) => CONCAT ((lhs, UNTESTED) :: lst, UNTESTED)
                | _ => CONCAT ([(lhs, UNTESTED), (rhs, UNTESTED)], UNTESTED)
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
      val chr = CHAR_LITERAL (chr, UNTESTED)
    in
      loop (1, str, chr)
    end
end
