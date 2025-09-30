structure Nfa =
struct
  datatype state = VALID of int | INVALID | UNTESTED

  datatype regex =
    CHAR_LITERAL of char * state
  | CONCAT of regex list * state
  | ALTERNATION of regex list * state
  | ZERO_OR_ONE of regex * state
  | ZERO_OR_MORE of regex * state
  | ONE_OR_MORE of regex * state
  | GROUP of regex * state
  | WILDCARD of state

  fun getState regex =
    case regex of
      CHAR_LITERAL (_, state) => state
    | CONCAT (_, state) => state
    | ALTERNATION (_, state) => state
    | ZERO_OR_ONE (_, state) => state
    | ZERO_OR_MORE (_, state) => state
    | ONE_OR_MORE (_, state) => state
    | GROUP (_, state) => state
    | WILDCARD state => state

  structure NfaMatch =
  struct
    (* test to see if NFA matches.
     * Algorithm: Walk down to the leaves/subnodes of the regex tree
     * and check if the current chr turns the leaf valid. 
     *
     * When we hit a concatenation node, we check if the list's hd
     * matches the same char. 
     * If it is valid and we have reached the last list element, 
     * then the whole concatenation is valid and we mark it as such.
     * If it is valid while we still have other nodes to test,
     * we filter the hd and the next loop with the next chr
     * checks the node's tl. 
     * If it isn't valid, then we mark the whole concatenation as invalid.
     *
     * When we hit an alternation node, we check each list element at once
     * to see if any of them are valid. 
     * We filter out the nodes in the alternation list that turn out to be
     * invalid.
     * At the end of the alternation loop, we check if all nodes are valid
     * and then mark the alternation as valid if so.
     * This helps us to implement "maximal munch", 
     * retrieving the maximum match instead of any other.
     * *)
    local
      fun loop (tl, maxValid) =
        case tl of
          hd :: tl =>
            (case getState hd of
               VALID curValid => loop (tl, Int.max (curValid, maxValid))
             | UNTESTED => UNTESTED
             | INVALID =>
                 raise Fail
                   "nfa.sml 24: \
                   \should not have INVALID state in acc")
        | [] => VALID maxValid
    in
      fun getAlternationState acc =
        case acc of
          hd :: tl =>
            (case getState hd of
               VALID maxValid => loop (tl, maxValid)
             | UNTESTED => UNTESTED
             | INVALID =>
                 raise Fail
                   "nfa.sml 65: \
                   \should not have INVALID state in acc")
        | [] => UNTESTED
    end

    fun rebuildConcat (lst, chr, idx) =
      case lst of
        [hd] =>
          let
            val (hd: regex, state: state) = rebuild (hd, chr, idx)
            val result = [hd]
            val concat = CONCAT (result, state)
          in
            (concat, state)
          end
      | hd :: tl =>
          let
            val (hd, state) = rebuild (hd, chr, idx)
          in
            case state of
              UNTESTED =>
                let val concat = CONCAT (hd :: tl, UNTESTED)
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
        [hd] =>
          let
            val (hd, state) = rebuild (hd, chr, idx)
            val acc =
              case state of
                VALID _ => hd :: acc
              | UNTESTED => hd :: acc
              | INVALID => acc
            val state = getAlternationState acc
          in
            (ALTERNATION (acc, state), state)
          end
      | hd :: tl =>
          let
            val (hd, state) = rebuild (hd, chr, idx)
            val acc =
              case state of
                VALID _ => hd :: acc
              | UNTESTED => hd :: acc
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

      | WILDCARD _ => let val nfa = WILDCARD (VALID idx) in (nfa, VALID idx) end

      | _ => raise Fail "nfa.sml 69: not char literal or concat or alternation"

    (* get all matches in string.
     * Todo: 
     * - Append {start: int, finish: int} into PersistentVector instead
     * - Search through gap buffer instead of string
     * *)
    local
      fun loop (pos, str, nfa, origNfa, startPos, acc) =
        if pos = String.size str then
          acc
        else
          let
            val chr = String.sub (str, pos)
            val (nfa, state) = rebuild (nfa, chr, pos)
          in
            case state of
              VALID finishIdx =>
                let
                  val acc = PersistentVector.append (startPos, finishIdx, acc)
                in
                  loop
                    (finishIdx + 1, str, origNfa, origNfa, finishIdx + 1, acc)
                end
            | INVALID =>
                (* backtrack to another position in the string
                 * to see if the NFA matches that portion of the string *)
                let val pos = startPos + 1
                in loop (pos, str, origNfa, origNfa, pos, acc)
                end
            | UNTESTED => loop (pos + 1, str, nfa, origNfa, startPos, acc)
          end
    in
      fun getMatches (str, nfa) =
        loop (0, str, nfa, nfa, 0, PersistentVector.empty)
    end

    local
      fun loop (pos, buffer, nfa, origNfa, startPos, acc, lastIdx) =
        if pos = #textLength buffer then
          (buffer, acc)
        else if pos > lastIdx then
          (buffer, acc)
        else
          let
            val buffer = LineGap.goToIdx (pos, buffer)
            val chr = LineGap.sub (pos, buffer)
            val (nfa, state) = rebuild (nfa, chr, pos)
          in
            case state of
              VALID finishIdx =>
                let
                  val acc = PersistentVector.append (startPos, finishIdx, acc)
                in
                  loop
                    ( finishIdx + 1
                    , buffer
                    , origNfa
                    , origNfa
                    , finishIdx + 1
                    , acc
                    , lastIdx
                    )
                end
            | INVALID =>
                let val pos = startPos + 1
                in loop (pos, buffer, origNfa, origNfa, pos, acc, lastIdx)
                end
            | UNTESTED =>
                loop (pos + 1, buffer, nfa, origNfa, startPos, acc, lastIdx)
          end
    in
      fun getMatchesInRange (startIdx, finishIdx, buffer: LineGap.t, nfa) =
        loop (startIdx, buffer, nfa, nfa, 0, PersistentVector.empty, finishIdx)
    end
  end

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
                            val rhs = GROUP (rhs, UNTESTED)
                            val result = CONCAT ([lhs, rhs], UNTESTED)
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
                val chr = CHAR_LITERAL (chr, UNTESTED)
              in
                case climb (pos + 2, str, chr, altLevel) of
                  SOME (pos, rhs) =>
                    let
                      val result =
                        case rhs of
                          ALTERNATION (lst, state) =>
                            ALTERNATION (lhs :: lst, UNTESTED)
                        | _ => ALTERNATION ([lhs, rhs], UNTESTED)
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
              let val lhs = ZERO_OR_ONE (lhs, UNTESTED)
              in climb (pos + 1, str, lhs, postfixLevel)
              end
        | #"*" =>
            if level < postfixLevel then
              SOME (pos, lhs)
            else
              let val lhs = ZERO_OR_MORE (lhs, UNTESTED)
              in climb (pos + 1, str, lhs, postfixLevel)
              end
        | #"+" =>
            if level < postfixLevel then
              SOME (pos, lhs)
            else
              let val lhs = ONE_OR_MORE (lhs, UNTESTED)
              in climb (pos + 1, str, lhs, postfixLevel)
              end
        | chr =>
            if level < concatLevel then
              SOME (pos, lhs)
            else
              let
                val currentState =
                  if chr = #"." then WILDCARD UNTESTED
                  else CHAR_LITERAL (chr, UNTESTED)
              in
                case climb (pos + 1, str, currentState, concatLevel) of
                  SOME (pos, rhs) =>
                    let
                      val result =
                        case rhs of
                          CONCAT (lst, _) => CONCAT (lhs :: lst, UNTESTED)
                        | _ => CONCAT ([lhs, rhs], UNTESTED)
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
          val chr = CHAR_LITERAL (chr, UNTESTED)
        in
          loop (1, str, chr)
        end
      else
        NONE
  end

  val parse = ParseNfa.parse
  val getMatches = NfaMatch.getMatches
  val getMatchesInRange = NfaMatch.getMatchesInRange
end
