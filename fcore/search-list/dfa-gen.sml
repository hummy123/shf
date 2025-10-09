signature DFA_GEN_PARAMS =
sig
  val endMarker: char
  val charIsEqual: char * char -> bool
  val charIsNotEqual: char * char -> bool
end

signature DFA_GEN =
sig
  type dfa = int vector vector
  type dfa_state = int

  val fromString: string -> dfa

  val nextState: dfa * dfa_state * char -> dfa_state
  val isFinal: dfa * dfa_state -> bool
  val isDead: dfa_state -> bool

  val matchString: dfa * string -> (int * int) list
end

functor MakeDfaGen(Fn: DFA_GEN_PARAMS): DFA_GEN =
struct
  datatype parse_tree =
    CHAR_LITERAL of {char: char, position: int}
  | WILDCARD of int
  | IS_ANY_CHARACTER of {chars: char vector, position: int}
  | NOT_ANY_CHARACTER of {chars: char vector, position: int}
  | CONCAT of
      {l: parse_tree, r: parse_tree, leftMaxState: int, rightMaxState: int}
  | ALTERNATION of
      {l: parse_tree, r: parse_tree, leftMaxState: int, rightMaxState: int}
  | ZERO_OR_ONE of parse_tree
  | ZERO_OR_MORE of parse_tree
  | ONE_OR_MORE of parse_tree
  | GROUP of parse_tree

  structure Set =
  struct
    datatype 'a set = BRANCH of 'a set * int * 'a * 'a set | LEAF

    fun insertOrReplace (newKey, newVal, tree) =
      case tree of
        BRANCH (l, curKey, curVal, r) =>
          if newKey > curKey then
            let val r = insertOrReplace (newKey, newVal, r)
            in BRANCH (l, curKey, curVal, r)
            end
          else if newKey < curKey then
            let val l = insertOrReplace (newKey, newVal, l)
            in BRANCH (l, curKey, curVal, r)
            end
          else
            BRANCH (l, newKey, newVal, r)
      | LEAF => BRANCH (LEAF, newKey, newVal, LEAF)

    fun getOrDefault (findKey, tree, default) =
      case tree of
        BRANCH (l, curKey, curVal, r) =>
          if findKey > curKey then getOrDefault (findKey, r, default)
          else if findKey < curKey then getOrDefault (findKey, l, default)
          else curVal
      | LEAF => default

    fun helpToList (tree, acc) =
      case tree of
        BRANCH (l, curKey, curVal, r) =>
          let
            val acc = helpToList (r, acc)
            val acc = (curKey, curVal) :: acc
          in
            helpToList (l, acc)
          end
      | LEAF => acc

    fun toList tree = helpToList (tree, [])

    fun helpKeysToList (tree, acc) =
      case tree of
        BRANCH (l, curKey, _, r) =>
          let
            val acc = helpKeysToList (r, acc)
            val acc = curKey :: acc
          in
            helpKeysToList (l, acc)
          end
      | LEAF => acc

    fun keysToList tree = helpKeysToList (tree, [])

    fun map (f, tree) =
      case tree of
        BRANCH (l, key, value, r) =>
          let
            val r = map (f, r)
            val l = map (f, l)
            val value = f value
          in
            BRANCH (l, key, value, r)
          end
      | LEAF => LEAF

    fun foldl (f, tree, acc) =
      case tree of
        BRANCH (l, k, v, r) =>
          let
            val acc = foldl (f, l, acc)
            val acc = f (v, acc)
          in
            foldl (f, r, acc)
          end
      | LEAF => acc
  end

  structure ParseDfa =
  struct
    (* parsing through precedence climbing algorithm. *)
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

    (* assumes previous char is a backslash *)
    fun isValidEscapeSequence chr =
      case chr of
      (* regex metacharacters *)
        #"(" => (true, chr)
      | #")" => (true, chr)
      | #"[" => (true, chr)
      | #"]" => (true, chr)
      | #"+" => (true, chr)
      | #"*" => (true, chr)
      | #"|" => (true, chr)
      | #"?" => (true, chr)
      | #"." => (true, chr)
      | #"-" => (true, chr)
      (* standard escape sequences *)
      | #"a" => (true, #"\a")
      | #"b" => (true, #"\b")
      | #"t" => (true, #"\t")
      | #"n" => (true, #"\n")
      | #"v" => (true, #"\v")
      | #"f" => (true, #"\f")
      | #"r" => (true, #"\r")
      | #"\\" => (true, chr)
      | _ => (false, chr)

    fun getCharsBetween (lowChr, highChr, acc) =
      if lowChr = highChr then
        highChr :: acc
      else
        let
          val acc = lowChr :: acc
          val lowChr = Char.succ lowChr
        in
          getCharsBetween (lowChr, highChr, acc)
        end

    fun getCharsInBrackets (pos, str, acc) =
      if pos = String.size str then
        NONE
      else
        case String.sub (str, pos) of
          #"\\" =>
            (* escape sequences *)
            if pos + 1 = String.size str then
              NONE
            else
              let
                val chr = String.sub (str, pos + 1)
                val (isValid, chr) = isValidEscapeSequence chr
              in
                if isValid then
                  (* Edge case: 
                   * We have to check if there is a char range like a-z,
                   * and if there is,
                   * we have to check if the second char in the range
                   * is another escaped-character *)
                  if
                    pos + 2 < String.size str
                    andalso String.sub (str, pos + 2) = #"-"
                    andalso pos + 3 < String.size str
                  then
                    (* we do have a character range, 
                     * which may possibly be escaped *)
                    case String.sub (str, pos + 3) of
                      #"(" => NONE
                    | #")" => NONE
                    | #"[" => NONE
                    | #"]" => NONE
                    | #"+" => NONE
                    | #"*" => NONE
                    | #"|" => NONE
                    | #"?" => NONE
                    | #"." => NONE
                    | #"-" => NONE
                    | #"\\" =>
                        if pos + 4 < String.size str then
                          let
                            val chr2 = String.sub (str, pos + 4)
                            val (isValid, chr2) = isValidEscapeSequence chr2
                            val acc =
                              if chr < chr2 then
                                getCharsBetween (chr, chr2, acc)
                              else
                                getCharsBetween (chr2, chr, acc)
                          in
                            getCharsInBrackets (pos + 5, str, acc)
                          end
                        else
                          NONE
                    | chr2 =>
                        let
                          val acc =
                            if chr < chr2 then getCharsBetween (chr, chr2, acc)
                            else getCharsBetween (chr2, chr, acc)
                        in
                          getCharsInBrackets (pos + 4, str, acc)
                        end
                  else
                    (* no character range we have to check *)
                    getCharsInBrackets (pos + 2, str, chr :: acc)
                else
                  NONE
              end
        | #"]" =>
            let val chars = Vector.fromList acc
            in SOME (pos + 1, chars)
            end
        | #"-" => NONE
        | chr =>
            if
              pos + 1 < String.size str andalso String.sub (str, pos + 1) = #"-"
              andalso pos + 2 < String.size str
            then
              (* handle character ranges like a-z.
               * There are edge cases regarding 
               * the second character in the range.
               * We have to check that any unescaped metacharacters
               * return an invalid parse state.
               * We also have to unescape any escape sequences.
               * *)
              case String.sub (str, pos + 2) of
                #"\\" =>
                  (* second char contains an escape sequence *)
                  if pos + 3 < String.size str then
                    let
                      val chr2 = String.sub (str, pos + 3)
                      val (isValid, chr2) = isValidEscapeSequence chr2
                      val acc =
                        if chr < chr2 then getCharsBetween (chr, chr2, acc)
                        else getCharsBetween (chr2, chr, acc)
                    in
                      if isValid then getCharsInBrackets (pos + 4, str, acc)
                      else NONE
                    end
                  else
                    NONE
              | #"(" => NONE
              | #")" => NONE
              | #"[" => NONE
              | #"]" => NONE
              | #"+" => NONE
              | #"*" => NONE
              | #"|" => NONE
              | #"?" => NONE
              | #"." => NONE
              | #"-" => NONE
              | chr2 =>
                  (* valid char range *)
                  let
                    val acc =
                      if chr < chr2 then getCharsBetween (chr, chr2, acc)
                      else getCharsBetween (chr2, chr, acc)
                  in
                    getCharsInBrackets (pos + 3, str, acc)
                  end
            else
              getCharsInBrackets (pos + 1, str, chr :: acc)

    fun parseCharacterClass (pos, str, stateNum) =
      case getCharsInBrackets (pos, str, []) of
        SOME (pos, chars) =>
          let
            val node = IS_ANY_CHARACTER {chars = chars, position = stateNum + 1}
          in
            SOME (pos, node, stateNum + 1)
          end
      | NONE => NONE

    fun parseNegateCharacterClass (pos, str, stateNum) =
      case getCharsInBrackets (pos, str, []) of
        SOME (pos, chars) =>
          let
            val node =
              NOT_ANY_CHARACTER {chars = chars, position = stateNum + 1}
          in
            SOME (pos, node, stateNum + 1)
          end
      | NONE => NONE

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
        | #"\\" =>
            (* escape sequences *)
            if pos + 1 = String.size str then
              NONE
            else
              let
                val chr = String.sub (str, pos + 1)
                val (isValid, chr) = isValidEscapeSequence chr
              in
                if Fn.charIsEqual (chr, Fn.endMarker) then
                  NONE
                else if isValid then
                  let
                    val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
                  in
                    SOME (pos + 2, chr, stateNum + 1)
                  end
                else
                  NONE
              end
        | #"." => SOME (pos + 1, WILDCARD (stateNum + 1), stateNum + 1)
        | #")" => NONE
        | #"?" => NONE
        | #"*" => NONE
        | #"+" => NONE
        | #"[" =>
            if pos + 1 = String.size str then
              NONE
            else if String.sub (str, pos + 1) = #"^" then
              parseNegateCharacterClass (pos + 2, str, stateNum)
            else
              parseCharacterClass (pos + 1, str, stateNum)
        | chr =>
            if Fn.charIsEqual (chr, Fn.endMarker) then
              NONE
            else
              let val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
              in SOME (pos + 1, chr, stateNum + 1)
              end

    and climb (pos, str, lhs, level, stateNum) : (int * parse_tree * int) option =
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
      | IS_ANY_CHARACTER _ => false
      | NOT_ANY_CHARACTER _ => false

      | CONCAT {l, r, ...} => isNullable l andalso isNullable r
      | ALTERNATION {l, r, ...} => isNullable l orelse isNullable r

      | ZERO_OR_ONE _ => true
      | ZERO_OR_MORE _ => true
      | ONE_OR_MORE regex => isNullable regex

      | GROUP regex => isNullable regex

    fun firstpos (tree, acc) =
      case tree of
        CHAR_LITERAL {position, ...} => position :: acc
      | IS_ANY_CHARACTER {position, ...} => position :: acc
      | NOT_ANY_CHARACTER {position, ...} => position :: acc
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
      | IS_ANY_CHARACTER {position, ...} => position :: acc
      | NOT_ANY_CHARACTER {position, ...} => position :: acc
      | WILDCARD i => i :: acc

      | CONCAT {l, r, ...} =>
          if isNullable r then
            let val acc = lastpos (l, acc)
            in lastpos (r, acc)
            end
          else
            lastpos (r, acc)
      | ALTERNATION {l, r, ...} =>
          let val acc = lastpos (l, acc)
          in lastpos (r, acc)
          end

      | ZERO_OR_ONE regex => lastpos (regex, acc)
      | ZERO_OR_MORE regex => lastpos (regex, acc)
      | ONE_OR_MORE regex => lastpos (regex, acc)
      | GROUP regex => lastpos (regex, acc)

    fun followpos (char, regex, acc) =
      case regex of
        CONCAT {r, ...} => firstpos (r, acc)
      | ZERO_OR_MORE r => firstpos (r, acc)
      | ONE_OR_MORE r => firstpos (r, acc)
      | _ => acc

    fun chrExistsInVec (idx, vec, curChr) =
      if idx = Vector.length vec then
        false
      else
        let
          val idxChr = Vector.sub (vec, idx)
        in
          Fn.charIsEqual (idxChr, curChr)
          orelse chrExistsInVec (idx + 1, vec, curChr)
        end

    (* Does two things:
     * 1. Descends to the leaf matching 'pos'.
     * 2. If the character at 'pos' matches the current character, 
     *    calls followpos at the appropriate nodes. 
     * In the end, we get a list of positions to follow. 
     * Note: The character #"\^@" is an endmarker
     * indicating that this is the final state.
     * We say that there is no match, 
     * even if the curChr is the endmarker. *)
    fun getFollowsForPositionAndChar (regex: parse_tree, pos, curChr) =
      case regex of
        CHAR_LITERAL {char, position = _} =>
          let val charIsMatch = Fn.charIsEqual (char, curChr)
          in {sawConcat = false, follows = [], charIsMatch = charIsMatch}
          end
      | WILDCARD _ =>
          let val isNotEndmarker = Fn.charIsNotEqual (curChr, Fn.endMarker)
          in {sawConcat = false, follows = [], charIsMatch = isNotEndmarker}
          end
      | IS_ANY_CHARACTER {chars, ...} =>
          let val chrExists = chrExistsInVec (0, chars, curChr)
          in {sawConcat = false, follows = [], charIsMatch = chrExists}
          end
      | NOT_ANY_CHARACTER {chars, ...} =>
          let
            val charIsValid = chrExistsInVec (0, chars, curChr)
            val charIsValid =
              not charIsValid andalso Fn.charIsNotEqual (curChr, Fn.endMarker)
          in
            {sawConcat = false, follows = [], charIsMatch = charIsValid}
          end
      | ALTERNATION {l, r, leftMaxState, rightMaxState} =>
          let val nodeToFollow = if pos <= leftMaxState then l else r
          in getFollowsForPositionAndChar (nodeToFollow, pos, curChr)
          end
      | GROUP regex => getFollowsForPositionAndChar (regex, pos, curChr)

      | CONCAT {l, r, leftMaxState, ...} =>
          if pos <= leftMaxState then
            let
              val result = getFollowsForPositionAndChar (l, pos, curChr)
              val {sawConcat, follows, charIsMatch} = result
            in
              if charIsMatch then
                if sawConcat then
                  (* we already saw a concat and got followpos *)
                  result
                else
                  let val fp = followpos (curChr, regex, follows)
                  in {sawConcat = true, follows = fp, charIsMatch = true}
                  end
              else
                (* char is not match, so don't get follow pos *)
                result
            end
          else
            getFollowsForPositionAndChar (r, pos, curChr)
      | ZERO_OR_MORE child =>
          let
            val result = getFollowsForPositionAndChar (child, pos, curChr)
            val {sawConcat, follows, charIsMatch} = result
          in
            if charIsMatch then
              { sawConcat = false
              , follows = firstpos (child, follows)
              , charIsMatch = true
              }
            else
              result
          end
      | ZERO_OR_ONE child => getFollowsForPositionAndChar (child, pos, curChr)
      | ONE_OR_MORE child =>
          let
            val result = getFollowsForPositionAndChar (child, pos, curChr)
            val {sawConcat, follows, charIsMatch} = result
          in
            if charIsMatch then
              { sawConcat = false
              , follows = firstpos (child, follows)
              , charIsMatch = true
              }
            else
              result
          end

    fun getFollowPositionsFromList (lst: int list, regex, char, followSet) =
      case lst of
        hd :: tl =>
          let
            val fpList = getFollowsForPositionAndChar (regex, hd, char)
            val {sawConcat, follows, charIsMatch} = fpList
            val follows =
              if charIsMatch andalso not sawConcat then
                (Char.ord Fn.endMarker) :: follows
              else
                follows

            val followSet =
              List.foldl
                (fn (fp, followSet) => Set.insertOrReplace (fp, (), followSet))
                followSet follows
          in
            getFollowPositionsFromList (tl, regex, char, followSet)
          end
      | [] => Set.keysToList followSet

    fun appendIfNew (pos, dstates, newStates) =
      if pos = Vector.length dstates then
        let
          val record = {transitions = newStates, marked = false}
          val dstates = Vector.concat [dstates, Vector.fromList [record]]
        in
          (pos, dstates)
        end
      else
        let
          val {transitions: int list, marked = _} = Vector.sub (dstates, pos)
        in
          if transitions = newStates then (pos, dstates)
          else appendIfNew (pos + 1, dstates, newStates)
        end

    fun getUnmarkedTransitionsIfExists (pos, dstates) =
      if pos = Vector.length dstates then
        NONE
      else
        let
          val record = Vector.sub (dstates, pos)
        in
          if #marked record then
            getUnmarkedTransitionsIfExists (pos + 1, dstates)
          else
            SOME (pos, #transitions record)
        end

    (* the int key in dtran refers to the char code
     * while the int value refers to the idx from dstates
     * that this char transitions to *)
    type dtran = int Set.set

    fun makeEmptyVec _ = ~1

    fun convertChar
      ( char
      , regex
      , dstates
      , dtran: dtran vector
      , curStates
      , curStatesIdx
      , setForCurStates
      ) =
      if char < 0 then
        let
          (* append setForCurStates which was accumulated in this function
           * to the end of dtran. *)
          val dtran = Vector.concat [dtran, Vector.fromList [setForCurStates]]
        in
          (dstates, dtran)
        end
      else
        let
          (* get union of all follow positions *)
          val u =
            getFollowPositionsFromList
              (curStates, regex, Char.chr char, Set.LEAF)
        in
          case u of
            [] =>
              (* no follow positions from here, so don't add to dstates *)
              convertChar
                ( char - 1
                , regex
                , dstates
                , dtran
                , curStates
                , curStatesIdx
                , setForCurStates
                )
          | _ =>
              let
                (* add follow positions to dstates if they are not already inside
                 * and if follow is not empty *)
                val (newStateIdx, dstates) = appendIfNew (0, dstates, u)

                (* update dtran to include transitions for char. *)
                val setForCurStates =
                  Set.insertOrReplace (char, newStateIdx, setForCurStates)
              in
                convertChar
                  ( char - 1
                  , regex
                  , dstates
                  , dtran
                  , curStates
                  , curStatesIdx
                  , setForCurStates
                  )
              end
        end

    fun convertLoop (regex, dstates, dtran) =
      case getUnmarkedTransitionsIfExists (0, dstates) of
        SOME (unmarkedIdx, unamarkedTransition) =>
          let
            (* mark transition *)
            val dstates =
              let
                val newMark = {marked = true, transitions = unamarkedTransition}
              in
                Vector.update (dstates, unmarkedIdx, newMark)
              end

            val (dstates, dtran) = convertChar
              ( 255
              , regex
              , dstates
              , dtran
              , unamarkedTransition
              , unmarkedIdx
              , Set.LEAF
              )
          in
            convertLoop (regex, dstates, dtran)
          end
      | NONE =>
          Vector.map
            (fn set =>
               Vector.tabulate (256, fn i => Set.getOrDefault (i, set, ~1)))
            dtran

    fun convert regex =
      let
        val first = List.rev (firstpos (regex, []))
        val dstates = Vector.fromList [{transitions = first, marked = false}]
      in
        convertLoop (regex, dstates, Vector.fromList [])
      end
  end

  fun fromString str =
    case ParseDfa.parse (str, 0) of
      SOME (ast, numStates) =>
        let
          val endMarker =
            CHAR_LITERAL {char = Fn.endMarker, position = numStates + 1}
          val ast = CONCAT
            { l = ast
            , leftMaxState = numStates
            , r = endMarker
            , rightMaxState = numStates + 1
            }
        in
          ToDfa.convert ast
        end
    | NONE => Vector.fromList []

  type dfa = int vector vector
  type dfa_state = int

  fun nextState (dfa: dfa, curState: dfa_state, chr) =
    let val curTable = Vector.sub (dfa, curState)
    in Vector.sub (curTable, Char.ord chr)
    end

  fun isFinal (dfa: dfa, curState: dfa_state) =
    curState <> ~1
    andalso
    let
      val curTable = Vector.sub (dfa, curState)
      val endMarkerCode = Char.ord Fn.endMarker
    in
      Vector.sub (curTable, endMarkerCode) <> ~1
    end

  fun isDead (curState: dfa_state) = curState = ~1

  fun helpMatchString (strPos, str, dfa, curState, startPos, prevFinalPos, acc) =
    if strPos = String.size str then
      let
        val acc =
          if prevFinalPos = ~1 then acc else (startPos, prevFinalPos) :: acc
      in
        List.rev acc
      end
    else
      let
        val chr = String.sub (str, strPos)
        val newState = nextState (dfa, curState, chr)
        val prevFinalPos =
          if isFinal (dfa, newState) then strPos else prevFinalPos
      in
        if isDead newState then
          if prevFinalPos = ~1 then
            (* restart from startPos *)
            helpMatchString (startPos + 1, str, dfa, 0, startPos + 1, ~1, acc)
          else
            let
              val acc = (startPos, prevFinalPos) :: acc
            in
              helpMatchString
                (prevFinalPos + 1, str, dfa, 0, prevFinalPos + 1, ~1, acc)
            end
        else
          helpMatchString
            (strPos + 1, str, dfa, newState, startPos, prevFinalPos, acc)
      end

  fun matchString (dfa, string) =
    if Vector.length dfa = 0 then []
    else helpMatchString (0, string, dfa, 0, 0, ~1, [])
end

structure CaseInsensitiveDfa =
  MakeDfaGen
    (struct
       val endMarker = #"\^@"
       fun charIsEqual (a: char, b: char) = Char.toLower a = Char.toLower b
       fun charIsNotEqual (a: char, b: char) = a <> b
     end)

structure CaseSensitiveDfa =
  MakeDfaGen
    (struct
       val endMarker = #"\^@"
       fun charIsEqual (a: char, b: char) = a = b
       fun charIsNotEqual (a: char, b: char) = a <> b
     end)
