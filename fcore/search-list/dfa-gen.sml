structure DfaGen =
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

    (* Does two things:
     * 1. Descends to the leaf matching 'pos'.
     * 2. If the character at 'pos' matches the current character, 
     *    calls followpos at the appropriate nodes. 
     * In the end, we get a list of positions to follow. *)
    fun getFollowsForPositionAndChar (regex: regex, pos, curChr) =
      case regex of
        CHAR_LITERAL {char, position = _} =>
          if Char.ord char = curChr then
            {sawConcat = false, follows = [], charIsMatch = true}
          else
            {sawConcat = false, follows = [], charIsMatch = false}
      | WILDCARD _ =>
          (* we are treating a char that has ASCII code 0
           * as an end marker which will not appear anywhere else.
           * So we don't want to match it, but the wildcard can match
           * any other character that has a different ASCII code. *)
          {sawConcat = false, follows = [], charIsMatch = curChr <> 0}
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
              if charIsMatch andalso not sawConcat then 0 :: follows
              else follows

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
          val u = getFollowPositionsFromList (curStates, regex, char, Set.LEAF)
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
          val endMarker = CHAR_LITERAL {char = #"\^@", position = numStates + 1}
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

  fun nextState (dfa: dfa, curState, chr) =
    let val curTable = Vector.sub (dfa, curState)
    in Vector.sub (curTable, Char.ord chr)
    end

  fun isFinal (dfa: dfa, curState) =
    curState <> ~1
    andalso
    let val curTable = Vector.sub (dfa, curState)
    in Vector.sub (curTable, 0) <> ~1
    end

  fun isDead curState = curState = ~1
end
