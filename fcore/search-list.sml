structure SearchList =
struct
  type t = int vector

  structure PersistentVector =
  struct
    (* Clojure-style persistent vector, 
     * as intermediary data structure 
     * for building search list *)
    datatype t =
      BRANCH of t vector
    | LEAF of int vector

    val maxSize = 32

    val empty = LEAF (Vector.fromList [])

    datatype append_result = APPEND of t | UPDATE of t

    fun helpAppend (key, tree) =
      case tree of
        BRANCH nodes =>
          let
            val lastNode = Vector.sub (nodes, Vector.length nodes - 1)
          in
            case helpAppend (key, lastNode) of
              UPDATE newLast =>
                let
                  val newNode = Vector.update
                    (nodes, Vector.length nodes - 1, newLast)
                  val newNode = BRANCH newNode
                in
                  UPDATE newNode
                end
            | APPEND newVec =>
                if Vector.length nodes + 1 > maxSize then
                  let val newNode = BRANCH #[newVec]
                  in APPEND newNode
                  end
                else
                  let
                    val newNodes = Vector.concat [nodes, #[newVec]]
                    val newNodes = BRANCH newNodes
                  in
                    UPDATE newNodes
                  end
          end
      | LEAF vec =>
          if Vector.length vec + 1 > maxSize then
            let val newNode = LEAF #[key]
            in APPEND newNode
            end
          else
            let
              val newNode = Vector.concat [vec, #[key]]
              val newNode = LEAF newNode
            in
              UPDATE newNode
            end

    fun append (key, tree) =
      case helpAppend (key, tree) of
        UPDATE t => t
      | APPEND newNode => BRANCH #[tree, newNode]

    fun branchToList (pos, nodes, acc) =
      if pos < 0 then
        acc
      else
        let
          val node = Vector.sub (nodes, pos)
          val acc = helpToVector (node, acc)
        in
          branchToList (pos - 1, nodes, acc)
        end

    and helpToVector (tree, acc) =
      case tree of
        BRANCH nodes => branchToList (Vector.length nodes - 1, nodes, acc)
      | LEAF vec => vec :: acc

    fun toVector tree = helpToVector (tree, [])
  end

  val empty = Vector.fromList []

  (* 
   * There is some slightly-unintuitive behaviour in most text
   * search functionality, including in Firefox, Vim and kwrite.
   * Say we have the text "abbabba" and we want to search for
   * "abba".
   * It looks like the search should highlight both
   * "[abba]bba" and "abb[abba]"
   * However, only the first of these two results is matched.
   * This is not a bug, and the behaviour is consistent across
   * different programs.
   *
   * In principle, we could match both, but we want to stick to the
   * same behaviour found in other programs.
   * Our search functionality here is implemented from back to
   * front, from the last index of the buffer/string to index 0.
   * So we can't avoid consing the second match.
   * However, what we can do is filter the second match: 
   * if the foundIdx we wish to cons is a "first match" 
   * in this edge case, then we can remove the hd of the list
   * and this will give us equivalent behaviour.
   *
   * This 'cons' function handles that edge case and abstracts over it.
   *
   * Todo: Handle another edge case:
   * When we have a string like "abbabbabba" and search for "abba",
   * there should be two results: "[abba]bb[abba]".
   * However, the last result gets filtered out.
   * *)
  fun cons (foundIdx, searchStringSize, acc, lastFilteredIdx) =
    case acc of
      hd :: tl =>
        if foundIdx + searchStringSize >= hd then
          case lastFilteredIdx of
            ~1 => (foundIdx :: tl, hd)
          | _ =>
              if hd + searchStringSize >= lastFilteredIdx then
                (foundIdx :: lastFilteredIdx :: tl, hd)
              else
                (foundIdx :: tl, hd)
        else
          (foundIdx :: acc, lastFilteredIdx)
    | [] => (foundIdx :: acc, lastFilteredIdx)

  fun searchStep
    (pos, hd, absIdx, tl, acc, searchPos, searchString, lastFilteredIdx) =
    if searchPos < 0 then
      let
        val (acc, lastFilteredIdx) =
          cons (absIdx + 1, String.size searchString, acc, lastFilteredIdx)
      in
        searchStep
          ( pos + 1
          , hd
          , absIdx + 1
          , tl
          , acc
          , String.size searchString - 1
          , searchString
          , lastFilteredIdx
          )
      end
    else if pos < 0 then
      case tl of
        hd :: tl =>
          searchStep
            ( String.size hd - 1
            , hd
            , absIdx
            , tl
            , acc
            , searchPos
            , searchString
            , lastFilteredIdx
            )
      | [] => acc
    else
      let
        val bufferChr = String.sub (hd, pos)
        val searchChr = String.sub (searchString, searchPos)
      in
        if bufferChr = searchChr then
          searchStep
            ( pos - 1
            , hd
            , absIdx - 1
            , tl
            , acc
            , searchPos - 1
            , searchString
            , lastFilteredIdx
            )
        else
          searchStep
            ( pos - 1
            , hd
            , absIdx - 1
            , tl
            , acc
            , String.size searchString - 1
            , searchString
            , lastFilteredIdx
            )
      end

  fun loopSearch (pos, hd, absIdx, tl, acc, searchString) =
    let
      val acc = searchStep
        ( pos
        , hd
        , absIdx
        , tl
        , acc
        , String.size searchString - 1
        , searchString
        , ~1
        )
    in
      Vector.fromList acc
    end

  fun search (buffer: LineGap.t, searchString) =
    let
      val {leftStrings, idx = absIdx, ...} = buffer
    in
      case leftStrings of
        hd :: tl =>
          loopSearch (String.size hd - 1, hd, absIdx - 1, tl, [], searchString)
      | [] => empty
    end

  (* Prerequisite: move buffer/LineGap to end *)
  fun build (buffer, searchString) =
    if String.size searchString > 0 then search (buffer, searchString)
    else empty

  fun loopNextMatch (pos, searchList, count) =
    if count = 0 then
      Vector.sub (searchList, pos)
    else
      let
        val pos = pos + 1
        val pos = if pos < Vector.length searchList then pos else 0
        val count = count - 1
      in
        loopNextMatch (pos, searchList, count)
      end

  fun nextMatch (cursorIdx, searchList, count) =
    if Vector.length searchList = 0 then
      ~1
    else
      let
        val pos = BinSearch.equalOrMore (cursorIdx + 1, searchList)
        val pos = if pos < Vector.length searchList then pos else 0
        val count = count - 1
      in
        loopNextMatch (pos, searchList, count)
      end

  fun loopPrevMatch (pos, searchList, count) =
    if count = 0 then
      Vector.sub (searchList, pos)
    else
      let
        val pos = pos - 1
        val pos = if pos < 0 then Vector.length searchList - 1 else pos
        val count = count - 1
      in
        loopPrevMatch (pos, searchList, count)
      end

  fun prevMatch (cursorIdx, searchList, count) =
    if Vector.length searchList = 0 then
      ~1
    else
      let
        val pos = BinSearch.equalOrLess (cursorIdx - 1, searchList)
        val pos = if pos < 0 then Vector.length searchList - 1 else pos
        val count = count - 1
      in
        loopPrevMatch (pos, searchList, count)
      end

  fun rangeSearchStep (pos, hd, absIdx, tl, acc, searchPos, searchString, low) =
    if searchPos < 0 then
      raise Fail "todo"
    else if absIdx < low then
      acc
    else if pos < 0 then
      case tl of
        hd :: tl =>
          rangeSearchStep
            ( String.size hd - 1
            , hd
            , absIdx
            , tl
            , acc
            , searchPos
            , searchString
            , low
            )
      | [] => acc
    else
      let
        val bufferChr = String.sub (hd, pos)
        val searchChr = String.sub (searchString, searchPos)
      in
        if bufferChr = searchChr then
          rangeSearchStep
            (pos - 1, hd, absIdx - 1, tl, acc, searchPos - 1, searchString, low)
        else
          acc
      end

  fun loopRange (pos, hd, absIdx, tl, acc, searchString, low) =
    if absIdx < low then
      Vector.fromList acc
    else if pos < 0 then
      case tl of
        hd :: tl =>
          loopRange (String.size hd - 1, hd, absIdx, tl, acc, searchString, low)
      | [] => Vector.fromList acc
    else
      let
        val acc = rangeSearchStep
          ( pos
          , hd
          , absIdx
          , tl
          , acc
          , String.size searchString - 1
          , searchString
          , low
          )
      in
        loopRange (pos - 1, hd, absIdx - 1, tl, acc, searchString, low)
      end

  fun searchRange (buffer: LineGap.t, searchString, low) =
    let
      val low = Int.max (low, 0)
      val {rightStrings, leftStrings, idx = absIdx, ...} = buffer
    in
      case rightStrings of
        hd :: _ =>
          let
            val pos = String.size hd - 1
            val absIdx = absIdx + String.size hd - 1
          in
            loopRange (pos, hd, absIdx, leftStrings, [], searchString, low)
          end
      | [] =>
          (case leftStrings of
             hd :: tl =>
               let
                 val pos = String.size hd - 1
                 val absIdx = absIdx - 1
               in
                 loopRange (pos, hd, absIdx, tl, [], searchString, low)
               end
           | [] => empty)
    end

  fun buildRange (buffer, searchString, low) =
    if String.size searchString > 0 then searchRange (buffer, searchString, low)
    else empty
end
