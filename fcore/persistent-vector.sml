structure PersistentVector =
struct
  (* Clojure-style persistent vector, 
   * as intermediary data structure 
   * for building search list *)
  datatype t =
    BRANCH of t vector
  | LEAF of int vector

  val maxSize = 32

  val empty = LEAF #[]

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

  fun toVector tree =
    let val lst = helpToVector (tree, [])
    in Vector.concat lst
    end
end
