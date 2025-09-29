structure PersistentVector =
struct
  (* Clojure-style persistent vector, for building search list.
   * There is an "int table" too, which stores the last index 
   * at the node with the same index.
   * We can use the size table for binary search.
   * *)
  datatype t =
    BRANCH of t vector * int vector
  | LEAF of {start: int, finish: int} vector * int vector

  val maxSize = 32

  fun isEmpty t =
    case t of
      LEAF (_, sizes) = Vector.length sizes = 0
    | _ => false

  val empty = LEAF (#[], #[])

  datatype append_result = APPEND of t | UPDATE of t

  fun isInRange (checkIdx, t) =
    case t of
      BRANCH (nodes, sizes) =>
        let
          val searchIdx = BinSearch.equalOrMore (checkIdx, sizes)
        in
          if searchIdx = ~1 then
            false
          else
            isInRange (checkIdx, Vector.sub (nodes, searchIdx))
        end
    | LEAF (values, sizes) =>
        let
          val searchIdx = BinSearch.equalOrMore (checkIdx, sizes)
        in
          if searchIdx = ~1 then
            false
          else
            let
              val {start, finish} = Vector.sub (values, searchIdx)
            in
              checkIdx >= start andalso checkIdx <= finish
            end
        end

  fun getFinishIdx t =
    case t of
      BRANCH (_, sizes) => Vector.sub (sizes, Vector.length sizes - 1)
    | LEAF (_, sizes) => Vector.sub (sizes, Vector.length sizes - 1)

  fun helpAppend (start, finish, tree) =
    case tree of
      BRANCH (nodes, sizes) =>
        let
          val lastNode = Vector.sub (nodes, Vector.length nodes - 1)
        in
          case helpAppend (start, finish, lastNode) of
            UPDATE newLast =>
              let
                val lastPos = Vector.length nodes - 1
                val newNode = Vector.update (nodes, lastPos, newLast)
                val newSizes = Vector.update (sizes, lastPos, finish)
                val newNode = BRANCH (newNode, newSizes)
              in
                UPDATE newNode
              end
          | APPEND newVec =>
              if Vector.length nodes + 1 > maxSize then
                let val newNode = BRANCH (#[newVec], #[finish])
                in APPEND newNode
                end
              else
                let
                  val newNodes = Vector.concat [nodes, #[newVec]]
                  val newSizes = Vector.concat [sizes, #[finish]]
                  val newNodes = BRANCH (newNodes, newSizes)
                in
                  UPDATE newNodes
                end
        end
    | LEAF (values, sizes) =>
        if Vector.length values + 1 > maxSize then
          let val newNode = LEAF (#[{start = start, finish = finish}], #[finish])
          in APPEND newNode
          end
        else
          let
            val newNode = Vector.concat
              [values, #[{start = start, finish = finish}]]
            val newSizes = Vector.concat [sizes, #[finish]]
            val newNode = LEAF (newNode, newSizes)
          in
            UPDATE newNode
          end

  fun append (start, finish, tree) =
      case helpAppend (start, finish, tree) of
        UPDATE t => t
      | APPEND newNode => 
          let
            val maxSize = getFinishIdx tree
          in
            BRANCH (#[tree, newNode], #[maxSize, finish])
          end
end
