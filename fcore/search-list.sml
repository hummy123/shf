signature SEARCH_LIST =
sig
  type t = {left: int vector list, right: int vector list}
  val empty: t

  val exists: int * t -> bool
  val insert: int * t -> t
  val append: int * t -> t
  val delete: int * int * string * t -> t

  val goToNum: int * t -> t
  val mapFrom: int * int * t -> t

  val toVector: t -> int vector
  val toString: t -> string
end

structure SearchList: SEARCH_LIST =
struct
  structure IntSet =
    MakeGapSet
      (struct
         type key = int

         val maxNodeSize = 32

         fun l (a: int, b) = a < b
         fun eq (a: int, b) = a = b
         fun g (a: int, b) = a > b
       end)

  type t = IntSet.t

  fun helpToVector (left, right) =
    case left of
      hd :: tl => helpToVector (tl, hd :: right)
    | [] => Vector.concat right

  (* for testing *)
  fun toVector {left, right} = helpToVector (left, right)


  val empty = IntSet.empty

  fun insert (num, set) =
    let val () = print ("adding num: " ^ Int.toString num ^ "\n")
    in IntSet.add (num, set)
    end

  val append = IntSet.add

  val goToNum = IntSet.moveTo

  fun delete (start, length, searchString, set) =
    if length > 0 then
      let
        val firstVec = toVector set
        val finish = start + length
        val start = start - String.size searchString + 1
        val result = IntSet.removeMany (start, finish, set)

        val secondVec = toVector result

        val () = print
          ("delete start has " ^ Int.toString (Vector.length firstVec)
           ^ "elements\n")
        val () = print
          ("delete result has " ^ Int.toString (Vector.length secondVec)
           ^ "elements\n")
      in
        result
      end
    else
      set

  fun isLessThanTarget (v1, v2) =
    Vector.length v1 + Vector.length v2 <= 32

  fun joinEndOfLeft (new, left) =
    case left of
      hd :: tail =>
        if isLessThanTarget (new, hd) then
          let val newHd = Vector.concat [hd, new]
          in newHd :: tail
          end
        else
          new :: left
    | [] => new :: left

  (* go all the way to the end of the list, mapping each hd,
   * joining the hd to the left,
   * and return when we have reached the end *)
  fun mapRight (mapBy, left, right) =
    case right of
      hd :: tl =>
        let val newHd = Vector.map (fn el => el + mapBy) hd
        in mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
        end
    | [] => {left = left, right = right}

  fun moveRightAndMap (from, mapBy, left, right) =
    case right of
      hd :: tl =>
        let
          val lastIdx = Vector.length hd - 1
          val last = Vector.sub (hd, lastIdx)
        in
          if from > last then
            moveRightAndMap (from, mapBy, joinEndOfLeft (hd, left), tl)
          else if from < last then
            (* need to map in middle *)
            let
              val startIdx = BinSearch.equalOrMore (from, hd)
              val mapEl = Vector.sub (hd, startIdx)
              val newHd =
                Vector.map (fn el => if el < from then el else el + mapBy) hd
            in
              mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
            end
          else
            (* from = last *)
            let
              val newHd =
                Vector.map (fn el => if el < from then el else el + mapBy) hd
            in
              mapRight (mapBy, joinEndOfLeft (newHd, left), tl)
            end
        end
    | [] => {left = left, right = right}

  fun mapFrom (num, mapBy, lst) =
    let
      (* goToNum always places vector where num was found to the right list *)
      val () = print ("mapping by " ^ Int.toString num ^ "\n")
      val {left, right} = goToNum (0, lst)
    in
      moveRightAndMap (num, 0, left, right)
    end

  val exists = IntSet.exists

  fun toString {left, right} =
    let
      val vec = toVector {left = left, right = right}

      val () = print
        ("toString has " ^ Int.toString (Vector.length vec) ^ "elements\n")

      val strList =
        Vector.foldr (fn (num, acc) => Int.toString num :: acc) [] vec
    in
      ""
    end
end
