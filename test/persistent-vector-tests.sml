structure PersistentVectorTests =
struct
  open Railroad
  open Railroad.Test

  fun isNotInRange (lst, pv) =
    let
      fun loopNotInRange lst =
        case lst of
          hd :: tl =>
            if PersistentVector.isInRange (hd, pv) then
              let
                val msg =
                  "idx " ^ Int.toString hd
                  ^ " is in range when it shouldn't be\n"
                val () = print msg
              in
                Expect.isTrue false
              end
            else
              loopNotInRange tl
        | [] => Expect.isTrue true
    in
      loopNotInRange lst
    end

  fun isInRange (lst, pv) =
    let
      fun loopInRange lst =
        case lst of
          hd :: tl =>
            if PersistentVector.isInRange (hd, pv) then
              loopInRange tl
            else
              let
                val msg =
                  "idx " ^ Int.toString hd
                  ^ " is not in range when it should be\n"
                val () = print msg
              in
                Expect.isTrue false
              end
        | [] => Expect.isTrue true
    in
      loopInRange lst
    end

  val appendTests = describe "PersistentVector.append"
    [ test "contains appended values in range" (fn _ =>
        let
          (* arrange *)
          val f = PersistentVector.append
          val pv = PersistentVector.empty

          (* act *)
          val pv = f (1, 3, pv)
          val pv = f (5, 7, pv)
          val pv = f (9, 13, pv)
          val pv = f (19, 27, pv)
          val pv = f (33, 33, pv)

          (* assert *)
          (* we split the list into several smaller lists
          * and then concatenate at the end
          * so that the formatter does not cause
          * each list element to take its own line *)
          val indicesInRange1 = [1, 2, 3, 5, 6, 7, 9]
          val indicesInRange2 = [10, 11, 12, 13, 19, 20]
          val indicesInRange3 = [21, 22, 23, 24, 25, 26, 27, 33]

          val indicesInRange =
            List.concat [indicesInRange1, indicesInRange2, indicesInRange3]
        in
          isInRange (indicesInRange, pv)
        end)
    , test "does not contain values in range that were not appended" (fn _ =>
        let
          (* arrange *)
          val f = PersistentVector.append
          val pv = PersistentVector.empty

          (* act *)
          val pv = f (1, 3, pv)
          val pv = f (5, 7, pv)
          val pv = f (9, 13, pv)
          val pv = f (19, 27, pv)
          val pv = f (33, 33, pv)

          (* assert *)
          val indicesNotInRange =
            [0, 4, 8, 14, 15, 16, 17, 18, 28, 29, 30, 31, 32, 34, 35]
        in
          isNotInRange (indicesNotInRange, pv)
        end)
    ]

  val tests = [appendTests]
end
