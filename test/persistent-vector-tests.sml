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
            indicesInRange1 @ indicesInRange2 @ indicesInRange3
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

  val toListTests = describe "PersistentVector.toList"
    [ test "returns input list when input list has 5 elements" (fn _ =>
        let
          (* arrange *)
          val inputList =
            [ {start = 1, finish = 3}
            , {start = 5, finish = 7}
            , {start = 9, finish = 13}
            , {start = 19, finish = 27}
            , {start = 33, finish = 33}
            ]
          val pv = PersistentVector.fromList inputList

          (* act *)
          val outputList = PersistentVector.toList pv

        (* assert *)
        in
          Expect.isTrue (inputList = outputList)
        end)
    , test "returns input list when input list has more than 32 elements"
        (fn _ =>
           let
             (* arrange *)
             val inputList =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 5, finish = 5}
               , {start = 6, finish = 6}
               , {start = 7, finish = 7}
               , {start = 8, finish = 8}
               , {start = 9, finish = 9}
               , {start = 10, finish = 10}
               , {start = 11, finish = 11}
               , {start = 12, finish = 12}
               , {start = 13, finish = 13}
               , {start = 14, finish = 14}
               , {start = 15, finish = 15}
               , {start = 16, finish = 16}
               , {start = 17, finish = 17}
               , {start = 18, finish = 18}
               , {start = 19, finish = 19}
               , {start = 20, finish = 20}
               , {start = 21, finish = 21}
               , {start = 22, finish = 22}
               , {start = 23, finish = 23}
               , {start = 24, finish = 24}
               , {start = 25, finish = 25}
               , {start = 26, finish = 26}
               , {start = 27, finish = 27}
               , {start = 28, finish = 28}
               , {start = 29, finish = 29}
               , {start = 30, finish = 30}
               , {start = 31, finish = 31}
               , {start = 32, finish = 32}
               , {start = 33, finish = 33}
               , {start = 34, finish = 34}
               , {start = 35, finish = 35}
               ]
             val pv = PersistentVector.fromList inputList

             (* act *)
             val outputList = PersistentVector.toList pv

           (* assert *)
           in
             Expect.isTrue (inputList = outputList)
           end)
    ]

  val splitLeftTests = describe "PersistentVector.splitLeft"
    [test "returns same vector when split idx is greater than any idx in vector"
       (fn _ =>
          let
            (* arrange *)
            val inputList =
              [ {start = 1, finish = 1}
              , {start = 2, finish = 2}
              , {start = 3, finish = 3}
              , {start = 4, finish = 4}
              , {start = 5, finish = 5}
              , {start = 6, finish = 6}
              , {start = 7, finish = 7}
              , {start = 8, finish = 8}
              ]
            val pv = PersistentVector.fromList inputList

            (* act *)
            val pv = PersistentVector.splitLeft (9, pv)

            (* assert *)
            val outputList = PersistentVector.toList pv
          in
            Expect.isTrue (inputList = outputList)
          end)]

  val tests = [appendTests, toListTests, splitLeftTests]
end
