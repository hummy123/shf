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

  fun printVec pv =
    let
      val outputList = PersistentVector.toList pv
      val str =
        List.map
          (fn {start, finish} =>
             "{start = " ^ Int.toString start ^ ", finish = "
             ^ Int.toString finish ^ "}") outputList
      val str = String.concatWith "\n " str ^ "\n"
    in
      print str
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
    [ test
        "returns same vector when split idx is greater than any idx in vector"
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
           end)
    , test "removes last element when split idx is = to last element" (fn _ =>
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
          val pv = PersistentVector.splitLeft (8, pv)

          (* assert *)
          val outputList = PersistentVector.toList pv
          val expectedOutput =
            [ {start = 1, finish = 1}
            , {start = 2, finish = 2}
            , {start = 3, finish = 3}
            , {start = 4, finish = 4}
            , {start = 5, finish = 5}
            , {start = 6, finish = 6}
            , {start = 7, finish = 7}
            ]
        in
          Expect.isTrue (outputList = expectedOutput)
        end)
    , test "removes all elements when split idx = first element" (fn _ =>
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
          val pv = PersistentVector.splitLeft (1, pv)

          (* assert *)
          val outputList = PersistentVector.toList pv
          val expectedOutput = []
        in
          Expect.isTrue (outputList = expectedOutput)
        end)
    , test
        "removes element whose start and finish is in range \
        \of the split idx, and removes all elements after it too"
        (fn _ =>
           let
             (* arrange *)
             val inputList =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 5, finish = 155}
               , {start = 200, finish = 200}
               , {start = 210, finish = 210}
               , {start = 220, finish = 220}
               , {start = 230, finish = 230}
               , {start = 240, finish = 240}
               , {start = 250, finish = 250}
               ]
             val pv = PersistentVector.fromList inputList

             (* act *)
             val pv = PersistentVector.splitLeft (7, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    ]

  val deleteTests = describe "PersistentVector.delete"
    [ test "returns empty vector when deletion range includes every element"
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
             val pv = PersistentVector.delete (0, 11, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput = []
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    , test
        "returns the left side of the vector \
        \when 'length' is greater than any element in the vector"
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
             val pv = PersistentVector.delete (5, 4, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    , test
        "decrements subsequent elements correctly \
        \when deletion range is before first element to middle element"
        (fn _ =>
           let
             (* arrange *)
             val inputList =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 50, finish = 50}
               , {start = 60, finish = 60}
               , {start = 70, finish = 70}
               , {start = 80, finish = 80}
               ]
             val pv = PersistentVector.fromList inputList

             (* act *)
             val pv = PersistentVector.delete (0, 3, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 1}
               , {start = 47, finish = 47}
               , {start = 57, finish = 57}
               , {start = 67, finish = 67}
               , {start = 77, finish = 77}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    , test
        "decrements subsequent elements correctly \
        \when deletion range is between two elements, \
        \but deletes no elements"
        (fn _ =>
           let
             (* arrange *)
             val inputList =
               [ {start = 1, finish = 3}
               , {start = 15, finish = 19}
               , {start = 35, finish = 39}
               ]
             val pv = PersistentVector.fromList inputList

             (* act *)
             val pv = PersistentVector.delete (21, 3, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 3}
               , {start = 15, finish = 19}
               , {start = 32, finish = 36}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    , test "deletes element when deletion range is inside that element" (fn _ =>
        let
          (* arrange *)
          val inputList =
            [ {start = 1, finish = 3}
            , {start = 15, finish = 19}
            , {start = 35, finish = 39}
            ]
          val pv = PersistentVector.fromList inputList

          (* act *)
          val pv = PersistentVector.delete (17, 1, pv)

          (* assert *)
          val outputList = PersistentVector.toList pv
          val expectedOutput =
            [{start = 1, finish = 3}, {start = 34, finish = 38}]
        in
          Expect.isTrue (outputList = expectedOutput)
        end)
    , test
        "returns preceding elements when \
        \deletion range starts in middle and deletes to end of vector"
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
             val pv = PersistentVector.delete (5, 9, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    , test
        "deletes middle elements and decrements subsequent elements \
        \when deletion range starts after first element \
        \and ends before last element"
        (fn _ =>
           let
             (* arrange *)
             val inputList =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 5, finish = 5}
               , {start = 60, finish = 60}
               , {start = 70, finish = 70}
               , {start = 80, finish = 80}
               ]
             val pv = PersistentVector.fromList inputList

             (* act *)
             val pv = PersistentVector.delete (3, 3, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 57, finish = 57}
               , {start = 67, finish = 67}
               , {start = 77, finish = 77}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    , test
        "maintains balance with all leaves at same depth \
        \when deleting a large portion of nodes in the middle"
        (fn _ =>
           let
             (* arrange *)
             val inputList = List.tabulate (228, fn i =>
               {start = i, finish = i})
             val pv = PersistentVector.fromList inputList

             (* act *)
             val pv = PersistentVector.delete (19, 15, pv)

             (* assert *)
             val isBalanced = PersistentVector.allLeavesAtSameDepth pv
           in
             Expect.isTrue isBalanced
           end)
    ]

  val extendExistingMatchTests = describe "PersistentVector.extendExistingMatch"
    [ test
        "leaves subsequent matches untouched \
        \if their 'finish' is greater than the extended finish"
        (fn _ =>
           let
             (* arrange *)
             val inputList =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 5, finish = 5}
               , {start = 60, finish = 60}
               , {start = 70, finish = 70}
               , {start = 80, finish = 80}
               ]
             val pv = PersistentVector.fromList inputList

             (* act *)
             val pv = PersistentVector.extendExistingMatch (5, 50, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 5, finish = 50}
               , {start = 60, finish = 60}
               , {start = 70, finish = 70}
               , {start = 80, finish = 80}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    , test
        "removes subsequent matches whose 'finish' is less than \
        \the newly extended element's 'finish'"
        (fn _ =>
           let
             (* arrange *)
             val inputList =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 5, finish = 5}
               , {start = 60, finish = 60}
               , {start = 70, finish = 70}
               , {start = 80, finish = 80}
               ]
             val pv = PersistentVector.fromList inputList

             (* act *)
             val pv = PersistentVector.extendExistingMatch (5, 75, pv)

             (* assert *)
             val outputList = PersistentVector.toList pv
             val expectedOutput =
               [ {start = 1, finish = 1}
               , {start = 2, finish = 2}
               , {start = 3, finish = 3}
               , {start = 4, finish = 4}
               , {start = 5, finish = 75}
               , {start = 80, finish = 80}
               ]
           in
             Expect.isTrue (outputList = expectedOutput)
           end)
    ]

  val tests =
    [ appendTests
    , toListTests
    , splitLeftTests
    , deleteTests
    , extendExistingMatchTests
    ]
end
