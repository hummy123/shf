structure PersistentVectorTests =
struct
  open Railroad
  open Railroad.Test

  structure Pv = PersistentVector

  val appendTests = describe "PersistentVector.append"
    [test "appends new values to end" (fn _ =>
       let
         (* arrange *)
         val pv = Pv.fromList [(1, 1), (3, 3)]

         (* act *)
         val pv = Pv.append (5, 7, pv)

         (* assert *)
         val outputList = Pv.toList pv
         val expectedList = [(1, 1), (3, 3), (5, 7)]
       in
         Expect.isTrue (outputList = expectedList)
       end)]

  fun printList lst =
    let
      val str =
        List.map
          (fn (start, finish) =>
             "(" ^ Int.toString start ^ ", " ^ Int.toString finish ^ ")") lst
      val str = "[" ^ String.concatWith ", " str ^ "]\n"
    in
      print str
    end

  val deleteTests = describe "PersistentVector.delete"
    [ test
        "deletes last value correctly \
        \when only last value is in deletion range"
        (fn _ =>
           let
             (* arrange *)
             val pv = Pv.fromList [(0, 0), (3, 3), (5, 5)]

             (* act *)
             val pv = Pv.delete (5, 1, pv)

             (* assert *)
             val outputList = Pv.toList pv
             val expectedList = [(0, 0), (3, 3)]
           in
             Expect.isTrue (outputList = expectedList)
           end)
    , test
        "deletes middle value correctly \
        \and adjusts values-after-middle as well"
        (fn _ =>
           let
             (* arrange *)
             val pv = Pv.fromList [(0, 0), (3, 3), (5, 5)]

             (* act *)
             val pv = Pv.delete (3, 1, pv)

             (* assert *)
             val outputList = Pv.toList pv
             val expectedList = [(0, 0), (4, 4)]
           in
             Expect.isTrue (outputList = expectedList)
           end)
    , test
        "deletes first value correctly \
        \and adjusts values-after-first as well"
        (fn _ =>
           let
             (* arrange *)
             val pv = Pv.fromList [(0, 0), (3, 3), (5, 5)]

             (* act *)
             val pv = Pv.delete (0, 1, pv)

             (* assert *)
             val outputList = Pv.toList pv
             val expectedList = [(2, 2), (4, 4)]
           in
             Expect.isTrue (outputList = expectedList)
           end)
    ]

  val tests = [appendTests, deleteTests]
end
