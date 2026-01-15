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

  val initial =
    let
      val f = PersistentVector.append
      val pv = PersistentVector.empty

      val pv = f (1, 3, pv)
      val pv = f (5, 7, pv)
      val pv = f (9, 13, pv)
      val pv = f (19, 27, pv)
      val pv = f (33, 33, pv)
    in
      pv
    end

  val tests = []
end
