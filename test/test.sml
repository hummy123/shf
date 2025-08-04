structure Test =
struct
  open Railroad
  open Railroad.Test

  fun main () =
    let
      val tests =
        List.concat [NormalMove.tests, NormalDelete.tests, Regression.tests]
      val tests = concat tests
    in
      runWithConfig [Configuration.PrintPassed false] tests
    end
end

val () = Test.main ()
