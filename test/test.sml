fun main () =
  let
    open Railroad
    open Railroad.Test

    val tests = NormalMove.tests @ NormalDelete.tests
    val tests = concat tests
  in
    runWithConfig [Configuration.PrintPassed false] tests
  end

val () = main ()
