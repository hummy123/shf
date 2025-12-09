structure Test =
struct
  open Railroad
  open Railroad.Test

  fun main () =
    let
      val tests = List.concat
        [ NormalMove.tests
        , NormalDelete.tests
        , Regression.tests
        , RegexTests.tests
        , PersistentVectorTests.tests
        ]
      val tests = concat tests
    in
      runWithConfig [Configuration.PrintPassed false] tests
      handle e => ExceptionLogger.log e
    end
end

val () = Test.main ()
