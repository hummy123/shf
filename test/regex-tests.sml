structure RegexTests =
struct
  open Railroad
  open Railroad.Test

  structure CiDfa = CaseInsensitiveDfa
  structure CsDfa = CaseSensitiveDfa

  val caseInsensitiveTests = describe "case insensitive regex"
    [ test "recognises word 'hello' in string 'Hello world'" (fn _ =>
        let
          (* arrange *)
          val regexString = "hello"
          val dfa = CiDfa.fromString regexString
          val inputString = "Hello world"

          (* act *)
          val matches = CiDfa.matchString (dfa, inputString)

          (* assert *)
          val expectedMatches = [(0, 4)]
        in
          Expect.isTrue (matches = expectedMatches)
        end)
    , test "recognises word 'world' in string 'HELLO WORLD'" (fn _ =>
        let
          (* arrange *)
          val regexString = "world"
          val dfa = CiDfa.fromString regexString
          val inputString = "HELLO WORLD"

          (* act *)
          val matches = CiDfa.matchString (dfa, inputString)

          (* assert *)
          val expectedMatches = [(6, 10)]
        in
          Expect.isTrue (matches = expectedMatches)
        end)
    ]

  val tests = [caseInsensitiveTests]
end
