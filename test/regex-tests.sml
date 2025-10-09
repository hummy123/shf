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

  val caseSensitiveTests = describe "case sensitive regex"
    [ test "does not recognise word 'hello' in string 'Hello world'" (fn _ =>
        let
          (* arrange *)
          val regexString = "hello"
          val dfa = CsDfa.fromString regexString
          val inputString = "Hello world"

          (* act *)
          val matches = CsDfa.matchString (dfa, inputString)

          (* assert *)
          val expectedMatches = []
        in
          Expect.isTrue (matches = expectedMatches)
        end)
    , test "recognises word 'Hello' in string 'Hello world'" (fn _ =>
        let
          (* arrange *)
          val regexString = "Hello"
          val dfa = CsDfa.fromString regexString
          val inputString = "Hello world"

          (* act *)
          val matches = CsDfa.matchString (dfa, inputString)

          (* assert *)
          val expectedMatches = [(0, 4)]
        in
          Expect.isTrue (matches = expectedMatches)
        end)
    , test "does not recognise word 'world' in string 'HELLO WORLD'" (fn _ =>
        let
          (* arrange *)
          val regexString = "world"
          val dfa = CsDfa.fromString regexString
          val inputString = "HELLO WORLD"

          (* act *)
          val matches = CsDfa.matchString (dfa, inputString)

          (* assert *)
          val expectedMatches = []
        in
          Expect.isTrue (matches = expectedMatches)
        end)
    , test "recognises word 'WORLD' in string 'HELLO WORLD'" (fn _ =>
        let
          (* arrange *)
          val regexString = "WORLD"
          val dfa = CsDfa.fromString regexString
          val inputString = "HELLO WORLD"

          (* act *)
          val matches = CsDfa.matchString (dfa, inputString)

          (* assert *)
          val expectedMatches = [(6, 10)]
        in
          Expect.isTrue (matches = expectedMatches)
        end)
    ]

  val endMarkerTests = describe "regex endMarker"
    [test "returns an empty DFA when regexString contains endMarker" (fn _ =>
       let
         (* arrange *)
         (* the end marker is #"\^@" *)
         val regexString = "hello \^@ world"

         (* act *)
         val dfa = CsDfa.fromString regexString

         (* assert *)
         val actualLength = Vector.length dfa
         val expectedLength = 0
       in
         Expect.isTrue (actualLength = expectedLength)
       end)]

  val tests = [caseInsensitiveTests, caseSensitiveTests, endMarkerTests]
end
