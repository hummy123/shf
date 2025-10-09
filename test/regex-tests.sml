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

  fun recogniseEscapeSequence (regexString, inputString) =
    let
      (* arrange *)
      val dfa = CiDfa.fromString regexString

      (* act *)
      val matches = CiDfa.matchString (dfa, inputString)

      (* assert *)
      val expectedMatches = [(6, 6)]
    in
      Expect.isTrue (matches = expectedMatches)
    end

  val escapeSequenceTests = describe "regex escape sequences"
    [ test "recognises alert" (fn _ =>
        recogniseEscapeSequence ("\\a", "hello \a world"))
    , test "recognises backspace" (fn _ =>
        recogniseEscapeSequence ("\\b", "hello \b world"))
    , test "recognises tab" (fn _ =>
        recogniseEscapeSequence ("\\t", "hello \t world"))
    , test "recognises newline" (fn _ =>
        recogniseEscapeSequence ("\\n", "hello \n world"))
    , test "recognises vertical tab" (fn _ =>
        recogniseEscapeSequence ("\\v", "hello \v world"))
    , test "recognises form feed" (fn _ =>
        recogniseEscapeSequence ("\\f", "hello \f world"))
    , test "recognises carriage return" (fn _ =>
        recogniseEscapeSequence ("\\r", "hello \r world"))
    , test "recognises backslash" (fn _ =>
        recogniseEscapeSequence ("\\\\", "hello \\ world"))
    ]

  val tests =
    [ caseInsensitiveTests
    , caseSensitiveTests
    , endMarkerTests
    , escapeSequenceTests
    ]
end
