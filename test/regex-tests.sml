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

  fun doesNotRecogniseUnescaped (regexString, inputString) =
    let
      (* arrange *)
      val dfa = CiDfa.fromString regexString

      (* act *)
      val matches = CiDfa.matchString (dfa, inputString)

      (* assert *)
      val expectedMatches = []
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

  val metacharacterEscapeTest = describe "regex metacharacter escape sequences"
    [ test "recognises (" (fn _ =>
        recogniseEscapeSequence ("\\(", "hello ( world"))
    , test "recognises )" (fn _ =>
        recogniseEscapeSequence ("\\)", "hello ) world"))
    , test "recognises [" (fn _ =>
        recogniseEscapeSequence ("\\[", "hello [ world"))
    , test "recognises ]" (fn _ =>
        recogniseEscapeSequence ("\\]", "hello ] world"))
    , test "recognises +" (fn _ =>
        recogniseEscapeSequence ("\\+", "hello + world"))
    , test "recognises |" (fn _ =>
        recogniseEscapeSequence ("\\|", "hello | world"))
    , test "recognises ?" (fn _ =>
        recogniseEscapeSequence ("\\?", "hello ? world"))
    , test "recognises ." (fn _ =>
        recogniseEscapeSequence ("\\.", "hello . world"))
    , test "recognises -" (fn _ =>
        recogniseEscapeSequence ("\\-", "hello - world"))

    (* checking that unescaped metacharacter is not recognised *)
    , test "does not recognise (" (fn _ =>
        doesNotRecogniseUnescaped ("(", "hello ( world"))
    , test "does not recognise )" (fn _ =>
        doesNotRecogniseUnescaped (")", "hello ) world"))
    , test "does not recognise [" (fn _ =>
        doesNotRecogniseUnescaped ("[", "hello [ world"))
    , test "does not recognise ]" (fn _ =>
        doesNotRecogniseUnescaped ("[", "hello ] world"))
    , test "does not recognise +" (fn _ =>
        doesNotRecogniseUnescaped ("+", "hello + world"))
    , test "does not recognise |" (fn _ =>
        doesNotRecogniseUnescaped ("|", "hello | world"))
    , test "does not recognise ?" (fn _ =>
        doesNotRecogniseUnescaped ("?", "hello ? world"))
    , test "does not recognise -" (fn _ =>
        doesNotRecogniseUnescaped ("-", "hello - world"))
    ]

  (* tests based on regex tutorial by FreeCodeCamp *)
  val freeCodeCampTests = describe "regex freeCodeCamp tests"
    [ test "The dog chased the cat" (fn _ =>
        let
          (* arrange *)
          val sentence = "The dog chased the cat"
          val regexString = "the"
          val caseSensitiveDfa = CsDfa.fromString regexString
          val caseInsensitiveDfa = CiDfa.fromString regexString

          (* act *)
          val caseSensitiveMatches =
            CsDfa.matchString (caseSensitiveDfa, sentence)
          val caseInsensitiveMatches =
            CiDfa.matchString (caseInsensitiveDfa, sentence)

          (* assert *)
          val expectedCaseSensitive = [(15, 17)]
          val expectedCaseInsensitive = [(0, 2), (15, 17)]
          val expected =
            caseSensitiveMatches = expectedCaseSensitive
            andalso caseInsensitiveMatches = expectedCaseInsensitive
        in
          Expect.isTrue (expected)
        end)
    , test "Somewhere Waldo is hiding in this text." (fn _ =>
        let
          (* arrange *)
          val sentence = "Somewhere Waldo is hiding in this text."
          val regexString = "Waldo"
          val dfa = CsDfa.fromString regexString

          (* act *)
          val matches = CsDfa.matchString (dfa, sentence)

          (* assert *)
          val expectedMatches = [(10, 14)]
        in
          Expect.isTrue (expected = matches)
        end)
    , test "James has a pet cat." (fn _ =>
        let
          (* arrange *)
          val sentence = "James has a pet cat."
          val regexString = "dog|cat|bird|fish"
          val dfa = CsDfa.fromString regexString

          (* act *)
          val matches = CsDfa.matchString (dfa, sentence)

          (* assert *)
          val expectedMatches = [(16, 18)]
        in
          Expect.isTrue (expected = matches)
        end)
    , test "Ignore Case While Matching" (fn _ =>
        let
          (* arrange *)
          val sentence = "freeCodeCamp"
          val regexString = "freecodecamp"
          val dfa = CiDfa.fromString regexString

          (* act *)
          val matches = CiDfa.matchString (dfa, sentence)

          (* assert *)
          val expectedMatches = [(0, 11)]
        in
          Expect.isTrue (expected = matches)
        end)
    ]

  val tests =
    [ caseInsensitiveTests
    , caseSensitiveTests
    , endMarkerTests
    , escapeSequenceTests
    , metacharacterEscapeTest
    , freeCodeCampTests
    ]
end
