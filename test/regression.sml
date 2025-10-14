structure Regression =
struct
  open Railroad
  open Railroad.Test

  fun updateLoop (pos, str, app) =
    if pos = String.size str then
      app
    else
      let
        val chr = String.sub (str, pos)
        val () = ExceptionLogger.addCommand (InputMsg.CHAR_EVENT chr)
        val app = TestUtils.update (app, InputMsg.CHAR_EVENT chr)
      in
        updateLoop (pos + 1, str, app)
      end

  fun appFromText text = TestUtils.init text

  fun loadFromFile (io, acc) =
    case TextIO.inputLine io of
      SOME line => loadFromFile (io, acc ^ line)
    | NONE => acc

  val initialText =
    let
      val io = TextIO.openIn "temp.txt"
      val str = loadFromFile (io, "")
      val () = TextIO.closeIn io
    in
      str
    end

  val charEventTests = describe "CHAR_EVENT regressions"
    [ test "SearchList.goToNum vector bounds regression (1)" (fn _ =>
        let
          val app = TestUtils.init initialText
          val history = "G12dk"
          val newApp = TestUtils.updateMany (app, history)
        in
          (* just expect that we do not fail or throw an exception *)
          Expect.isTrue true
        end)
    , test "No error raised when moving cursor up/down after deleting" (fn _ =>
        let
          val app = TestUtils.init initialText
          val history =
            "16G18ddjjjjjjjjjdkdkdkjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj"
          val newApp = TestUtils.updateMany (app, history)
        in
          Expect.isTrue true
        end)
    , test
        "SearchList.buildRange does not cause exception \
        \when deleting (1)"
        (fn _ =>
           let
             val app = TestUtils.init "h             ello world\n"

             (* search *)
             val search = "/ello"
             val app = TestUtils.updateMany (app, search)
             val app = TestUtils.update (app, InputMsg.KEY_ENTER)

             (* move and then delete twice *)
             val app = TestUtils.updateMany (app, "edede")
           in
             Expect.isTrue true
           end)
    , test
        "DfaGen does not cause exception \
        \when parsing alternation that contains a char \
        \from the previous alternation (1)"
        (fn _ =>
           (let val dfa = CaseSensitiveDfa.fromString "str|s"
            in Expect.isTrue true
            end)
           handle _ => Expect.isTrue false)
    ]

  val tests = [charEventTests]
end
