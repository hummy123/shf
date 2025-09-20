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

  fun applyChars (historyString, app) = updateLoop (0, historyString, app)

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
          val newApp = applyChars (history, app)
        in
          (* just expect that we do not fail or throw an exception *)
          Expect.isTrue true
        end)
    , test "No error raised when moving cursor up/down after deleting" (fn _ =>
        let
          val app = TestUtils.init initialText
          val history =
            "16G18ddjjjjjjjjjdkdkdkjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj"
          val newApp = applyChars (history, app)
        in
          Expect.isTrue true
        end)
    ]

  val tests = [charEventTests]
end
