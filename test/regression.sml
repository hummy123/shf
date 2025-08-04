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
        val app = AppUpdate.update (app, InputMsg.CHAR_EVENT chr)
      in
        updateLoop (pos + 1, str, app)
      end

  fun updateAppWithChars (historyString, app) =
    updateLoop (0, historyString, app)

  fun appFromText text =
    let val buffer = LineGap.fromString text
    in AppType.init (buffer, 0, 0)
    end

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
    [test "placeholder" (fn _ => Expect.isTrue true)]

  val tests = [charEventTests]
end
