open Railroad
open Railroad.Test
open InputMsg

val emptyVec = Vector.fromList []

val cursorTests = describe "cursor operations"
  [ test "'w' moves cursor to start of next word in contiguous string" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello world"
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val chr = String.sub ("hello world", cursorIdx)
      in
        Expect.isTrue (chr = #"w")
      end)

  , test "'w' moves cursor to start of next word in split string" (fn _ =>
      let
        (* arrange *)
        val buffer =
          { idx = 0
          , line = 0
          , leftStrings = []
          , leftLines = []
          , rightStrings = ["hello ", "world"]
          , rightLines = [emptyVec, emptyVec]
          }
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val chr = String.sub ("hello world", cursorIdx)
      in
        Expect.isTrue (chr = #"w")
      end)
  ]

val tests = concat [cursorTests]

val _ = run tests
