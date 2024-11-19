open Railroad
open Railroad.Test
open InputMsg

val emptyVec = Vector.fromList []

fun withIdx (app: AppType.app_type, idx) =
  let
    val
      { startLine
      , buffer
      , searchList
      , searchString
      , mode
      , windowWidth
      , windowHeight
      , cursorIdx = _
      } = app
  in
    { startLine = startLine
    , buffer = buffer
    , searchList = searchList
    , searchString = searchString
    , mode = mode
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    , cursorIdx = idx
    }
  end

val cursorTests = describe "cursor operations"
  [ test "'h' does not move cursor when cursorIdx = 0" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello world"
        val app = AppType.init (buffer, 0, 0)
        val {cursorIdx = oldCursorIdx, ...} = app

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"h")
      in
        (* assert *)
        Expect.isTrue (oldCursorIdx = 0 andalso cursorIdx = 0)
      end)

  , test "'h' moves cursor left by one in contiguous string when cursorIdx > 0"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello world"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 1)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"h")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 0)
         end)

  , test "'h' moves cursor left by one in split string when cursorIdx > 0"
      (fn _ =>
         let
           (* arrange *)
           val buffer =
             { idx = 0
             , line = 0
             , leftStrings = []
             , leftLines = []
             , rightStrings = ["hello", " world"]
             , rightLines = [emptyVec, emptyVec]
             }
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 5)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"h")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 4)
         end)

  , test
      "'h' moves cursor left by two in contiguous string when prev chr is \\n"
      (fn _ =>
         let
           (* arrange *)
           val buffer =
             { idx = 0
             , line = 0
             , leftStrings = []
             , leftLines = []
             , rightStrings = ["hello\nworld"]
             , rightLines = [emptyVec]
             }
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 6)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"h")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 4)
         end)

  , test "'h' moves cursor left by two in split string when prev chr is \\n"
      (fn _ =>
         let
           (* arrange *)
           val buffer =
             { idx = 0
             , line = 0
             , leftStrings = []
             , leftLines = []
             , rightStrings = ["hello\n", " world"]
             , rightLines = [emptyVec, emptyVec]
             }
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 6)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"h")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 4)
         end)

  , test
      "'l' moves cursor right by one in contiguous string when cursorIdx < length"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello world"
           val app = AppType.init (buffer, 0, 0)
           val {cursorIdx = oldCursorIdx, ...} = app

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"l")
         in
           (* assert *)
           Expect.isTrue (oldCursorIdx = 0 andalso cursorIdx = 1)
         end)

  , test "'l' moves cursor right by one in split string when cursorIdx < length"
      (fn _ =>
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
           val {cursorIdx = oldCursorIdx, ...} = app

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"l")
         in
           (* assert *)
           Expect.isTrue (oldCursorIdx = 0 andalso cursorIdx = 1)
         end)

  , test "'l' does not move cursor right by one when cursorIdx = length"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello world\n"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 10)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"l")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 10)
         end)

  , test
      "'l' moves right by two in contiguous string when char is followed by \\n"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello\nworld\n"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 4)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"l")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 6)
         end)

  , test "'l' moves right by two in split string when char is followed by \\n"
      (fn _ =>
         let
           (* arrange *)
           val buffer =
             { idx = 0
             , line = 0
             , leftStrings = []
             , leftLines = []
             , rightStrings = ["hello\n", "world"]
             , rightLines = [emptyVec, emptyVec]
             }
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 4)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"l")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 6)
         end)

  , test "'w' moves cursor to start of next word in contiguous string" (fn _ =>
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
