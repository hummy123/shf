open Railroad
open Railroad.Test
open InputMsg

local
  fun helpCountLineBreaks (pos, acc, str) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val chr = String.sub (str, pos)
      in
        if chr = #"\n" then
          (* Is this a \r\n pair? Then the position of \r should be consed. *)
          if pos = 0 then
            Vector.fromList (0 :: acc)
          else
            let
              val prevChar = String.sub (str, pos - 1)
            in
              if prevChar = #"\r" then
                helpCountLineBreaks (pos - 2, (pos - 1) :: acc, str)
              else
                helpCountLineBreaks (pos - 1, pos :: acc, str)
            end
        else if chr = #"\r" then
          helpCountLineBreaks (pos - 1, pos :: acc, str)
        else
          helpCountLineBreaks (pos - 1, acc, str)
      end

  fun countLineBreaks str =
    helpCountLineBreaks (String.size str - 1, [], str)
in
  (* creates a LineGap.t with valid metadata from a list of strings *)
  fun fromList lst =
    { idx = 0
    , line = 0
    , leftStrings = []
    , leftLines = []
    , rightStrings = lst
    , rightLines = List.map countLineBreaks lst
    }
end

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

val movementTests = describe "movement operations"
  [ test "'h' moves cursor left by one in contiguous string when cursorIdx > 0"
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
           val buffer = fromList ["hello", " world"]
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 5)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"h")
         in
           (* assert *)
           Expect.isTrue (cursorIdx = 4)
         end)

  , test "'h' does not move cursor when cursorIdx = 0" (fn _ =>
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

  , test
      "'h' moves cursor left by two in contiguous string when prev chr is \\n"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello\nworld"
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
           val buffer = fromList ["hello\n", " world"]
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
           val buffer = fromList ["hello ", "world"]
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
           val buffer = fromList ["hello\n", "world"]
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
        val buffer = fromList ["hello ", "world"]
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val chr = String.sub ("hello world", cursorIdx)
      in
        Expect.isTrue (chr = #"w")
      end)
  ]

val tests = concat [movementTests]

val _ = run tests
