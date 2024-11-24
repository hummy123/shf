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

fun getChr (app: AppType.app_type) =
  let
    val {cursorIdx, buffer, ...} = app
    val c = LineGap.substring (cursorIdx, 1, buffer)
  in
    String.sub (c, 0)
  end

val hMove = describe "move motion 'h'"
  [ test "moves cursor left by one in contiguous string when cursorIdx > 0"
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

  , test "moves cursor left by one in split string when cursorIdx > 0" (fn _ =>
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

  , test "does not move cursor when cursorIdx = 0" (fn _ =>
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

  , test "moves cursor left by two in contiguous string when prev chr is \\n"
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

  , test "moves cursor left by two in split string when prev chr is \\n"
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
  ]

val lMove = describe "move motion 'l'"
  [ test
      "moves cursor right by one in contiguous string when cursorIdx < length"
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

  , test "moves cursor right by one in split string when cursorIdx < length"
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

  , test "does not move cursor when cursorIdx = length" (fn _ =>
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

  , test "moves right by two in contiguous string when char is followed by \\n"
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

  , test "moves right by two in split string when char is followed by \\n"
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
  ]

val jMove = describe "move motion 'j'"
  [ test "moves cursur down one column in contiguous string when column = 0"
      (fn _ =>
         let
           (* arrange *)
           (* "world" at end of string is intentionally misspelled as "qorld"
            * since "world" appears twice and it is useful to differentiate them
            * *)
           val buffer = LineGap.fromString "hello \nworld \ngoodbye \nqorld \n"
           val app = AppType.init (buffer, 0, 0)

           (* act *)
           val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"j")
           val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"j")
           val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"j")

           (* assert *)
           val c1 = getChr app1 = #"w"
           val c2 = getChr app2 = #"g"
           val c3 = getChr app3 = #"q"
         in
           Expect.isTrue (c1 andalso c2 andalso c3)
         end)

  , test "moves cursur down one column in split string when column = 0" (fn _ =>
      let
        (* arrange *)
        val buffer = fromList ["hello \n", "world \n", "goodbye \n", "qorld"]
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"j")
        val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"j")
        val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"j")

        (* assert *)
        val c1 = getChr app1 = #"w"
        val c2 = getChr app2 = #"g"
        val c3 = getChr app3 = #"q"
      in
        Expect.isTrue (c1 andalso c2 andalso c3)
      end)

  , test "moves cursur down one column in contiguous string when column = 1"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello \nworld \nbye \nfriends \n"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 1)

           (* act *)
           val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"j")
           val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"j")
           val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"j")

           (* assert *)
           val c1 = getChr app1 = #"o"
           val c2 = getChr app2 = #"y"
           val c3 = getChr app3 = #"r"
         in
           Expect.isTrue (c1 andalso c2 andalso c3)
         end)

  , test "moves cursur down one column in split string when column = 1" (fn _ =>
      let
        (* arrange *)
        val buffer = fromList ["hello \n", "world ", "\nb", "ye \nfriends \n"]
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 1)

        (* act *)
        val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"j")
        val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"j")
        val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"j")

        (* assert *)
        val c1 = getChr app1 = #"o"
        val c2 = getChr app2 = #"y"
        val c3 = getChr app3 = #"r"
      in
        Expect.isTrue (c1 andalso c2 andalso c3)
      end)

  , test "moves cursur down one column in contiguous string when column = 2"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello \nworld \nbye \nfriends \n"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 2)

           (* act *)
           val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"j")
           val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"j")
           val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"j")

           (* assert *)
           val c1 = getChr app1 = #"r"
           val c2 = getChr app2 = #"e"
           val c3 = getChr app3 = #"i"
         in
           Expect.isTrue (c1 andalso c2 andalso c3)
         end)

  , test "moves cursur down one column in split string when column = 2" (fn _ =>
      let
        (* arrange *)
        val buffer = fromList ["hello \n", "world ", "\nb", "ye \nfriends \n"]
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 2)

        (* act *)
        val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"j")
        val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"j")
        val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"j")

        (* assert *)
        val c1 = getChr app1 = #"r"
        val c2 = getChr app2 = #"e"
        val c3 = getChr app3 = #"i"
      in
        Expect.isTrue (c1 andalso c2 andalso c3)
      end)

  , test "skips '\\n' when cursor is on non-\\n and is followed by two '\\n's"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello\n\n nworld\n"
           val app = AppType.init (buffer, 0, 0)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"j")

           (* assert *)
           val isSkipped = cursorIdx = 6
         in
           Expect.isTrue isSkipped
         end)

  , test "moves to end of buffer when on last line" (fn _ =>
      let
        (* arrange *)
        val str = "hello \nworld \ntime to go\n"
        val buffer = LineGap.fromString str
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 15)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"j")

        (* assert *)
        (* String.size str - 1 is a valid char position
         * but we are counting String.size str - 2 as the end
         * because, in Vim, saved files always end with \n
         * but the last char, \n, is not visible *)
        val isAtEnd = cursorIdx = String.size str - 2
      in
        Expect.isTrue isAtEnd
      end)

  , test "leaves cursor at same idx when already at end of buffer" (fn _ =>
      let
        (* arrange *)
        val str = "hello \nworld \ntime to go\n"
        val len = String.size str - 2
        val buffer = LineGap.fromString str
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, len)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"j")

        (* assert *)
        (* String.size str - 1 is a valid char position
         * but we are counting String.size str - 2 as the end
         * because, in Vim, saved files always end with \n
         * but the last char, \n, is not visible *)
        val isAtEnd = cursorIdx = len
      in
        Expect.isTrue isAtEnd
      end)
  ]

val kMove = describe "move motion 'k'"
  [ test "moves cursur up one column in contiguous string when column = 0"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "0__\n4___\n9___\n14_"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 14)

           (* act *)
           val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"k")
           val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"k")
           val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"k")

           (* assert *)
           val c1 = getChr app1 = #"9"
           val c2 = getChr app2 = #"4"
           val c3 = getChr app3 = #"0"
         in
           Expect.isTrue (c1 andalso c2 andalso c3)
         end)
  , test "moves cursur up one column in split string when column = 0" (fn _ =>
      let
        (* arrange *)
        val buffer = fromList ["0__", "\n4__", "_\n9_", "__\n14_"]
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 14)

        (* act *)
        val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"k")
        val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"k")
        val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"k")

        (* assert *)
        val c1 = getChr app1 = #"9"
        val c2 = getChr app2 = #"4"
        val c3 = getChr app3 = #"0"
      in
        Expect.isTrue (c1 andalso c2 andalso c3)
      end)

  , test "moves cursur up one column in contiguous string when column = 1"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "_w_\n_5__\n_10_\n_15"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 15)

           (* act *)
           val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"k")
           val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"k")
           val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"k")

           (* assert *)
           val c1 = getChr app1 = #"1"
           val c2 = getChr app2 = #"5"
           val c3 = getChr app3 = #"w"
         in
           Expect.isTrue (c1 andalso c2 andalso c3)
         end)

  , test "moves cursur up one column in split string when column = 1" (fn _ =>
      let
        (* arrange *)
        val buffer = fromList ["_w_\n", "_5__", "\n_10_\n", "_15"]
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 15)

        (* act *)
        val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"k")
        val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"k")
        val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"k")

        (* assert *)
        val c1 = getChr app1 = #"1"
        val c2 = getChr app2 = #"5"
        val c3 = getChr app3 = #"w"
      in
        Expect.isTrue (c1 andalso c2 andalso c3)
      end)

  , test "moves cursur up one column in contiguous string when column = 2"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "__2\n__6\n__10\n__15\n"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 15)

           (* act *)
           val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"k")
           val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"k")
           val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"k")

           (* assert *)
           val c1 = getChr app1 = #"1"
           val c2 = getChr app2 = #"6"
           val c3 = getChr app3 = #"2"
         in
           Expect.isTrue (c1 andalso c2 andalso c3)
         end)

  , test "moves cursur up one column in split string when column = 2" (fn _ =>
      let
        (* arrange *)
        val buffer = fromList ["__", "2\n", "__6", "\n__10", "\n__1", "5\n"]
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 15)

        (* act *)
        val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"k")
        val (app2, _) = AppUpdate.update (app1, CHAR_EVENT #"k")
        val (app3, _) = AppUpdate.update (app2, CHAR_EVENT #"k")

        (* assert *)
        val c1 = getChr app1 = #"1"
        val c2 = getChr app2 = #"6"
        val c3 = getChr app3 = #"2"
      in
        Expect.isTrue (c1 andalso c2 andalso c3)
      end)

  , test
      "skips '\\n' when cursor is on '\\n', prev-char is '\\n' and prev-prev char is not '\\n'"
      (fn _ =>
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello\n\n world\n"
           val app = AppType.init (buffer, 0, 0)
           val app = withIdx (app, 6)

           (* act *)
           val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"k")

           (* assert *)
           val _ = print ("cursorIdx: " ^ Int.toString cursorIdx ^ "\n")
           val isSkipped = cursorIdx = 0
         in
           Expect.isTrue isSkipped
         end)

  , test "moves to 0 of buffer when on first line" (fn _ =>
      let
        (* arrange *)
        val str = "hello \nworld \ntime to go\n"
        val buffer = LineGap.fromString str
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 5)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"k")

        (* assert *)
        val isAtStart = cursorIdx = 0
      in
        Expect.isTrue isAtStart
      end)

  , test "leaves cursor at same idx when already at start of buffer" (fn _ =>
      let
        (* arrange *)
        val str = "hello \nworld \ntime to go\n"
        val buffer = LineGap.fromString str
        val app = AppType.init (buffer, 0, 0)
        (* line below does nothing; just for explicitness *)
        val app = withIdx (app, 0)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"k")

        (* assert *)
        (* String.size str - 1 is a valid char position
         * but we are counting String.size str - 2 as the end
         * because, in Vim, saved files always end with \n
         * but the last char, \n, is not visible *)
        val isAtStart = cursorIdx = 0
      in
        Expect.isTrue isAtStart
      end)
  ]

val wMove = describe "move motion 'w'"
  [ test "moves cursor to start of next word in contiguous string" (fn _ =>
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

  , test "moves cursor to start of next word in split string" (fn _ =>
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

  , test "moves cursor past newline when next word is after newline" (fn _ =>
      (* This behaviour makes behaviour different from vi,
       * where "w" when a newline is in between causes cursor 
       * to go to newline and not next word.
       * I don't personally like this behaviour from vi 
       * since one can just press "j" to go to the newline instead
       * and it is more intuitive for the cursor to go the next word
       * as usual with "w". *)
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello \n\n\n world"
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val cursorChr = getChr app
      in
        Expect.isTrue (cursorChr = #"w")
      end)

  , test "does not break on undescore when cursor is on alphanumeric char"
      (fn _ =>
         (* This behaviour makes behaviour different from vi,
          * where "w" when a newline is in between causes cursor 
          * to go to newline and not next word.
          * I don't personally like this behaviour from vi 
          * since one can just press "j" to go to the newline instead
          * and it is more intuitive for the cursor to go the next word
          * as usual with "w". *)
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello_world goodbye_world"
           val app = AppType.init (buffer, 0, 0)

           (* act *)
           val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

           (* assert *)
           val cursorChr = getChr app
         in
           Expect.isTrue (cursorChr = #"g")
         end)

  , test "breaks on punctuation when cursor is on alphanumeric char" (fn _ =>
      (* vi's definition of 'word' instead of 'WORD' *)
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello, world"
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val cursorChr = getChr app
      in
        Expect.isTrue (cursorChr = #",")
      end)

  , test "breaks on alphanumeric char when cursor is on punctuation" (fn _ =>
      (* vi's definition of 'word' instead of 'WORD' *)
      let
        (* arrange *)
        val buffer = LineGap.fromString "!#%^()hello\n"
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val cursorChr = getChr app
      in
        Expect.isTrue (cursorChr = #"h")
      end)

  , test "breaks on non-blank char when on blank char" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "0123   \t   \n   \t 789\n"
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 4)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val cursorChr = getChr app
      in
        Expect.isTrue (cursorChr = #"7")
      end)

  , test "moves cursor to first alphanumeric char when on punctuation" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "!!! hello\n"
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val (app1, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val startsAtExc = getChr app = #"!"
        val movedToH = getChr app1 = #"h"
      in
        Expect.isTrue (startsAtExc andalso movedToH)
      end)

  , test "moves cursor to last char when cursor is on last word" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello world\n"
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 6)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val chrIsEnd = getChr app = #"d"
      in
        Expect.isTrue chrIsEnd
      end)
  ]

val WMove = describe "move motion 'W'"
  [ test "moves cursor to start of next WORD in contiguous string" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello world"
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"W")

        (* assert *)
        val chr = String.sub ("hello world", cursorIdx)
      in
        Expect.isTrue (chr = #"w")
      end)

  , test "moves cursor to start of next WORD in split string" (fn _ =>
      let
        (* arrange *)
        val buffer = fromList ["hello ", "world"]
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val ({cursorIdx, ...}, _) = AppUpdate.update (app, CHAR_EVENT #"W")

        (* assert *)
        val chr = String.sub ("hello world", cursorIdx)
      in
        Expect.isTrue (chr = #"w")
      end)

  , test "moves cursor past newline when next WORD is after newline" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello \n\n\n world"
        val app = AppType.init (buffer, 0, 0)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"W")

        (* assert *)
        val cursorChr = getChr app
      in
        Expect.isTrue (cursorChr = #"w")
      end)

  , test "does not break on punctuation when cursor is on alphanumeric char"
      (fn _ =>
         (* vi's definition of 'WORD' instead of 'word' *)
         let
           (* arrange *)
           val buffer = LineGap.fromString "hello, world"
           val app = AppType.init (buffer, 0, 0)

           (* act *)
           val (app, _) = AppUpdate.update (app, CHAR_EVENT #"W")

           (* assert *)
           val cursorChr = getChr app
         in
           Expect.isTrue (cursorChr = #"w")
         end)

  , test "does not break on alphanumeric char when cursor is on punctuation"
      (fn _ =>
         (* vi's definition of 'WORD' instead of 'word' *)
         let
           (* arrange *)
           val buffer = LineGap.fromString "#!hello!!! world!!!\n"
           val app = AppType.init (buffer, 0, 0)

           (* act *)
           val (app, _) = AppUpdate.update (app, CHAR_EVENT #"W")

           (* assert *)
           val cursorChr = getChr app
         in
           Expect.isTrue (cursorChr = #"w")
         end)

  , test "moves cursor to first non-blank when cursor is on blank" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "0123   \t   \n   \t 789\n"
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 4)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val cursorChr = getChr app
      in
        Expect.isTrue (cursorChr = #"7")
      end)

  , test "moves cursor to last char when cursor is on last word" (fn _ =>
      let
        (* arrange *)
        val buffer = LineGap.fromString "hello world\n"
        val app = AppType.init (buffer, 0, 0)
        val app = withIdx (app, 6)

        (* act *)
        val (app, _) = AppUpdate.update (app, CHAR_EVENT #"w")

        (* assert *)
        val chrIsEnd = getChr app = #"d"
      in
        Expect.isTrue chrIsEnd
      end)
  ]

val tests = concat [hMove, lMove, jMove, kMove, wMove, WMove]

val _ = run tests
