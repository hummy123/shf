structure NormalMove =
struct
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
        , msgs
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
      , msgs = msgs
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
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"h")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 0)
           end)
    , test "moves cursor left by one in split string when cursorIdx > 0"
        (fn _ =>
           let
             (* arrange *)
             val buffer = fromList ["hello", " world"]
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 5)

             (* act *)
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"h")
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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"h")
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
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"h")
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
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"h")
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
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"l")
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
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"l")
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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"l")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 10)
        end)
    , test
        "moves right by two in contiguous string when char is followed by \\n"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello\nworld\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 4)

             (* act *)
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"l")
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
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"l")
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
             val buffer = LineGap.fromString
               "hello \nworld \ngoodbye \nqorld \n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app1 = AppUpdate.update (app, CHAR_EVENT #"j")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"j")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"j")

             (* assert *)
             val c1 = getChr app1 = #"w"
             val c2 = getChr app2 = #"g"
             val c3 = getChr app3 = #"q"
           in
             Expect.isTrue (c1 andalso c2 andalso c3)
           end)
    , test "moves cursur down one column in split string when column = 0"
        (fn _ =>
           let
             (* arrange *)
             val buffer =
               fromList ["hello \n", "world \n", "goodbye \n", "qorld"]
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app1 = AppUpdate.update (app, CHAR_EVENT #"j")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"j")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"j")

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
             val app1 = AppUpdate.update (app, CHAR_EVENT #"j")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"j")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"j")

             (* assert *)
             val c1 = getChr app1 = #"o"
             val c2 = getChr app2 = #"y"
             val c3 = getChr app3 = #"r"
           in
             Expect.isTrue (c1 andalso c2 andalso c3)
           end)
    , test "moves cursur down one column in split string when column = 1"
        (fn _ =>
           let
             (* arrange *)
             val buffer =
               fromList ["hello \n", "world ", "\nb", "ye \nfriends \n"]
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 1)

             (* act *)
             val app1 = AppUpdate.update (app, CHAR_EVENT #"j")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"j")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"j")

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
             val app1 = AppUpdate.update (app, CHAR_EVENT #"j")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"j")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"j")

             (* assert *)
             val c1 = getChr app1 = #"r"
             val c2 = getChr app2 = #"e"
             val c3 = getChr app3 = #"i"
           in
             Expect.isTrue (c1 andalso c2 andalso c3)
           end)
    , test "moves cursur down one column in split string when column = 2"
        (fn _ =>
           let
             (* arrange *)
             val buffer =
               fromList ["hello \n", "world ", "\nb", "ye \nfriends \n"]
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 2)

             (* act *)
             val app1 = AppUpdate.update (app, CHAR_EVENT #"j")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"j")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"j")

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
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"j")

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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"j")

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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"j")

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
             val app1 = AppUpdate.update (app, CHAR_EVENT #"k")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"k")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"k")

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
          val app1 = AppUpdate.update (app, CHAR_EVENT #"k")
          val app2 = AppUpdate.update (app1, CHAR_EVENT #"k")
          val app3 = AppUpdate.update (app2, CHAR_EVENT #"k")

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
             val app1 = AppUpdate.update (app, CHAR_EVENT #"k")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"k")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"k")

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
          val app1 = AppUpdate.update (app, CHAR_EVENT #"k")
          val app2 = AppUpdate.update (app1, CHAR_EVENT #"k")
          val app3 = AppUpdate.update (app2, CHAR_EVENT #"k")

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
             val app1 = AppUpdate.update (app, CHAR_EVENT #"k")
             val app2 = AppUpdate.update (app1, CHAR_EVENT #"k")
             val app3 = AppUpdate.update (app2, CHAR_EVENT #"k")

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
          val app1 = AppUpdate.update (app, CHAR_EVENT #"k")
          val app2 = AppUpdate.update (app1, CHAR_EVENT #"k")
          val app3 = AppUpdate.update (app2, CHAR_EVENT #"k")

          (* assert *)
          val c1 = getChr app1 = #"1"
          val c2 = getChr app2 = #"6"
          val c3 = getChr app3 = #"2"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test
        "skips '\\n' when cursor is on '\\n',\
        \prev-char is '\\n' and prev-prev char is not '\\n'"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello\n\n world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 6)

             (* act *)
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"k")

             (* assert *)
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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"k")

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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"k")

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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"w")

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
             val app = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"w")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"7")
        end)
    , test "moves cursor to first alphanumeric char when on punctuation"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "!!! hello\n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app1 = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"W")

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
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"W")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"W")

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
             val app = AppUpdate.update (app, CHAR_EVENT #"W")

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
             val app = AppUpdate.update (app, CHAR_EVENT #"W")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"w")

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
          val app = AppUpdate.update (app, CHAR_EVENT #"w")

          (* assert *)
          val chrIsEnd = getChr app = #"d"
        in
          Expect.isTrue chrIsEnd
        end)
    ]

  val eMove = describe "move motion 'e'"
    [ test
        "moves cursor to last alphanumeric char in contiguous string\
        \when in alphanumeric word and there is at least one\
        \alphanumeric char after cursor"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello world\n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"e")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"o")
           end)
    , test
        "moves cursor to last alphanumeric char in split string\
          \when in alphanumeric word and there is at least one\
          \alphanumeric char after cursor"
        (fn _ =>
           let
             (* arrange *)
             val buffer = fromList ["hello ", "world", "\n"]
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"e")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"o")
           end)
    , test
        "moves cursor to last punctuation char in contiguous string\
        \when in punctuation word and there is at least one\
        \punctuation char after cursor"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "#$%!^ world\n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"e")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"^")
           end)
    , test
        "moves cursor to last punctuation char in split string\
          \when in punctuation word and there is at least one\
          \punctuation char after cursor"
        (fn _ =>
           let
             (* arrange *)
             val buffer = fromList ["#$", "%!^ ", "world", "\n"]
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"e")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"^")
           end)
    , test
        "moves cursor to last char of next word,\ 
        \when cursor is on last char of current word"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 4)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"e")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"d")
           end)
    , test "does not break on undescore when cursor is on alphanumeric char"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello_world\n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"e")

             (* assert *)
             val cursorChr = getChr app
           in
             Expect.isTrue (cursorChr = #"d")
           end)
    , test "breaks on undescore when cursor is on punctuation char" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "#!^*(_#!@*(\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"e")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"(")
        end)
    , test "breaks on punctuation when cursor is on alphanumeric char" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello, world"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"e")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"o")
        end)
    , test "breaks on alphanumeric char when cursor is on punctuation" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val buffer = LineGap.fromString "!#%^()hello\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"e")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #")")
        end)
    , test "skips 'space' chars: '\\n', '\\t', ' '" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "0123   \t   \n   \t 789\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 4)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"e")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"9")
        end)
    , test
        "moves cursor to last char in punctuation string \
        \when cursor is on punctuation"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "!!! hello\n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"e")
             val newIdx = #cursorIdx app
           in
             (* assert *)
             Expect.isTrue (newIdx = 2)
           end)
    , test "moves cursor to last char when cursor is on last word" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 7)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"e")

          (* assert *)
          val chrIsEnd = getChr app = #"d"
        in
          Expect.isTrue chrIsEnd
        end)
    ]

  val EMove = describe "move motion 'E'"
    [ test "moves cursor to last char in WORD when in contiguous string"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hel!!!lo world\n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"E")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"o")
           end)
    , test "moves cursor to last char in WORD when in split string" (fn _ =>
        let
          (* arrange *)
          val buffer = fromList ["hel", "!!!", "lo ", "world", "\n"]
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"E")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"o")
        end)
    , test
        "moves cursor to last char of next WORD,\ 
        \when cursor is on last char of current WORD"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "#ELL) !@*(ORL$\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 4)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"E")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"$")
           end)
    , test "does not break on punctuation when in alphanumeric char" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello, world"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"E")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #",")
        end)
    , test "does not break on alphanumeric char when in punctuation" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val buffer = LineGap.fromString "!#%^()hello world\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"E")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"o")
        end)
    , test "skips 'space' chars: '\\n', '\\t', ' '" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "0123   \t   \n   \t 789\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 4)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"E")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"9")
        end)
    , test "moves cursor to last char when cursor is on last word" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world!\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 7)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"E")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"!")
        end)
    ]

  val bMove = describe "move motion 'b'"
    [ test "leaves cursor at 0 when cursor is already at 0" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"b")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "moves cursor to 0 when cursor > 0 and cursor is on first word"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 3)

             (* act *)
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"b")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 0)
           end)
    , test
        "moves cursor to first alphanumeric char after whitespace \
        \when in alphanumeric word"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "   hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"b")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"h")
           end)
    , test
        "moves cursor to first alphanumeric char after punctuation \
        \when in alphanumeric word"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "!*#hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"b")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"h")
           end)
    , test
        "moves cursor to first punctuation char after whitespace \
        \when in punctuation word"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "   !@#$%^&*()"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"b")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"!")
           end)
    , test
        "moves cursor to first punctuation char after \
        \alphanumeric char when in punctuation word"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "abc!@#$%^&*()"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"b")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"!")
           end)
    ]

  val BMove = describe "move motion 'B'"
    [ test "leaves cursor at 0 when cursor is already at 0" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"B")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "moves cursor to 0 when cursor > 0 and cursor is on first WORD"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 3)

             (* act *)
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"B")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 0)
           end)
    , test
        "moves cursor to first non-space char after whitespace \
        \when in WORD"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "   hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"B")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"h")
           end)
    , test
        "moves cursor to 0 when cursor is on first letter of first WORD \
        \and there are leadinng spaces before first letter"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "   hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 3)

             (* act *)
             val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"B")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 0)
           end)
    , test
        "moves cursor to first char in WORD \
        \when in alphanumeric word preceded by punctuation"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "!*#hello world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"B")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"!")
           end)
    , test "moves cursor to first char after whitespace when in WORD" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "   !qwerty@#$%^&*()\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 17)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"B")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"!")
        end)
    , test
        "moves cursor to first char in WORD \
        \when in punctuation word preceded by alphanumeric"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "abc!@#$%^&*()"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 11)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"B")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"a")
           end)
    ]


  val zeroMove = describe "move motion '0'"
    [ test "moves cursor to 0 in contiguous string when on first line" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello w7rld\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 7)

          (* act *)
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"0")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "moves cursor to 0 in split string when on first line" (fn _ =>
        let
          (* arrange *)
          val buffer = fromList ["hel", "lo ", "w7r", "ld\n"]
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 7)

          (* act *)
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"0")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "leaves cursor on 0 when cursor is already on 0" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val {cursorIdx, ...} = AppUpdate.update (app, CHAR_EVENT #"0")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "leaves cursor at same idx when cursor is on '\\n'" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world\n hello again\n"
          val app = AppType.init (buffer, 0, 0)

          val app = withIdx (app, 11)
          val {cursorIdx = oldIdx, ...} = app

          (* act *)
          val {cursorIdx = newIdx, ...} =
            AppUpdate.update (app, CHAR_EVENT #"0")
        in
          (* assert *)
          Expect.isTrue (oldIdx = newIdx)
        end)
    , test
        "moves cursor to first char after '\\n' in contiguous string\
        \when cursor is after first line"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello world\n#ello again\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 21)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"0")

             (* assert *)
             val chr = getChr app
           in
             (* assert *)
             Expect.isTrue (chr = #"#")
           end)
    , test
        "moves cursor to first char after '\\n' in split string\
        \when cursor is after first line"
        (fn _ =>
           let
             (* arrange *)
             val buffer = fromList
               ["hel", "lo ", "wor", "ld\n", "#el", "lo ", "aga", "in\n"]
             val buffer = LineGap.fromString "hello world\n#ello again\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 21)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"0")

             (* assert *)
             val chr = getChr app
           in
             (* assert *)
             Expect.isTrue (chr = #"#")
           end)
    ]

  val dlrMove = describe "move motion '$'"
    [ test "moves cursor to char before '\\n' in contiguous string" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello wor9\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"$")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"9")
        end)
    , test "moves cursor to char before '\\n' in split string" (fn _ =>
        let
          (* arrange *)
          val buffer = fromList ["hel", "lo ", " wor9\n"]
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"$")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"9")
        end)
    , test
        "leaves cursor at same idx in contiguous string\
        \when char after cursor is '\\n'"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello\n world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 11)
             val oldIdx = #cursorIdx app

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"$")
             val newIdx = #cursorIdx app

             val nchr = getChr app
             val nchr = Char.toString nchr ^ "\n"
           in
             (* assert *)
             Expect.isTrue (oldIdx = newIdx)
           end)
    , test
        "leaves cursor at same idx in split string\
        \when char after cursor is '\\n'"
        (fn _ =>
           let
             (* arrange *)
             val buffer = fromList ["hel", "lo\n", " wo", "rld", "\n"]
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 11)
             val oldIdx = #cursorIdx app

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"$")
             val newIdx = #cursorIdx app

             val nchr = getChr app
             val nchr = Char.toString nchr ^ "\n"
           in
             (* assert *)
             Expect.isTrue (oldIdx = newIdx)
           end)
    ]

  val hatMove = describe "move motion '^'"
    [ test
        "moves cursor to first non-space char in first line\
        \when first line starts with spaces\
        \and cursor is on first space"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "   3ello world\n"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"^")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"3")
           end)
    , test
        "moves cursor to first non-space char in first line\
        \when first line starts with space\
        \and cursor is after first non-space char"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "   3ell7 world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"^")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"3")
           end)
    , test
        "moves cursor to first non-space char\ 
        \when cursor is after first line\
        \and before first non-space char"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello\n   world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 7)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"^")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"w")
           end)
    , test
        "moves cursor to first non-space char\ 
        \when cursor is after first line\
        \and after first non-space char"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello\n   world\n"
             val app = AppType.init (buffer, 0, 0)
             val app = withIdx (app, 11)

             (* act *)
             val app = AppUpdate.update (app, CHAR_EVENT #"^")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"w")
           end)
    , test "leaves cursor in same position when on '\\n'" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hel\nlo\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 3)
          val oldIdx = #cursorIdx app

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"^")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = oldIdx)
        end)
    ]

  val GMove = describe "move motion 'G'"
    [test "moves cursor to last char in buffer" (fn _ =>
       (* Note: We assume unix-style line endings:
        * End of file always has \n at the end.
        * We don't want cursor to ever reach this last \n
        * so we say last char is the char before \n
        * *)
       let
         (* arrange *)
         val buffer = LineGap.fromString "01234\n56789\n"
         val app = AppType.init (buffer, 0, 0)

         (* act *)
         val app = AppUpdate.update (app, CHAR_EVENT #"G")
       in
         (* assert *)
         Expect.isTrue (getChr app = #"9")
       end)]

  val percentMove = describe "move motion '%'"
    [ test "moves to next ) when cursor is on (" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(hello)\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #")")
        end)
    , test "moves to preceding ( when cursur is on )" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(hello)\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 6)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"(")
        end)
    (* testing that cursor goes to correct level of nesting *)
    , test "moves to outermost ) when cursor is on outermost (" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(((hello)))\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 10)
        end)
    , test "moves to outermost ( when cursor is on outermost )" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(((hello)))\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 10)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 0)
        end)
    , test "moves to middle ) when cursor is on middle (" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(((hello)))\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 1)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 9)
        end)
    , test "moves to middle ( when cursor is on middle )" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(((hello)))\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 9)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 1)
        end)
    , test "moves to innermost ) when cursor is on innermost (" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(((hello)))\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 2)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 8)
        end)
    , test "moves to innermost ( when cursor is on innermost )" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "(((hello)))\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 8)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 2)
        end)
    (* testing different pair combinations *)
    , test "moves to next ] when cursor is on [" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "[hello]\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"]")
        end)
    , test "moves to preceding [ when cursur is on ]" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "[hello]\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 6)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"[")
        end)
    , test "moves to next } when cursor is on {" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "{hello}\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"}")
        end)
    , test "moves to preceding { when cursur is on }" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "{hello}\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 6)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"{")
        end)
    , test "moves to next > when cursor is on <" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "<hello>\n"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #">")
        end)
    , test "moves to preceding < when cursur is on >" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "<hello>\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 6)

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"<")
        end)
    (* testing that % on a non-pair character is a no-op *)
    , test "does not move when cursor is on a non-pair-character" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello, world\n"
          val app = AppType.init (buffer, 0, 0)
          val app = withIdx (app, 5)
          val oldIdx = #cursorIdx app

          (* act *)
          val app = AppUpdate.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = oldIdx)
        end)
    ]

  (* movements which use multiple chars *)
  fun updateMany (app, str) =
    let
      fun loop (pos, app) =
        if pos = String.size str then
          app
        else
          let
            val chr = String.sub (str, pos)
            val app = AppUpdate.update (app, CHAR_EVENT chr)
          in
            loop (pos + 1, app)
          end
    in
      loop (0, app)
    end

  val tMove = describe "move motion 't'"
    [ test
        "motion 'td' moves cursor to char before 'd' in string \"hello world\""
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello world"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app = updateMany (app, "td")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"l")
           end)
    , test "repeating 't' motion with same char does not move cursor" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app1 = updateMany (app, "td")
          val app2 = updateMany (app1, "td")
        in
          (* assert *)
          Expect.isTrue
            (#cursorIdx app1 = #cursorIdx app2 andalso getChr app1 = #"l")
        end)
    , test
        "does not move cursor at all when char following 't' is not in string"
        (fn _ =>
           let
             (* arrange *)
             val buffer = LineGap.fromString "hello world"
             val app = AppType.init (buffer, 0, 0)

             (* act *)
             val app1 = updateMany (app, "t;")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app1 = #cursorIdx app)
           end)
    , test "is cancellable by pressing escape" (fn _ =>
        let
          (* arrange *)
          val buffer = LineGap.fromString "hello world"
          val app = AppType.init (buffer, 0, 0)

          (* act *)
          val app1 = AppUpdate.update (app, CHAR_EVENT #"t")
          val app2 = AppUpdate.update (app1, KEY_ESC)
          val app3 = AppUpdate.update (app2, CHAR_EVENT #"d")
        in
          (* assert *)
          Expect.isTrue
            (#cursorIdx app1 = #cursorIdx app2
             andalso #cursorIdx app2 = #cursorIdx app3)
        end)
    ]

  val tests = concat
    [ hMove
    , jMove
    , kMove
    , lMove
    , wMove
    , WMove
    , bMove
    , BMove
    , eMove
    , EMove
    , zeroMove
    , dlrMove
    , hatMove
    , GMove
    , percentMove
    (* multi-char motions *)
    , tMove
    ]
end
