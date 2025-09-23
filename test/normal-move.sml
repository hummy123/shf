structure NormalMove =
struct
  open Railroad
  open Railroad.Test
  open InputMsg

  fun getChr (app: AppType.app_type) =
    let
      val {cursorIdx, buffer, ...} = app
      val c = LineGap.substring (cursorIdx, 1, buffer)
    in
      String.sub (c, 0)
    end

  val hMove = describe "move motion 'h'"
    [ test "moves cursor left by one when cursorIdx > 0 and is not on a newline"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 1)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"h")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 0)
           end)
    , test "does not move cursor when cursor is already at index 0" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello\n\nworld"
          val app = AppWith.idx (app, 0)

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"h")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test
        "moves cursor to char before a newline\
        \ when there is just one newline to the left"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\nworld"
             val app = AppWith.idx (app, 6)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"h")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 4)
           end)
    , test
        "moves cursor past first newline immediately following \
        \a non-newline character"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\n\nworld"
             val app = AppWith.idx (app, 7)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"h")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 6)
           end)
    , test
        "moves cursor past newline when we see, to the left, \
        \ a newline with a chr prior to it"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\nworld"
             val app = AppWith.idx (app, 6)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"h")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 4)
           end)
    , test "moves cursor to a newline when newline is not preceded by char"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "\n\n\nhello\n"
             val app = AppWith.idx (app, 3)

             (* act *)
             val app1 = TestUtils.update (app, CHAR_EVENT #"h")
             val app2 = TestUtils.update (app1, CHAR_EVENT #"h")
             val app3 = TestUtils.update (app2, CHAR_EVENT #"h")
             val app4 = TestUtils.update (app3, CHAR_EVENT #"h")

             (* assert *)
             val c1 = #cursorIdx app1 = 2
             val c2 = #cursorIdx app2 = 1
             val c3 = #cursorIdx app3 = 0
             val c4 = #cursorIdx app4 = 0
           in
             Expect.isTrue (c1 andalso c2 andalso c3 andalso c4)
           end)
    ]

  val lMove = describe "move motion 'l'"
    [ test "moves cursor right by one when cursorIdx < length" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val {cursorIdx = oldCursorIdx, ...} = app

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"l")
        in
          (* assert *)
          Expect.isTrue (oldCursorIdx = 0 andalso cursorIdx = 1)
        end)
    , test
        "does not move cursor when cursorIdx is at end of buffer \
        \and last char is a newline preceded by a newline"
        (fn _ =>
           let
             (* arrange *)
             val str = "hello world\n\n"
             val initialCursorIdx = String.size str - 1

             val app = TestUtils.init str
             val app = AppWith.idx (app, initialCursorIdx)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"l")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = initialCursorIdx)
           end)
    , test
        "does not move cursor when cursorIdx is at end of buffer \
        \and last char is a non-newline preceded by a non-newline"
        (fn _ =>
           let
             (* arrange *)
             val str = "hello world"
             val initialCursorIdx = String.size str - 1

             val app = TestUtils.init str
             val app = AppWith.idx (app, initialCursorIdx)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"l")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = initialCursorIdx)
           end)
    , test
        "does not move cursor when cursorIdx is at end of buffer \
        \and last char is a newline preceded by a non-newline"
        (fn _ =>
           let
             (* arrange *)
             val str = "hello world\n"
             val initialCursorIdx = String.size str - 2

             val app = TestUtils.init str
             val app = AppWith.idx (app, initialCursorIdx)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"l")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = initialCursorIdx)
           end)
    , test "moves cursor to char past newline when newline is preceded by char"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\nworld\n"
             val app = AppWith.idx (app, 4)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"l")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 6)
           end)
    , test "moves cursor to second newline when newline is preceded by char"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\n\nworld"
             val app = AppWith.idx (app, 4)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"l")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 6)
           end)
    , test
        "moves cursor to each newline without skipping when no newline \
        \is preceded by char"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "\n\n\nhello\n"
             val app = AppWith.idx (app, 0)

             (* act *)
             val app1 = TestUtils.update (app, CHAR_EVENT #"l")
             val app2 = TestUtils.update (app1, CHAR_EVENT #"l")
             val app3 = TestUtils.update (app2, CHAR_EVENT #"l")
             val app4 = TestUtils.update (app3, CHAR_EVENT #"l")

             (* assert *)
             val c1 = #cursorIdx app1 = 1
             val c2 = #cursorIdx app2 = 2
             val c3 = #cursorIdx app3 = 3
             val c4 = #cursorIdx app4 = 4
           in
             Expect.isTrue (c1 andalso c2 andalso c3 andalso c4)
           end)
    ]

  val jMove = describe "move motion 'j'"
    [ test "moves cursur down one column when column = 0" (fn _ =>
        let
          (* arrange *)
          (* "world" at end of string is intentionally misspelled as "qorld"
           * since "world" appears twice and it is useful to differentiate them
           * *)
          val app = TestUtils.init "hello \nworld \ngoodbye \nqorld \n"

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"j")
          val app2 = TestUtils.update (app1, CHAR_EVENT #"j")
          val app3 = TestUtils.update (app2, CHAR_EVENT #"j")

          (* assert *)
          val c1 = getChr app1 = #"w"
          val c2 = getChr app2 = #"g"
          val c3 = getChr app3 = #"q"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test "moves cursur down one column when column = 1" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello \nworld \nbye \nfriends \n"
          val app = AppWith.idx (app, 1)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"j")
          val app2 = TestUtils.update (app1, CHAR_EVENT #"j")
          val app3 = TestUtils.update (app2, CHAR_EVENT #"j")

          (* assert *)
          val c1 = getChr app1 = #"o"
          val c2 = getChr app2 = #"y"
          val c3 = getChr app3 = #"r"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test "moves cursur down one column when column = 2" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello \nworld \nbye \nfriends \n"
          val app = AppWith.idx (app, 2)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"j")
          val app2 = TestUtils.update (app1, CHAR_EVENT #"j")
          val app3 = TestUtils.update (app2, CHAR_EVENT #"j")

          (* assert *)
          val c1 = getChr app1 = #"r"
          val c2 = getChr app2 = #"e"
          val c3 = getChr app3 = #"i"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test
        "moves to last char on below column \
        \when cursor is on a column that is greater than \
        \the number of columns on the next line"
        (fn _ =>
           let
             (* arrange *)
             val str =
               "hello world!\n\
               \bye!\n"
             val app = TestUtils.init str
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"j")

             (* assert *)
             val c1 = getChr app = #"!"
           in
             Expect.isTrue c1
           end)
    , test "when next newline is preceded by char, goes to idx after newline"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\n\nworld\n"

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"j")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 6)
           end)
    , test "moves to same column on last line after a count" (fn _ =>
        let
          (* arrange *)
          val str =
            "let\n\
            \hello\n\
            \in\n\
            \0\n\
            \end\n"

          val app = TestUtils.init str
          val app1 = AppWith.idx (app, 0)
          val app2 = AppWith.idx (app, 1)
          val app3 = AppWith.idx (app, 2)

          (* act *)
          val newApp1 = TestUtils.updateMany (app1, "4j")
          val newApp2 = TestUtils.updateMany (app2, "4j")
          val newApp3 = TestUtils.updateMany (app3, "4j")

          (* assert *)
          val c1 = getChr newApp1 = #"e"
          val c2 = getChr newApp2 = #"n"
          val c3 = getChr newApp3 = #"d"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test "leaves cursor at same idx when on the last line" (fn _ =>
        let
          (* arrange *)
          val str = "hello \nworld \ntime to go\n"
          val app = TestUtils.init str

          val initialCursorIdx = String.size str - 1
          val app = AppWith.idx (app, initialCursorIdx)

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"j")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = initialCursorIdx)
        end)
    , test "goes to next idx when cursor is on a newline" (fn _ =>
        let
          (* arrange *)
          val str = "hello\n\nworld\n"
          val app = TestUtils.init str
          val app = AppWith.idx (app, 6)

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"j")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 7)
        end)
    ]

  val kMove = describe "move motion 'k'"
    [ test "moves cursur up one column when column = 0" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "0__\n4___\n9___\n14_"
          val app = AppWith.idx (app, 14)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"k")
          val app2 = TestUtils.update (app1, CHAR_EVENT #"k")
          val app3 = TestUtils.update (app2, CHAR_EVENT #"k")

          (* assert *)
          val c1 = getChr app1 = #"9"
          val c2 = getChr app2 = #"4"
          val c3 = getChr app3 = #"0"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test "moves cursur up one column when column = 1" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "_w_\n_5__\n_10_\n_15"
          val app = AppWith.idx (app, 15)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"k")
          val app2 = TestUtils.update (app1, CHAR_EVENT #"k")
          val app3 = TestUtils.update (app2, CHAR_EVENT #"k")

          (* assert *)
          val c1 = getChr app1 = #"1"
          val c2 = getChr app2 = #"5"
          val c3 = getChr app3 = #"w"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test "moves cursur up one column when column = 2" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "__2\n__6\n__10\n__15\n"
          val app = AppWith.idx (app, 15)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"k")
          val app2 = TestUtils.update (app1, CHAR_EVENT #"k")
          val app3 = TestUtils.update (app2, CHAR_EVENT #"k")

          (* assert *)
          val c1 = getChr app1 = #"1"
          val c2 = getChr app2 = #"6"
          val c3 = getChr app3 = #"2"
        in
          Expect.isTrue (c1 andalso c2 andalso c3)
        end)
    , test "goes to last newline when there are two newlines preceding cursor"
        (fn _ =>
           let
             (* arrange *)
             val str = "hello\n\n world\n"
             val app = TestUtils.init str
             val app = AppWith.idx (app, 7)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"k")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 6)
           end)
    , test "leaves cursor at same idx when already on first line" (fn _ =>
        let
          (* arrange *)
          val str = "hello \nworld \ntime to go\n"
          val app = TestUtils.init str
          (* line below does nothing; just for explicitness *)
          val app = AppWith.idx (app, 0)

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"k")

          (* assert *)
          val isAtStart = cursorIdx = 0
        in
          Expect.isTrue isAtStart
        end)
    , test
        "goes to last column of previous line when cursor is \
        \on a column greater than the number of columns in the previous line"
        (fn _ =>
           let
             (* arrange *)
             val str =
               "hello world\n\
               \now a quite long line is next\n"

             val app = TestUtils.init str
             val app = AppWith.idx (app, String.size str - 2)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"k")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 10)
           end)
    , test
        "when the previous newline is preceded by a non-newline, \
        \jumps past newline"
        (fn _ =>
           let
             (* arrange *)
             val str = "hello\n\nworld\n"

             val app = TestUtils.init str
             val app = AppWith.idx (app, 6)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"k")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 0)
           end)
    , test
        "when file ends with two newlines, \
        \and cursor is on second-last newline, \
        \we should be able to move up by one line"
        (fn _ =>
           let
             (* arrange *)
             val str = "hello\nworld\n\n"

             val app = TestUtils.init str
             val app = AppWith.idx (app, 12)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"k")
           in
             (* assert *)
             Expect.isTrue (cursorIdx = 6)
           end)
    ]

  val wMove = describe "move motion 'w'"
    [ test "moves cursor to start of next word in contiguous string" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"w")

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
          val app = TestUtils.init "hello \n\n\n world"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"w")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"w")
        end)
    , test "does not break on undescore when cursor is on alphanumeric char"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello_world goodbye_world"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"w")

             (* assert *)
             val cursorChr = getChr app
           in
             Expect.isTrue (cursorChr = #"g")
           end)
    , test "breaks on punctuation when cursor is on alphanumeric char" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val app = TestUtils.init "hello, world"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"w")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #",")
        end)
    , test "breaks on alphanumeric char when cursor is on punctuation" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val app = TestUtils.init "!#%^()hello\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"w")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"h")
        end)
    , test "breaks on non-blank char when on blank char" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "0123   \t   \n   \t 789\n"
          val app = AppWith.idx (app, 4)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"w")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"7")
        end)
    , test "moves cursor to first alphanumeric char when on punctuation"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "!!! hello\n"

             (* act *)
             val app1 = TestUtils.update (app, CHAR_EVENT #"w")

             (* assert *)
             val startsAtExc = getChr app = #"!"
             val movedToH = getChr app1 = #"h"
           in
             Expect.isTrue (startsAtExc andalso movedToH)
           end)
    , test "moves cursor to last char when cursor is on last word" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world\n"
          val app = AppWith.idx (app, 6)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"w")

          (* assert *)
          val chrIsEnd = getChr app = #"d"
        in
          Expect.isTrue chrIsEnd
        end)
    , test
        "moves cursor to second newline when cursor is on the last word \
        \and the file ends with two newlines"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\n\n"
             val app = AppWith.idx (app, 0)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"w")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = 6)
           end)
    , test
        "does not move to or past newline when cursor is on last word \
        \and text ends with newline"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello\n"
             val app = AppWith.idx (app, 0)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"w")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = 4)
           end)
    ]

  val WMove = describe "move motion 'W'"
    [ test "moves cursor to start of next WORD in contiguous string" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"W")

          (* assert *)
          val chr = String.sub ("hello world", cursorIdx)
        in
          Expect.isTrue (chr = #"w")
        end)
    , test "moves cursor past newline when next WORD is after newline" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello \n\n\n world"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"W")

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
             val app = TestUtils.init "hello, world"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"W")

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
             val app = TestUtils.init "#!hello!!! world!!!\n"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"W")

             (* assert *)
             val cursorChr = getChr app
           in
             Expect.isTrue (cursorChr = #"w")
           end)
    , test "moves cursor to first non-blank when cursor is on blank" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "0123   \t   \n   \t 789\n"
          val app = AppWith.idx (app, 4)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"w")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"7")
        end)
    , test "moves cursor to last char when cursor is on last word" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world\n"
          val app = AppWith.idx (app, 6)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"w")

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
             val app = TestUtils.init "hello world\n"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"e")
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
             val app = TestUtils.init "#$%!^ world\n"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"e")
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
             val app = TestUtils.init "hello world\n"
             val app = AppWith.idx (app, 4)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"e")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"d")
           end)
    , test "does not break on undescore when cursor is on alphanumeric char"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello_world\n"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"e")

             (* assert *)
             val cursorChr = getChr app
           in
             Expect.isTrue (cursorChr = #"d")
           end)
    , test "breaks on undescore when cursor is on punctuation char" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "#!^*(_#!@*(\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"e")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"(")
        end)
    , test "breaks on punctuation when cursor is on alphanumeric char" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val app = TestUtils.init "hello, world"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"e")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"o")
        end)
    , test "breaks on alphanumeric char when cursor is on punctuation" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val app = TestUtils.init "!#%^()hello\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"e")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #")")
        end)
    , test "skips 'space' chars: '\\n', '\\t', ' '" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "0123   \t   \n   \t 789\n"
          val app = AppWith.idx (app, 4)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"e")

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
             val app = TestUtils.init "!!! hello\n"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"e")
             val newIdx = #cursorIdx app
           in
             (* assert *)
             Expect.isTrue (newIdx = 2)
           end)
    , test "moves cursor to last char when cursor is on last word" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world\n"
          val app = AppWith.idx (app, 7)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"e")

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
             val app = TestUtils.init "hel!!!lo world\n"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"E")
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
             val app = TestUtils.init "#ELL) !@*(ORL$\n"
             val app = AppWith.idx (app, 4)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"E")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"$")
           end)
    , test "does not break on punctuation when in alphanumeric char" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val app = TestUtils.init "hello, world"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"E")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #",")
        end)
    , test "does not break on alphanumeric char when in punctuation" (fn _ =>
        (* vi's definition of 'word' instead of 'WORD' *)
        let
          (* arrange *)
          val app = TestUtils.init "!#%^()hello world\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"E")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"o")
        end)
    , test "skips 'space' chars: '\\n', '\\t', ' '" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "0123   \t   \n   \t 789\n"
          val app = AppWith.idx (app, 4)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"E")

          (* assert *)
          val cursorChr = getChr app
        in
          Expect.isTrue (cursorChr = #"9")
        end)
    , test "moves cursor to last char when cursor is on last word" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world!\n"
          val app = AppWith.idx (app, 7)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"E")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"!")
        end)
    ]

  val bMove = describe "move motion 'b'"
    [ test "leaves cursor at 0 when cursor is already at 0" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world\n"

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"b")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "moves cursor previous word when on first character of next word"
        (fn _ =>
           let
             val app = TestUtils.init "hello world\n"
             val app = AppWith.idx (app, 6)
             val chr1 = getChr app

             val app2 = TestUtils.update (app, CHAR_EVENT #"b")
             val chr2 = getChr app2
           in
             Expect.isTrue (chr1 = #"w" andalso chr2 = #"h")
           end)
    , test "moves cursor to 0 when cursor > 0 and cursor is on first word"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world\n"
             val app = AppWith.idx (app, 3)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"b")
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
             val app = TestUtils.init "   hello world\n"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"b")
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
             val app = TestUtils.init "!*#hello world\n"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"b")
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
             val app = TestUtils.init "   !@#$%^&*()"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"b")
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
             val app = TestUtils.init "abc!@#$%^&*()"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"b")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"!")
           end)
    ]

  val BMove = describe "move motion 'B'"
    [ test "leaves cursor at 0 when cursor is already at 0" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world\n"

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"B")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "moves cursor to 0 when cursor > 0 and cursor is on first WORD"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world\n"
             val app = AppWith.idx (app, 3)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"B")
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
             val app = TestUtils.init "   hello world\n"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"B")
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
             val app = TestUtils.init "   hello world\n"
             val app = AppWith.idx (app, 3)

             (* act *)
             val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"B")
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
             val app = TestUtils.init "!*#hello world\n"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"B")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"!")
           end)
    , test "moves cursor to first char after whitespace when in WORD" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "   !qwerty@#$%^&*()\n"
          val app = AppWith.idx (app, 17)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"B")
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
             val app = TestUtils.init "abc!@#$%^&*()"
             val app = AppWith.idx (app, 11)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"B")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"a")
           end)
    ]


  val zeroMove = describe "move motion '0'"
    [ test "moves cursor to 0 in contiguous string when on first line" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello w7rld\n"
          val app = AppWith.idx (app, 7)

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"0")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "leaves cursor on 0 when cursor is already on 0" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world\n"

          (* act *)
          val {cursorIdx, ...} = TestUtils.update (app, CHAR_EVENT #"0")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = 0)
        end)
    , test "leaves cursor at same idx when cursor is on '\\n'" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world\n hello again\n"

          val app = AppWith.idx (app, 11)
          val {cursorIdx = oldIdx, ...} = app

          (* act *)
          val {cursorIdx = newIdx, ...} =
            TestUtils.update (app, CHAR_EVENT #"0")
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
             val app = TestUtils.init "hello world\n#ello again\n"
             val app = AppWith.idx (app, 21)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"0")

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
          val app = TestUtils.init "hello wor9\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"$")
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
             val app = TestUtils.init "hello\n world\n"
             val app = AppWith.idx (app, 11)
             val oldIdx = #cursorIdx app

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"$")
             val newIdx = #cursorIdx app

             val nchr = getChr app
             val nchr = Char.toString nchr ^ "\n"
           in
             (* assert *)
             Expect.isTrue (oldIdx = newIdx)
           end)
    , test "does not move cursor when cursor is on a newline" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello\n\nworld\n"
          val app = AppWith.idx (app, 6)
          val oldIdx = #cursorIdx app

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"$")
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
             val app = TestUtils.init "   3ello world\n"

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"^")
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
             val app = TestUtils.init "   3ell7 world\n"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"^")
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
             val app = TestUtils.init "hello\n   world\n"
             val app = AppWith.idx (app, 7)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"^")
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
             val app = TestUtils.init "hello\n   world\n"
             val app = AppWith.idx (app, 11)

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"^")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"w")
           end)
    , test "leaves cursor in same position when on '\\n'" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hel\nlo\n"
          val app = AppWith.idx (app, 3)
          val oldIdx = #cursorIdx app

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"^")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = oldIdx)
        end)
    ]

  val GMove = describe "move motion 'G'"
    [ test
        "moves cursor to second last char in buffer, \
        \if last char is a newline preced by a non-newline"
        (fn _ =>
           (* Note: We assume unix-style line endings:
            * End of file always has \n at the end. *)
           let
             (* arrange *)
             val str = "01234\n56789\n"
             val app = TestUtils.init str

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"G")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = String.size str - 2)
           end)
    , test
        "moves cursor to last char in buffer, \
        \if last char is a newline and second-last char is also a newline"
        (fn _ =>
           let
             (* arrange *)
             val str = "01234\n5678\n\n"
             val app = TestUtils.init str

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"G")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = String.size str - 1)
           end)
    , test
        "moves cursor to last char in buffer, \
        \if last char is not a newline and second-last char \
        \is also not a newline"
        (fn _ =>
           let
             (* arrange *)
             val str = "01234\n5678\n\n"
             val app = TestUtils.init str

             (* act *)
             val app = TestUtils.update (app, CHAR_EVENT #"G")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = String.size str - 1)
           end)
    ]

  val percentMove = describe "move motion '%'"
    [ test "moves to next ) when cursor is on (" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(hello)\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #")")
        end)
    , test "moves to preceding ( when cursur is on )" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(hello)\n"
          val app = AppWith.idx (app, 6)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"(")
        end)
    (* testing that cursor goes to correct level of nesting *)
    , test "moves to outermost ) when cursor is on outermost (" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(((hello)))\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 10)
        end)
    , test "moves to outermost ( when cursor is on outermost )" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(((hello)))\n"
          val app = AppWith.idx (app, 10)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 0)
        end)
    , test "moves to middle ) when cursor is on middle (" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(((hello)))\n"
          val app = AppWith.idx (app, 1)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 9)
        end)
    , test "moves to middle ( when cursor is on middle )" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(((hello)))\n"
          val app = AppWith.idx (app, 9)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 1)
        end)
    , test "moves to innermost ) when cursor is on innermost (" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(((hello)))\n"
          val app = AppWith.idx (app, 2)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 8)
        end)
    , test "moves to innermost ( when cursor is on innermost )" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "(((hello)))\n"
          val app = AppWith.idx (app, 8)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
          val newIdx = #cursorIdx app
        in
          (* assert *)
          Expect.isTrue (newIdx = 2)
        end)
    (* testing different pair combinations *)
    , test "moves to next ] when cursor is on [" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "[hello]\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"]")
        end)
    , test "moves to preceding [ when cursur is on ]" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "[hello]\n"
          val app = AppWith.idx (app, 6)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"[")
        end)
    , test "moves to next } when cursor is on {" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "{hello}\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"}")
        end)
    , test "moves to preceding { when cursur is on }" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "{hello}\n"
          val app = AppWith.idx (app, 6)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"{")
        end)
    , test "moves to next > when cursor is on <" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "<hello>\n"

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #">")
        end)
    , test "moves to preceding < when cursur is on >" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "<hello>\n"
          val app = AppWith.idx (app, 6)

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"<")
        end)
    (* testing that % on a non-pair character is a no-op *)
    , test "does not move when cursor is on a non-pair-character" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello, world\n"
          val app = AppWith.idx (app, 5)
          val oldIdx = #cursorIdx app

          (* act *)
          val app = TestUtils.update (app, CHAR_EVENT #"%")
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
            val app = TestUtils.update (app, CHAR_EVENT chr)
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
             val app = TestUtils.init "hello world"

             (* act *)
             val app = updateMany (app, "td")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"l")
           end)
    , test "repeating 't' motion with same char does not move cursor" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"

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
             val app = TestUtils.init "hello world"

             (* act *)
             val app1 = updateMany (app, "t;")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app1 = #cursorIdx app)
           end)
    , test "is cancellable by pressing escape" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"t")
          val app2 = TestUtils.update (app1, KEY_ESC)
          (* should not move cursor like other 't' tests do *)
          val app3 = TestUtils.update (app2, CHAR_EVENT #"d")
        in
          (* assert *)
          Expect.isTrue
            (#cursorIdx app1 = #cursorIdx app2
             andalso #cursorIdx app2 = #cursorIdx app3)
        end)
    ]

  val TMove = describe "move motion 'T'"
    [ test
        "motion 'Th' moves cursor to char after 'h' in string \"hello world\""
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 10)

             (* act *)
             val app = updateMany (app, "Th")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"e")
           end)
    , test "repeating 'T' motion with same char does not move cursor" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val app = AppWith.idx (app, 10)

          (* act *)
          val app1 = updateMany (app, "Te")
          val app2 = updateMany (app1, "Te")
        in
          (* assert *)
          Expect.isTrue
            (#cursorIdx app1 = #cursorIdx app2 andalso getChr app1 = #"l")
        end)
    , test
        "does not move cursor at all when char following 'T' is not in string"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 10)

             (* act *)
             val app1 = updateMany (app, "T;")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app1 = #cursorIdx app)
           end)
    , test "is cancellable by pressing escape" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val app = AppWith.idx (app, 10)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"T")
          val app2 = TestUtils.update (app1, KEY_ESC)
          (* should ordinarily move cursor to 'w' but in this case should not
           * as escape key should cancel motion which was in progress *)
          val app3 = TestUtils.update (app2, CHAR_EVENT #" ")
        in
          (* assert *)
          Expect.isTrue
            (#cursorIdx app1 = #cursorIdx app2
             andalso #cursorIdx app2 = #cursorIdx app3)
        end)
    ]

  val fMove = describe "move motion 'f'"
    [ test "motion 'fw' moves cursor to first 'w' in string \"hello world\""
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"

             (* act *)
             val app = updateMany (app, "fw")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"w")
           end)
    , test "count followed by f<char> moves forwards to count'th match" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"

          (* act *)
          val app = updateMany (app, "3fl")
        in
          (* assert *)
          Expect.isTrue (#cursorIdx app = 9 andalso getChr app = #"l")
        end)
    , test
        "'count f<char>' goes to last match when count is greater than number of chars"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"

             (* act *)
             val app = updateMany (app, "9fl")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = 9 andalso getChr app = #"l")
           end)
    , test
        "does not move cursor at all when char following 'f' is not in string"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"

             (* act *)
             val app1 = updateMany (app, "f;")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app1 = #cursorIdx app)
           end)
    , test "is cancellable by pressing escape" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"f")
          val app2 = TestUtils.update (app1, KEY_ESC)
          val app3 = TestUtils.update (app2, CHAR_EVENT #"d")
        in
          (* assert *)
          Expect.isTrue
            (#cursorIdx app1 = #cursorIdx app2
             andalso #cursorIdx app2 = #cursorIdx app3)
        end)
    ]

  val FMove = describe "move motion 'F'"
    [ test "motion 'Fe' moves cursor to first 'e' before cursor" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val app = AppWith.idx (app, 10)

          (* act *)
          val app = updateMany (app, "Fe")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"e")
        end)
    , test "count followed by F<char> moves backwards to count'th match"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 10)

             (* act *)
             val app = updateMany (app, "3Fl")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = 2 andalso getChr app = #"l")
           end)
    , test
        "'count F<char>' goes to first match when count is greater than number of chars"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 10)

             (* act *)
             val app = updateMany (app, "9Fl")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app = 2 andalso getChr app = #"l")
           end)
    , test
        "does not move cursor at all when char following 'F' is not in string"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 10)

             (* act *)
             val app1 = updateMany (app, "F;")
           in
             (* assert *)
             Expect.isTrue (#cursorIdx app1 = #cursorIdx app)
           end)
    , test "is cancellable by pressing escape" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val app = AppWith.idx (app, 10)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"F")
          val app2 = TestUtils.update (app1, KEY_ESC)
          val app3 = TestUtils.update (app2, CHAR_EVENT #"r")
        in
          (* assert *)
          Expect.isTrue
            (#cursorIdx app1 = #cursorIdx app2
             andalso #cursorIdx app2 = #cursorIdx app3)
        end)
    ]

  val ggMove = describe "move motion 'gg'"
    [ test "moves cursor to start when cursor is at end" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val app = AppWith.idx (app, 10)

          (* act *)
          val app = updateMany (app, "gg")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"h")
        end)
    , test "moves cursor to start when cursor is in middle" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val app = AppWith.idx (app, 5)

          (* act *)
          val app = updateMany (app, "gg")
        in
          (* assert *)
          Expect.isTrue (getChr app = #"h")
        end)
    , test "leaves cursor in same place when cursor is already at start"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"

             (* act *)
             val app = updateMany (app, "gg")
           in
             (* assert *)
             Expect.isTrue (getChr app = #"h")
           end)
    , test "is cancellable by pressing escape" (fn _ =>
        let
          (* arrange *)
          val app = TestUtils.init "hello world"
          val app = AppWith.idx (app, 5)

          (* act *)
          val app1 = TestUtils.update (app, CHAR_EVENT #"g")
          val app2 = TestUtils.update (app1, KEY_ESC)
          val app3 = TestUtils.update (app2, CHAR_EVENT #"g")
        in
          (* assert *)
          Expect.isTrue (#cursorIdx app3 = 5)
        end)
    ]

  val tests =
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
    , TMove
    , fMove
    , FMove
    , ggMove
    ]
end
