structure NormalYankTests =
struct
  open Railroad
  open Railroad.Test
  open InputMsg

  val yhYank = describe "yank motion 'yh'"
    [ test "yanks empty string when cursor is at index 0" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello world\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 0)

          (* act *)
          val app = TestUtils.updateMany (app, "yh")

          (* assert *)
          val expectedString = ""
        in
          TestUtils.expectYank (app, expectedString)
        end)
    , test "yanks empty string when character before cursor is a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 6)

             (* act *)
             val app = TestUtils.updateMany (app, "yh")

             (* assert *)
             val expectedString = ""
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test "yanks one char to the left when on a non-newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello world\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 5)

          (* act *)
          val app = TestUtils.updateMany (app, "yh")

          (* assert *)
          val expectedString = "o"
        in
          TestUtils.expectYank (app, expectedString)
        end)
    , test "yanks 3 chars when count is 3" (fn _ =>
        let
          (* arrange *)
          val originalIdx = 5
          val originalString = "hello world\n"

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "3yh")

          (* assert *)
          val expectedString = "llo"
        in
          TestUtils.expectYank (app, expectedString)
        end)
    , test
        "yanks from cursor position to start column when \
        \count is greater than current column"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 5
             val originalString = "hello world\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "9yh")

             (* assert *)
             val expectedString = "hello"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    ]

  val ylYank = describe "yank motion 'yl'"
    [ test "yanks last char in line when next char is newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val originalIdx = 4

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yl")

          (* assert *)
          val expectedString = "o"
        in
          TestUtils.expectYank (app, expectedString)
        end)
    , test "yanks char that cursor is currently on when not on newline" (fn _ =>
        let
          (* arrange *)
          val originalIdx = 0
          val originalString = "hello world\n"

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yl")

          (* assert *)
          val expectedString = "h"
        in
          TestUtils.expectYank (app, expectedString)
        end)
    , test "yanks newline character when cursor is on a newline" (fn _ =>
        let
          (* arrange *)
          val originalIdx = 5
          val originalString = "hello\n\nworld\n"

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yl")

          (* assert *)
          val expectedString = "\n"
        in
          TestUtils.expectYank (app, expectedString)
        end)
    , test
        "does not yank past newline when specifying a range \
        \greater than number of columns"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 2
             val originalString = "hello\nworld\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yl")

             (* assert *)
             val expectedString = "llo"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks last line, excluding newline, \
        \when cursor is on first character of last line \
        \and last line ends with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 6
             val originalString = "hello\nworld\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yl")

             (* assert *)
             val expectedString = "world"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks last line, excluding newline, \
        \when cursor is on first character of last line \
        \and last line does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 6
             val originalString = "hello\nworld"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yl")

             (* assert *)
             val expectedString = "world"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    ]

  val ykYank = describe "yank motion 'yk'"
    [ test "does not yank when cursor is on first line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val originalIdx = 0

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yk")
        in
          (* assert *)
          TestUtils.expectNoYank app
        end)
    , test
        "yanks first two lines \
        \when there are two lines and cursor is on second line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 6

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yk")

             (* assert *)
             val expectedString = "hello\nworld\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks last two lines when there are three lines in the buffer \
        \and cursor is on third line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val originalIdx = 15

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yk")

             (* assert *)
             val expectedString = "world\nagain\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks whole buffer when on last line \
        \and count is greater than number of lines"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val originalIdx = 15

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yk")

             (* assert *)
             val expectedString = originalString
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks newline and preceding line when cursor is second line \
        \and second line contains only a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\n\nagain\n"
             val originalIdx = 6

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yk")

             (* assert *)
             val expectedString = "hello\n\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks just newline and line above when cursor is on third line \
        \and third line contains only a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString =
               "hello\n\
               \world\n\
               \\n\
               \trello\n\
               \brillo\n"
             val originalIdx = 12

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yk")

             (* assert *)
             val expectedString = "world\n\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks second and third lines when cursor is on \
        \last non-newline character of third line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\n\nagain\n"
             val originalString =
               "hello\n\
               \world\n\
               \trello\n\
               \brillo\n"
             val originalIdx = 17

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yk")

             (* assert *)
             val expectedString = "world\ntrello\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks last two lines when cursor is on last line \
        \and last line only has a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString =
               "hello\n\
               \world\n\
               \\n"
             val originalIdx = String.size originalString - 1

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yk")

             (* assert *)
             val expectedString = "world\n\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    ]

  val yjYank = describe "yank motion 'yj'"
    [ test "does not yank any text when cursor is on last line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val originalIdx = String.size originalString - 3

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yj")
        in
          (* assert *)
          TestUtils.expectNoYank app
        end)
    , test "does not yank when there is only one line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\n"
          val originalIdx = 0

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yj")
        in
          (* assert *)
          TestUtils.expectNoYank app
        end)
    , test
        "yanks first two lines when cursor is on first line \
        \and there are at least two lines"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yj")

             (* assert *)
             val expectedString = originalString
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks first two lines when there are three lines \
        \and cursor is on first line"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 0
             val originalString = "hello\nworld\nbye world\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yj")

             (* assert *)
             val expectedString = "hello\nworld\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks last two lines when there are three lines \
        \and cursor is on second line"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 6
             val originalString = "hello\nworld\nbye world\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yj")

             (* assert *)
             val expectedString = "world\nbye world\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks entire file when cursor is on first line \
        \and a count is given which is larger \
        \than the total number of lines in the file"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yj")

             (* assert *)
             val expectedString = originalString
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks entire file when a count greater than the total number of lines \
        \is given, while the file does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yj")

             (* assert *)
             val expectedString = originalString
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test "yanks two lines when cursor is on a newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "\nhello\nworld\ntrello\nbrillo\n"
          val originalIdx = 0

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yj")

          (* assert *)
          val expectedString = "\nhello\n"
        in
          TestUtils.expectYank (app, expectedString)
        end)
    ]

  val yyYank = describe "yank motion 'yy'"
    [ test
        "yanks last line when there is more than one line \
        \and cursor is on last line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = String.size originalString - 3

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yy")

             (* assert *)
             val expectedString = "world\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test "yanks whole buffer when buffer consists of one line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\n"
          val originalIdx = 0

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val app = TestUtils.updateMany (app, "yy")

          (* assert *)
          val expectedString = originalString
        in
          TestUtils.expectYank (app, expectedString)
        end)
    , test
        "yanks first line when cursor is on first line \
        \and there are only two lines"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yy")

             (* assert *)
             val expectedString = "hello\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks whole file when cursor is on first line \
        \and a count is given which is greater than \
        \the number of total lines"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yy")

             (* assert *)
             val expectedString = originalString
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks whole file when cursor is on first line, \
        \count given is greater than number of lines, \
        \and the file does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "33yy")

             (* assert *)
             val expectedString = originalString
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test
        "yanks just newline when cursor is on a line \
        \that contains only a single newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "\nhello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yy")

             (* assert *)
             val expectedString = "\n"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    ]

  val ywYank = describe "yank motion 'yw'"
    [ test "yanks last character when cursor is on last character of line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val originalIdx = String.size originalString - 2

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val app = TestUtils.updateMany (app, "yw")

             (* assert *)
             val expectedString = "d"
           in
             TestUtils.expectYank (app, expectedString)
           end)
    , test "deletes second word as expected when there are three words" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello world again\n"

          val app = TestUtils.init originalString

          (* all the different positions the cursor can be
           * on the second word *)
          val app1 = AppWith.idx (app, 6)
          val app2 = AppWith.idx (app, 7)
          val app3 = AppWith.idx (app, 8)
          val app4 = AppWith.idx (app, 9)
          val app5 = AppWith.idx (app, 10)

          (* act *)
          val newApp1 = TestUtils.updateMany (app1, "yw")
          val newApp2 = TestUtils.updateMany (app2, "yw")
          val newApp3 = TestUtils.updateMany (app3, "yw")
          val newApp4 = TestUtils.updateMany (app4, "yw")
          val newApp5 = TestUtils.updateMany (app5, "yw")

          (* assert *)
          val expectedString1 = "hello again\n"
          val expectedString2 = "hello wagain\n"
          val expectedString3 = "hello woagain\n"
          val expectedString4 = "hello woragain\n"
          val expectedString5 = "hello worlagain\n"

          val actualString1 = LineGap.toString (#buffer newApp1)
          val actualString2 = LineGap.toString (#buffer newApp2)
          val actualString3 = LineGap.toString (#buffer newApp3)
          val actualString4 = LineGap.toString (#buffer newApp4)
          val actualString5 = LineGap.toString (#buffer newApp5)

          val stringsAreExpected =
            expectedString1 = actualString1
            andalso expectedString2 = actualString2
            andalso expectedString3 = actualString3
            andalso expectedString4 = actualString4
            andalso expectedString5 = actualString5

          val expectedCursor1 = 6
          val expectedCursor2 = 7
          val expectedCursor3 = 8
          val expectedCursor4 = 9
          val expectedCursor5 = 10

          val actualCursor1 = #cursorIdx newApp1
          val actualCursor2 = #cursorIdx newApp2
          val actualCursor3 = #cursorIdx newApp3
          val actualCursor4 = #cursorIdx newApp4
          val actualCursor5 = #cursorIdx newApp5

          val cursorsAreExpected =
            expectedCursor1 = actualCursor1
            andalso expectedCursor2 = actualCursor2
            andalso expectedCursor3 = actualCursor3
            andalso expectedCursor4 = actualCursor4
            andalso expectedCursor5 = actualCursor5
        in
          Expect.isTrue (stringsAreExpected andalso cursorsAreExpected)
        end)
    , test "does not delete newline following word" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\nagain\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 0)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "yw")

          (* assert *)
          val expectedString = "\nworld\nagain\n"
          val expectedCursor = 0

          val actualString = LineGap.toString buffer

          val stringIsExpected = expectedString = actualString
          val cursorIsExpected = expectedCursor = cursorIdx
        in
          Expect.isTrue (stringIsExpected andalso cursorIsExpected)
        end)
    , test
        "deletes until first punctuation char when on an alpha char \
        \and there is no space between alpha and punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "yw")

             (* assert *)
             val expectedString = "!world!again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes until first alpha char when on punctuation \
        \and there is no space between punctuation and alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!#%&QWERTY#!\n"
             val app = TestUtils.init originalString

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "yw")

             (* assert *)
             val expectedString = "QWERTY#!\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes until first alpha char \
        \when cursor is on space and next char is alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "h ello\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "yw")

             (* assert *)
             val expectedString = "hello\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 1

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes until first alpha char \
        \when cursor is on space, many spaces are ahead, \
        \and first char after spaces is alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "h             ello\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 3)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "yw")

             (* assert *)
             val expectedString = "h  ello\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 3

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes until first punctuation char \
        \when cursor is on space and next non-space char is punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!     @#$%\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 2)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "yw")

             (* assert *)
             val expectedString = "! @#$%\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 2

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes last char when on last word \
        \and there is no newline after current word"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "yw")

             (* assert *)
             val expectedString = "hello "
             val actualString = LineGap.toString buffer

             val expectedIdx = 5

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = cursorIdx = expectedIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val tests = [yhYank, ylYank, ykYank, yjYank, yyYank, ywYank]
end
