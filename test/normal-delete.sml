structure NormalDelete =
struct
  open Railroad
  open Railroad.Test
  open InputMsg

  val dhDelete = describe "delete motion 'dh'"
    [ test "does not delete when cursor is at index 0" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello world\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 0)

          (* act *)
          val app = TestUtils.updateMany (app, "dh")

          (* assert *)
          val expectedString = originalString
          val actualString = LineGap.toString (#buffer app)
        in
          Expect.isTrue (expectedString = actualString)
        end)
    , test "does not delete when character before cursor is a newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 6)

          (* act *)
          val app = TestUtils.updateMany (app, "dh")

          (* assert *)
          val expectedString = originalString
          val actualString = LineGap.toString (#buffer app)
        in
          Expect.isTrue (expectedString = actualString)
        end)
    , test "deletes one char to the left when on a non-newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello world\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 5)

          (* act *)
          val app = TestUtils.updateMany (app, "dh")

          (* assert *)
          val expectedString = "hell world\n"
          val actualString = LineGap.toString (#buffer app)
        in
          Expect.isTrue (expectedString = actualString)
        end)
    , test "moves cursor left by one after deleting left char" (fn _ =>
        let
          (* arrange *)
          val originalIdx = 5
          val originalString = "hello world\n"

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val {cursorIdx, ...} = TestUtils.updateMany (app, "dh")
        in
          (* assert *)
          Expect.isTrue (cursorIdx = originalIdx - 1)
        end)
    , test "deletes 3 chars and moves cursor left by 3 when count is 3" (fn _ =>
        let
          (* arrange *)
          val originalIdx = 5
          val originalString = "hello world\n"

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "3dh")

          (* assert *)
          val expectedString = "he world\n"
          val deleted3CharsInString = expectedString = LineGap.toString buffer
          val cursorIdxIsDecrementedBy3 = cursorIdx = originalIdx - 3
        in
          Expect.isTrue
            (cursorIdxIsDecrementedBy3 andalso deleted3CharsInString)
        end)
    , test
        "deletes until start column when \
        \count is greater than current column"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 5
             val originalString = "hello world\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "9dh")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = " world\n"
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    ]

  (* 'dl' motion and 'x' motion have identical behaviour *)
  val dlDelete = describe "delete motion 'dl'"
    [ test
        "deletes last char and moves cursor back by 1 \
        \when next char is a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 4

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dl")

             (* assert *)
             val expectedString = "hell\nworld\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = originalIdx - 1
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test "deletes char that cursor is currently on when not on newline"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 0
             val originalString = "hello world\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dl")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "ello world\n"
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test "does not delete any characters or move the cursor when on a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 5
             val originalString = "hello\n\nworld\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dl")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = originalString
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = originalIdx
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "does not delete past newline when specifying a range \
        \greater than number of columns"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 2
             val originalString = "hello\nworld\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "33dl")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "he\nworld\n"
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = 1
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "moves cursor to last newline after deleting \
        \all non-newline chars on last line \
        \and the original string ends with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 6
             val originalString = "hello\nworld\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "33dl")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\n\n"
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = String.size expectedString - 1
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "moves cursor to last newline after deleting \
        \all non-newline chars on last line \
        \and the original string does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 6
             val originalString = "hello\nworld"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "33dl")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\n"
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = 4
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    ]

  val djDelete = describe "delete motion 'dj'"
    [ test "does not delete when cursor is on last line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val originalIdx = String.size originalString - 3

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dj")

          (* assert *)
          val expectedString = originalString
          val actualString = LineGap.toString buffer
          val stringIsExpected = expectedString = actualString

          val expectedCursorIdx = originalIdx
          val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
        in
          Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
        end)
    , test "does not delete when there is only one line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\n"
          val originalIdx = 0

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dj")

          (* assert *)
          val expectedString = originalString
          val actualString = LineGap.toString buffer
          val stringIsExpected = expectedString = actualString

          val expectedCursorIdx = originalIdx
          val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
        in
          Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
        end)
    , test
        "deletes when cursor is on first line and there are at least two lines"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dj")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "deletes first two lines when there are three lines \
        \and cursor is on first line"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 0
             val originalString = "hello\nworld\nbye world\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dj")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "bye world\n"
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "deletes last two lines when there are three lines \
        \and cursor is on second line"
        (fn _ =>
           let
             (* arrange *)
             val originalIdx = 6
             val originalString = "hello\nworld\nbye world\n"

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dj")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\n"
             val stringIsExpected = actualString = expectedString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = cursorIdx = expectedCursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "leaves a newline at the end when deleting the whole file \
        \and file ends with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "33dj")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "leaves a newline at the end when deleting the whole file \
        \and file does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "33dj")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test "deletes two lines when cursor is on a newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "\nhello\nworld\ntrello\nbrillo\n"
          val originalIdx = 0

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dj")

          (* assert *)
          val expectedString = "world\ntrello\nbrillo\n"
          val actualString = LineGap.toString buffer
          val stringIsExpected = expectedString = actualString

          val expectedCursorIdx = 0
          val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
        in
          Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
        end)
    ]

  val ddDelete = describe "delete motion 'dd'"
    [ test
        "deletes last line when there is more than one line \
        \and cursor is on last line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = String.size originalString - 3

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dd")

             (* assert *)
             val expectedString = "hello\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "leaves a newline in the buffer \
        \when deleting and there is only one line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dd")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "deletes when cursor is on first line and there are at least two lines"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dd")

             (* assert *)
             val expectedString = "world\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "leaves a newline at the end when deleting the whole file using a count \
        \and file ends with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "33dd")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "leaves a newline at the end when deleting the whole file \
        \and file does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "33dd")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    , test
        "deletes just newline when cursor is on a line \
        \that contains only a single newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "\nhello\nworld\n"
             val originalIdx = 0

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {cursorIdx, buffer, ...} = TestUtils.updateMany (app, "dd")

             (* assert *)
             val expectedString = "hello\nworld\n"
             val actualString = LineGap.toString buffer
             val stringIsExpected = expectedString = actualString

             val expectedCursorIdx = 0
             val cursorIdxIsExpected = expectedCursorIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIdxIsExpected)
           end)
    ]

  val dkDelete = describe "delete motion 'dk'"
    [ test "does not delete when cursor is on first line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val originalIdx = 0

          val app = TestUtils.init originalString
          val app = AppWith.idx (app, originalIdx)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

          (* assert *)
          val expectedString = originalString
          val actualString = LineGap.toString buffer

          val expectedIdx = 0
        in
          Expect.isTrue
            (expectedString = actualString andalso expectedIdx = cursorIdx)
        end)
    , test
        "deletes first two lines, leaving a newline in the buffer \
        \ when there are two lines \and cursor is on second line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val originalIdx = 6

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = 0
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test
        "deletes last two lines when there are three lines in the buffer \
        \ and cursor is on third line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val originalIdx = 15

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

             (* assert *)
             val expectedString = "hello\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = 0
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test
        "leaves a buffer containing just a newline when\
        \deleting from last line with a counter greater than total lines"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val originalIdx = 15

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "33dk")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = 0
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test
        "deletes newline and preceding line when cursor is second line \
        \and second line contains only a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\n\nagain\n"
             val originalIdx = 6

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

             (* assert *)
             val expectedString = "again\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = 0
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test
        "deletes just newline and line above when cursor is on third line \
        \and third line contains only a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\n\nagain\n"
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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

             (* assert *)
             val expectedString =
               "hello\n\
               \trello\n\
               \brillo\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = 6
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test
        "deletes second and third lines when cursor is on \
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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

             (* assert *)
             val expectedString =
               "hello\n\
               \brillo\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = 6
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test
        "deletes last two lines when cursor is on last line \
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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

             (* assert *)
             val expectedString = "hello\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = 0
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test
        "moves cursor to newline at end of file \
        \when cursor is two lines below"
        (fn _ =>
           let
             (* arrange *)
             val originalString =
               "hello\n\
               \world\n\
               \\n\
               \world\n\
               \hello\n"
             val originalIdx = String.size originalString - 2

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dk")

             (* assert *)
             val expectedString =
               "hello\n\
               \world\n\
               \\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = String.size expectedString - 1
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    ]

  val dwDelete = describe "delete motion 'dw'"
    [ test
        "deletes last char and moves cursor back by one \
        \when used on last char of last word in buffer \
        \and buffer ends with a newline preceded by a non-newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val originalIdx = String.size originalString - 2

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

             (* assert *)
             val expectedString = "hello worl\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = String.size expectedString - 2
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
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
          val newApp1 = TestUtils.updateMany (app1, "dw")
          val newApp2 = TestUtils.updateMany (app2, "dw")
          val newApp3 = TestUtils.updateMany (app3, "dw")
          val newApp4 = TestUtils.updateMany (app4, "dw")
          val newApp5 = TestUtils.updateMany (app5, "dw")

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
    , test
        "deletes newline when there is a newline after current word \
        \and there is another word following that newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

             (* assert *)
             val expectedString = "hworld\nagain\n"
             val expectedCursor = 1

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

             (* assert *)
             val expectedString = "! @#$%\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 2

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val dWDelete = describe "delete motion 'dW'"
    [ test
        "deletes last char and moves cursor back by one \
        \when used on last char of last word in buffer \
        \and buffer ends with a newline preceded by a non-newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val originalIdx = String.size originalString - 2

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dW")

             (* assert *)
             val expectedString = "hello worl\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = String.size expectedString - 2
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test "deletes second WORD as expected when there are three words" (fn _ =>
        let
          (* arrange *)
          (* 'l' in world is replaced with '!' 
           * so it fits with the definition of a WORD *)
          val originalString = "hello wor!d again\n"

          val app = TestUtils.init originalString

          (* all the different positions the cursor can be
           * on the second word *)
          val app1 = AppWith.idx (app, 6)
          val app2 = AppWith.idx (app, 7)
          val app3 = AppWith.idx (app, 8)
          val app4 = AppWith.idx (app, 9)
          val app5 = AppWith.idx (app, 10)

          (* act *)
          val newApp1 = TestUtils.updateMany (app1, "dW")
          val newApp2 = TestUtils.updateMany (app2, "dW")
          val newApp3 = TestUtils.updateMany (app3, "dW")
          val newApp4 = TestUtils.updateMany (app4, "dW")
          val newApp5 = TestUtils.updateMany (app5, "dW")

          (* assert *)
          val expectedString1 = "hello again\n"
          val expectedString2 = "hello wagain\n"
          val expectedString3 = "hello woagain\n"
          val expectedString4 = "hello woragain\n"
          val expectedString5 = "hello wor!again\n"

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
    , test
        "deletes newline when there is a newline after current word \
        \and there is another word following that newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dW")

             (* assert *)
             val expectedString = "hworld\nagain\n"
             val expectedCursor = 1

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes past first punctuation char \
        \when on an alpha char and there is no space \
        \between alpha and punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world again\n"
             val app = TestUtils.init originalString

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dW")

             (* assert *)
             val expectedString = "again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes past first alpha char when on punctuation \
        \and there is no space between punctuation and alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!#%&QWERTY#! hello\n"
             val app = TestUtils.init originalString

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dW")

             (* assert *)
             val expectedString = "hello\n"
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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dW")

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dW")

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dW")

             (* assert *)
             val expectedString = "! @#$%\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 2

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val tests =
    [dhDelete, dlDelete, djDelete, ddDelete, dkDelete, dwDelete, dWDelete]
end
