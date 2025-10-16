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
    , test
        "deletes last char when on last word \
        \and there is no newline after current word"
        (fn _ =>
           let
             (* arrange *)
             val app = TestUtils.init "hello world"
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dw")

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

  val deDelete = describe "delete motion 'de'"
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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "de")

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
          val newApp1 = TestUtils.updateMany (app1, "de")
          val newApp2 = TestUtils.updateMany (app2, "de")
          val newApp3 = TestUtils.updateMany (app3, "de")
          val newApp4 = TestUtils.updateMany (app4, "de")
          val newApp5 = TestUtils.updateMany (app5, "de")

          (* assert *)
          val expectedString1 = "hello  again\n"
          val expectedString2 = "hello w again\n"
          val expectedString3 = "hello wo again\n"
          val expectedString4 = "hello wor again\n"
          val expectedString5 = "hello worl\n"

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
          val expectedCursor5 = 9

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
        "deletes from cursor until newline when on last word \
        \before a newline and another word follows that newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "de")

             (* assert *)
             val expectedString = "h\nworld\nagain\n"
             (* moves cursor back by one when next char is newline *)
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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "de")

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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "de")

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
        "deletes end of next world \
        \when cursor is on space and next char is alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "h ello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "de")

             (* assert *)
             val expectedString = "h world\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 1

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes next world \
        \when cursor is on space, many spaces are ahead, \
        \and first char after spaces is alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "h             ello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 3)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "de")

             (* assert *)
             val expectedString = "h   world\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 3

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes next word \
        \when cursor is on space and next non-space char is punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!     @#$% world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 2)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "de")

             (* assert *)
             val expectedString = "!  world\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 2

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val dEdelete = describe "delete motion 'dE'"
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
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dE")

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
          (* "world" is spelled with an ! instead of an l
           * to make the second word fit the definition of a WORD *)
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
          val newApp1 = TestUtils.updateMany (app1, "dE")
          val newApp2 = TestUtils.updateMany (app2, "dE")
          val newApp3 = TestUtils.updateMany (app3, "dE")
          val newApp4 = TestUtils.updateMany (app4, "dE")
          val newApp5 = TestUtils.updateMany (app5, "dE")

          (* assert *)
          val expectedString1 = "hello  again\n"
          val expectedString2 = "hello w again\n"
          val expectedString3 = "hello wo again\n"
          val expectedString4 = "hello wor again\n"
          val expectedString5 = "hello wor!\n"

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
          val expectedCursor5 = 9

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
        "deletes from cursor until newline when on last word \
        \before a newline and another word follows that newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dE")

             (* assert *)
             val expectedString = "h\nworld\nagain\n"
             (* moves cursor back by one when next char is newline *)
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes beyond first punctuation char when on an alpha char \
        \and there is no space between alpha and punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world again\n"
             val app = TestUtils.init originalString

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dE")

             (* assert *)
             val expectedString = " again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes beyond first alpha char when on punctuation \
        \and there is no space between punctuation and alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!#%&QWERTY#!\n"
             val app = TestUtils.init originalString

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dE")

             (* assert *)
             val expectedString = "\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes end of next word \
        \when cursor is on space and next char is alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "h e!!o world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dE")

             (* assert *)
             val expectedString = "h world\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 1

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes next world \
        \when cursor is on space, many spaces are ahead, \
        \and first char after spaces is alpha"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "h             e!!o world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 3)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dE")

             (* assert *)
             val expectedString = "h   world\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 3

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes next word \
        \when cursor is on space and next non-space char is punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!     @#$% world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 2)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dE")

             (* assert *)
             val expectedString = "!  world\n"
             val actualString = LineGap.toString buffer
             val expectedCursor = 2

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val dbDelete = describe "delete motion 'db'"
    [ test
        "deletes all characters in last word except last character \
        \when cursor is on last character of last word in buffer"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val originalIdx = String.size originalString - 2

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = "hello d\n"
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
          val app6 = AppWith.idx (app, 11)

          (* act *)
          val newApp1 = TestUtils.updateMany (app1, "db")
          val newApp2 = TestUtils.updateMany (app2, "db")
          val newApp3 = TestUtils.updateMany (app3, "db")
          val newApp4 = TestUtils.updateMany (app4, "db")
          val newApp5 = TestUtils.updateMany (app5, "db")
          val newApp6 = TestUtils.updateMany (app6, "db")

          (* assert *)
          val expectedString1 = "world again\n"
          val expectedString2 = "hello orld again\n"
          val expectedString3 = "hello rld again\n"
          val expectedString4 = "hello ld again\n"
          val expectedString5 = "hello d again\n"
          val expectedString6 = "hello  again\n"

          val actualString1 = LineGap.toString (#buffer newApp1)
          val actualString2 = LineGap.toString (#buffer newApp2)
          val actualString3 = LineGap.toString (#buffer newApp3)
          val actualString4 = LineGap.toString (#buffer newApp4)
          val actualString5 = LineGap.toString (#buffer newApp5)
          val actualString6 = LineGap.toString (#buffer newApp6)

          val stringsAreExpected =
            expectedString1 = actualString1
            andalso expectedString2 = actualString2
            andalso expectedString3 = actualString3
            andalso expectedString4 = actualString4
            andalso expectedString5 = actualString5
            andalso expectedString6 = actualString6

          val expectedCursor1 = 0
          val expectedCursor2 = 6
          val expectedCursor3 = 6
          val expectedCursor4 = 6
          val expectedCursor5 = 6
          val expectedCursor6 = 6

          val actualCursor1 = #cursorIdx newApp1
          val actualCursor2 = #cursorIdx newApp2
          val actualCursor3 = #cursorIdx newApp3
          val actualCursor4 = #cursorIdx newApp4
          val actualCursor5 = #cursorIdx newApp5
          val actualCursor6 = #cursorIdx newApp6

          val cursorsAreExpected =
            expectedCursor1 = actualCursor1
            andalso expectedCursor2 = actualCursor2
            andalso expectedCursor3 = actualCursor3
            andalso expectedCursor4 = actualCursor4
            andalso expectedCursor5 = actualCursor5
            andalso expectedCursor6 = actualCursor6
        in
          Expect.isTrue (stringsAreExpected andalso cursorsAreExpected)
        end)
    , test
        "deletes newline and preceding word when cursor is \
        \on first character of word that has a newline before it"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = "world\nagain\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes first punctuation char when on an alpha char \
        \which is immediately preceded by a punctuation char"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = "helloworld!again\n"
             val expectedCursor = 5

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes chars until reaching punctuation when \
        \cursor is on alpha char, preceded by more alpha chars, \
        \until preceded by punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = "hello!orld!again\n"
             val expectedCursor = 6

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes alpha chars when on punctuation which is immediately preceded \
        \by more alpha chars, until preceded by punctuation again"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 11)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = "hello!!again\n"
             val expectedCursor = 6

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test "deletes spaces and word before spaces, when cursor is on space"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello           again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 13)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = "   again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test "does not delete when cursor is on first character of first line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 0)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = originalString
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val dBDelete = describe "delete motion 'dB'"
    [ test
        "deletes all characters in last WORD except last character \
        \when cursor is on last character of last WORD in buffer"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val originalIdx = String.size originalString - 2

             val app = TestUtils.init originalString
             val app = AppWith.idx (app, originalIdx)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dB")

             (* assert *)
             val expectedString = "hello d\n"
             val actualString = LineGap.toString buffer

             val expectedIdx = String.size expectedString - 2
           in
             Expect.isTrue
               (expectedString = actualString andalso expectedIdx = cursorIdx)
           end)
    , test "deletes second WORD as expected when there are three WORDs" (fn _ =>
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
          val app6 = AppWith.idx (app, 11)

          (* act *)
          val newApp1 = TestUtils.updateMany (app1, "dB")
          val newApp2 = TestUtils.updateMany (app2, "dB")
          val newApp3 = TestUtils.updateMany (app3, "dB")
          val newApp4 = TestUtils.updateMany (app4, "dB")
          val newApp5 = TestUtils.updateMany (app5, "dB")
          val newApp6 = TestUtils.updateMany (app6, "dB")

          (* assert *)
          val expectedString1 = "wor!d again\n"
          val expectedString2 = "hello or!d again\n"
          val expectedString3 = "hello r!d again\n"
          val expectedString4 = "hello !d again\n"
          val expectedString5 = "hello d again\n"
          val expectedString6 = "hello  again\n"

          val actualString1 = LineGap.toString (#buffer newApp1)
          val actualString2 = LineGap.toString (#buffer newApp2)
          val actualString3 = LineGap.toString (#buffer newApp3)
          val actualString4 = LineGap.toString (#buffer newApp4)
          val actualString5 = LineGap.toString (#buffer newApp5)
          val actualString6 = LineGap.toString (#buffer newApp6)

          val stringsAreExpected =
            expectedString1 = actualString1
            andalso expectedString2 = actualString2
            andalso expectedString3 = actualString3
            andalso expectedString4 = actualString4
            andalso expectedString5 = actualString5
            andalso expectedString6 = actualString6

          val expectedCursor1 = 0
          val expectedCursor2 = 6
          val expectedCursor3 = 6
          val expectedCursor4 = 6
          val expectedCursor5 = 6
          val expectedCursor6 = 6

          val actualCursor1 = #cursorIdx newApp1
          val actualCursor2 = #cursorIdx newApp2
          val actualCursor3 = #cursorIdx newApp3
          val actualCursor4 = #cursorIdx newApp4
          val actualCursor5 = #cursorIdx newApp5
          val actualCursor6 = #cursorIdx newApp6

          val cursorsAreExpected =
            expectedCursor1 = actualCursor1
            andalso expectedCursor2 = actualCursor2
            andalso expectedCursor3 = actualCursor3
            andalso expectedCursor4 = actualCursor4
            andalso expectedCursor5 = actualCursor5
            andalso expectedCursor6 = actualCursor6
        in
          Expect.isTrue (stringsAreExpected andalso cursorsAreExpected)
        end)
    , test
        "deletes newline and preceding word when cursor is \
        \on first character of word that has a newline before it"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "he!!o\nwor!d\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dB")

             (* assert *)
             val expectedString = "wor!d\nagain\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes past punctuation char when on an alpha char \
        \which is immediately preceded by a punctuation char"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dB")

             (* assert *)
             val expectedString = "world!again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes chars past punctuation when \
        \cursor is on alpha char, preceded by more alpha chars, \
        \and then preceded by punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "he!!o!wor!d!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dB")

             (* assert *)
             val expectedString = "or!d!again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes alpha and punctuation characters \
        \when string is not separated by spaces"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 11)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dB")

             (* assert *)
             val expectedString = "!again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test "deletes spaces and word before spaces, when cursor is on space"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "he!!o           again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 13)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dB")

             (* assert *)
             val expectedString = "   again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test "does not delete when cursor is on first character of first line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 0)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "db")

             (* assert *)
             val expectedString = originalString
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val dgeDelete = describe "delete motion 'dge'"
    [ test "does not delete when cursor is at start of file" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 0)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dge")

          (* assert *)
          val actualString = LineGap.toString buffer
          val expectedString = originalString
          val expectedCursorIdx = 0
        in
          Expect.isTrue
            (actualString = expectedString andalso cursorIdx = expectedCursorIdx)
        end)
    , test "deletes as expected when on second word and there are three words"
        (fn _ =>
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
             val newApp1 = TestUtils.updateMany (app1, "dge")
             val newApp2 = TestUtils.updateMany (app2, "dge")
             val newApp3 = TestUtils.updateMany (app3, "dge")
             val newApp4 = TestUtils.updateMany (app4, "dge")
             val newApp5 = TestUtils.updateMany (app5, "dge")

             (* assert *)
             val expectedString1 = "hellorld again\n"
             val expectedString2 = "hellrld again\n"
             val expectedString3 = "hellld again\n"
             val expectedString4 = "helld again\n"
             val expectedString5 = "hell again\n"

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

             val expectedCursor1 = 4
             val expectedCursor2 = 4
             val expectedCursor3 = 4
             val expectedCursor4 = 4
             val expectedCursor5 = 4

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
        "deletes all characters in word \
        \when on last character of last word in buffer"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, String.size originalString - 2)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dge")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hell\n"
             val expectedCursorIdx = String.size expectedString - 2
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test "deletes past newline when on first word after newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\nagain\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 6)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dge")

          (* assert *)
          val expectedString = "hellorld\nagain\n"
          val expectedCursor = 4

          val actualString = LineGap.toString buffer

          val stringIsExpected = expectedString = actualString
          val cursorIsExpected = expectedCursor = cursorIdx
        in
          Expect.isTrue (stringIsExpected andalso cursorIsExpected)
        end)
    , test
        "deletes only two chars when cursor is \
        \on an a punctuation char which is immediately preceded \
        \by an alpha char"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 5)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dge")

             (* assert *)
             val expectedString = "hellworld!again\n"
             val expectedCursor = 4

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes only one punctuation char when on an alpha char \
        \which is preceded by punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!#%&(hello\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dge")

             (* assert *)
             val expectedString = "!#%&lo\n"
             val expectedCursor = 4

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes spaces until reaching non-space char, \
        \when cursor is on space and is preceded by more spaces"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello           again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 13)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dge")

             (* assert *)
             val expectedString = "hell  again\n"
             val expectedCursor = 4

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val dgEDelete = describe "delete motion 'dgE'"
    [ test "does not delete when cursor is at start of file" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 0)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgE")

          (* assert *)
          val actualString = LineGap.toString buffer
          val expectedString = originalString
          val expectedCursorIdx = 0
        in
          Expect.isTrue
            (actualString = expectedString andalso cursorIdx = expectedCursorIdx)
        end)
    , test "deletes as expected when on second word and there are three words"
        (fn _ =>
           let
             (* arrange *)
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
             val newApp1 = TestUtils.updateMany (app1, "dgE")
             val newApp2 = TestUtils.updateMany (app2, "dgE")
             val newApp3 = TestUtils.updateMany (app3, "dgE")
             val newApp4 = TestUtils.updateMany (app4, "dgE")
             val newApp5 = TestUtils.updateMany (app5, "dgE")

             (* assert *)
             val expectedString1 = "hellor!d again\n"
             val expectedString2 = "hellr!d again\n"
             val expectedString3 = "hell!d again\n"
             val expectedString4 = "helld again\n"
             val expectedString5 = "hell again\n"

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

             val expectedCursor1 = 4
             val expectedCursor2 = 4
             val expectedCursor3 = 4
             val expectedCursor4 = 4
             val expectedCursor5 = 4

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
        "deletes all characters in word \
        \when on last character of last word in buffer"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello wor!d\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, String.size originalString - 2)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgE")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hell\n"
             val expectedCursorIdx = String.size expectedString - 2
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test "deletes past newline when on first word after newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\n#orld\nagain\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 6)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgE")

          (* assert *)
          val expectedString = "hellorld\nagain\n"
          val expectedCursor = 4

          val actualString = LineGap.toString buffer

          val stringIsExpected = expectedString = actualString
          val cursorIsExpected = expectedCursor = cursorIdx
        in
          Expect.isTrue (stringIsExpected andalso cursorIsExpected)
        end)
    , test
        "deletes past punctuation \
        \when cursor is on alpha char preceded by punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello!world!again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 5)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgE")

             (* assert *)
             val expectedString = "world!again\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes all punctuation char until beginning of string \
        \when on alpha char in first WORD, preceded by punctuation"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "!#%&(hello\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgE")

             (* assert *)
             val expectedString = "lo\n"
             val expectedCursor = 0

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes spaces until reaching non-space char, \
        \when cursor is on space and is preceded by more spaces"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello           again\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 13)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgE")

             (* assert *)
             val expectedString = "hell  again\n"
             val expectedCursor = 4

             val actualString = LineGap.toString buffer

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursor = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    ]

  val dGDelete = describe "delete motion 'dG'"
    [ test
        "deletes whole buffer, leaving only a newline, \
        \when cursor is on first line of buffer"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 3)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dG")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from second line to end of buffer \
        \when first and second line only contain one newline each"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "\n\nhello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dG")

             (* assert *)
             val expectedString = "\n"
             val actualString = LineGap.toString buffer

             val expectedCursurIdx = 0

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursurIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test "deletes from second line onwards when cursor is on second line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dG")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    ]

  val dggDelete = describe "delete motion 'dgg'"
    [ test "leaves newline behind when deleting from last line" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 7)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgg")

          (* assert *)
          val actualString = LineGap.toString buffer
          val expectedString = "\n"
          val expectedCursorIdx = 0
        in
          Expect.isTrue
            (actualString = expectedString andalso cursorIdx = expectedCursorIdx)
        end)
    , test
        "deletes whole line that cursor is currently on, \
        \and all lines preceding cursor"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgg")

             (* assert *)
             val expectedString = "again\n"
             val actualString = LineGap.toString buffer

             val expectedCursurIdx = 0

             val stringIsExpected = expectedString = actualString
             val cursorIsExpected = expectedCursurIdx = cursorIdx
           in
             Expect.isTrue (stringIsExpected andalso cursorIsExpected)
           end)
    , test
        "deletes current line and preceding lines \
        \when current line does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgg")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes current line and preceding line \
        \when cursor is on a newline preceded by a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "\n\nhello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgg")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello world\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes current line and preceding line \
        \when cursor is on a newline, followed by another newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "\n\n\nhello world\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 1)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "dgg")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "\nhello world\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    ]

  val d0Delete = describe "delete motion 'd0'"
    [ test "does not delete when on first word in buffer" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\nworld\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 0)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d0")

          (* assert *)
          val actualString = LineGap.toString buffer
          val expectedString = originalString
          val expectedCursorIdx = 0
        in
          Expect.isTrue
            (actualString = expectedString andalso cursorIdx = expectedCursorIdx)
        end)
    , test "does not delete when cursor is on a newline" (fn _ =>
        let
          (* arrange *)
          val originalString = "hello\n\nworld\n"
          val app = TestUtils.init originalString
          val app = AppWith.idx (app, 6)

          (* act *)
          val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d0")

          (* assert *)
          val expectedString = originalString
          val actualString = LineGap.toString buffer

          val expectedCursurIdx = 6

          val stringIsExpected = expectedString = actualString
          val cursorIsExpected = expectedCursurIdx = cursorIdx
        in
          Expect.isTrue (stringIsExpected andalso cursorIsExpected)
        end)
    , test
        "deletes from cursor to first character in buffer \
        \when cursor is on first line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 3)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d0")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "lo\nworld"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from cursor to up to (not including) \
        \closest newline, when cursor is on second line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d0")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\norld\n"
             val expectedCursorIdx = 6
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    ]

  val dDlrDelete = describe "delete motion 'd$'"
    [ test
        "deletes only the last character on the line \
        \and moves cursor back by one index \
        \when cursor is on last character"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 4)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d$")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hell\nworld\n"
             val expectedCursorIdx = 3
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from first character in buffer \
        \up to (and excluding) the first newline \
        \when the cursor is on the first character in the buffer"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 0)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d$")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "\nworld\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from first character in line \
        \up to (and excluding) the next newline \
        \when the cursor is on the first character of second newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\nagain\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d$")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\n\nagain\n"
             val expectedCursorIdx = 6
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from middle character on line to last character in line \
        \when cursor is on middle character"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 2)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d$")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "he\nworld\n"
             val expectedCursorIdx = 1
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "does not delete when cursor is on a line \
        \which contains only a single newline and nothing else"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\n\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 6)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d$")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = originalString
             val expectedCursorIdx = 6
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from middle character on last line \
        \when the last line ends with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d$")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\nw\n"
             val expectedCursorIdx = 6
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from middle character on last line \
        \when the last line does not end with a newline"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d$")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\nw"
             val expectedCursorIdx = 6
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    ]

  val dCaretDelete = describe "delete motion 'd^'"
    [ test
        "does not delete when cursor is on first character of first line \
        \and first character is not a space"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 0)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d^")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = originalString
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes preceding characters when cursor is on \
        \third character of first line, and line starts with an alpha character"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 2)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d^")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "llo\nworld\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes preceding characters on line when \
        \cursor is on third character of second line \
        \and line starts with alpha character"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "hello\nworld\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 8)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d^")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\nrld\n"
             val expectedCursorIdx = 6
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from start of line until first non-space char \
        \when cursor is on first character of line \
        \and line starts with spaces"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "   hello\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 0)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d^")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "hello\n"
             val expectedCursorIdx = 0
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    , test
        "deletes from cursor position to first non-space char \
        \when cursor is on last non-space char at end of line"
        (fn _ =>
           let
             (* arrange *)
             val originalString = "   hello\n"
             val app = TestUtils.init originalString
             val app = AppWith.idx (app, 7)

             (* act *)
             val {buffer, cursorIdx, ...} = TestUtils.updateMany (app, "d^")

             (* assert *)
             val actualString = LineGap.toString buffer
             val expectedString = "   o\n"
             val expectedCursorIdx = 3
           in
             Expect.isTrue
               (actualString = expectedString
                andalso cursorIdx = expectedCursorIdx)
           end)
    ]

  val tests =
    [ dhDelete
    , dlDelete
    , djDelete
    , ddDelete
    , dkDelete
    , dwDelete
    , dWDelete
    , deDelete
    , dEdelete
    , dbDelete
    , dBDelete
    , dgeDelete
    , dgEDelete
    , dGDelete
    , dggDelete
    , d0Delete
    , dDlrDelete
    , dCaretDelete
    ]
end
