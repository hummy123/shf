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
    [test
       "deletes last char and moves cursor back by one \
       \when used on last char of last word in buffer"
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
          end)]

  val tests = [dhDelete, dlDelete, djDelete, ddDelete, dkDelete, dwDelete]
end
