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
             Expect.isTrue (stringIsExpected)
           end)
    ]

  val tests = [dhDelete, dlDelete]
end
