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

  val tests = [yhYank, ylYank, ykYank]
end
