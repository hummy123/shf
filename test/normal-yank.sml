structure NormalYank =
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


  val tests = [yhYank]
end
