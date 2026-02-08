# To-do list
- Add normal-delete tests for each motion, checking that searchList is as expected
  - Three cases for searchList:
    1. Deletion causes two words to join to form a new match
    2. Deletion causes an existing match to be extended
    3. Deletion introduces no match
- Add tests for indent, dedent and remove-line-break motions
  - Add tests that searchList updates as expected too
- Add tests for other yank motoins
  - Tests should be based on existing tests for delete-motions, and in the same order.
- Bind gamepad functions from GLFW and/or RGFW
- Add tests for NormalYankDelete functions, to make sure that they are yanking the expected string.
