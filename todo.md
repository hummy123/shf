# To-do list
- Make sure that all delete function in make-normal-delete.sml also delete from searchList
- Handle edge cases regarding deletion from searchList.
  - Edge case 1: deletion causes a match to be extended
  - Any other edge cases possible?
- Add normal-delete tests for each motion, checking that searchList is as expected
- Add tests for other yank motoins
  - Tests should be based on existing tests for delete-motions, and in the same order.
- Bind gamepad functions from GLFW and/or RGFW
- Add tests for NormalYankDelete functions, to make sure that they are yanking the expected string.
