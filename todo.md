# To-do list

- Add tests for:
  - `df<char>`
  - `dF<char>`
  - `dt<char>`
  - `dT<char>`
  - delete inside word
- Implement delete-around-word and test it
- Reimplement `%` motion and `d%` motion.
  - They should both search for the next character in any pair, the same way in Vim
  - Add tests for reimplemented movements and motions
- Reimplement `di<symbol>` and `da<symbol>`
  - They should search for the next char in the specific pair, the same way in Vim
  - Add tests for both

  Afterwards, add tests for yanking.
