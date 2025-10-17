structure InputMsg =
struct
  datatype t =
    CHAR_EVENT of char
  | KEY_ESC
  | KEY_ENTER
  | KEY_BACKSPACE
  | RESIZE_EVENT of int * int
  | ARROW_LEFT
  | ARROW_UP
  | ARROW_RIGHT
  | ARROW_DOWN
end
