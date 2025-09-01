structure InputMsg =
struct
  datatype t =
    CHAR_EVENT of char
  | KEY_ESC
  | KEY_ENTER
  | KEY_BACKSPACE
  | RESIZE_EVENT of int * int
  | WITH_SEARCH_LIST of int vector
end
