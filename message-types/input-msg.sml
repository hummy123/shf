structure InputMsg =
struct
  datatype t =
    CHAR_EVENT of char
  | KEY_ESC
  | RESIZE_EVENT of int * int
  | WITH_SEARCH_LIST of int vector
end
