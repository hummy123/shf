structure InputMsg =
struct datatype t = CHAR_EVENT of char | KEY_ESC | RESIZE_EVENT of int * int end
