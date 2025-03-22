signature INPUT_MSG =
sig
  datatype t = CHAR_EVENT of char | KEY_ESC | RESIZE_EVENT of int * int
end

structure InputMsg :> INPUT_MSG =
struct datatype t = CHAR_EVENT of char | KEY_ESC | RESIZE_EVENT of int * int end
