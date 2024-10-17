signature INPUT_MSG =
sig
  datatype t = 
    RESIZE_EVENT of int * int 
  | CHAR_EVENT of char
end

structure InputMsg :> INPUT_MSG = 
struct 
  datatype t = 
    RESIZE_EVENT of int * int 
  | CHAR_EVENT of char
end
