signature DRAW_MSG =
sig
  datatype t =
    REDRAW_TEXT of Real32.real vector
  | REDRAW_CURSOR of Real32.real vector
  | REDRAW_BG of Real32.real vector
  | YANK of string
end

structure DrawMsg :> DRAW_MSG =
struct
  datatype t =
    REDRAW_TEXT of Real32.real vector
  | REDRAW_CURSOR of Real32.real vector
  | REDRAW_BG of Real32.real vector
  | YANK of string
end
