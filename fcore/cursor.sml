structure Cursor =
struct
  (* Prerequisite: lineGap is moved to requested idx first *)
  fun viL (lineGap: LineGap.t, cursorIdx) =
    let
      val {rightStrings, idx = bufferIdx, ...} = lineGap
    in
      case rightStrings of
        hd :: tl =>
          let
            (* idx relative to this string *)
            val strIdx = cursorIdx - bufferIdx
          in
            if strIdx < String.size hd - 2 then
              (* increment if there is a character after where cursor is *)
              cursorIdx + 1
            else
              (case tl of
                 _ :: _ =>
                   (* if there is another string after current head, we can increment cursorIdx *)
                   cursorIdx + 1
               | _ =>
                   (* if there is no string after current head, return original cursorIdx *)
                   cursorIdx)
          end
      | [] =>
          (* return original cursorIdx if there is nothing to the right *)
          cursorIdx
    end
end
