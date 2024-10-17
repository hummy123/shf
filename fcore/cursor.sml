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
              (* increment if there is a character after where cursor is 
               * but first, check if we are in a \r\n pair; 
               * if we are, then increment cursor by 2 *)
              if
                String.size hd > strIdx + 2
                andalso String.sub (hd, strIdx + 1) = #"\r"
                andalso String.sub (hd, strIdx + 2) = #"\n"
              then
                (* increment cursor by 2 if we are inside \r\n pair *)
                cursorIdx + 2
              else
                (* inccrement cursor by 1 if we are not inside \r\n pair *)
                cursorIdx + 1
            else
              (case tl of
                 _ :: _ =>
                   (* if there is another string after current head, we can increment cursorIdx 
                    * however, first we need to check if next string is \r\n pair. *)
                   if
                     String.size hd > strIdx + 2
                     andalso String.sub (hd, strIdx + 1) = #"\r"
                     andalso String.sub (hd, strIdx + 2) = #"\n"
                   then
                     (* increment cursor by 2 if we are inside \r\n pair *)
                     cursorIdx + 2
                   else
                     (* inccrement cursor by 1 if we are not inside \r\n pair *)
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
