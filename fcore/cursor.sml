structure Cursor =
struct
  (* todo: viL does not handle \r\n pair right now
   * but this is not a priority so it's okay *)
  local
    fun helpVilLf (hd, strIdx, bufferIdx, cursorIdx, tl) =
      (* if we are in \n, 
       * move cursor to character after newline 
       * if possible *)
      if strIdx < String.size hd - 3 then
        (* we know it is safe to move cursorIdx by 2 
         * if strIdx + 2 is before or at the end of the string. *)
        cursorIdx + 2
      else
        (* we have to check the tl to see if it is not empty  
         * so we know it is safe to move. *)
        case tl of
          _ :: _ => 
            (* not empty, so we can increment by 2 *) 
            cursorIdx + 2
        | [] =>
            (* empty, so return end of string which is \n *)
            bufferIdx + String.size hd - 1
  in
    (* Prerequisite: lineGap is moved to requested idx first 
     * todo: check if we are in a \r\n pair, but this is not a priority *)
    fun viL (lineGap: LineGap.t, cursorIdx) =
      let
        val {rightStrings, idx = bufferIdx, ...} = lineGap
      in
        case rightStrings of
          hd :: tl =>
            let
              (* convert absolute cursorIdx to idx relative to hd string *)
              val strIdx = cursorIdx - bufferIdx
            in
              if strIdx < String.size hd - 2 then
                (case String.sub (hd, strIdx + 1) of
                   #"\n" => helpVilLf (hd, strIdx, bufferIdx, cursorIdx, tl)
                 | _ => cursorIdx + 1)
              else
                (case tl of
                   tlhd :: tltl =>
                     (* if there is another string after current head, we can increment cursorIdx 
                      * however, first we need to check if next char is \n. *)
                     (case String.sub (tlhd, 0) of
                        #"\n" => helpVilLf (tlhd, 0, bufferIdx, cursorIdx, tltl)
                      | _ => cursorIdx + 1)
                 | _ =>
                     (* if there is no string after current head, return original cursorIdx *)
                     cursorIdx)
            end
        | [] =>
            (* return original cursorIdx if there is nothing to the right *)
            cursorIdx
      end
  end
end
