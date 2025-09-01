structure EscapeString =
struct
  (* The specific escape sequences handled are in the 
   * `rewriteAcc` function below.
   * These escape sequences were decided on because
   * they are the same escape sequences recognised in Standard ML.
   * A reference is available here: 
   * https://smlfamily.github.io/Basis/char.html#SIG:CHAR.fromCString:VAL
   *
   * However, thre omissions have been made:
   * - \ddd denoting an integer in the range 0 - 255
   * - \uxxxx denoting the character whose code is the integer xxxx
   * - \f f\ denoting a sequence of characters to ignore
   *
   * In the first two cases, it is easier to type the character directly.
   * In the third case, there is little use for it.
   * *)
  fun rewriteAcc (#"\\", acc) =
        (case acc of
           #"a" :: tl => #"\a" :: tl
         | #"b" :: tl => #"\b" :: tl
         | #"t" :: tl => #"\t" :: tl
         | #"n" :: tl => #"\n" :: tl
         | #"v" :: tl => #"\v" :: tl
         | #"f" :: tl => #"\f" :: tl
         | #"r" :: tl => #"\r" :: tl
         | #"?" :: tl => #"?" :: tl
         | #"\\" :: tl => #"\\" :: tl
         | #"\"" :: tl => #"\"" :: tl
         | #"'" :: tl => #"'" :: tl
         | #"^" :: hd :: tl =>
             (* handle control characters *)
             let
               val code = Char.ord hd
             in
               if code >= 64 andalso code <= 95 then
                 let val chr = Char.chr (code - 64)
                 in chr :: tl
                 end
               else
                 (* invalid escape sequence: leave alone *)
                 #"\\" :: acc
             end
         | _ =>
             (* when there is no valid escape sequence, 
              * just leave slash in output *)
             #"\\" :: acc)
    | rewriteAcc (chr, acc) = chr :: acc

  fun help (pos, str, acc) =
    if pos < 0 then
      String.implode acc
    else
      let
        val chr = String.sub (str, pos)
        val acc = rewriteAcc (chr, acc)
      in
        help (pos - 1, str, acc)
      end

  fun unescape str =
    help (String.size str - 1, str, [])
end
