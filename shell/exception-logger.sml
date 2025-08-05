structure ExceptionLogger =
struct
  open InputMsg

  val textCommands = ref ""

  fun addCommand inputMsg =
    case inputMsg of
      CHAR_EVENT chr =>
        let
          val chr = CharVector.fromList [chr]
          val newInput = !textCommands ^ chr
        in
          textCommands := newInput
        end
    | _ => ()

  fun log e =
    let
      (* print stack trace for debugging purposes, 
       * and then raise another exception to exit the program *)
      val errName = General.exnName e ^ "\n"
      val stackTrace = MLton.Exn.history e
      val stackTrace = (String.concatWith "\n" stackTrace) ^ "\n"
      val history = !textCommands ^ "\n\n"

      val log = String.concat
        ["ERROR: ", errName, stackTrace, "HISTORY: ", history]

      val () = print ("\n" ^ log)

      val io = TextIO.openAppend "exceptions.log"
      val () = TextIO.output (io, log)
      val () = TextIO.closeOut io
    in
      raise e
    end
end
