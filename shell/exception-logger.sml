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
      val stackTrace = MLton.Exn.history e
      val stackTrace = String.concatWith "\n" stackTrace
      val () = print "ERROR:\n"
      val () = print (stackTrace ^ "\n\n")

      val history = !textCommands ^ "\n\n"
      val () = print ("HISTORY: " ^ history)

      val textOutput = stackTrace ^ "\n" ^ history

      val io = TextIO.openAppend "exceptions.log"
      val () = TextIO.output (io, textOutput)
      val () = TextIO.closeOut io
    in
      raise e
    end
end
