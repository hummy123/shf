structure UpdateThread =
struct
  open CML
  open MailboxType
  open InputMsg

  fun sendMsg (msg, drawMailbox) =
    case msg of DRAW msg => Mailbox.send (drawMailbox, msg)

  fun sendMsgs (msgList, drawMailbox) =
    case msgList of
      hd :: tl =>
        let val _ = sendMsg (hd, drawMailbox)
        in sendMsgs (tl, drawMailbox)
        end
    | [] => ()

  val textCommands = ref ""

  fun addTextCommand inputMsg =
    case inputMsg of
      CHAR_EVENT chr =>
        let
          val chr = CharVector.fromList [chr]
          val newInput = !textCommands ^ chr
        in
          textCommands := newInput
        end
    | _ => ()

  fun handleException e =
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
      raise Empty
    end

  fun loop (app: AppType.app_type, inputMailbox, drawMailbox) =
    let
      val inputMsg = Mailbox.recv inputMailbox
      val () = addTextCommand inputMsg

      val app = AppUpdate.update (app, inputMsg) handle e => handleException e

      val () = sendMsgs (#msgs app, drawMailbox)
    in
      loop (app, inputMailbox, drawMailbox)
    end
end
