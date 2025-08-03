structure UpdateThread =
struct
  open CML
  open MailboxType

  fun sendMsg (msg, drawMailbox) =
    case msg of DRAW msg => Mailbox.send (drawMailbox, msg)

  fun sendMsgs (msgList, drawMailbox) =
    case msgList of
      hd :: tl =>
        let val _ = sendMsg (hd, drawMailbox)
        in sendMsgs (tl, drawMailbox)
        end
    | [] => ()

  fun loop (app: AppType.app_type, inputMailbox, drawMailbox) =
    let
      val inputMsg = Mailbox.recv inputMailbox
      val app = 
        AppUpdate.update (app, inputMsg) 
          handle e => 
            let
              (* print stack trace for debugging purposes, 
               * and then raise another exception to exit the program *)
              val stackTrace = MLton.Exn.history e
              val stackTrace = String.concatWith "\n" stackTrace
              val () = print (stackTrace ^ "\n")
            in
              raise Empty
            end

      val () = sendMsgs (#msgs app, drawMailbox)
    in
      loop (app, inputMailbox, drawMailbox)
    end
end
