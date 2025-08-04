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
      val () = ExceptionLogger.addCommand inputMsg

      val app = AppUpdate.update (app, inputMsg) handle e => ExceptionLogger.log e

      val () = sendMsgs (#msgs app, drawMailbox)
    in
      loop (app, inputMailbox, drawMailbox)
    end
end
