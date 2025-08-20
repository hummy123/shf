structure UpdateThread =
struct
  open CML
  open MailboxType
  open InputMsg

  fun sendMsg (msg, drawMailbox, searchMailbox) =
    case msg of
      DRAW msg => Mailbox.send (drawMailbox, msg)
    | SEARCH (buffer, searchString) =>
        Mailbox.send (searchMailbox, (buffer, searchString))

  fun sendMsgs (msgList, drawMailbox, searchMailbox) =
    case msgList of
      hd :: tl =>
        let val _ = sendMsg (hd, drawMailbox, searchMailbox)
        in sendMsgs (tl, drawMailbox, searchMailbox)
        end
    | [] => ()

  fun loop (app: AppType.app_type, inputMailbox, drawMailbox, searchMailbox) =
    let
      val time = Time.now ()
      val inputMsg = Mailbox.recv inputMailbox

      val () = ExceptionLogger.addCommand inputMsg

      val app = AppUpdate.update (app, inputMsg, time)
                handle e => ExceptionLogger.log e

      val () = sendMsgs (#msgs app, drawMailbox, searchMailbox)
    in
      loop (app, inputMailbox, drawMailbox, searchMailbox)
    end
end
