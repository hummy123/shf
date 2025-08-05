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

  fun loop (app: AppType.app_type, inputMailbox, drawMailbox) =
    let
      val inputMsg = Mailbox.recv inputMailbox
      val () =
        (* if a certain CHAR_EVENT is sent, 
         * we trigger an exception and log the command history.
         * This is helpful for manually triggering logs when,
         * for example, we encounter a bug and would like to see
         * the history of events that caused it. *)
        case inputMsg of
          CHAR_EVENT #"~" => ExceptionLogger.log (Fail "")
        | _ => ()

      val () = ExceptionLogger.addCommand inputMsg

      val app = AppUpdate.update (app, inputMsg)
                handle e => ExceptionLogger.log e

      val () = sendMsgs (#msgs app, drawMailbox)
    in
      loop (app, inputMailbox, drawMailbox)
    end
end
