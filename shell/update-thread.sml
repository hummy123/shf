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

      val () = sendMsgs (#msgs app, drawMailbox, searchMailbox)
    in
      loop (app, inputMailbox, drawMailbox, searchMailbox)
    end
end
