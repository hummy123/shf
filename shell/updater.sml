structure Updater =
struct
  open MailboxType
  open InputMsg

  fun sendMsg msg =
    case msg of
      DRAW msg => DrawMailbox.append msg
    | SEARCH (buffer, searchString, time) =>
        Mailbox.send (SearchMailbox.mailbox, (buffer, searchString, time))

  fun sendMsgs msgList =
    case msgList of
      hd :: tl => let val () = sendMsg hd in sendMsgs tl end
    | [] => ()

  fun update (app: AppType.app_type, inputMsg) =
    let
      val time = Time.now ()

      val () =
        case inputMsg of
          CHAR_EVENT #"~" =>
            ExceptionLogger.log (Fail "intentionally caused exception")
        | _ => ()

      val () = ExceptionLogger.addCommand inputMsg

      val app = AppUpdate.update (app, inputMsg, time)
                handle e => ExceptionLogger.log e

      val () = sendMsgs (#msgs app)
    in
      app
    end
end
