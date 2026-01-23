structure Updater =
struct
  open MailboxType
  open InputMsg

  fun sendMsg msg =
    case msg of DRAW msg => DrawMailbox.append msg

  fun sendMsgs msgList =
    case msgList of
      hd :: tl => let val () = sendMsg hd in sendMsgs tl end
    | [] => ()

  fun updateOne (app: AppType.app_type, inputMsg) =
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

  fun updateLoop (pos, msgVec, app) =
    if pos = Vector.length msgVec then
      app
    else
      let
        val msg = Vector.sub (msgVec, pos)
        val app = updateOne (app, msg)
      in
        updateLoop (pos + 1, msgVec, app)
      end

  fun update app =
    updateLoop (0, InputMailbox.getMessagesAndClear (), app)
end
