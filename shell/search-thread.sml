structure SearchThread =
struct
  open CML

  (* Prerequisite to sending message: move buffer to end. *)
  fun loop () =
    let
      val (buffer, dfa, time) = Mailbox.recv SearchMailbox.mailbox
      val searchList =
        raise Fail "todo: reimplement full builder for searchList"
      val msg = InputMsg.WITH_SEARCH_LIST (searchList, time)
      val () = InputMailbox.append msg
    in
      loop ()
    end
end
