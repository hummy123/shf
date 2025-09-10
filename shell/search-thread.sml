structure SearchThread =
struct
  open CML

  (* Prerequisite to sending message: move buffer to end. *)
  fun loop () =
    let
      val (buffer, searchString, time) = Mailbox.recv SearchMailbox.mailbox
      val searchList = SearchList.build (buffer, searchString)
      val msg = InputMsg.WITH_SEARCH_LIST (searchList, time)
      val () = InputMailbox.append msg
    in
      loop ()
    end
end
