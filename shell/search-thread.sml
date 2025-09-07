structure SearchThread =
struct
  open CML

  (* Prerequisite to sending message: move buffer to end. *)
  fun loop (searchMailbox, inputMailbox) =
    let
      val (buffer, searchString, time) = Mailbox.recv searchMailbox
      val searchList = SearchList.build (buffer, searchString)
      val msg = InputMsg.WITH_SEARCH_LIST (searchList, time)
      val () = Mailbox.send (inputMailbox, msg)
    in
      loop (searchMailbox, inputMailbox)
    end
end
