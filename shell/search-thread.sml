structure SearchThread =
struct
  open CML

  (* Prerequisite to sending message: move buffer to end. *)
  fun loop (searchMailbox, inputMailbox) =
    let
      val (buffer, searchString, time) = Mailbox.recv searchMailbox
      val searchList = SearchList.build (buffer, searchString)
      val () = Mailbox.send (inputMailbox, InputMsg.WITH_SEARCH_LIST searchList)
    in
      loop (searchMailbox, inputMailbox)
    end
end
