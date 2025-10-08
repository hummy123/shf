structure SearchThread =
struct
  open CML

  fun loop () =
    let
      val (buffer, dfa, time) = Mailbox.recv SearchMailbox.mailbox
      val iterator = LineGap.makeStringIterator buffer
      val searchList = SearchList.build (iterator, dfa)
      val msg = InputMsg.WITH_SEARCH_LIST (searchList, time)
      val () = InputMailbox.append msg
    in
      loop ()
    end
end
