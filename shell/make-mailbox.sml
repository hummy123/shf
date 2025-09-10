signature MAKE_MAILBOX =
sig
  type t
end

functor MakeMailbox(Fn: MAKE_MAILBOX) =
struct
  val messages: Fn.t vector ref = ref #[]

  fun getMessagesAndClear () =
    let
      val () = MLton.Thread.atomicBegin ()
      val msgs = !messages
      val () = messages := #[]
      val () = MLton.Thread.atomicEnd ()
    in
      msgs
    end

  fun append newMsg =
    let
      val () = MLton.Thread.atomicBegin ()
      val msgs = !messages
      val msgs = Vector.concat [msgs, #[newMsg]]
      val () = messages := msgs
    in
      MLton.Thread.atomicEnd ()
    end
end
