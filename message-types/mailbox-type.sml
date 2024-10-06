signature MAILBOX_TYPE =
sig
  datatype t =
    DRAW of DrawMsg.t
end

structure MailboxType :> MAILBOX_TYPE =
struct
  datatype t =
    DRAW of DrawMsg.t
end
