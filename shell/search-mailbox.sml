structure SearchMailbox =
struct open CML val mailbox: SearchMsg.t Mailbox.mbox = Mailbox.mailbox () end
