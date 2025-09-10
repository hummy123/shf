structure MailboxType =
struct datatype t = DRAW of DrawMsg.t | SEARCH of SearchMsg.t end
