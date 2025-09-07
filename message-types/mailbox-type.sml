structure MailboxType =
struct datatype t = DRAW of DrawMsg.t | SEARCH of LineGap.t * string * Time.time end
