structure NormalYankDelete =
  MakeNormalDelete
    (struct
       open DrawMsg
       open MailboxType

       fun initMsgs (low, length, buffer) =
         let
           val str = LineGap.substring (low, length, buffer)
           val msg = YANK str
         in
           [DRAW msg]
         end
     end)
