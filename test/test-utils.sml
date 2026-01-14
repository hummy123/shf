structure TestUtils =
struct
  fun init bufferString =
    let val buffer = LineGap.fromString bufferString
    in AppType.init (buffer, 0, 0, Time.now ())
    end

  fun update (app, cmd) =
    AppUpdate.update (app, cmd, Time.now ())

  fun updateMany (app, str) =
    let
      fun loop (pos, app) =
        if pos = String.size str then
          app
        else
          let
            val chr = String.sub (str, pos)
            val chr = InputMsg.CHAR_EVENT chr
            val app = update (app, chr)
          in
            loop (pos + 1, app)
          end
    in
      loop (0, app)
    end

  fun expectYank (app: AppType.app_type, expectedString) =
    let
      open MailboxType
      open DrawMsg
      open Railroad
      open Railroad.Test

      fun loop (hd :: tl) =
            (case hd of
               DRAW (YANK actualString) =>
                 if actualString = expectedString then
                   Expect.isTrue (actualString = expectedString)
                 else
                   let
                     val () = print
                       ("expectedString = [" ^ expectedString ^ "]\n")
                     val () = print ("actualString = [" ^ actualString ^ "]\n")
                     val () = print "\n"
                   in
                     Expect.isTrue (actualString = expectedString)
                   end
             | _ => loop tl)
        | loop ([]) =
            let val () = print "no string yanked\n"
            in Expect.isTrue false
            end
    in
      loop (#msgs app)
    end

  fun expectNoYank (app: AppType.app_type) =
    let
      open MailboxType
      open DrawMsg
      open Railroad
      open Railroad.Test

      fun loop (DRAW (YANK _) :: _) = Expect.isTrue false
        | loop (hd :: tl) = loop tl
        | loop ([]) = Expect.isTrue true
    in
      loop (#msgs app)
    end
end
