structure PosData =
struct
  (* I don't like introducing new bindings in the global name space
   * so the pos_data type is introduced in a new struct. *)
  type t = {chr: char, strIdx: int, absIdx: int, hd: string, tl: string list}
end

signature MAKE_TEXT_BUILDER =
sig
  type state
  type env

  val folder: PosData.t * env * state -> state
  val stopFold: state -> bool
end

functor MakeTextBuilder(Fn: MAKE_TEXT_BUILDER) =
struct
  fun buildLoop (strIdx, absIdx, hd, tl, env, state) =
    if Fn.stopFold state then
      state
    else if strIdx = String.size hd then
      case tl of
        hd :: tl => buildLoop (0, absIdx, hd, tl, env, state)
      | [] => state
    else
      let
        val chr = String.sub (hd, strIdx)
        val posData =
          {chr = chr, strIdx = strIdx, absIdx = absIdx, hd = hd, tl = tl}
        val state = Fn.folder (posData, env, state)
      in
        buildLoop (strIdx + 1, absIdx + 1, hd, tl, env, state)
      end
end

structure Good =
  MakeTextBuilder
    (struct
       type state = unit
       type env = unit

       fun folder (_, _, _) = ()
       fun stopFold () = false
       fun finish () = 33
     end)
