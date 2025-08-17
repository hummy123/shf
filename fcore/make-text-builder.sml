signature MAKE_TEXT_BUILDER =
sig
  type state
  type env

  type pos_data =
    {chr: char, strIdx: int, absIdx: int, hd: string, tl: string list}

  val folder: pos_data * env * state -> state
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
