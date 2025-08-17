structure TextBuilderUtils =
struct
  type pos_data =
    {chr: char, strIdx: int, absIdx: int, hd: string, tl: string list}

  (* gets line start idx, relative to right hd *)
  fun helpGetLineStartIdx (startLine, curLine, rLnHd) =
    if startLine > curLine then
      let val lnPos = startLine - curLine - 1
      in Vector.sub (rLnHd, lnPos) + 1
      end
    else
      0

  (* gets line start idx, absolute *)
  fun helpGetLineAbsIdx (curIdx, startLine, curLine, rLnHd) =
    let
      val startIdx =
        if startLine > curLine then
          let val lnPos = startLine - curLine - 1
          in Vector.sub (rLnHd, lnPos) + 1
          end
        else
          0
    in
      curIdx + startIdx
    end

  fun getLineAbsIdx (startLine, lineGap: LineGap.t) =
    let
      val {rightLines, line = curLine, idx = curIdx, ...} = lineGap
    in
      case rightLines of
        rLnHd :: _ => helpGetLineAbsIdx (curIdx, startLine, curLine, rLnHd)
      | [] => (* should never happen *) 0
    end
end

signature MAKE_TEXT_BUILDER =
sig
  type state
  type env

  val folder: TextBuilderUtils.pos_data * env * state -> state
  val stopFold: env * state -> bool
end

functor MakeTextBuilder(Fn: MAKE_TEXT_BUILDER) =
struct
  fun buildLoop (strIdx, absIdx, hd, tl, env, state) =
    if Fn.stopFold (env, state) then
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

(* Text builder loop in normal mode, when there is no search to perform. *)
structure NormalTextBuilder =
  MakeTextBuilder
    (struct
       type state =
         { cursorAcc: Real32.real vector
         , bgAcc: Real32.real vector
         , textAcc: Real32.real vector list
         , posX: int
         , posY: int
         }

       type env =
         { cursorPos: int
         , startX: int

         , r: Real32.real
         , g: Real32.real
         , b: Real32.real
         , hr: Real32.real
         , hg: Real32.real
         , hb: Real32.real

         , maxWidth: int
         , maxHeight: int
         , floatWidth: Real32.real
         , floatHeight: Real32.real
         }

       fun stopFold ({maxWidth, maxHeight, ...}: env, {posX, posY, ...}: state) =
         posX >= maxWidth andalso posY >= maxHeight

       open TextConstants

       fun makeSpace (env: env, state, absIdx) =
         let
           val {textAcc, cursorAcc, bgAcc, posX, posY} = state
           val {cursorPos, r, g, b, floatWidth, floatHeight, ...} = env

           (* if inside cursor, then create cursorAcc;
            * else, just skip as usual *)
           val cursorAcc =
             if absIdx = cursorPos then
               Rect.lerp
                 ( posX
                 , posY
                 , fontSize
                 , fontSize
                 , floatWidth
                 , floatHeight
                 , r
                 , g
                 , b
                 )
             else
               cursorAcc
         in
           { posX = posX + xSpace
           , cursorAcc = cursorAcc
           , posY = posY
           , bgAcc = bgAcc
           , textAcc = textAcc
           }
         end

       fun makeNewLine (env: env, state, absIdx) =
         let
           val {textAcc, cursorAcc, bgAcc, posX, posY} = state
           val {cursorPos, floatWidth, floatHeight, r, g, b, ...} = env
           val cursorAcc =
             if absIdx = cursorPos then
               Rect.lerp
                 ( posX
                 , posY
                 , fontSize
                 , fontSize
                 , floatWidth
                 , floatHeight
                 , r
                 , g
                 , b
                 )
             else
               cursorAcc
         in
           { posY = posY + ySpace
           , cursorAcc = cursorAcc
           , posX = posX
           , bgAcc = bgAcc
           , textAcc = textAcc
           }
         end

       fun makeChrHandlingNewLine
         (posX, posY, textAcc, cursorAcc, bgAcc, chrFun, env: env, r, g, b) =
         if posX + xSpace < #maxWidth env then
           let
             val {floatWidth, floatHeight, ...} = env
             val textAcc =
               chrFun
                 ( posX
                 , posY
                 , fontSize
                 , fontSize
                 , floatWidth
                 , floatHeight
                 , r
                 , g
                 , b
                 ) :: textAcc
             val posX = posX + xSpace
           in
             { posX = posX
             , posY = posY
             , textAcc = textAcc
             , cursorAcc = cursorAcc
             , bgAcc = bgAcc
             }
           end
         else
           (* posX >= maxWidth, so we draw on a new line.
            * Since we reached the end of this one. *)
           let
             val {floatWidth, floatHeight, ...} = env
             val startX = #startX env
             val posY = posY + ySpace
             val textAcc =
               chrFun
                 ( startX
                 , posY
                 , fontSize
                 , fontSize
                 , floatWidth
                 , floatHeight
                 , r
                 , g
                 , b
                 ) :: textAcc
           in
             { posX = startX + xSpace
             , posY = posY
             , textAcc = textAcc
             , cursorAcc = cursorAcc
             , bgAcc = bgAcc
             }
           end

       fun makeChr (env: env, state: state, absIdx, chr) =
         let
           val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)
           val {posX, posY, textAcc, cursorAcc, bgAcc} = state
         in
           if absIdx <> #cursorPos env then
             let
               val {r, g, b, ...} = env
             in
               makeChrHandlingNewLine
                 (posX, posY, textAcc, cursorAcc, bgAcc, chrFun, env, r, g, b)
             end
           else
             (* at cursorPos *)
             let
               val {floatWidth, floatHeight, r, g, b, ...} = env
               val cursorAcc = Rect.lerp
                 ( posX
                 , posY
                 , fontSize
                 , fontSize
                 , floatWidth
                 , floatHeight
                 , r
                 , g
                 , b
                 )
               val {hr, hg, hb, ...} = env
             in
               makeChrHandlingNewLine
                 ( posX
                 , posY
                 , textAcc
                 , cursorAcc
                 , bgAcc
                 , chrFun
                 , env
                 , hr
                 , hg
                 , hb
                 )
             end
         end

       fun folder (posData: TextBuilderUtils.pos_data, env, state) =
         case #chr posData of
           #" " => makeSpace (env, state, #absIdx posData)
         | #"\n" => makeNewLine (env, state, #absIdx posData)
         | chr => makeChr (env, state, #absIdx posData, chr)
     end)
