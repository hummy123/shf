structure BuildSearchList =
struct
  fun helpNextMatch (idx, hd, tl, absIdx, searchString, matchedChrs) =
    if idx = String.size hd then
      case tl of
        tlhd :: tltl =>
          helpNextMatch (0, tlhd, tltl, absIdx, searchString, matchedChrs)
      | [] => 
          NONE
    else
      let
        val hdChr = String.sub (hd, idx)
        val searchChr = String.sub (searchString, matchedChrs)
      in
        if hdChr = searchChr then
          if matchedChrs + 1 = String.size searchString then
            let
              val matchedIdx = absIdx - String.size searchString + 1
            in
              SOME matchedIdx
            end
          else
            helpNextMatch 
              (idx + 1, hd, tl, absIdx + 1, searchString, matchedChrs + 1)
        else
          helpNextMatch (idx + 1, hd, tl, absIdx + 1, searchString, 0)
      end

  fun nextMatch (bufferIdx, absIdx, rightStrings, searchString) =
    case rightStrings of
      hd :: tl =>
        let 
          val strIdx = absIdx - bufferIdx
        in
          if strIdx < String.size hd then
            helpNextMatch (strIdx, hd, tl, absIdx, searchString, 0)
          else
            (case tl of
              tlhd :: tltl =>
                let
                  val strIdx = strIdx - String.size hd
                in
                  helpNextMatch (strIdx, tlhd, tltl, absIdx, searchString, 0)
                end
            | [] => NONE)
        end
    | [] => NONE

  fun helpFromStart (app, origIdx, absIdx, buffer, searchString, searchList) =
    let
      val buffer = LineGap.goToIdx (absIdx, buffer)
      val {idx = bufferIdx, rightStrings, ...} = buffer
    in
      case nextMatch (bufferIdx, absIdx, rightStrings, searchString) of
        SOME matchedIdx =>
          let
            val searchList = SearchList.append (matchedIdx, searchList)
          in
            helpFromStart
              (app, origIdx, matchedIdx + 1, buffer, searchString, searchList)
          end
      | NONE =>
          let
            val buffer = LineGap.goToIdx (origIdx, buffer)
            val searchList = SearchList.goToNum (origIdx, searchList)
          in
            AppWith.searchList (app, searchList, buffer, searchString)
          end
    end

  fun fromStart (app, cursorIdx, buffer, searchString) =
    if String.size searchString > 0 then
      let
        val buffer = LineGap.goToStart buffer
      in
        helpFromStart
          (app, cursorIdx, 0, buffer, searchString, SearchList.empty)
      end
    else
      app

  (* searches for matchedIdx within a range from the buffer instead of from start *)
  fun helpFromRange 
    (origIdx, curIdx, finishIdx, buffer, searchString, searchList) =
      let
        val buffer = LineGap.goToIdx (curIdx, buffer) 
        val {idx = bufferIdx, rightStrings, ...} = buffer 
      in
        case nextMatch (bufferIdx, curIdx, rightStrings, searchString) of
          SOME matchedIdx =>
            if matchedIdx > finishIdx then
              let
                val buffer = LineGap.goToIdx (origIdx, buffer)
                val searchList = SearchList.goToNum (origIdx, searchList)
              in
                (buffer, searchList)
              end
            else
              let
                val searchList = SearchList.insert (matchedIdx, searchList)
              in
                helpFromRange
                  ( origIdx, matchedIdx + 1, finishIdx
                  , buffer, searchString, searchList
                  )
              end
        | NONE =>
            let
              val buffer = LineGap.goToIdx (origIdx, buffer)
              val searchList = SearchList.goToNum (origIdx, searchList)
            in
              (buffer, searchList)
            end
      end

  fun fromRange (startIdx, length, buffer, searchString, searchList) =
    let
      val finishIdx = startIdx + length + String.size searchString
      val bufferIdx = startIdx - String.size searchString
    in
      helpFromRange
        (startIdx, bufferIdx, finishIdx, buffer, searchString, searchList)
    end
end
