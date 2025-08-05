structure BuildSearchList =
struct
  fun fromStart (app, cursorIdx, buffer, searchString) =
    if String.size searchString > 0 then
      let
        val buffer = LineGap.goToEnd buffer
        val searchList = SearchLineGap.search (buffer, searchString)
        val buffer = LineGap.goToStart buffer
      in
        AppWith.searchList (app, searchList, buffer, searchString)
      end
    else
      app

  fun fromRange (startIdx, length, buffer, searchString, searchList) =
    let
      val buffer = LineGap.goToEnd buffer
      val searchList = SearchLineGap.search (buffer, searchString)
      val buffer = LineGap.goToStart buffer
    in
      (buffer, searchList)
    end
end
