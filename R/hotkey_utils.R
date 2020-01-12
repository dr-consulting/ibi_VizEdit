library(shiny)

#' Internal utility to enable hotkey functionality during an editing session
#'
#' see additional examples here:
#' https://stackoverflow.com/questions/32002170/r-shiny-enabling-keyboard-shortcuts
#' and here:
#' https://stackoverflow.com/questions/44500617/r-shiny-key-and-actionbutton-binding-to-reactive-values
#'

track_hotkey_presses <- function(){
  # look-up codes here: http://keycode.info/
  # a = average, c = combine, d = divide, i = insert, r = remove
  js_code <- "
  $(function(){
    $(document).keydown(function(e){
      if (e.keyCode == 65){
        $('#average').click();
      } else if (e.keyCode == 67){
        $('#combine').click();
      } else if (e.keyCode == 68){
        $('#divide').click();
      } else if (e.keyCode == 73){
        $('#insert').click();
      } else if (e.keyCode == 82){
        $('#remove').click();
      }
    });
  })
  "
  return(js_code)
}
