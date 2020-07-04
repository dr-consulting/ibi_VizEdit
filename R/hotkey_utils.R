library(glue)

#' Internal utility to enable hotkey functionality during an editing session
#'
#' see additional examples here:
#' https://stackoverflow.com/questions/32002170/r-shiny-enabling-keyboard-shortcuts
#' and here:
#' https://stackoverflow.com/questions/44500617/r-shiny-key-and-actionbutton-binding-to-reactive-values
#'
#' @export

track_hotkey_presses <- function(key=NULL, key_map=NULL, button_name=NULL){
  # look-up codes here: http://keycode.info/
  # a = average, c = combine, d = divide, r = remove
  js_button_code <- key_map[[key]]
  js_code <- "
  $(function(){{
    $(document).keydown(function(e){{
      if (e.keyCode == {js_button_code}){{
        $('#{button_name}').click();
      }}
    }});
  }})
  "

  out_js_code <- glue(js_code)

  return(out_js_code)
}
