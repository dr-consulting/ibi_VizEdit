#' Utility function for launching \code{ibiVizEdit} RShiny graphic user interface
#' @export
launch_ibiVizEdit <- function() {
  appDir <- system.file("application", package = "ibiVizEdit")
  if (appDir == "") {
    stop("Could not find application folder. Try re-installing `ibiVizEdit`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
