#' Run app antaresViz
#' 
#' \code{runAppAntaresViz} run antaresViz App.
#' 
#' @return 
#' an App Shiny. 
#' 
#' @import shiny shinydashboard shinyWidgets
#' @export
runAppAntaresViz <- function() {
  ctrl <- shiny::runApp(system.file("application", package = "antaresVizMedTSO") , launch.browser = TRUE)
  suppressWarnings(try(rm(list = c("directoryInput", "readDirectoryInput", 
                                   "updateDirectoryInput"), envir = .GlobalEnv), silent = TRUE))
  gc(reset = TRUE)
  invisible(TRUE)
}