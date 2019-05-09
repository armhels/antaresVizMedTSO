#' Run app antaresViz
#' 
#' \code{runAppAntaresViz} run antaresViz App.
#' 
#' @return 
#' an App Shiny. 
#' 
#' @import shiny shinydashboard
#' @importFrom shinyWidgets useShinydashboard
#' @importFrom DT renderDT DTOutput
#' @export
runAppAntaresViz <- function() {
  ctrl <- suppressWarnings({
    suppressMessage({
      shiny::runApp(system.file("application", package = "antaresVizMedTSO") , launch.browser = TRUE)
    })
  })
  suppressWarnings(try(rm(list = c("directoryInput", "readDirectoryInput", 
                                   "updateDirectoryInput"), envir = .GlobalEnv), silent = TRUE))
  gc(reset = TRUE)
  invisible(TRUE)
}