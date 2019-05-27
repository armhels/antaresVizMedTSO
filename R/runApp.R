#' Run app antaresViz
#' 
#' \code{runAppAntaresViz} run antaresViz App.
#' 
#' @return 
#' an App Shiny. 
#' 
#' @import shiny shinydashboard ggplot2 ggrepel ggforce sp
#' @importFrom shinyWidgets useShinydashboard
#' @importFrom DT renderDT DTOutput
#' @importFrom colourpicker colourInput
#' @export
runAppAntaresViz <- function() {
  ctrl <- suppressPackageStartupMessages({
      shiny::runApp(system.file("application", package = "antaresVizMedTSO") , launch.browser = TRUE)
  })
  suppressWarnings(try(rm(list = c("directoryInput", "readDirectoryInput", 
                                   "updateDirectoryInput"), envir = .GlobalEnv), silent = TRUE))
  gc(reset = TRUE)
  invisible(TRUE)
}