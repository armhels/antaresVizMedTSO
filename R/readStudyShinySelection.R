#' Read areas, links, clusters & districts selection from file
#'
#' @param input_path \code{character}. Path of .xlsx input file
#'
#' @return \code{list}
#'
#' @export
#'
#'
#'@import openxlsx
#'
# input_path <- "C:\\Users\\Datastorm\\Documents\\git\\GRTgazAntaresViz\\readAntares_selection.xlsx"
readStudyShinySelection <- function(input_path){
  
  sel <- list(areas = "", links = "", clusters = "", districts = "")
  
  if(!file.exists(input_path)){
    stop("Le fichier '", input_path, "' est introuvable")
  }
  
  # areas
  sel_areas <- suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Noeuds", check.names = FALSE, colNames = FALSE),
                                         error = function(e) {
                                           stop("Erreur lors de l'importation de l'onglet de 'Noeuds'")
                                         }))
  
  if(!is.null(sel_areas) && nrow(sel_areas) > 0){
    sel$areas <- tolower(as.character(sel_areas[, 1]))
  }
  
  # links
  sel_links <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Liens", check.names = FALSE, colNames = FALSE),
                                          error = function(e) {
                                            stop("Erreur lors de l'importation de l'onglet de 'Liens'")
                                          }))
  
  if(!is.null(sel_links) && nrow(sel_links) > 0){
    sel$links <- tolower(as.character(sel_links[, 1]))
  }
  
  # clusters
  sel_clusters <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Clusters", check.names = FALSE, colNames = FALSE),
                                             error = function(e) {
                                               stop("Erreur lors de l'importation de l'onglet de 'Clusters'")
                                             }))
  
  if(!is.null(sel_clusters) && nrow(sel_clusters) > 0){
    sel$clusters <- tolower(as.character(sel_clusters[, 1]))
  }
  
  # districts
  sel_districts <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Districts", check.names = FALSE, colNames = FALSE),
                                              error = function(e) {
                                                stop("Erreur lors de l'importation de l'onglet de 'Districts'")
                                              }))
  
  if(!is.null(sel_districts) && nrow(sel_districts) > 0){
    sel$districts <- tolower(as.character(sel_districts[, 1]))
  }
  
  sel
  
}
