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
# input_path <- "C:\\Users\\Datastorm\\Documents\\git\\antaresVizMedTSO\\inst\\application\\www\\readAntares_selection.xlsx"
readStudyShinySelection <- function(input_path){
  
  sel <- list(areas = "", links = "", clusters = "", districts = "", 
              "misc" = FALSE, "thermalAvailability" = FALSE, "hydroStorage" = FALSE, 
              "hydroStorageMaxPower" = FALSE, "reserve" = FALSE, 
              "linkCapacity" = FALSE, "mustRun" = FALSE, "thermalModulation" = FALSE, 
              timeStep = "hourly", select = NULL, mcYears = NULL, removeVirtualAreas = FALSE,
              storageFlexibility = NULL, production = NULL, reassignCost = FALSE, newCols = FALSE)
  
  if(!file.exists(input_path)){
    stop("Le fichier '", input_path, "' est introuvable")
  }
  
  # areas
  sel_areas <- suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Areas", check.names = FALSE, colNames = FALSE),
                                         error = function(e) {
                                           stop("Error reading sheet 'Areas' : ", e)
                                         }))
  
  if(!is.null(sel_areas) && nrow(sel_areas) > 0){
    sel$areas <- tolower(as.character(sel_areas[, 1]))
  }
  
  # links
  sel_links <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Links", check.names = FALSE, colNames = FALSE),
                                          error = function(e) {
                                            stop("Error reading sheet 'Links' : ", e)
                                          }))
  
  if(!is.null(sel_links) && nrow(sel_links) > 0){
    sel$links <- tolower(as.character(sel_links[, 1]))
  }
  
  # clusters
  sel_clusters <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Clusters", check.names = FALSE, colNames = FALSE),
                                             error = function(e) {
                                               stop("Error reading sheet 'Clusters' : ", e)
                                             }))
  
  if(!is.null(sel_clusters) && nrow(sel_clusters) > 0){
    sel$clusters <- tolower(as.character(sel_clusters[, 1]))
  }
  
  # districts
  sel_districts <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Districts", check.names = FALSE, colNames = FALSE),
                                              error = function(e) {
                                                stop("Error reading sheet 'Districts' : ", e)
                                              }))
  
  if(!is.null(sel_districts) && nrow(sel_districts) > 0){
    sel$districts <- tolower(as.character(sel_districts[, 1]))
  }
  
  # readAntares parameters
  sel_params <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "readAntares", check.names = FALSE, colNames = TRUE),
                                           error = function(e) {
                                             stop("Error reading sheet 'readAntares' : ", e)
                                           }))
  
  if(!is.null(sel_params) && nrow(sel_params) > 0){
    
    sel_params[[1]] <- gsub("^([[:space:]]*) | ([[:space:]]*)$", "", sel_params[[1]])
    sel_params[[2]] <- gsub("^([[:space:]]*) | ([[:space:]]*)$", "", sel_params[[2]])
    
    sel_params[[1]] <- na.locf0(sel_params[[1]])
    
    for(var in c("misc", "thermalAvailability", "hydroStorage", "hydroStorageMaxPower", "reserve", 
                 "linkCapacity", "mustRun", "thermalModulation", "reassignCost", "newCols", "removeVirtualAreas")){
      if(var %in% sel_params[[1]]){
        sel[[var]] <- as.logical(as.numeric(as.character(sel_params[sel_params[[1]] %in% var, 2])))
      }
    }
    
    if("timeStep" %in% sel_params[[1]]){
      ts <- tolower(as.character(sel_params[sel_params[[1]] %in% "timeStep", 2]))
      stopifnot(ts %in% c("hourly", "daily", "weekly", "monthly", "annual"))
      sel$timeStep <- ts
    }
    
    if("mcYears" %in% sel_params[[1]] && !is.na(sel_params[sel_params[[1]] %in% "mcYears", 2])){
      tmp <- as.character(sel_params[sel_params[[1]] %in% "mcYears", 2])
      tmp <- gsub("^([[:space:]]*) | ([[:space:]]*)$", "", unlist(strsplit(tmp, ";")))
      mcy <- suppressWarnings(as.numeric(tmp))
      if(!any(is.na(mcy))) sel$mcYears <- mcy
    }
    
    for(var in c("select", "storageFlexibility", "production")){
      tmp <- as.character(sel_params[sel_params[[1]] %in% var, 2])
      tmp[tolower(tmp) %in% c("na", "empty", "", "null")] <- NA
      tmp <- tmp[!is.na(tmp)]
      if(length(tmp) > 0){
        if(var %in% c("storageFlexibility", "production")) tmp <- tolower(tmp)
        sel[[var]] <- tmp
      }
    }
  }
  
  sel
  
}


.fill_short_gaps <- function (x, fill, maxgap){
  if (maxgap <= 0) return(x)
  if (maxgap >= length(x)) return(fill)
  naruns <- base::rle(is.na(x))
  naruns$values[naruns$lengths > maxgap] <- FALSE
  naok <- base::inverse.rle(naruns)
  x[naok] <- fill[naok]
  return(x)
}

na.locf0 <- function (object, fromLast = FALSE, maxgap = Inf) {
  
  if (fromLast) object <- rev(object)
  
  ok <- which(!is.na(object))
  if (is.na(object[1L])) ok <- c(1L, ok)
  
  gaps <- diff(c(ok, length(object) + 1L))
  object <- if (any(gaps > maxgap)) {
    .fill_short_gaps(object, rep(object[ok], gaps), maxgap = maxgap)
  } else {
    rep(object[ok], gaps)
  }
  if (fromLast) object <- rev(object)
  
  object
}

#' @export
writeStudyShinySelection <- function(val, output_path){
  
  ## Create a new workbook
  wb <- openxlsx::createWorkbook("antaresVizMedTSO")
  
  ## init worksheets
  addWorksheet(wb, "Areas")
  addWorksheet(wb, "Links")
  addWorksheet(wb, "Clusters")
  addWorksheet(wb, "Districts")
  addWorksheet(wb, "readAntares")
  
  ## Need data on worksheet to see all headers and footers
  if(!is.null(val$areas)){
    writeData(wb, sheet = "Areas", data.frame(val$areas), 
              colNames = FALSE, rowNames = FALSE)
  }
  
  if(!is.null(val$links)){
    writeData(wb, sheet = "Links", data.frame(val$links), 
              colNames = FALSE, rowNames = FALSE)
  }
  
  if(!is.null(val$districts)){
    writeData(wb, sheet = "Districts", data.frame(val$districts), 
              colNames = FALSE, rowNames = FALSE)
  }
  
  if(!is.null(val$clusters)){
    writeData(wb, sheet = "Clusters", data.frame(val$clusters), 
              colNames = FALSE, rowNames = FALSE)
  }
  
  antares_read_params <- do.call("rbind.data.frame", 
                                 lapply(c("misc", "thermalAvailability", "hydroStorage", 
                                          "hydroStorageMaxPower", "reserve", "linkCapacity", 
                                          "mustRun", "thermalModulation"), function(x){
                                            tmp <- 0
                                            if(!is.null(val[[x]])){
                                              tmp <- as.numeric(val[[x]])
                                            }
                                            data.frame(parameters = x,	value = tmp, comment = "0 disabled - 1 enabled")
                                          }))
  
  ts_params <- data.frame(parameters = "timeStep",	value = "hourly", comment = "hourly, daily, weekly, monthly, annual")
  if(!is.null(val$timeStep)){
    ts_params$value <- val$timeStep
  }
  antares_read_params <- rbind.data.frame(antares_read_params, ts_params)
  
  mcy_params <- data.frame(parameters = "mcYears",	value = NA, comment = "one or more numbers (separated by ;) or synthetic, or empty / NULL / NA. Multiple years : 1;2")
  if(!is.null(val$mcYears)){
    mcy_params$value <- paste(val$mcYears, collapse = ";")
  } else {
    mcy_params$value <- NA
  }
  antares_read_params <- rbind.data.frame(antares_read_params, mcy_params)
  
  if(!is.null(val$select)){
    sel_params <- data.frame(parameters = "select",	value = val$select, comment = "name of the columns to import")
    sel_params$parameters[-1] <- NA
    sel_params$comment[-1] <- NA
  } else {
    sel_params <- data.frame(parameters = "select",	value = NA, comment = "name of the columns to import")
  }
  antares_read_params <- rbind.data.frame(antares_read_params, sel_params)
  
  rmv_params <- data.frame(parameters = "removeVirtualAreas",	value = 0, comment = "0 disabled - 1 enabled")
  if(!is.null(val$removeVirtualAreas)){
    rmv_params$value <- as.numeric(val$removeVirtualAreas)
  }
  antares_read_params <- rbind.data.frame(antares_read_params, rmv_params)
  
  if(!is.null(val$storageFlexibility)){
    sel_params <- data.frame(parameters = "storageFlexibility",	value = val$storageFlexibility, comment = "names of the virtual storage/flexibility areas")
    sel_params$parameters[-1] <- NA
    sel_params$comment[-1] <- NA
  } else {
    sel_params <- data.frame(parameters = "storageFlexibility",	value = NA, comment = "names of the virtual storage/flexibility areas")
  }
  antares_read_params <- rbind.data.frame(antares_read_params, sel_params)
  
  if(!is.null(val$production)){
    sel_params <- data.frame(parameters = "production",	value = val$production, comment = "names of the virtual production areas, or empty / NULL / NA")
    sel_params$parameters[-1] <- NA
    sel_params$comment[-1] <- NA
  } else {
    sel_params <- data.frame(parameters = "production",	value = NA, comment = "names of the virtual production areas, or empty / NULL / NA")
  }
  antares_read_params <- rbind.data.frame(antares_read_params, sel_params)
  
  rmv_params <- data.frame(parameters = "reassignCost",	value = 0, comment = "0 disabled - 1 enabled")
  if(!is.null(val$reassignCost)){
    rmv_params$value <- as.numeric(val$reassignCost)
  }
  antares_read_params <- rbind.data.frame(antares_read_params, rmv_params)
  
  rmv_params <- data.frame(parameters = "newCols",	value = 0, comment = "0 disabled - 1 enabled")
  if(!is.null(val$newCols)){
    rmv_params$value <- as.numeric(val$newCols)
  }
  antares_read_params <- rbind.data.frame(antares_read_params, rmv_params)
  
  writeData(wb, sheet = "readAntares", antares_read_params, 
            colNames = TRUE, rowNames = FALSE, keepNA = FALSE)
  
  ## Save workbook
  saveWorkbook(wb, output_path, overwrite = TRUE)
  
}
