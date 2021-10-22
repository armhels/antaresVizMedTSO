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
# input_path <- "C:\\Users\\BenoitThieurmel\\Documents\\git\\antaresVizMedTSO\\inst\\application\\www\\readAntares_selection.xlsx"
readStudyShinySelection <- function(input_path){
  
  sel <- list(areas = "", links = "", clusters = "", clustersRes = "", districts = "", 
              "misc" = FALSE, "thermalAvailability" = FALSE, "hydroStorage" = FALSE, 
              "hydroStorageMaxPower" = FALSE, "reserve" = FALSE, 
              "linkCapacity" = FALSE, "mustRun" = FALSE, "thermalModulation" = FALSE, 
              timeStep = "hourly", select = NULL, mcYears = NULL, 
              removeVirtualAreas = FALSE, "storageFlexibility (PSP)" = "", 
              "Hydro Storage (PSP_Closed)" = "", "Battery Storage (BATT)"  = "",
              "Demand Side (DSR)" = "", "Electric Vehicle (EV)" = "",
              "Power-to-gas (P2G)" = "", "Hydrogen (H2)" = "",
              production = "", reassignCost = FALSE, newCols = FALSE,
              removeVirtualAreas_2 = FALSE, "storageFlexibility (PSP)_2" = "", 
              "Hydro Storage (PSP_Closed)_2" = "", "Battery Storage (BATT)_2"  = "",
              "Demand Side (DSR)_2" = "", "Electric Vehicle (EV)_2" = "",
              "Power-to-gas (P2G)_2" = "", "Hydrogen (H2)_2" = "",
              production_2 = "", reassignCost_2 = FALSE, newCols_2 = FALSE,
              removeVirtualAreas_3 = FALSE, "storageFlexibility (PSP)_3" = "", 
              "Hydro Storage (PSP_Closed)_3" = "", "Battery Storage (BATT)_3"  = "",
              "Demand Side (DSR)_3" = "", "Electric Vehicle (EV)_3" = "",
              "Power-to-gas (P2G)_3" = "", "Hydrogen (H2)_3" = "",
              production_3 = "", reassignCost_3 = FALSE, newCols_3 = FALSE)
  
  if(!file.exists(input_path)){
    stop("Le fichier '", input_path, "' est introuvable")
  }
  
  # areas
  if("Areas" %in% openxlsx::getSheetNames(input_path)){
    sel_areas <- suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Areas", 
                                                               check.names = FALSE, colNames = FALSE),
                                           error = function(e) {
                                             stop("Error reading sheet 'Areas' : ", e)
                                           }))
    
    if(!is.null(sel_areas) && nrow(sel_areas) > 0){
      sel$areas <- tolower(as.character(sel_areas[, 1]))
    }
  }
  
  # links
  if("Links" %in% openxlsx::getSheetNames(input_path)){
    sel_links <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Links", 
                                                                check.names = FALSE, colNames = FALSE),
                                            error = function(e) {
                                              stop("Error reading sheet 'Links' : ", e)
                                            }))
    
    if(!is.null(sel_links) && nrow(sel_links) > 0){
      sel$links <- tolower(as.character(sel_links[, 1]))
    }
  }
  
  # clusters
  if("Clusters" %in% openxlsx::getSheetNames(input_path)){
    sel_clusters <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Clusters", 
                                                                   check.names = FALSE, colNames = FALSE),
                                               error = function(e) {
                                                 stop("Error reading sheet 'Clusters' : ", e)
                                               }))
    
    if(!is.null(sel_clusters) && nrow(sel_clusters) > 0){
      sel$clusters <- tolower(as.character(sel_clusters[, 1]))
    }
  }
  
  # clustersRes
  if("ClustersRes" %in% openxlsx::getSheetNames(input_path)){
    sel_clusters_res <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "ClustersRes", 
                                                                       check.names = FALSE, colNames = FALSE),
                                                   error = function(e) {
                                                     stop("Error reading sheet 'ClustersRes' : ", e)
                                                   }))
    
    if(!is.null(sel_clusters_res) && nrow(sel_clusters_res) > 0){
      sel$clustersRes <- tolower(as.character(sel_clusters_res[, 1]))
    }
  }
  
  
  # districts
  if("Districts" %in% openxlsx::getSheetNames(input_path)){
    sel_districts <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Districts", 
                                                                    check.names = FALSE, colNames = FALSE),
                                                error = function(e) {
                                                  stop("Error reading sheet 'Districts' : ", e)
                                                }))
    
    if(!is.null(sel_districts) && nrow(sel_districts) > 0){
      sel$districts <- tolower(as.character(sel_districts[, 1]))
    }
  }
  
  # readAntares parameters
  if("readAntares" %in% openxlsx::getSheetNames(input_path)){
    sel_params <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "readAntares", 
                                                                 check.names = FALSE, colNames = TRUE),
                                             error = function(e) {
                                               stop("Error reading sheet 'readAntares' : ", e)
                                             }))
    
    if(!is.null(sel_params) && nrow(sel_params) > 0){
      
      sel_params[[1]] <- gsub("^([[:space:]]*) | ([[:space:]]*)$", "", sel_params[[1]])
      sel_params[[2]] <- gsub("^([[:space:]]*) | ([[:space:]]*)$", "", sel_params[[2]])
      
      # compatibility previous version
      sel_params[[1]] <- gsub("^storageFlexibility$", "storageFlexibility (PSP)", sel_params[[1]])
      sel_params[[1]] <- zoo::na.locf0(sel_params[[1]])
      
      for(var in c("misc", "thermalAvailability", "hydroStorage", "hydroStorageMaxPower", "reserve", 
                   "linkCapacity", "mustRun", "thermalModulation", "reassignCost", "newCols", "removeVirtualAreas",
                   "reassignCost_2", "newCols_2", "removeVirtualAreas_2", "reassignCost_3", "newCols_3", "removeVirtualAreas_3")){
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
      
      areas_var <- c("select", 
                     
                     "storageFlexibility (PSP)", "Hydro Storage (PSP_Closed)", 
                     "Battery Storage (BATT)", "Demand Side (DSR)", "Electric Vehicle (EV)",
                     "Power-to-gas (P2G)", "Hydrogen (H2)", "production",
                     
                     "storageFlexibility (PSP)_2", "Hydro Storage (PSP_Closed)_2", 
                     "Battery Storage (BATT)_2", "Demand Side (DSR)_2", "Electric Vehicle (EV)_2",
                     "Power-to-gas (P2G)_2", "Hydrogen (H2)_2", "production_2",
                     
                     "storageFlexibility (PSP)_3", "Hydro Storage (PSP_Closed)_3", 
                     "Battery Storage (BATT)_3", "Demand Side (DSR)_3", "Electric Vehicle (EV)_3",
                     "Power-to-gas (P2G)_3", "Hydrogen (H2)_3", "production_3"
      )
      
      for(var in areas_var){
        tmp <- as.character(sel_params[sel_params[[1]] %in% var, 2])
        tmp[tolower(tmp) %in% c("na", "empty", "", "null")] <- NA
        tmp <- tmp[!is.na(tmp)]
        if(length(tmp) > 0){
          if(var %in% c("storageFlexibility", "production")) tmp <- tolower(tmp)
          sel[[var]] <- tmp
        }
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
  addWorksheet(wb, "ClustersRes")
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
  
  if(!is.null(val[["clusters"]])){
    writeData(wb, sheet = "Clusters", data.frame(val[["clusters"]]), 
              colNames = FALSE, rowNames = FALSE)
  }
  
  if(!is.null(val[["clustersRes"]])){
    writeData(wb, sheet = "ClustersRes", data.frame(val[["clustersRes"]]), 
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
  
  for(jj in c("", "_2", "_3")){
    
    rmv_params <- data.frame(parameters = paste0("removeVirtualAreas", jj),	value = 0, comment = "0 disabled - 1 enabled")
    
    if(!is.null(val[[paste0("removeVirtualAreas", jj)]])){
      rmv_params$value <- as.numeric(val[[paste0("removeVirtualAreas", jj)]])
    }
    antares_read_params <- rbind.data.frame(antares_read_params, rmv_params)
    
    v_name <- c("storageFlexibility (PSP)", "Hydro Storage (PSP_Closed)",
                "Battery Storage (BATT)", "Demand Side (DSR)", "Electric Vehicle (EV)",
                "Power-to-gas (P2G)", "Hydrogen (H2)", "production")
    
    v_comment <- c("names of the virtual storage/flexibility areas PSP", 
                   "names of the virtual hydro storage areas PSP_Closed",
                   "names of the virtual battery storage areas BATT", 
                   "names of the virtual demand side areas DSR", 
                   "names of the virtual electric vehicle areas EV",
                   "names of the virtual power to gas areas P2G", 
                   "names of the virtual hydrogen areas H2", 
                   "names of the virtual productions areas")
    
    for(vi in 1:length(v_name)){
      tmp_name <- paste0(v_name[vi], jj)
      comment <- v_comment[vi]
      
      if(!is.null(val[[tmp_name]])){
        sel_params <- data.frame(parameters = tmp_name,	value = val[[tmp_name]], comment = comment)
        sel_params$parameters[-1] <- NA
        sel_params$comment[-1] <- NA
      } else {
        sel_params <- data.frame(parameters = tmp_name,	value = NA, comment = comment)
      }
      antares_read_params <- rbind.data.frame(antares_read_params, sel_params)
    }
    
    
    v_name <- c("reassignCost", "newCols")
    v_comment <- c("0 disabled - 1 enabled", "0 disabled - 1 enabled")
    
    for(vi in 1:length(v_name)){
      tmp_name <- paste0(v_name[vi], jj)
      comment <- v_comment[vi]
      
      rmv_params <- data.frame(parameters = tmp_name,	value = 0, comment = comment)
      if(!is.null(val[[tmp_name]])){
        rmv_params$value <- as.numeric(val[[tmp_name]])
      }
      antares_read_params <- rbind.data.frame(antares_read_params, rmv_params)
    }
  }
  
  writeData(wb, sheet = "readAntares", antares_read_params, 
            colNames = TRUE, rowNames = FALSE, keepNA = FALSE)
  
  ## Save workbook
  saveWorkbook(wb, output_path, overwrite = TRUE)
  
}
