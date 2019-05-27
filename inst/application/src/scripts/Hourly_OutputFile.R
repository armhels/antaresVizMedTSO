makeTabStats <- function(data){
  dt <- data.table("Min [MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) min(x, na.rm = T))]),
                   "Max[MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) max(x, na.rm = T))]),
                   "Avg [MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) sd(x, na.rm = T))]),
                   "SUM [GWh]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) sum(as.numeric(x), na.rm = T)/1000)]))
  dt <- dt[, lapply(.SD, round)]
  dt[, colnames(dt) := lapply(.SD, function(x) {x[is.nan(x)] <- 0 ; x}), .SDcols = colnames(dt)]
  dt <- data.table(v1 = colnames(dt), t(dt))
  colnames(dt) <- rep(" ", ncol(dt))
  dt  
}

# param d'entree des imports

mcYears = 1
dico <- fread("dictionnary.csv")
setkey(dico, "ANTARES_naming")

# parametres :
areas_selectionsUpper <- c("DZ00", "TN00", "ukni", "IT_ALL", "FR00", "FR_ALL")
areas_selections <- tolower(areas_selectionsUpper)
market_data_code <- c("ROW BAL.", "MISC. NDG", "PSP", "NODU", "gas_ccgt old 1", "gas_conventional old 1", 
                      "gas_conventional old 2", "gas_ocgt new")
links_selections = c(  "AL00 - GR00", "AL00 - ME00", "AL00 - MK00", "AL00 - RS00", "AT00 - CZ00")



#============================== HOURLY MARKET DATA ========================================

format_hourlyOutputs <- function(data_clustH, data_links, areas_selections, 
                                 market_data_code, links_selections, dico = dico){
  
  areas_selections <- tolower(areas_selections)
  data_clustH$clusters <- aggregateClusters(data_clustH$clusters, "production", hourly = T)

  cols_areas <- intersect(colnames(data_clustH$areas), market_data_code)
  data_areas_dist <- rbindlist(list(data_clustH$areas, data_clustH$districts))
  data_areas_dist <- 
    data_areas_dist[area %in% areas_selections, 
                      c(colnames(data_areas_dist)[c(1,4)], cols_areas), with = F]
  
  temp <- dcast(data_clustH$clusters, area + time ~ cluster, value.var = "var", fill = NA)
  
  data_all <- merge(temp, data_areas_dist, by = c("area", "time"), all = TRUE)
  data_all <- data_all[, c("area", "time", intersect(market_data_code, colnames(data_all))), with = F]
  
  cols_availables <- intersect(market_data_code, colnames(data_all))
  
  if("PSP" %in% cols_availables) {
    data_all[, c("PSP_pos", "PSP_neg") := list(PSP > 0, PSP < 0)]
    data_all[, PSP := NULL]
    ind <- which(cols_availables == "PSP")
    cols_availables <- c(cols_availables[1:(ind-1)], "PSP_pos", "PSP_neg", 
                         cols_availables[(ind+1):length(cols_availables)]) #FIXME attention, ne marche pas si PSP est premier/dernier !
  }
  
  setcolorder(data_all, neworder = c("area", "time", cols_availables))
  
  data_out <- unique(data_all[,.(Hour = .I, Date = time)])
  for(i in areas_selections){
    data_out <- cbind(data_out, data_all[area %in% i, -(1:2)])
  }
  areas_districts <- data.table(t(rep(toupper(areas_selections), each = ncol(data_all) - 2)))
  colnames(areas_districts) <- dico[colnames(data_out)[-c(1:2)]]$Category
  
  dt_stats <- makeTabStats(data_out)
  
  #============================== Crossborder exchanges ========================================
  data_out_2 <- dcast(data_links, time ~ link, value.var = "FLOW LIN.")
  data_out_2[, Hour := .I]
  setnames(data_out_2, "time", "Date")
  setcolorder(data_out_2, c("Hour", "Date", tolower(links_selections)))
  colnames(data_out_2)[-(1:2)] <- toupper(colnames(data_out_2)[-(1:2)])
  dt_stats_2 <- makeTabStats(data_out_2)
  
  list(data_out = data_out, dt_stats = dt_stats, areas_districts = areas_districts,
       data_out_2 = data_out_2, dt_stats_2 = dt_stats_2)
  
}


hourly_outputs <- format_hourlyOutputs(
  data_clustH = antares_datas$data_areas_dist_clustH, 
  data_links = antares_datas$data_areas_dist_clustH$links,
  areas_selections = areas_selections,
  market_data_code = market_data_code,
  links_selections = links_selections,
  dico = dico)


data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                         "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", 1, NA, NA))


infile_name <- "hourly_OutputFile_Template__R.xlsx"
outfile_name <-  "C:/Users/lyesb/Documents/MED-Tso/Hourly_OutputFile_TEST.xlsx"

export_hourlyOutputs <- function(infile_name, outfile_name, hourly_outputs, data_intro, 
                                 areas_districts_selections, market_data_code, links_selections){
  
  wb <- loadWorkbook(infile_name)
  
  writeData(wb, "Identification", data_intro)
  writeData(wb, "Identification", areas_districts_selections, startRow = 14, startCol = 2)
  writeData(wb, "Identification", market_data_code, startRow = 14, startCol = 2)
  writeData(wb, "Identification", links_selections, startRow = 14, startCol = 3)
  writeData(wb, "Identification", data_intro[4, `2030 - Scenario 1`], startRow = 14, startCol = 4)
  
  writeData(wb, "Hourly Market Data", data_intro[-4,])
  writeData(wb, "Hourly Market Data", hourly_outputs$dt_stats[,1], startRow = 5, startCol = 2)
  writeData(wb, "Hourly Market Data", hourly_outputs$dt_stats[,-1], startRow = 5, startCol = 3, borders = "all")
  
  writeData(wb, "Hourly Market Data", hourly_outputs$areas_districts, startRow = 11,  startCol = 3, borders = "all")
  
  writeData(wb, "Hourly Market Data", hourly_outputs$data_out[,c(1:2)], startRow = 13)
  writeData(wb, "Hourly Market Data", hourly_outputs$data_out[,-c(1:2)], startRow = 13, startCol = 3, borders = "all")
  
  writeData(wb, "Crossborder exchanges", data_intro[-4,])
  writeData(wb, "Crossborder exchanges", hourly_outputs$dt_stats_2[,1], startRow = 5, startCol = 2)
  writeData(wb, "Crossborder exchanges", hourly_outputs$dt_stats_2[,-1], startRow = 5, startCol = 3, borders = "all")
  
  writeData(wb, "Crossborder exchanges", hourly_outputs$data_out_2[,c(1:2), with = T], startRow = 11)
  writeData(wb, "Crossborder exchanges", hourly_outputs$data_out_2[,-c(1:2)], startRow = 11, startCol = 3, borders = "all")
  
  system.time(saveWorkbook(wb, outfile_name, overwrite = T))
}

export_hourlyOutputs(hourly_outputs = hourly_outputs, infile_name = infile_name, outfile_name = outfile_name,
                     data_intro = data_intro, 
                     areas_districts_selections = areas_districts_selections,
                     market_data_code = market_data_code, links_selections = links_selections)





