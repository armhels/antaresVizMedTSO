source("functions.R")
require(data.table)
require(antaresRead)
require(antaresProcessing)
require(stringr)
require(openxlsx)
setwd("~/MED-Tso")


data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                         "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", 1, NA, NA))

# Somme des variables finissant par _chiffre :
aggregateClusters <- function(data, var, hourly = F){
  
  data <- copy(data)
  ind <- grep("_[0-9]", data[["cluster"]])
  data[ind, cluster := gsub(pattern = "_[0-9]", "", cluster)]
  if(hourly){
    data <- data[, .(var = sum(get(var))), by = .(area, cluster, time)]  
  }else {
    data <- data[, .(var = sum(get(var))), by = .(area, cluster)]
  }
  data
}

formater_tab <- function(data, output_type, new_names){
  t_tmp <- t(data)
  if(!is.null(output_type)){
    t_data <- data.table(output_type, row.names(t_tmp), t_tmp)
    names(t_data) <- c("Output type", "output_type", new_names)
  } else{
    t_data <- data.table(row.names(t_tmp), t_tmp)
    names(t_data) <- c("Output type", new_names)
  }
  t_data
}


areas_districts_selections = c("IT_ALL", "FR_ALL",
                               "gr_all", "medi_all","medi_ne","medi_nw","medi_se","medi_sw","medtso","ue_nomedtso")
areas_districts_selections = c("tn00", "fr00")

links_selections = c("AL00 - GR00", "AL00 - ME00", "AL00 - MK00", "AL00 - RS00", "AT00 - CZ00")

mcYears = 1

importAntaresDatas <- function(
  path = "C:\\Users\\lyesb\\Documents\\MED-Tso\\Full%20MedTSO\\Full MedTSO",
  areas_districts_selections, links_selections, mcYears = 1){
  
  links_selections <- tolower(links_selections)
  areas_districts_selections <- tolower(areas_districts_selections)
  
  # Imports :
  opts <- setSimulationPath(path)
  
  # opts$areaList
  # opts$districtList
  # opts$districtsDef
  # opts$areasWithClusters

  areasForDist <- as.character(opts$districtsDef[district %in% areas_districts_selections, area])
  areas_districts_selections_add <- unique(c(areas_districts_selections, areasForDist))
  # import des des donnees avec les areas demandees + necessaire pour les disctricts*
  # pour les resultats a afficher : faire le bon filtre
  data_areas_dist_clust <- readAntares(areas = areas_districts_selections, 
                                       districts = areas_districts_selections,
                                       clusters = areas_districts_selections_add,
                                       timeStep = "annual", select = NULL, mcYears = mcYears)
  
  data_areas_dist_clustH <- readAntares(areas = areas_districts_selections, 
                                        districts = areas_districts_selections,
                                        clusters = areas_districts_selections_add,
                                        links = links_selections, linkCapacity = T,
                                        timeStep = "hourly", select = NULL, mcYears = mcYears)
  
  #FIXME mettre une condition sur le bloc ci-dessous 
  # Enrichissement par la somme des clusters par districts :  
  tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
               data_areas_dist_clust$clusters, by = "area", allow.cartesian = T)
  tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                      by = c("district", "cluster"), .SDcols = c("production", "NP Cost", "NODU")))
  setnames(tmp, "district", "area")
  data_areas_dist_clust$clusters <- 
    rbindlist(list(data_areas_dist_clust$clusters[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
  
  tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
               data_areas_dist_clustH$clusters, by = c("area"), allow.cartesian = T)
  tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                      by = c("district", "cluster", "time"), .SDcols = c("production", "NP Cost", "NODU")))
  setnames(tmp, "district", "area")
  data_areas_dist_clustH$clusters <- 
    rbindlist(list(data_areas_dist_clustH$clusters[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
  
  
  
  dataForSurplus <- readAntares(areas = areas_districts_selections_add,
                                districts = areas_districts_selections,
                                select = "surplus", mcYears = mcYears)
  

  areas_districtsH <- rbindlist(list(data_areas_dist_clustH$areas, data_areas_dist_clustH$districts))
  
  try(setnames(areas_districtsH, "district", "area"), T)
  data_PSP <- areas_districtsH[, .(PSP_positif = sum(PSP[PSP > 0]),
                                   PSP_negatif = sum(PSP[PSP < 0])), by = .(area)] 
  
  data_areas_districts <- rbindlist(list(data_areas_dist_clust$areas, data_areas_dist_clust$districts))
  try(setnames(data_areas_districts, "district", "area"), T)
  data_areas_districts <- merge(data_areas_districts, data_PSP, by = "area")
  
  list(opts = opts,
       data_areas_dist_clust = data_areas_dist_clust, 
       data_areas_dist_clustH = data_areas_dist_clustH,
       dataForSurplus = dataForSurplus,
       data_areas_districts = data_areas_districts)
}


antares_datas <- importAntaresDatas(areas_districts_selections = areas_districts_selections,
  links_selections = links_selections)

vars <- fread("~/MED-Tso/variablesAnnualOutputLong.csv")


require(openxlsx)

options(scipen = 0)

annual_outputs <- format_annualOutputs(data_areas_dist_clust = antares_datas$data_areas_dist_clust,
                                       data_areas_dist_clustH = antares_datas$data_areas_dist_clustH,
                                       dataForSurplus = antares_datas$dataForSurplus,
                                       data_areas_districts = antares_datas$data_areas_districts,
                                       links_selections = tolower(links_selections),
                                       areas_districts_selections = tolower(areas_districts_selections),
                                       vars = vars, opts = antares_datas$opts, data_intro = data_intro)
# data_areas_dist_clust = antares_datas$data_areas_dist_clust
# data_areas_dist_clustH = antares_datas$data_areas_dist_clustH
# dataForSurplus = antares_datas$dataForSurplus
# data_areas_districts = antares_datas$data_areas_districts
# links_selections = links_selections
# areas_districts_selections = areas_districts_selections
# vars = vars

setwd("~/MED-Tso")
infile_name <- "Annual_OutputFile_Template__R.xlsx"
outfile_name <- "C:/Users/lyesb/Documents/MED-Tso/Annual_OutputFile_TEST.xlsx"

export_annual_outputs <- function(infile_name, outfile_name, annual_outputs,
                                  links_selections, areas_districts_selections, data_intro){

  wb <- createWorkbook()
  wb <- loadWorkbook(infile_name)
  options(scipen = 0)
  
  writeData(wb, "Identification", annual_outputs$data_intro)
  writeData(wb, "Identification", toupper(areas_districts_selections), startRow = 14, startCol = 2)
  writeData(wb, "Identification", toupper(links_selections), startRow = 14, startCol = 3)
  writeData(wb, "Identification", mcYears, startRow = 14, startCol = 4)
  
  
  writeData(wb, "Yearly Outputs Short", annual_outputs$data_intro)
  
  writeData(wb, "Yearly Outputs Short", annual_outputs$t_all[,1:2], startRow = 6)
  writeData(wb, "Yearly Outputs Short", annual_outputs$t_all[,-(1:2)], startRow = 6, borders = "all", startCol = 3)
  
  writeData(wb, "Yearly Outputs Short", annual_outputs$t_welfare_block[,1:2], startRow = 42)
  writeData(wb, "Yearly Outputs Short", annual_outputs$t_welfare_block[,-(1:2)], startRow = 42, borders = "all", startCol = 3)
  
  writeData(wb, "Yearly Outputs Short", annual_outputs$line_style, startRow = 48)

  writeData(wb, "Yearly Outputs Short", annual_outputs$data_links_sums, startRow = 50, borders = "all")
  
  
  writeData(wb, "Yearly Outputs Long", annual_outputs$data_intro[,1:2])
  writeData(wb, "Yearly Outputs Long", annual_outputs$data_intro[,-(1:2)], borders = "all", startCol = 3)
  
  writeData(wb, "Yearly Outputs Long", annual_outputs$data_long_out[,1:2], startRow = 6)
  writeData(wb, "Yearly Outputs Long", annual_outputs$data_long_out[,-(1:2)], startRow = 6, borders = "all", startCol = 3)
  
  saveWorkbook(wb, file = outfile_name, overwrite = T)
  
}
# options(scipen = 0)

export_annual_outputs(infile_name = infile_name, outfile_name = outfile_name,
                      annual_outputs = annual_outputs, links_selections = links_selections, 
                      areas_districts_selections = areas_districts_selections, 
                       data_intro = data_intro)


