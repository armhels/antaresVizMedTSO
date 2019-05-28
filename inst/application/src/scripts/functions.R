
require(data.table)
require(antaresRead)
require(antaresProcessing)
require(stringr)
require(openxlsx)


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

#import
importAntaresDatas <- function(
  path = "C:\\Users\\lyesb\\Documents\\MED-Tso\\Full%20MedTSO\\Full MedTSO",
  areas_districts_selections, links_selections, mcYears = 1){
  
  #init :
  data_areas_dist_clust = NULL
  data_areas_dist_clustH = NULL
  dataForSurplus = NULL
  data_areas_districts = NULL
  
  # browser()
  # Imports :
  opts <- setSimulationPath(path)
  
  if(!is.null(areas_districts_selections)){
    
    areas_districts_selections <- tolower(areas_districts_selections)
    areasForDist <- as.character(opts$districtsDef[district %in% areas_districts_selections, area])
    areas_districts_selections_add <- unique(c(areas_districts_selections, areasForDist))
    
    data_areas_dist_clust <- readAntares(areas = areas_districts_selections, 
                                         districts = areas_districts_selections,
                                         clusters = areas_districts_selections_add,
                                         timeStep = "annual", select = NULL, mcYears = mcYears)
  }
  
  if(!is.null(links_selections)){
    
    links_selections <- tolower(links_selections)
    data_areas_dist_clustH <- readAntares(areas = areas_districts_selections, 
                                          districts = areas_districts_selections,
                                          clusters = areas_districts_selections_add,
                                          links = links_selections, linkCapacity = T,
                                          timeStep = "hourly", select = NULL, mcYears = mcYears)
  } else {
    data_areas_dist_clustH <- readAntares(areas = areas_districts_selections, 
                                          districts = areas_districts_selections,
                                          clusters = areas_districts_selections_add,
                                          timeStep = "hourly", select = NULL, mcYears = mcYears)
  }
  # Enrichissement par la somme des clusters par districts :  
  try({
    tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                 data_areas_dist_clust$clusters, by = "area", allow.cartesian = T)
    tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                        by = c("district", "cluster"), .SDcols = c("production", "NP Cost", "NODU")))
    setnames(tmp, "district", "area")
    data_areas_dist_clust$clusters <- 
      rbindlist(list(data_areas_dist_clust$clusters[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
  }, T)
  
  try({
    tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                 data_areas_dist_clustH$clusters, by = c("area"), allow.cartesian = T)
    tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                        by = c("district", "cluster", "time"), .SDcols = c("production", "NP Cost", "NODU")))
    setnames(tmp, "district", "area")
    data_areas_dist_clustH$clusters <- 
      rbindlist(list(data_areas_dist_clustH$clusters[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
  }, T)
  
  
  dataForSurplus <- readAntares(areas = areas_districts_selections_add,
                                districts = areas_districts_selections,
                                select = "surplus", mcYears = mcYears)
  
  
  areas_districtsH <- rbindlist(list(data_areas_dist_clustH$areas, data_areas_dist_clustH$districts))
  
  data_areas_districts <- rbindlist(list(data_areas_dist_clust$areas, data_areas_dist_clust$districts))
  try({setnames(areas_districtsH, "district", "area")}, T)
  try({setnames(data_areas_districts, "district", "area")}, T)
  
  data_PSP <- areas_districtsH[, .(PSP_positif = sum(PSP[PSP > 0]),
                                   PSP_negatif = sum(PSP[PSP < 0])), by = .(area)] 
  data_areas_districts <- merge(data_areas_districts, data_PSP, by = "area")
  
  list(opts = opts,
       data_areas_dist_clust = data_areas_dist_clust, 
       data_areas_dist_clustH = data_areas_dist_clustH,
       dataForSurplus = dataForSurplus,
       data_areas_districts = data_areas_districts)
}


#formatage

format_annualOutputs <- function(data_areas_dist_clust,
                                 data_areas_dist_clustH,
                                 dataForSurplus,
                                 data_areas_districts,
                                 areas_districts_selections,
                                 links_selections,
                                 vars, opts, data_intro){
  
  # init :
  t_all = data.table(); t_welfare_block = data.table(); data_links_sums = data.table()
  data_long_out = data.table()
  # browser()
  if(!(is.null(data_areas_dist_clust))){
    #============================== YEARLY OUTPUT SHORT ========================================
    #=====================================================annual_generation_block =================================
    tmp <- c("Nuclear generation [GWh]",
             "Coal generation [GWh]",
             "Lignite generation [GWh]",
             "Gas generation [GWh]", 
             "Oil generation [GWh]", 
             "Others non-renewable generation [GWh]",
             "Total hydro generation excluding pumping [GWh]",
             "Pump Storage (turbine) [GWh]",
             "Battery Storage discharge (gen.) [GWh]",
             "Wind generation [GWh]",
             "Solar generation [GWh]",
             "Others renewable [GWh]",
             "Total Generation before RES Curtailment [GWh]",
             "RES Curtailment [GWh]")
    
    annual_generation_block <- copy(data_areas_districts)
    annual_generation_block <- 
      annual_generation_block[, c(tmp) := list(
        NUCLEAR, COAL, LIGNITE,
        GAS, OIL, `MIX. FUEL`+`MISC. DTG`, `H. ROR`+`H. STOR`, PSP_positif, NA, WIND, SOLAR, `MISC. NDG`,  #FIXME à checker
        GAS+OIL+`MIX. FUEL`+`MISC. DTG`+`H. ROR`+`H. STOR`+PSP_positif+0+WIND+SOLAR+`MISC. NDG`,`SPIL. ENRG`)]
    annual_generation_block[, "Net Total Generation [GWh]" := get("Total Generation before RES Curtailment [GWh]") - `SPIL. ENRG`]
    annual_generation_block <- annual_generation_block[, c(tmp,"Net Total Generation [GWh]"), with = F]
    
    t_annual_generation_block <- formater_tab(annual_generation_block, "Annual generation [GWh]", 
                                              new_names =  as.character(data_areas_districts$area))
    
    #=====================================================annual_demand_block =================================
    tmp <- c("Native Demand (excl. Pump load & Battery charge) [GWh]",
             "Pump storage consumption [GWh]",
             "Battery Storage charge (load) [GWh]",
             "Net annual country balance [GWh] (Export is positive)",
             "Exchanges with non-modeled nodes [GWh] (Export is positive)",
             "Net Total Demand [GWh]")
    
    
    annual_demand_block <- copy(data_areas_districts)
    annual_demand_block <- 
      annual_demand_block[, c(tmp) := list(
        LOAD, PSP_negatif, NA, BALANCE, `ROW BAL.`,  #FIXME à checker
        LOAD+PSP_negatif+0+BALANCE-`ROW BAL.`)]
    
    annual_demand_block <- annual_demand_block[, tmp, with = F]
    
    t_annual_demand_block <- formater_tab(annual_demand_block, "Annual demand [GWh]", 
                                          new_names =  as.character(data_areas_districts$area))
    
    #=====================================================indicators_block =================================
    tmp <- c("Unserved energy [GWh]", 
             "Loss of load expectation [hour]",
             "Loss of load probability [%]",  
             "Total RES Generation before curtailment [GWh]",
             "Net RES Generation after Curtailment [GWh]",
             "ratio RES/Native demand [%]",
             "RES Curtailment ratio [%]",
             "CO2 Emissions [Mtons]",
             "Overall Cost [M€]",
             "Operating Cost [M€]",
             "Non Proport. costs Thermal [M€]",
             "Marginal Price [€/MWh]")
    
    indicators_block <- copy(data_areas_districts)
    indicators_block <- 
      indicators_block[, c(tmp) := list(
        `UNSP. ENRG`, LOLD, NA,  #FIXME à checker
        `H. STOR`+`H. ROR`+WIND+SOLAR+`MISC. NDG`,
        (`H. STOR`+`H. ROR`+WIND+SOLAR+`MISC. NDG`) - `SPIL. ENRG`,
        ((`H. STOR`+`H. ROR`+WIND+SOLAR+`MISC. NDG`) - `SPIL. ENRG`)/LOAD*100,
        annual_generation_block[,get("RES Curtailment [GWh]")/get("Total Generation before RES Curtailment [GWh]")]*100,
        `CO2 EMIS.`, `OV. COST`, `OP. COST`, `NP COST`, `MRG. PRICE`)]
    
    indicators_block <- indicators_block[, tmp, with = F]
    t_indicators_block <- formater_tab(indicators_block, "Indicators", new_names =  as.character(data_areas_districts$area))
    
    t_all <- rbindlist(list(t_annual_generation_block, t_annual_demand_block, t_indicators_block)) 
    colnames(t_all)[-1] <- c("Output type", toupper(colnames(t_all)[-(1:2)]))
    #=====================================================Welfare System =================================
    # areas_districts_selections
    # dataForSurplus <- readAntares(areas = c("is00", "fr00"),
    #                               select = "surplus", mcYears = mcYears)
    
    surplus <- antaresProcessing::surplus(dataForSurplus)
    
    surplus_districts <- antaresProcessing::surplus(dataForSurplus, groupByDistrict = T)
    surplus <- rbindlist(list(surplus, surplus_districts))
    
    surplus  <- surplus[area %in% tolower(areas_districts_selections)] 
    
    tmp <- c("WELFARE [M€/Year]",
             "PRODUCER SURPLUS [M€/Year]",
             "CONSUMER SURPLUS [M€/Year]",
             "CONGESTION RENT [M€/Year]")
    
    welfare_block <- surplus[, c("area", tmp) := list(
      area, globalSurplus, producerSurplus, consumerSurplus, congestionFees)]
    welfare_block <- welfare_block[, tmp, with = F]
    t_welfare_block <- formater_tab(welfare_block, "Welfare system", new_names = as.character(surplus$area))
    
    try(setcolorder(t_welfare_block, c(colnames(t_welfare_block)[1:2], tolower(colnames(t_all))[-(1:2)])), T)
    # 
    colnames(t_welfare_block)[-1] <- c("Output type", toupper(colnames(t_welfare_block)[-(1:2)]))
    # 
    
  }
  #===================================================== table interconnections =================================
  
  line_style <- data.table(A = "Output type", B = "Interconnection", C = "NTC[MW]", D = " ", E = "Saturation hours [h]",
                           "F" = " ", G = "Energy exchange [GWh]", H = " ")
  names(line_style) <- rep(" ", ncol(line_style))
  
  try({
    if(!(is.null(data_areas_dist_clustH))){
      if(!is.null(data_areas_dist_clustH$links)){
        data_links_sums <- data_areas_dist_clustH$links[, list(
          "NTC A->B" = max(transCapacityDirect),
          "NTC B->A" = min(transCapacityIndirect), 
          "A->B " = sum(`FLOW LIN.`[`FLOW LIN.` > 0]),
          "B->A " = abs(sum(`FLOW LIN.`[`FLOW LIN.` < 0])),
          "A->B" = sum(`CONG. PROB +` > 0),
          "B->A" = sum(`CONG. PROB -` > 0)), by = .(toupper(link))]
      } else {
        data_links_sums <- data_areas_dist_clustH[, list(
          "NTC A->B" = max(transCapacityDirect),
          "NTC B->A" = min(transCapacityIndirect), 
          "A->B " = sum(`FLOW LIN.`[`FLOW LIN.` > 0]),
          "B->A " = abs(sum(`FLOW LIN.`[`FLOW LIN.` < 0])),
          "A->B" = sum(`CONG. PROB +` > 0),
          "B->A" = sum(`CONG. PROB -` > 0)), by = .(toupper(link))]
      }
      data_links_sums[, "Output type" := " "]
      setcolorder(data_links_sums, c("Output type", names(data_links_sums)[-ncol(data_links_sums)]))
    }
  }, T)
  
  if(!(is.null(data_areas_dist_clust))){
    #============================== YEARLY OUTPUT LONG ========================================
    #=====================================Annual generation [GWh] (production / 1000)=================================
    
    data_areas_dist_clust$clusters <- aggregateClusters(data_areas_dist_clust$clusters, "production")
    setnames(data_areas_dist_clust$clusters, "var", "production")
    
    annual_generation <- dcast(data_areas_dist_clust$clusters, cluster ~ area, value.var = "production", fill = 0)
    annual_generation <- merge(vars, annual_generation, by.x = "variable", by.y = "cluster", all.x = T, sort = FALSE )
    
    tmp_block <- copy(data_areas_districts)
    tmp_block <- tmp_block[, .(area, `H. ROR`, `H. STOR`, 
                               "PSP (somme valeurs horaires positives)" = PSP_positif, 
                               "PSP (somme valeurs horaires négatives)" = PSP_negatif, 
                               "MIX. FUEL + MISC. DTG" = `MIX. FUEL` + `MISC. DTG`,
                               WIND, SOLAR, `MISC. NDG`)]
    
    t_tmp_block <- data.table(variable = row.names(t(tmp_block)), t(tmp_block))
    names(t_tmp_block) <- c("variable", unlist(unname(t_tmp_block[1,-1])))
    t_tmp_block <- t_tmp_block[-1,]
    
    annual_generation[, index_order := .I]
    cols_common <- names(annual_generation)[-c(1, 2, ncol(annual_generation))]
    
    annual_generation[variable %in% t_tmp_block$variable, (cols_common) := t_tmp_block[, cols_common, with = F]]
    setorder(annual_generation, "index_order")
    annual_generation[variable %in% t_tmp_block$variable]
    annual_generation[, index_order := NULL]
    
    #=====================================Installed Capacities [MW]=================================
    cluster_desc <- readClusterDesc()
    tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                 cluster_desc, by = "area")
    tmp <- tmp[,.(nominalcapacity = sum(nominalcapacity)), by = c("district", "cluster")]
    setnames(tmp, "district", "area")
    cluster_desc <- 
      rbindlist(list(cluster_desc[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
    
    # Somme des variables finissant par _chiffre :
    cluster_desc <- aggregateClusters(cluster_desc, "nominalcapacity")
    setnames(cluster_desc, "var", "nominalcapacity")
    
    annual_capacity <- dcast(cluster_desc, cluster ~ area, value.var = "nominalcapacity", fill = 0)
    annual_capacity <- merge(vars, annual_capacity, by.x = "variable", by.y = "cluster", all.x = T, sort = FALSE )
    setcolorder(annual_capacity, colnames(annual_generation))
    #=====================================total_income=================================
    # Vide pour l'instant..
    total_income <- copy(annual_capacity)
    total_income[, colnames(total_income)[-(1:2)] := lapply(.SD, function(x) 0), .SDcols =  colnames(total_income)[-(1:2)]]
    
    #=====================================equivalent_full_load=================================
    
    equivalent_full_load <- data.table(
      copy(annual_capacity[,1:2]),
      annual_generation[,-c(1:2)]/1000/copy(annual_capacity)[,-c(1:2)])
    cols_num <- colnames(equivalent_full_load)[-(1:2)]
    equivalent_full_load[, (cols_num) := lapply(.SD, function(x) {x[is.nan(x)] <- 0 ; x}), .SDcols = (cols_num)]
    
    #=============================== block intermediaire =========================================
    tmp <- c( "Total hydro generation excluding pumping [GWh]",	
              "Native Demand (excl. Pump load & Battery charge) [GWh]",	
              "Pump storage consumption [GWh]",	
              "Net annual country balance [GWh] (Export is positive)",	
              "Exchanges with non-modeled nodes [GWh] (Export is positive)",	
              "Annual country balance excl. exchanges wih non-modeled nodes [GWh] (Export is positive)",	
              "Dump energy [GWh]",	
              "Unserved energy [GWh]",	
              "Loss of load expectation [hour]",  	
              "CO2 Emissions [tons]")
    
    tmp_block_comp <- copy(data_areas_districts)
    tmp_block_comp <- tmp_block_comp[, c(tmp) := list(
      `H. STOR`+`H. ROR`, LOAD,
      PSP_negatif, BALANCE, `ROW BAL.`, BALANCE- `ROW BAL.`, `SPIL. ENRG`,
      `UNSP. ENRG`, LOLD, `CO2 EMIS.`)]
    tmp_block_comp <- tmp_block_comp[, tmp, with = F]
    tmp_block_comp <- formater_tab(tmp_block_comp, output_type = NULL, new_names =  as.character(data_areas_districts$area))
    
    #=============================== Overall costs =========================================
    tmp <- c("Overall costs [M€]",
             "Marginal Cost Yearly Average [€]",
             "Marginal Cost Yearly Average (excl. 3000 €/MWh) [€]",
             "Pan-EU Marginal Cost Yearly Average (excl. 3000 €/MWh) Weighted By Demand [€]")
    overall_block <- copy(data_areas_districts)
    overall_block <- overall_block[, c(tmp) := list(`OV. COST`, `MRG. PRICE`, NA, NA)]
    overall_block <- overall_block[, tmp, with = F]
    overall_block <- formater_tab(overall_block, output_type = NULL, new_names =  as.character(data_areas_districts$area))
    
    #============================== YEARLY OUTPUT LONG - xlsx========================================
    
    #Renommage de la premiere colonne :
    annual_generation$variable <- "Annual generation [GWh]"
    tmp_block_comp[, variable := `Output type`] ; tmp_block_comp[, `Output type` := " "] ; setcolorder(tmp_block_comp, c("variable", "Output type", colnames(tmp_block_comp)[2:(ncol(tmp_block_comp)-1)]))
    equivalent_full_load$variable <- "Equivalent full load hours [hours]"
    total_income$variable <- "Total income [M€]"
    annual_capacity$variable <- "Installed Capacities [MW]"
    overall_block[, variable := `Output type`] ; overall_block[, `Output type` := " "] ; setcolorder(overall_block, c("variable", "Output type", colnames(overall_block)[2:(ncol(overall_block)-1)]))
    
    data_long_out <- rbindlist(list(annual_generation, tmp_block_comp, equivalent_full_load, 
                                    total_income, annual_capacity, overall_block), fill = T)
    setnames(data_long_out, "variable", "Output type")
    
    colnames(data_long_out)[-(1:2)] <- toupper(colnames(data_long_out)[-(1:2)])
  }
  try({
    t_all[, colnames(t_all)[-c(1,2)] := lapply(.SD, function(x) {
      x[is.nan(x)] <- 0 ; 
      x[is.infinite(x)] <- 0;
      x[is.na(x)] <- 0; x}), .SDcols = (colnames(t_all)[-c(1,2)])]
    t_welfare_block[, colnames(t_welfare_block)[-c(1,2)] := lapply(.SD, function(x) {
      x[is.nan(x)] <- 0 ; 
      x[is.infinite(x)] <- 0;
      x[is.na(x)] <- 0; x}), .SDcols = (colnames(t_welfare_block)[-c(1,2)])]
    data_long_out[, colnames(data_long_out)[-c(1,2)] := lapply(.SD, function(x) {
      x[is.nan(x)] <- 0 ; 
      x[is.infinite(x)] <- 0;
      x[is.na(x)] <- 0; x}), .SDcols = (colnames(data_long_out)[-c(1,2)])]
  }, T)
  
  list(t_all = t_all, t_welfare_block = t_welfare_block, line_style = line_style, data_links_sums = data_links_sums,
       data_intro = data_intro, data_long_out = data_long_out) 
}


#export
export_annual_outputs <- function(infile_name, outfile_name, annual_outputs,
                                  links_selections, areas_districts_selections, data_intro){
  
  wb <- createWorkbook()
  wb <- loadWorkbook(infile_name)
  options(scipen = 0)
  try({
    
    writeData(wb, "Identification", annual_outputs$data_intro[-4,])
    writeData(wb, "Identification", toupper(areas_districts_selections), startRow = 14, startCol = 2)
    writeData(wb, "Identification", toupper(links_selections), startRow = 14, startCol = 3)
    writeData(wb, "Identification", mcYears, startRow = 14, startCol = 4)
  }, T)
  
  try({
    
    writeData(wb, "Yearly Outputs Short", annual_outputs$data_intro)
    
    writeData(wb, "Yearly Outputs Short", annual_outputs$t_all[,1:2], startRow = 6)
    writeData(wb, "Yearly Outputs Short", annual_outputs$t_all[,-(1:2)], startRow = 6, borders = "all", startCol = 3)
    
    writeData(wb, "Yearly Outputs Short", annual_outputs$t_welfare_block[,1:2], startRow = 42)
    writeData(wb, "Yearly Outputs Short", annual_outputs$t_welfare_block[,-(1:2)], startRow = 42, borders = "all", startCol = 3)
    
    writeData(wb, "Yearly Outputs Short", annual_outputs$line_style, startRow = 48)
  }, T)
  
  try({
    writeData(wb, "Yearly Outputs Short", annual_outputs$data_links_sums, startRow = 50, borders = "all")
  }, T)
  try({
    writeData(wb, "Yearly Outputs Long", annual_outputs$data_intro[,1:2])
    writeData(wb, "Yearly Outputs Long", annual_outputs$data_intro[,-(1:2)], borders = "all", startCol = 3)
    
    writeData(wb, "Yearly Outputs Long", annual_outputs$data_long_out[,1:2], startRow = 6)
    writeData(wb, "Yearly Outputs Long", annual_outputs$data_long_out[,-(1:2)], startRow = 6, borders = "all", startCol = 3)
    
  }, T)
  saveWorkbook(wb, file = outfile_name, overwrite = T)
  
}






#============================== Hourly output ========================================

makeTabStats <- function(data){
  dt <- data.table("Min [MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) min(x, na.rm = T))]),
                   "Max[MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) max(x, na.rm = T))]),
                   "Avg [MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) sd(x, na.rm = T))]),
                   "SUM [GWh]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) sum(as.numeric(x), na.rm = T)/1000)]))
  dt <- dt[, lapply(.SD, round)]
  dt[, colnames(dt) := lapply(.SD, function(x) {x[is.nan(x)] <- 0 ; x[is.infinite(x)] <- 0; x}), .SDcols = colnames(dt)]
  dt <- data.table(v1 = colnames(dt), t(dt))
  colnames(dt) <- rep(" ", ncol(dt))
  dt  
}

#import
importAntaresDatas_hourly <- function(
  path = "C:\\Users\\lyesb\\Documents\\MED-Tso\\Full%20MedTSO\\Full MedTSO",
  areas_districts_selections, links_selections, mcYears = 1){
  
  #init :
  data_areasH = NULL
  data_linksH = NULL
  
  # Imports :
  opts <- setSimulationPath(path)
  
  if(!is.null(areas_districts_selections)){
    
    areas_districts_selections <- tolower(areas_districts_selections)
    areasForDist <- as.character(opts$districtsDef[district %in% areas_districts_selections, area])
    areas_districts_selections_add <- unique(c(areas_districts_selections, areasForDist))
    
    data_areasH <- readAntares(areas = areas_districts_selections, 
                               districts = areas_districts_selections,
                               clusters = areas_districts_selections_add,
                               timeStep = "hourly", select = NULL, mcYears = mcYears)
  }
  
  if(!is.null(links_selections)){
    
    links_selections <- tolower(links_selections)
    data_linksH <- readAntares(links = links_selections, linkCapacity = T,
                               timeStep = "hourly", select = NULL, mcYears = mcYears)
  }
  
  # Enrichissement par la somme des clusters par districts :  
  try({
    tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                 data_areasH$clusters, by = c("area"), allow.cartesian = T)
    tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                        by = c("district", "cluster", "time"), .SDcols = c("production", "NP Cost", "NODU")))
    setnames(tmp, "district", "area")
    data_areasH$clusters <- 
      rbindlist(list(data_areasH$clusters[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
  }, T)
  
  list(data_areasH = data_areasH, data_linksH = data_linksH)
}

# formatage
format_hourlyOutputs <- function(data_areas, data_links, areas_selections, 
                                 market_data_code, links_selections, dico = dico){
  data_out <- NA ; dt_stats <- NA ; areas_districts <- NA
  try({
    
    areas_selections <- tolower(areas_selections)
    data_areas$clusters <- aggregateClusters(data_areas$clusters, "production", hourly = T)
    
    data_areas_dist <- rbindlist(list(data_areas$areas, data_areas$districts))
    try({setnames(data_areas_dist, "district", "area")}, T)
    
    cols_areas <- intersect(colnames(data_areas$areas), market_data_code)
    
    data_areas_dist <- 
      data_areas_dist[area %in% areas_selections, 
                      c(colnames(data_areas_dist)[c(1,4)], cols_areas), with = F]
    
    temp <- dcast(data_areas$clusters, area + time ~ cluster, value.var = "var", fill = 0)
    
    data_all <- merge(temp, data_areas_dist, by = c("area", "time"), all = TRUE)
    data_all <- data_all[, c("area", "time", intersect(market_data_code, colnames(data_all))), with = F]
    
    # ajout d'éventuelles variables demandées mais qui n'existent pas 
    cols_comp <- setdiff(market_data_code, colnames(data_all))
    try({data_all[, (cols_comp) :=  0]}, T)
    
    cols_availables <- intersect(market_data_code, colnames(data_all))
    
    if("PSP" %in% cols_availables) {
      data_all[, c("PSP_pos", "PSP_neg") := list(ifelse(PSP > 0, PSP, 0), ifelse(PSP < 0, PSP, 0))]
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
    
    vars_miss <- setdiff(cols_availables, unique(dico[, ANTARES_naming]))
    dico <- rbindlist(list(dico, data.table(ANTARES_naming = vars_miss, Category = vars_miss)))
    setkey(dico, ANTARES_naming)
    colnames(areas_districts) <- dico[colnames(data_out)[-c(1:2)]]$Category
    dt_stats <- makeTabStats(data_out)
  }, T)
  
  #============================== Crossborder exchanges ========================================
  if(!is.null(data_links)){
    
    data_out_2 <- dcast(data_links, time ~ link, value.var = "FLOW LIN.")
    data_out_2[, Hour := .I]
    setnames(data_out_2, "time", "Date")
    setcolorder(data_out_2, c("Hour", "Date", tolower(links_selections)))
    colnames(data_out_2)[-(1:2)] <- toupper(colnames(data_out_2)[-(1:2)])
    dt_stats_2 <- makeTabStats(data_out_2)
  } else{
    data_out_2 <- NA ; dt_stats_2 <- NA
  } 
  
  list(data_out = data_out, dt_stats = dt_stats, areas_districts = areas_districts,
       data_out_2 = data_out_2, dt_stats_2 = dt_stats_2)
  
}

# export
export_hourlyOutputs <- function(infile_name, outfile_name, hourly_outputs, data_intro, 
                                 areas_districts_selections, market_data_code, links_selections){
  
  wb <- loadWorkbook(infile_name)
  
  writeData(wb, "Identification", data_intro[-4,])
  writeData(wb, "Identification", areas_districts_selections, startRow = 14, startCol = 2)
  writeData(wb, "Identification", market_data_code, startRow = 14, startCol = 2)
  writeData(wb, "Identification", links_selections, startRow = 14, startCol = 3)
  writeData(wb, "Identification", data_intro[4, `2030 - Scenario 1`], startRow = 14, startCol = 4)
  
  writeData(wb, "Hourly Market Data", data_intro[1:3,])
  writeData(wb, "Hourly Market Data", hourly_outputs$dt_stats, startRow = 5, startCol = 2)
  
  writeData(wb, "Hourly Market Data", hourly_outputs$areas_districts, startRow = 11,  startCol = 3)
  
  writeData(wb, "Hourly Market Data", hourly_outputs$data_out, startRow = 13)
  
  writeData(wb, "Crossborder exchanges", data_intro[1:3,])
  writeData(wb, "Crossborder exchanges", hourly_outputs$dt_stats_2, startRow = 5, startCol = 2)
  writeData(wb, "Crossborder exchanges", hourly_outputs$data_out_2, startRow = 11)
  
  system.time(saveWorkbook(wb, outfile_name, overwrite = T))
}

