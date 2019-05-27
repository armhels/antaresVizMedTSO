format_annualOutputs <- function(data_areas_dist_clust,
                                 data_areas_dist_clustH,
                                 dataForSurplus,
                                 data_areas_districts,
                                 areas_districts_selections,
                                 links_selections,
                                 vars, opts, data_intro){
  
  
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
  
  #===================================================== table interconnections =================================
  
  line_style <- data.table(A = "Output type", B = "Interconnection", C = "NTC[MW]", D = " ", E = "Saturation hours [h]",
                           "F" = " ", G = "Energy exchange [GWh]", H = " ")
  names(line_style) <- rep(" ", ncol(line_style))
  data_links_sums <- data_areas_dist_clustH$links[, list(
    "NTC A->B" = max(transCapacityDirect),
    "NTC B->A" = min(transCapacityIndirect), 
    "A->B " = sum(`FLOW LIN.`[`FLOW LIN.` > 0]),
    "B->A " = abs(sum(`FLOW LIN.`[`FLOW LIN.` < 0])),
    "A->B" = sum(`CONG. PROB +` > 0),
    "B->A" = sum(`CONG. PROB -` > 0)), by = .(toupper(link))]
  data_links_sums[, "Output type" := " "]
  setcolorder(data_links_sums, c("Output type", names(data_links_sums)[-ncol(data_links_sums)]))
  
  
  #============================== YEARLY OUTPUT LONG ========================================
  #=====================================Annual generation [GWh] (production / 1000)=================================
  
  data_areas_dist_clust$clusters <- aggregateClusters(data_areas_dist_clust$clusters, "production")
  setnames(data_areas_dist_clust$clusters, "var", "production")
  
  annual_generation <- dcast(data_areas_dist_clust$clusters, cluster ~ area, value.var = "production", fill = NA)
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
  
  annual_capacity <- dcast(cluster_desc, cluster ~ area, value.var = "nominalcapacity", fill = NA)
  annual_capacity <- merge(vars, annual_capacity, by.x = "variable", by.y = "cluster", all.x = T, sort = FALSE )
  setcolorder(annual_capacity, colnames(annual_generation))
  #=====================================total_income=================================
  # Vide pour l'instant..
  total_income <- copy(annual_capacity)
  total_income[, colnames(total_income)[-(1:2)] := lapply(.SD, function(x) NA), .SDcols =  colnames(total_income)[-(1:2)]]
  
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
  
  list(t_all = t_all, t_welfare_block = t_welfare_block, line_style = line_style, data_links_sums = data_links_sums,
       data_intro = data_intro, data_long_out = data_long_out) 
}