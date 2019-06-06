require(data.table)
require(antaresRead)
require(antaresProcessing)
require(stringr)
require(openxlsx)


# Somme des variables finissant par _chiffre :
aggregateClusters <- function(data, var, hourly = F, div = 1){
  data <- copy(data)
  ind <- grep("_[0-9]$", data[["cluster"]])
  data[ind, cluster := gsub(pattern = "_[0-9]$", "", cluster)]
  if(hourly){
    data <- data[, .(var = sum(get(var)) / div), by = .(area, cluster, time)]  
  }else {
    data <- data[, .(var = sum(get(var)) / div), by = .(area, cluster)]
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

# import
importAntaresDatasAnnual <- function(opts, areas_districts_selections, links_selections, mcYears = 1, 
                                     removeVirtualAreas = FALSE,
                                     storageFlexibility = NULL, production = NULL,
                                     reassignCosts = FALSE, newCols = TRUE, rowBal = TRUE){
  
  #init :
  data_areas_dist_clust = NULL
  data_areas_dist_clustH = NULL
  dataForSurplus = NULL
  data_areas_districts = NULL
  
  if(length(storageFlexibility) > 0) storageFlexibility <- tolower(storageFlexibility)
  if(length(production) > 0) production <- tolower(production)
  
  # browser()
  # Imports :
  
  opts$districtsDef[, district := tolower(district)]
  opts$districtsDef[, area := tolower(area)]
  
  if("all" %in% links_selections){
    links_selections <- unique(tolower(opts$linkList))
    if(removeVirtualAreas && (length(storageFlexibility) > 0 || length(production) > 0)){
      ind_rm <- grepl(paste(paste0("(", c(storageFlexibility, production), ")"), collapse = "|"), 
                      links_selections)
      links_selections <- links_selections[!ind_rm]
    }
  }
  if("all" %in% areas_districts_selections){
    areas_districts_selections <- unique(c(tolower(opts$areaList), tolower(opts$districtList)))
    
    if(removeVirtualAreas && (length(storageFlexibility) > 0 || length(production) > 0)){
      ind_rm <- grepl(paste(paste0("(", c(storageFlexibility, production), ")"), collapse = "|"), 
                      areas_districts_selections)
      areas_districts_selections <- areas_districts_selections[!ind_rm]
    }
  }
  
  if(length(areas_districts_selections) > 0){
    
    
    areas_districts_selections <- tolower(areas_districts_selections)
    areasForDist <- as.character(opts$districtsDef[tolower(district) %in% areas_districts_selections, tolower(area)])
    areas_districts_selections_add <- unique(c(areas_districts_selections, areasForDist))
    
    areas_selection <- intersect(areas_districts_selections, tolower(opts$areaList))
    districts_selection <- intersect(areas_districts_selections, tolower(opts$districtList))
    
    areas_clusters_selection <- intersect(areas_districts_selections_add, tolower(opts$areasWithClusters))
    
    if(!removeVirtualAreas){
      data_areas_dist_clust <- readAntares(areas = areas_selection, 
                                           districts = districts_selection,
                                           clusters = areas_clusters_selection,
                                           links = NULL,
                                           timeStep = "annual", select = NULL, mcYears = mcYears)
      
    } else {
      
      data_areas_dist_clust <- readAntares(areas = "all", 
                                           districts = "all",
                                           clusters = "all",
                                           links = "all",
                                           timeStep = "annual", select = NULL, mcYears = mcYears)
      
      data_areas_dist_clust <- suppressWarnings({removeVirtualAreas(data_areas_dist_clust, 
                                                                    storageFlexibility = storageFlexibility, production = production,
                                                                    reassignCosts = reassignCosts, newCols = newCols, rowBal = rowBal)})
      
      if(!is.null(data_areas_dist_clust$areas) && nrow(data_areas_dist_clust$areas) > 0){
        data_areas_dist_clust$areas <- data_areas_dist_clust$areas[area %in% areas_selection, ]
      }
      
      if(!is.null(data_areas_dist_clust$clusters) && nrow(data_areas_dist_clust$clusters) > 0){
        data_areas_dist_clust$clusters[area %in% areas_clusters_selection, ]
      }
      
      if(!is.null(data_areas_dist_clust$districts) && nrow(data_areas_dist_clust$districts) > 0){
        data_areas_dist_clust$districts <- data_areas_dist_clust$districts[district %in% districts_selection, ]
      }
      
      data_areas_dist_clust$links <- NULL
      gc(reset = T)
      
    }
    
  } else {
    areas_selection <- NULL
    districts_selection <- NULL
    areas_clusters_selection <- NULL
    areas_districts_selections_add <- NULL
  }
  
  links_selections <- tolower(links_selections)
  linkCapacity <- ifelse(is.null(links_selections), F, T)
  
  
  # if(!removeVirtualAreas){
  #   
  #  
  #   
  #   data_areas_dist_clustH <- readAntares(areas = intersect(areas_districts_selections_add, tolower(opts$areaList)), 
  #                                         districts = districts_selection,
  #                                         clusters = areas_clusters_selection,
  #                                         links = links_selections, linkCapacity = linkCapacity,
  #                                         timeStep = "hourly", 
  #                                         select = NULL, mcYears = mcYears)
  #   
  #   surplus <- suppressWarnings({antaresProcessing::surplus(data_areas_dist_clustH)}) 
  #   surplus_districts <- suppressWarnings({antaresProcessing::surplus(data_areas_dist_clustH, groupByDistrict = T)})
  #   surplus <- rbindlist(list(surplus, surplus_districts), use.names = FALSE)
  #   
  #   data_areas_dist_clustH$areas <- data_areas_dist_clustH$areas[area %in% areas_selection, list(area, mcYear, timeId, time, day, month, hour, PSP)]
  #   data_areas_dist_clustH$clusters <- data_areas_dist_clustH$clusters[area %in% areas_clusters_selection, ]
  #   data_areas_dist_clustH$districts <- data_areas_dist_clustH$districts[district %in% districts_selection, list(district, mcYear, timeId, time, day, month, hour, PSP)]
  #   gc(reset = T)
  #   
  # } else {
  
  data_areas_dist_clustH <- readAntares(areas = "all", 
                                        districts = "all",
                                        clusters = "all",
                                        links = "all",
                                        timeStep = "hourly", 
                                        select = NULL, 
                                        linkCapacity = linkCapacity, mcYears = mcYears)
  
  if(removeVirtualAreas){
    data_areas_dist_clustH <- suppressWarnings({removeVirtualAreas(data_areas_dist_clustH, storageFlexibility = storageFlexibility, production = production,
                                                                   reassignCosts = reassignCosts, newCols = newCols, rowBal = rowBal)})
    
  }
  
  if(!is.null(mcYears)){
    
    # fix surplus
    if(removeVirtualAreas && length(storageFlexibility) > 0){
      for(v in tolower(storageFlexibility)){
        if(!v %in% colnames(data_areas_dist_clustH$areas)){
          data_areas_dist_clustH$areas[, c(v) := 0]
        }
      }
    }
    
    if(removeVirtualAreas && length(production) > 0){
      for(v in tolower(production)){
        if(!v %in% colnames(data_areas_dist_clustH$areas)){
          data_areas_dist_clustH$areas[, c(v) := 0]
        }
      }
    }
    
    surplus <- suppressWarnings({antaresProcessing::surplus(data_areas_dist_clustH)}) 
    surplus_districts <- suppressWarnings({antaresProcessing::surplus(data_areas_dist_clustH, groupByDistrict = T)})
    surplus <- rbindlist(list(surplus, surplus_districts), use.names = FALSE)
    
    if(!is.null(data_areas_dist_clustH$areas) && nrow(data_areas_dist_clustH$areas) > 0){
      data_areas_dist_clustH$areas <- data_areas_dist_clustH$areas[area %in% areas_selection, list(area, mcYear, timeId, time, day, month, hour, PSP)]
    }
    
    if(!is.null(data_areas_dist_clustH$clusters) && nrow(data_areas_dist_clustH$clusters) > 0){
      data_areas_dist_clustH$clusters <- data_areas_dist_clustH$clusters[area %in% areas_clusters_selection, ]
    }
    
    if(!is.null(data_areas_dist_clustH$districts) && nrow(data_areas_dist_clustH$districts) > 0){
      data_areas_dist_clustH$districts <- data_areas_dist_clustH$districts[district %in% districts_selection, list(district, mcYear, timeId, time, day, month, hour, PSP)]
    }
    
    if(!is.null(data_areas_dist_clustH$links) && nrow(data_areas_dist_clustH$links) > 0){
      data_areas_dist_clustH$links <- data_areas_dist_clustH$links[link %in% links_selections, ]
    }
    
  } else {
    surplus <- data.table(area = character(0), globalSurplus = numeric(0), 
                          producerSurplus = numeric(0), consumerSurplus = numeric(0), congestionFees = numeric(0))
    
    if(!is.null(data_areas_dist_clustH$areas) && nrow(data_areas_dist_clustH$areas) > 0){
      data_areas_dist_clustH$areas <- data_areas_dist_clustH$areas[area %in% areas_selection, list(area, timeId, time, day, month, hour, PSP)]
    }
    
    if(!is.null(data_areas_dist_clustH$clusters) && nrow(data_areas_dist_clustH$clusters) > 0){
      data_areas_dist_clustH$clusters <- data_areas_dist_clustH$clusters[area %in% areas_clusters_selection, ]
    }
    
    if(!is.null(data_areas_dist_clustH$districts) && nrow(data_areas_dist_clustH$districts) > 0){
      data_areas_dist_clustH$districts <- data_areas_dist_clustH$districts[district %in% districts_selection, list(district, timeId, time, day, month, hour, PSP)]
    }
    
    if(!is.null(data_areas_dist_clustH$links) && nrow(data_areas_dist_clustH$links) > 0){
      data_areas_dist_clustH$links <- data_areas_dist_clustH$links[link %in% links_selections, ]
    }
  }
  
  
  gc(reset = T)
  
  # }
  
  
  # Enrichissement par la somme des clusters par districts : 
  if(!is.null(data_areas_dist_clust)){
    try({
      tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                   data_areas_dist_clust$clusters, by = "area", allow.cartesian = T)
      tmp[, c("production", "NP Cost", "NODU") := list(as.numeric(production), as.numeric(`NP Cost`), as.numeric(NODU))]
      tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                          by = c("district", "cluster"), .SDcols = c("production", "NP Cost", "NODU")))
      setnames(tmp, "district", "area")
      data_areas_dist_clust$clusters <- rbindlist(list(data_areas_dist_clust$clusters[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
      rm(tmp)
      gc()
    }, T)
  }
  
  areas_districtsH <- rbindlist(list(data_areas_dist_clustH$areas, data_areas_dist_clustH$districts), use.names=FALSE)
  data_areas_districts <- rbindlist(list(data_areas_dist_clust$areas, data_areas_dist_clust$districts), use.names=FALSE)
  
  if((!is.null(data_areas_districts) && nrow(data_areas_districts) > 0) & !is.null(areas_districtsH) && nrow(areas_districtsH) > 0){
    data_PSP <- areas_districtsH[, .(PSP_positif = sum(PSP[PSP > 0]),
                                     PSP_negatif = sum(PSP[PSP < 0])), by = .(area)] 
    
    rm(areas_districtsH)
    gc()
    
    data_areas_districts <- merge(data_areas_districts, data_PSP, by = "area", all.x = TRUE)
  }
  
  
  list(opts = opts,
       data_areas_dist_clust = data_areas_dist_clust, 
       data_areas_dist_clustH = data_areas_dist_clustH,
       dataForSurplus = surplus,
       data_areas_districts = data_areas_districts, 
       areas_districts_selections = areas_districts_selections, 
       links_selections = links_selections)
}


#formatage

formatAnnualOutputs <- function(data_areas_dist_clust,
                                data_areas_dist_clustH,
                                dataForSurplus,
                                data_areas_districts,
                                areas_districts_selections,
                                links_selections,
                                vars, opts, data_intro){
  
  # init :
  # browser()
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
        NUCLEAR / 1000, COAL  / 1000, LIGNITE  / 1000,
        GAS / 1000, OIL  / 1000, (`MIX. FUEL`+`MISC. DTG`)  / 1000, 
        (`H. ROR`+`H. STOR`)  / 1000, PSP_positif  / 1000, NA, 
        WIND  / 1000, SOLAR  / 1000, `MISC. NDG`  / 1000,  #FIXME à checker
        (GAS+OIL+`MIX. FUEL`+`MISC. DTG`+`H. ROR`+`H. STOR`+PSP_positif+0+WIND+SOLAR+`MISC. NDG`)  / 1000,
        `SPIL. ENRG` / 1000)]
    annual_generation_block[, "Net Total Generation [GWh]" := get("Total Generation before RES Curtailment [GWh]") - (`SPIL. ENRG`  / 1000)]
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
        LOAD  / 1000, PSP_negatif  / 1000, NA, BALANCE  / 1000, `ROW BAL.`  / 1000,  #FIXME à checker
        (LOAD+PSP_negatif+0+BALANCE-`ROW BAL.`)  / 1000)]
    
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
        `UNSP. ENRG`  / 1000, LOLD, NA,  #FIXME à checker
        (`H. STOR`+`H. ROR`+WIND+SOLAR+`MISC. NDG`)  / 1000,
        ((`H. STOR`+`H. ROR`+WIND+SOLAR+`MISC. NDG`) - `SPIL. ENRG`)  / 1000,
        ((`H. STOR`+`H. ROR`+WIND+SOLAR+`MISC. NDG`) - `SPIL. ENRG`)/LOAD*100,
        annual_generation_block[,get("RES Curtailment [GWh]")/get("Total Generation before RES Curtailment [GWh]")]*100,
        `CO2 EMIS.`/ 1000000, `OV. COST` / 1000000, `OP. COST` / 1000000, `NP COST`  / 1000000, `MRG. PRICE`)]
    
    indicators_block <- indicators_block[, tmp, with = F]
    t_indicators_block <- formater_tab(indicators_block, "Indicators", new_names =  as.character(data_areas_districts$area))
    
    t_all <- rbindlist(list(t_annual_generation_block, t_annual_demand_block, t_indicators_block)) 
    colnames(t_all)[-1] <- c("Output type 2", toupper(colnames(t_all)[-(1:2)]))
    
    # order
    template <- data.table()
    template[, c("Output type", "Output type 2") := character(0)]
    template[, c(toupper(areas_districts_selections)) := numeric(0)]
    t_all <- rbindlist(list(template, t_all), fill = T)
    
    #=====================================================Welfare System =================================
    surplus  <- dataForSurplus[area %in% tolower(areas_districts_selections)] 
    
    tmp <- c("WELFARE [M€/Year]",
             "PRODUCER SURPLUS [M€/Year]",
             "CONSUMER SURPLUS [M€/Year]",
             "CONGESTION RENT [M€/Year]")
    
    welfare_block <- surplus[, c("area", tmp) := list(
      area, globalSurplus / 1000000, producerSurplus / 1000000, consumerSurplus / 1000000, congestionFees / 1000000)]
    welfare_block <- welfare_block[, tmp, with = F]
    t_welfare_block <- formater_tab(welfare_block, "Welfare system", new_names = as.character(surplus$area))
    
    colnames(t_welfare_block)[-1] <- c("Output type 2", toupper(colnames(t_welfare_block)[-(1:2)]))
    
    # order
    template <- data.table()
    template[, c("Output type", "Output type 2") := character(0)]
    template[, c(toupper(areas_districts_selections)) := numeric(0)]
    t_welfare_block <- rbindlist(list(template, t_welfare_block), fill = T)  
    
  }
  #===================================================== table interconnections =================================
  
  line_style <- data.table(A = "Output type", 
                           B = "Interconnection", 
                           C = "NTC[MW]", 
                           D = " ", 
                           E = "Saturation hours [h]",
                           "F" = " ", 
                           G = "Energy exchange [GWh]", 
                           H = " ")
  names(line_style) <- rep(" ", ncol(line_style))
  
  try({
    if(!(is.null(data_areas_dist_clustH))){
      if(!is.null(data_areas_dist_clustH$links) && nrow(data_areas_dist_clustH$links) > 0){
        data_links_sums <- data_areas_dist_clustH$links[, list(
          "NTC A->B" = max(transCapacityDirect),
          "NTC B->A" = min(transCapacityIndirect), 
          "A->B" = sum(`CONG. PROB +` > 0),
          "B->A" = sum(`CONG. PROB -` > 0),
          "A->B " = sum(`FLOW LIN.`[`FLOW LIN.` > 0]) / 1000,
          "B->A " = abs(sum(`FLOW LIN.`[`FLOW LIN.` < 0])) / 1000), by = list(link = toupper(link))]
      } else {
        data_links_sums <- data_areas_dist_clustH[, list(
          "NTC A->B" = max(transCapacityDirect),
          "NTC B->A" = min(transCapacityIndirect), 
          "A->B" = sum(`CONG. PROB +` > 0),
          "B->A" = sum(`CONG. PROB -` > 0),
          "A->B " = sum(`FLOW LIN.`[`FLOW LIN.` > 0]) / 1000,
          "B->A " = abs(sum(`FLOW LIN.`[`FLOW LIN.` < 0])) / 1000), by = list(link = toupper(link))]
      }
      setorder(data_links_sums[, .r := match(toupper(links_selections), link)], .r)[, .r := NULL]
      data_links_sums[, "Output type" := " "]
      setcolorder(data_links_sums, c("Output type", names(data_links_sums)[-ncol(data_links_sums)]))
    }
  }, T)
  
  if(!(is.null(data_areas_dist_clust))){
    #============================== YEARLY OUTPUT LONG ========================================
    #=====================================Annual generation [GWh] (production / 1000)=================================
    
    
    data_areas_dist_clust$clusters <- aggregateClusters(data_areas_dist_clust$clusters, "production", div = 1000)
    setnames(data_areas_dist_clust$clusters, "var", "production")
    
    annual_generation <- dcast(data_areas_dist_clust$clusters, cluster ~ area, value.var = "production", fill = 0)
    annual_generation <- merge(vars, annual_generation, by.x = "variable", by.y = "cluster", all.x = T, sort = FALSE )
    
    template <- data.table()
    template[, c("variable", "Output type") := character(0)]
    template[, c(tolower(areas_districts_selections)) := numeric(0)]
    annual_generation <- rbindlist(list(template, annual_generation), fill = T)
    
    tmp_block <- copy(data_areas_districts)
    tmp_block <- tmp_block[, .(area, "H. ROR" = `H. ROR`/ 1000, "H. STOR" = `H. STOR`/ 1000, 
                               "PSP (somme valeurs horaires positives)" = PSP_positif / 1000, 
                               "PSP (somme valeurs horaires négatives)" = PSP_negatif / 1000, 
                               "MIX. FUEL + MISC. DTG" = (`MIX. FUEL` + `MISC. DTG`) / 1000,
                               "WIND" = WIND / 1000, "SOLAR" = SOLAR / 1000, "MISC. NDG" = `MISC. NDG` / 1000)]
    
    t_tmp_block <- data.table(variable = colnames(tmp_block)[-1], t(tmp_block[, -1]))
    names(t_tmp_block) <- c("variable", as.character(tmp_block$area))
    t_tmp_block[, c(names(t_tmp_block)[-1]) := lapply(.SD, function(x) as.numeric(x)), .SDcol = names(t_tmp_block)[-1]]
    
    template <- data.table()
    template[, c("variable") := character(0)]
    template[, c(tolower(areas_districts_selections)) := numeric(0)]
    t_tmp_block <- rbindlist(list(template, t_tmp_block), fill = T)
    
    annual_generation[, index_order := .I]
    cols_common <- names(annual_generation)[-c(1, 2, ncol(annual_generation))]
    
    order_var <- match(t_tmp_block$variable, annual_generation$variable)
    annual_generation[order_var, (cols_common) := t_tmp_block[, cols_common, with = F]]
    setorder(annual_generation, "index_order")
    annual_generation[, index_order := NULL]
    
    #=====================================Installed Capacities [MW]=================================
    cluster_desc <- readClusterDesc(opts = opts)
    if(!is.null(cluster_desc) && nrow(cluster_desc) > 0){
      cluster_desc[, nominalcapacity := nominalcapacity *  unitcount]
    }
    
    tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                 cluster_desc, by = "area")
    tmp <- tmp[,.(nominalcapacity = sum(nominalcapacity)), by = c("district", "cluster")]
    setnames(tmp, "district", "area")
    cluster_desc <- rbindlist(list(cluster_desc[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
    
    # Somme des variables finissant par _chiffre :
    cluster_desc <- aggregateClusters(cluster_desc, "nominalcapacity")
    setnames(cluster_desc, "var", "nominalcapacity")
    
    annual_capacity <- dcast(cluster_desc, cluster ~ area, value.var = "nominalcapacity", fill = 0)
    annual_capacity <- merge(vars, annual_capacity, by.x = "variable", by.y = "cluster", all.x = T, sort = FALSE )
    template <- data.table()
    template[, c("variable", "Output type") := character(0)]
    template[, c(tolower(areas_districts_selections)) := numeric(0)]
    annual_capacity <- rbindlist(list(template, annual_capacity), fill = T)
    
    annual_capacity <- annual_capacity[, colnames(annual_generation), with = FALSE]
    
    
    
    #=====================================total_income=================================
    # Vide pour l'instant..
    total_income <- copy(annual_capacity)
    total_income[, colnames(total_income)[-(1:2)] := lapply(.SD, function(x) 0), .SDcols =  colnames(total_income)[-(1:2)]]
    
    #=====================================equivalent_full_load=================================
    
    equivalent_full_load <- data.table(
      copy(annual_capacity[,1:2]),
      (annual_generation[,-c(1:2)]*1000)/copy(annual_capacity)[,-c(1:2)])
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
      (`H. STOR`+`H. ROR`) / 1000, LOAD / 1000,
      PSP_negatif / 1000, BALANCE / 1000, `ROW BAL.` / 1000, (BALANCE- `ROW BAL.`) / 1000, `SPIL. ENRG` / 1000,
      `UNSP. ENRG` / 1000, LOLD, `CO2 EMIS.`)]
    tmp_block_comp <- tmp_block_comp[, tmp, with = F]
    tmp_block_comp <- formater_tab(tmp_block_comp, output_type = NULL, new_names =  as.character(data_areas_districts$area))
    
    #=============================== Overall costs =========================================
    tmp <- c("Overall costs [M€]",
             "Marginal Cost Yearly Average [€]",
             "Marginal Cost Yearly Average (excl. 3000 €/MWh) [€]",
             "Pan-EU Marginal Cost Yearly Average (excl. 3000 €/MWh) Weighted By Demand [€]")
    overall_block <- copy(data_areas_districts)
    overall_block <- overall_block[, c(tmp) := list(`OV. COST` / 1000000, `MRG. PRICE`, NA, NA)]
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
    
    colnames(data_long_out)[1:2] <- c("Output type", "Output type 2")
    colnames(data_long_out)[-(1:2)] <- toupper(colnames(data_long_out)[-(1:2)])
    
    # order
    template <- data.table()
    template[, c("Output type", "Output type 2") := character(0)]
    template[, c(toupper(areas_districts_selections)) := numeric(0)]
    data_long_out <- rbindlist(list(template, data_long_out), fill = T)  
  }
  
  if(nrow(t_all) > 0){
    try({
      t_all[, colnames(t_all)[-c(1,2)] := lapply(.SD, function(x) {
        x[is.nan(x)] <- 0 ; 
        x[is.infinite(x)] <- 0;
        x[is.na(x)] <- 0; x}), .SDcols = (colnames(t_all)[-c(1,2)])]  }, T)
  }
  
  if(nrow(t_welfare_block) > 0){
    try({
      t_welfare_block[, colnames(t_welfare_block)[-c(1,2)] := lapply(.SD, function(x) {
        x[is.nan(x)] <- 0 ; 
        x[is.infinite(x)] <- 0;
        x[is.na(x)] <- 0; x}), .SDcols = (colnames(t_welfare_block)[-c(1,2)])]}, T)
  }
  
  if(nrow(data_long_out) > 0){
    try({
      data_long_out[, colnames(data_long_out)[-c(1,2)] := lapply(.SD, function(x) {
        x[is.nan(x)] <- 0 ; 
        x[is.infinite(x)] <- 0;
        x[is.na(x)] <- 0; x}), .SDcols = (colnames(data_long_out)[-c(1,2)])]}, T)
  }
  
  list(t_all = t_all, t_welfare_block = t_welfare_block, line_style = line_style, data_links_sums = data_links_sums,
       data_intro = data_intro, data_long_out = data_long_out, areas_districts_selections = areas_districts_selections,
       links_selections = links_selections) 
}


#export
exportAnnualOutputs <- function(infile_name, outfile_name, annual_outputs, data_intro){
  
  wb <- createWorkbook()
  wb <- loadWorkbook(infile_name)
  options(scipen = 0)
  try({
    writeData(wb, "Identification", annual_outputs$data_intro[-4,])
    writeData(wb, "Identification", toupper(annual_outputs$areas_districts_selections), startRow = 14, startCol = 2)
    writeData(wb, "Identification", toupper(annual_outputs$links_selections), startRow = 14, startCol = 3)
    writeData(wb, "Identification", data_intro[4, get(colnames(data_intro)[2])], startRow = 14, startCol = 4)
  }, T)
  
  try({
    writeData(wb, "Yearly Outputs Short", annual_outputs$data_intro)
    
    if(nrow(annual_outputs$t_all) > 0){
      colnames(annual_outputs$t_all)[2] <- "Output type"
      writeData(wb, "Yearly Outputs Short", annual_outputs$t_all[,1:2], startRow = 6)
      writeData(wb, "Yearly Outputs Short", annual_outputs$t_all[,-(1:2)], startRow = 6, borders = "all", startCol = 3)
    } 
    
    if(nrow(annual_outputs$t_welfare_block) > 0){
      colnames(annual_outputs$t_welfare_block)[2] <- "Output type"
      writeData(wb, "Yearly Outputs Short", annual_outputs$t_welfare_block[,1:2], startRow = 42)
      writeData(wb, "Yearly Outputs Short", annual_outputs$t_welfare_block[,-(1:2)], startRow = 42, borders = "all", startCol = 3)
    }
    
    writeData(wb, "Yearly Outputs Short", annual_outputs$line_style, startRow = 48)
  }, T)
  
  try({
    if(!is.null(annual_outputs$data_links_sums) && nrow(annual_outputs$data_links_sums) > 0){
      writeData(wb, "Yearly Outputs Short", annual_outputs$data_links_sums, startRow = 50, borders = "all")
    }
  }, T)
  try({
    
    writeData(wb, "Yearly Outputs Long", annual_outputs$data_intro[,1:2])
    writeData(wb, "Yearly Outputs Long", annual_outputs$data_intro[,-(1:2)], borders = "all", startCol = 3)
    
    if(nrow(annual_outputs$data_long_out) > 0){
      colnames(annual_outputs$data_long_out)[2] <- "Output type"
      writeData(wb, "Yearly Outputs Long", annual_outputs$data_long_out[,1:2], startRow = 6)
      writeData(wb, "Yearly Outputs Long", annual_outputs$data_long_out[,-(1:2)], startRow = 6, borders = "all", startCol = 3)
      
    }

  }, T)
  saveWorkbook(wb, file = outfile_name, overwrite = T)
  
}

#============================== Hourly output ========================================

makeTabStats <- function(data){
  dt <- data.table("Min [MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) min(x, na.rm = T))]),
                   "Max[MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) max(x, na.rm = T))]),
                   "Avg [MW]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) mean(x, na.rm = T))]),
                   "SUM [GWh]" = unlist(data[, -(1:2)][, lapply(.SD, function(x) sum(as.numeric(x), na.rm = T)/1000)]))
  dt <- dt[, lapply(.SD, round)]
  dt[, colnames(dt) := lapply(.SD, function(x) {x[is.nan(x)] <- 0 ; x[is.infinite(x)] <- 0; x}), .SDcols = colnames(dt)]
  dt <- data.table(v1 = colnames(dt), t(dt))
  colnames(dt) <- rep(" ", ncol(dt))
  dt  
}

#import
importAntaresDatasHourly <- function(opts, areas_districts_selections, links_selections, mcYears = 1,
                                     removeVirtualAreas = FALSE,
                                     storageFlexibility = NULL, production = NULL,
                                     reassignCosts = FALSE, newCols = TRUE, rowBal = TRUE){
  
  #init :
  data_areasH = NULL
  data_linksH = NULL
  
  if(length(storageFlexibility) > 0) storageFlexibility <- tolower(storageFlexibility)
  if(length(production) > 0) production <- tolower(production)
  
  
  opts$districtsDef[, district := tolower(district)]
  opts$districtsDef[, area := tolower(area)]
  
  if(length(areas_districts_selections) > 0){
    
    if("all" %in% areas_districts_selections){
      areas_districts_selections <- unique(c(tolower(opts$areaList), tolower(opts$districtList)))
      
      if(removeVirtualAreas && (length(storageFlexibility) > 0 || length(production) > 0)){
        ind_rm <- grepl(paste(paste0("(", c(storageFlexibility, production), ")"), collapse = "|"), 
                        areas_districts_selections)
        areas_districts_selections <- areas_districts_selections[!ind_rm]
      }
    }
    
    areas_districts_selections <- tolower(areas_districts_selections)
    areasForDist <- as.character(opts$districtsDef[tolower(district) %in% areas_districts_selections, tolower(area)])
    areas_districts_selections_add <- unique(c(areas_districts_selections, areasForDist))
    
    areas_selection <- intersect(areas_districts_selections, tolower(opts$areaList))
    districts_selection <- intersect(areas_districts_selections, tolower(opts$districtList))
    
    areas_clusters_selection <- intersect(areas_districts_selections_add, tolower(opts$areasWithClusters))
    
  } else {
    areas_districts_selections <- NULL
    areas_districts_selections_add <- NULL
    areas_selection <- NULL
    districts_selection <- NULL
    areas_clusters_selection <- NULL
  }
  
  if(length(links_selections) > 0){
    links_selections <- tolower(links_selections)
  } else {
    links_selections <- NULL
  }
  
  if("all" %in% links_selections){
    links_selections <- unique(tolower(opts$linkList))
    if(removeVirtualAreas && (length(storageFlexibility) > 0 || length(production) > 0)){
      ind_rm <- grepl(paste(paste0("(", c(storageFlexibility, production), ")"), collapse = "|"), 
                      links_selections)
      links_selections <- links_selections[!ind_rm]
    }
  }

  if(!removeVirtualAreas){
    data_h <- readAntares(areas = areas_selection, 
                          districts = districts_selection,
                          clusters = areas_clusters_selection,
                          links = links_selections,
                          timeStep = "hourly", select = NULL, mcYears = mcYears)
  } else {
    
    data_h <- readAntares(areas = "all", 
                          districts = "all",
                          clusters = "all",
                          links = "all",
                          timeStep = "hourly", 
                          select = NULL, 
                          linkCapacity = FALSE, mcYears = mcYears)
    
    data_h <- suppressWarnings({removeVirtualAreas(data_h, storageFlexibility = storageFlexibility, production = production,
                                                   reassignCosts = reassignCosts, newCols = newCols, rowBal = rowBal)})
    
    
    if(!is.null(data_h$areas) && nrow(data_h$areas) > 0){
      data_h$areas <- data_h$areas[area %in% areas_selection, ]
    }
    
    if(!is.null(data_h$clusters) && nrow(data_h$clusters) > 0){
      data_h$clusters <- data_h$clusters[area %in% areas_clusters_selection, ]
    }
    
    if(!is.null(data_h$districts) && nrow(data_h$districts) > 0){
      data_h$districts <- data_h$districts[district %in% districts_selection, ]
    }
    
    if(!is.null(data_h$links) && nrow(data_h$links) > 0){
      data_h$links <- data_h$links[link %in% links_selections, ]
    }
    
    gc(reset = T)
    
  }
  
  
  # Enrichissement par la somme des clusters par districts : 
  if(!is.null(data_h$clusters) && nrow(data_h$clusters) > 0){
    try({
      tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                   data_h$clusters, by = c("area"), allow.cartesian = T)
      tmp[, c("production", "NP Cost", "NODU") := list(as.numeric(production), as.numeric(`NP Cost`), as.numeric(NODU))]
      tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                          by = c("district", "cluster", "time"), .SDcols = c("production", "NP Cost", "NODU")))
      setnames(tmp, "district", "area")
      data_h$clusters <- rbindlist(list(data_h$clusters[area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
      rm(tmp)
      gc()
    }, T)
  }

  
  list(data = data_h, areas_districts_selections = areas_districts_selections, links_selections = links_selections)
}

# formatage
formatHourlyOutputs <- function(data_h, areas_selections,
                                market_data_code, links_selections, dico = dico){
  data_out <- NA ; dt_stats <- NA ; areas_districts <- NA
  
  if("data.table" %in% class(data_h) && attr(data_h, "type") == "links"){
    data_links <- data_h
  } else {
    data_links <- data_h$links
  }
  
  try({
    
    areas_selections <- tolower(areas_selections)
    data_h$clusters <- aggregateClusters(data_h$clusters, "production", hourly = T)
    
    data_h_dist <- rbindlist(list(data_h$areas, data_h$districts), use.names = F)
    colnames(data_h_dist)[1] <- "area"
    
    if("PSP" %in% colnames(data_h_dist) && "PSP_TURB" %in% market_data_code) {
      data_h_dist[, PSP_TURB := ifelse(PSP > 0, PSP, 0)]
    }
    
    if("PSP" %in% colnames(data_h_dist) && "PSP_TURB" %in% market_data_code) {
      data_h_dist[, PSP_PUMP := ifelse(PSP < 0, PSP, 0)]
    }
    
    cols_areas <- intersect(colnames(data_h_dist), market_data_code)
    
    data_h_dist <- data_h_dist[area %in% areas_selections,
                               c("area", "time", cols_areas), with = F]
    
    temp <- dcast(data_h$clusters, area + time ~ cluster, value.var = "var", fill = 0)
    
    data_all <- merge(temp, data_h_dist, by = c("area", "time"), all = TRUE)
    data_all <- data_all[, c("area", "time", intersect(market_data_code, colnames(data_all))), with = F]
    
    # ajout d'éventuelles variables demandées mais qui n'existent pas
    cols_comp <- setdiff(market_data_code, colnames(data_all))
    
    if(length(cols_comp) > 0) data_all[, (cols_comp) :=  0]
    data_all <- data_all[, c("area", "time", market_data_code), with = F]
    
    cols_availables <- intersect(market_data_code, colnames(data_all))
    
    setcolorder(data_all, neworder = c("area", "time", cols_availables))
    
    data_out <- unique(data_all[,.(Hour = .I, Date = time)])
    areas_selections <- intersect(unique(data_all$area), areas_selections)
    for(i in areas_selections){
      data_out <- data.table(data_out, data_all[area %in% i, -(1:2)])
    }
    areas_districts <- data.table(t(rep(toupper(areas_selections), each = ncol(data_all) - 2)))
    rm(data_all)
    gc(reset = T)
    
    vars_miss <- setdiff(cols_availables, unique(dico$ANTARES_naming))
    dico <- rbindlist(list(dico, data.table(ANTARES_naming = vars_miss, Category = vars_miss)))
    setkey(dico, ANTARES_naming)
    colnames(areas_districts) <- dico[colnames(data_out)[-c(1:2)]]$Category
    dt_stats <- makeTabStats(data_out)
  }, T)
  
  
  #============================== Crossborder exchanges ========================================
  if(!is.null(data_links) && nrow(data_links) > 0){
    
    
    data_out_2 <- dcast(data_links, time ~ link, value.var = "FLOW LIN.")
    rm(data_links)
    gc(reset = T)
    
    data_out_2[, Hour := .I]
    setnames(data_out_2, "time", "Date")
    setcolorder(data_out_2, c("Hour", "Date", intersect(colnames(data_out_2), tolower(links_selections))))
    colnames(data_out_2)[-(1:2)] <- toupper(colnames(data_out_2)[-(1:2)])
    dt_stats_2 <- makeTabStats(data_out_2)
  } else{
    data_out_2 <- NA ; dt_stats_2 <- NA
  }
  
  list(data_out = data_out, dt_stats = dt_stats, areas_districts = areas_districts,
       data_out_2 = data_out_2, dt_stats_2 = dt_stats_2, links_selections = links_selections, 
       areas_districts_selections = areas_selections)
  
}

# export
exportHourlyOutputs <- function(infile_name, outfile_name, hourly_outputs, data_intro, market_data_code){
  
  wb <- loadWorkbook(infile_name)
  
  writeData(wb, "Identification", data_intro[-4,])
  writeData(wb, "Identification", toupper(hourly_outputs$areas_districts_selections), startRow = 14, startCol = 2)
  writeData(wb, "Identification", market_data_code, startRow = 14, startCol = 3)
  writeData(wb, "Identification", toupper(hourly_outputs$links_selections), startRow = 14, startCol = 4)
  writeData(wb, "Identification", data_intro[4, get(colnames(data_intro)[2])], startRow = 14, startCol = 5)
  
  writeData(wb, "Hourly Market Data", data_intro[1:3,])
  writeData(wb, "Hourly Market Data", hourly_outputs$dt_stats, startRow = 5, startCol = 2)
  
  writeData(wb, "Hourly Market Data", hourly_outputs$areas_districts, startRow = 11,  startCol = 3)
  
  writeData(wb, "Hourly Market Data", hourly_outputs$data_out, startRow = 13)
  
  writeData(wb, "Crossborder exchanges", data_intro[1:3,])
  writeData(wb, "Crossborder exchanges", hourly_outputs$dt_stats_2, startRow = 5, startCol = 2)
  writeData(wb, "Crossborder exchanges", hourly_outputs$data_out_2, startRow = 11)
  
  system.time(saveWorkbook(wb, outfile_name, overwrite = T))
}


# input_path = "C:\\Users\\Datastorm\\Documents\\git\\antaresVizMedTSO\\inst\\application\\www\\Output_Selection_template.xlsx"
readTemplateFile <- function(input_path){
  sel <- list(areas_districts_annual = NULL, links_annual = NULL, 
              areas_districts_hourly = NULL, links_hourly = NULL,
              variables_hourly = NULL,
              dico = NULL)
  
  if(!file.exists(input_path)){
    stop("Le fichier '", input_path, "' est introuvable")
  }
  
  # annual
  sel_annual <- suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Annual_Selection", check.names = FALSE, colNames = TRUE),
                                          error = function(e) {
                                            stop("Error reading sheet 'Annual_Selection' : ", e)
                                          }))
  
  if(!is.null(sel_annual) && nrow(sel_annual) > 0){
    stopifnot(all(c("Areas_Districts", "Links") %in% colnames(sel_annual)))
    
    areas_districts_annual <- setdiff(unique(tolower(sel_annual$Areas_Districts)), NA)
    if(length(areas_districts_annual) > 0){
      sel$areas_districts_annual <- areas_districts_annual
    }
    
    links_annual <- setdiff(unique(tolower(sel_annual$Links)), NA)
    if(length(links_annual) > 0){
      sel$links_annual <- links_annual
    }
  }
  
  # hourly dico 
  sel_hourly_dico <- suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Hourly_Dico", check.names = FALSE, colNames = TRUE),
                                               error = function(e) {
                                                 stop("Error reading sheet 'Hourly_Dico' : ", e)
                                               }))
  
  if(!is.null(sel_hourly_dico) && nrow(sel_hourly_dico) > 0){
    stopifnot(all(c("ANTARES_naming", "Category") %in% colnames(sel_hourly_dico)))
    sel_hourly_dico$ANTARES_naming <- as.character(sel_hourly_dico$ANTARES_naming)
    sel_hourly_dico$Category <- as.character(sel_hourly_dico$Category)
    sel$dico <- sel_hourly_dico[, c("ANTARES_naming", "Category")]
  }
  
  
  sel_hourly <- suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Hourly_Selection", check.names = FALSE, colNames = TRUE),
                                          error = function(e) {
                                            stop("Error reading sheet 'Hourly_Selection' : ", e)
                                          }))
  
  if(!is.null(sel_hourly) && nrow(sel_hourly) > 0){
    stopifnot(all(c("Areas_Districts", "Links", "Market_Data_Code") %in% colnames(sel_hourly)))
    
    areas_districts_hourly <- setdiff(unique(tolower(sel_hourly$Areas_Districts)), NA)
    if(length(areas_districts_hourly) > 0){
      sel$areas_districts_hourly <- areas_districts_hourly
    }
    
    links_hourly <- setdiff(unique(tolower(sel_hourly$Links)), NA)
    if(length(links_hourly) > 0){
      sel$links_hourly <- links_hourly
    }
    
    variables_hourly <- setdiff(unique(sel_hourly$Market_Data_Code), NA)
    if(length(variables_hourly) > 0){
      
      if(!is.null(sel$dico)){
        variables_hourly_fix <- sel$dico$ANTARES_naming[match(toupper(variables_hourly), toupper(sel$dico$ANTARES_naming))]
        variables_hourly_fix[is.na(variables_hourly_fix)] <- variables_hourly[is.na(variables_hourly_fix)]
        variables_hourly <- variables_hourly_fix
      }
      sel$variables_hourly <- variables_hourly
    }
    
    
  }
  
  sel
}