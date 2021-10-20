require(data.table)
require(antaresRead)
require(antaresProcessing)
require(stringr)
require(openxlsx)

# Somme des variables finissant par _chiffre :
aggregateClusters <- function(data, var, opts, hourly = F, div = 1){
  data <- copy(data)
  if("group" %in% colnames(data)){
    data$group <- NULL
  }
  
  cdesc <- readClusterDesc(opts)
  # data <- merge(data, cdesc[, list(area, cluster, group)], all.x = T, by = c("area", "cluster"))
  data <- merge(data, unique(cdesc[, list(cluster, group = tolower(group))]), all.x = T, by = c("cluster"))
  
  # ind <- grep("_[0-9]$", data[["cluster"]])
  # data[ind, cluster := gsub(pattern = "_[0-9]$", "", cluster)]
  data$cluster <- sapply(data$cluster, function(x){
    tmp <- strsplit(as.character(x), "_")[[1]]
    if(length(tmp) > 1){
      tmp <- paste(tmp[1], tmp[2], sep = "_")
    } 
    tmp
  })
  
  # other non res + mixed fioul
  data[tolower(group) %in% c("other", "mixed fuel"), cluster := "other_nonrenewable"]
  
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
importAntaresDatasAnnual <- function(opts, 
                                     areas_districts_selections, 
                                     links_selections, mcYears = 1, 
                                     removeVirtualAreas = FALSE,
                                     storageFlexibility = NULL, production = NULL,
                                     reassignCosts = FALSE, newCols = TRUE, rowBal = TRUE, 
                                     storage_vars = c("PSP", "PSP_Closed", "BATT", "DSR", "EV", "P2G", "H2")){
  
  # init :
  # browser()
  
  data_areas_dist_clust = NULL
  data_areas_dist_clustH = NULL
  dataForSurplus = NULL
  data_areas_districts = NULL
  
  any_removeVirtualAreas <- FALSE
  check <- lapply(removeVirtualAreas, function(x) if(!is.null(x) && length(x) == 1 && x) {any_removeVirtualAreas <<- TRUE})
  all_storageFlexibility <- NULL
  all_production <- NULL
  if(length(storageFlexibility) > 0) all_storageFlexibility <- tolower(unlist(storageFlexibility))
  if(length(production) > 0) all_production <- tolower(unlist(production))
  
  # browser()
  # Imports :
  
  opts$districtsDef[, district := tolower(district)]
  opts$districtsDef[, area := tolower(area)]
  
  if("all" %in% links_selections){
    links_selections <- unique(tolower(opts$linkList))
    if(any_removeVirtualAreas && (length(all_storageFlexibility) > 0 || length(all_production) > 0)){
      ind_rm <- grepl(paste(paste0("(", c(all_storageFlexibility, all_production), ")"), collapse = "|"), 
                      links_selections)
      links_selections <- links_selections[!ind_rm]
    }
  }
  if("all" %in% areas_districts_selections){
    areas_districts_selections <- unique(c(tolower(opts$areaList), tolower(opts$districtList)))
    
    if(any_removeVirtualAreas && (length(all_storageFlexibility) > 0 || length(all_production) > 0)){
      ind_rm <- grepl(paste(paste0("(", c(all_storageFlexibility, all_production), ")"), collapse = "|"), 
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
    if(opts$antaresVersion >= 810 | length(opts$areasWithResClusters) > 0){
      areas_clusters_res_selection <- intersect(areas_districts_selections_add, tolower(opts$areasWithResClusters))
    } else {
      areas_clusters_res_selection <- NULL
    }
    
    if(!any_removeVirtualAreas){
      data_areas_dist_clust <- readAntares(areas = areas_selection, 
                                           districts = districts_selection,
                                           clusters = areas_clusters_selection,
                                           clustersRes = areas_clusters_res_selection,
                                           links = NULL,
                                           timeStep = "annual", select = NULL, mcYears = mcYears)
      
    } else {
      
      if(opts$antaresVersion >= 810 | length(opts$areasWithResClusters) > 0){
        clusters_res <- "all"
      } else {
        clusters_res <- NULL
      }
      
      data_areas_dist_clust <- readAntares(areas = "all", 
                                           districts = "all",
                                           clusters = "all",
                                           clustersRes = clusters_res,
                                           links = "all",
                                           timeStep = "annual", select = NULL, mcYears = mcYears)
      
      for(ii in 1:length(removeVirtualAreas)){
        if(!is.null(removeVirtualAreas[[ii]]) && length(removeVirtualAreas[[ii]]) == 1 && removeVirtualAreas[[ii]]){
          data_areas_dist_clust <- suppressWarnings({
            removeVirtualAreas(
              data_areas_dist_clust, 
              storageFlexibility = storageFlexibility[[ii]], 
              production = production[[ii]],
              reassignCosts = reassignCosts[[ii]], 
              newCols = newCols[[ii]], 
              rowBal = rowBal
            )
          })
        }
      }
      
      
      if(!is.null(data_areas_dist_clust$areas) && nrow(data_areas_dist_clust$areas) > 0){
        data_areas_dist_clust$areas <- data_areas_dist_clust$areas[area %in% areas_selection, ]
      }
      
      if(!is.null(data_areas_dist_clust[["clusters"]]) && nrow(data_areas_dist_clust[["clusters"]]) > 0){
        data_areas_dist_clust[["clusters"]][area %in% areas_clusters_selection, ]
      }
      
      if(!is.null(data_areas_dist_clust[["clustersRes"]]) && nrow(data_areas_dist_clust[["clustersRes"]]) > 0){
        data_areas_dist_clust[["clustersRes"]][area %in% areas_clusters_res_selection, ]
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
    areas_clusters_res_selection <- NULL
    areas_districts_selections_add <- NULL
  }
  
  links_selections <- tolower(links_selections)
  linkCapacity <- ifelse(is.null(links_selections), F, T)
  
  if(opts$antaresVersion >= 810 | length(opts$areasWithResClusters) > 0){
    clusters_res <- "all"
  } else {
    clusters_res <- NULL
  }
  
  data_areas_dist_clustH <- readAntares(areas = "all", 
                                        districts = "all",
                                        clusters = "all",
                                        clustersRes = clusters_res,
                                        links = "all",
                                        timeStep = "hourly", 
                                        select = NULL, 
                                        linkCapacity = linkCapacity, mcYears = mcYears)
  
  if(any_removeVirtualAreas){
    
    for(ii in 1:length(removeVirtualAreas)){
      if(!is.null(removeVirtualAreas[[ii]]) && length(removeVirtualAreas[[ii]]) == 1 && removeVirtualAreas[[ii]]){
        data_areas_dist_clustH <- suppressWarnings({
          removeVirtualAreas(
            data_areas_dist_clustH, 
            storageFlexibility = storageFlexibility[[ii]], 
            production = production[[ii]],
            reassignCosts = reassignCosts[[ii]], 
            newCols = newCols[[ii]], 
            rowBal = rowBal
          )
        })
      }
    }
  }
  
  if(!is.null(mcYears)){
    
    # fix surplus
    if(any_removeVirtualAreas && length(all_storageFlexibility) > 0){
      for(v in tolower(all_storageFlexibility)){
        if(!v %in% colnames(data_areas_dist_clustH$areas)){
          data_areas_dist_clustH$areas[, c(v) := 0]
        }
      }
    }
    
    if(any_removeVirtualAreas && length(all_production) > 0){
      for(v in tolower(all_production)){
        if(!v %in% colnames(data_areas_dist_clustH$areas)){
          data_areas_dist_clustH$areas[, c(v) := 0]
        }
      }
    }
    
    surplus <- suppressWarnings({antaresProcessing::surplus(data_areas_dist_clustH)}) 
    
    # des bugs dans antaresProcessing du a des NA....
    # surplus_districts <- suppressWarnings({antaresProcessing::surplus(data_areas_dist_clustH, groupByDistrict = T)})
    
    opts$districtsDef[, district := tolower(district)]
    opts$districtsDef[, area := tolower(area)]
    
    surplus_districts <- merge(surplus, opts$districtsDef, by = "area")
    
    cols_surplus <- c("consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                      "storageSurplus", "congestionFees", "globalSurplus")
    
    surplus_districts <- surplus_districts[, list(consumerSurplus = sum(consumerSurplus, na.rm = T), 
                                                  producerSurplus = sum(producerSurplus, na.rm = T),
                                                  rowBalanceSurplus = sum(rowBalanceSurplus, na.rm = T),
                                                  storageSurplus = sum(storageSurplus, na.rm = T),
                                                  congestionFees = sum(congestionFees, na.rm = T),
                                                  globalSurplus = sum(globalSurplus, na.rm = T)),
                                           by = setdiff(colnames(surplus_districts), c("area", cols_surplus))]
    
    setnames(surplus_districts, "district", "area")
    surplus <- rbindlist(list(surplus, surplus_districts), use.names = T, fill = T)
    
    if(!is.null(data_areas_dist_clustH$areas) && nrow(data_areas_dist_clustH$areas) > 0){
      target_var <- c("area", "mcYear", "timeId", "time", "day", "month", "hour", storage_vars)
      data_areas_dist_clustH$areas <- data_areas_dist_clustH$areas[area %in% areas_selection, intersect(target_var, colnames(data_areas_dist_clustH$areas)), with = FALSE]
    }
    
    if(!is.null(data_areas_dist_clustH[["clusters"]]) && nrow(data_areas_dist_clustH[["clusters"]]) > 0){
      data_areas_dist_clustH[["clusters"]] <- data_areas_dist_clustH[["clusters"]][area %in% areas_clusters_selection, ]
    }
    
    if(!is.null(data_areas_dist_clustH[["clustersRes"]]) && nrow(data_areas_dist_clustH[["clustersRes"]]) > 0){
      data_areas_dist_clustH[["clustersRes"]] <- data_areas_dist_clustH[["clustersRes"]][area %in% areas_clusters_res_selection, ]
    }
    
    if(!is.null(data_areas_dist_clustH$districts) && nrow(data_areas_dist_clustH$districts) > 0){
      target_var <- c("district", "mcYear", "timeId", "time", "day", "month", "hour", storage_vars)
      data_areas_dist_clustH$districts <- data_areas_dist_clustH$districts[district %in% districts_selection, intersect(target_var, colnames(data_areas_dist_clustH$districts)), with = FALSE]
    }
    
    if(!is.null(data_areas_dist_clustH$links) && nrow(data_areas_dist_clustH$links) > 0){
      data_areas_dist_clustH$links <- data_areas_dist_clustH$links[link %in% links_selections, ]
    }
    
  } else {
    surplus <- data.table(area = character(0), globalSurplus = numeric(0), 
                          producerSurplus = numeric(0), consumerSurplus = numeric(0), congestionFees = numeric(0))
    
    if(!is.null(data_areas_dist_clustH$areas) && nrow(data_areas_dist_clustH$areas) > 0){
      target_var <- c("area", "timeId", "time", "day", "month", "hour", storage_vars)
      data_areas_dist_clustH$areas <- data_areas_dist_clustH$areas[area %in% areas_selection, intersect(target_var, colnames(data_areas_dist_clustH$areas)), with = FALSE]
    }
    
    if(!is.null(data_areas_dist_clustH[["clusters"]]) && nrow(data_areas_dist_clustH[["clusters"]]) > 0){
      data_areas_dist_clustH[["clusters"]] <- data_areas_dist_clustH[["clusters"]][area %in% areas_clusters_selection, ]
    }
    
    if(!is.null(data_areas_dist_clustH[["clustersRes"]]) && nrow(data_areas_dist_clustH[["clustersRes"]]) > 0){
      data_areas_dist_clustH[["clustersRes"]] <- data_areas_dist_clustH[["clustersRes"]][area %in% areas_clusters_res_selection, ]
    }
    
    if(!is.null(data_areas_dist_clustH$districts) && nrow(data_areas_dist_clustH$districts) > 0){
      target_var <- c("district", "timeId", "time", "day", "month", "hour", storage_vars)
      data_areas_dist_clustH$districts <- data_areas_dist_clustH$districts[district %in% districts_selection, intersect(target_var, colnames(data_areas_dist_clustH$districts)), with = FALSE]
    }
    
    if(!is.null(data_areas_dist_clustH$links) && nrow(data_areas_dist_clustH$links) > 0){
      data_areas_dist_clustH$links <- data_areas_dist_clustH$links[link %in% links_selections, ]
    }
  }
  
  
  gc(reset = T)
  
  
  # Enrichissement par la somme des clusters par districts : 
  if(!is.null(data_areas_dist_clust)){
    try({
      if(!is.null(data_areas_dist_clust[["clusters"]]) && nrow(data_areas_dist_clust[["clusters"]]) > 0){
        
        # browser()
        cluster_desc <- readClusterDesc(opts = opts)
        if(all(c("unitcount", "nominalcapacity") %in% colnames(cluster_desc))){
          cluster_desc[, installed.capacity := unitcount * nominalcapacity]
        } else if("nominalcapacity" %in% colnames(cluster_desc)){
          cluster_desc[, installed.capacity := nominalcapacity]
        }
        cluster_desc_class <- sapply(cluster_desc, class)
        keep_col <- c("area", "cluster", "group", names(cluster_desc_class)[cluster_desc_class %in% c("integer", "numeric")])
        cluster_desc <- cluster_desc[, keep_col, with = FALSE]
        
        data_areas_dist_clust[["clusters"]] <- merge(data_areas_dist_clust[["clusters"]], cluster_desc, c("area", "cluster"))
        
        data_areas_dist_clust[["clusters"]]$cluster <- sapply(data_areas_dist_clust[["clusters"]]$cluster, function(x){
          tmp <- strsplit(as.character(x), "_")[[1]]
          if(length(tmp) > 1){
            tmp <- paste(tmp[1], tmp[2], sep = "_")
          } 
          tmp
        })
        
        # other non res + mixed fioul
        data_areas_dist_clust[["clusters"]][tolower(group) %in% c("other", "mixed fuel"), cluster := "other_nonrenewable"]
        
        tmp_district <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                              data_areas_dist_clust[["clusters"]], by = "area", allow.cartesian = TRUE)
        
        setnames(tmp_district, "district", "area_tmp")
        tmp_district[, area := NULL]
        setnames(tmp_district, "area_tmp", "area")
        tmp_district <- rbindlist(
          list(
            data_areas_dist_clust[["clusters"]][area %in% areas_districts_selections], 
            tmp_district[!is.na(area) & !is.na(cluster), ]
          ), 
          use.names = T, fill = TRUE
        )
        
        num_col <- sapply(tmp_district, class)
        num_col <- names(num_col)[num_col %in% c("integer", "numeric")]
        num_col <- setdiff(num_col, c("mcYear", "time"))
        
        tmp_district <- cube(tmp_district, j = lapply(.SD, sum), 
                             by = c("area", "cluster"), .SDcols = num_col)
        
        data_areas_dist_clust[["clusters"]] <- tmp_district[!is.na(area) & !is.na(cluster), ]
        rm(tmp_district)
        gc()
      }
      
      if(!is.null(data_areas_dist_clust[["clustersRes"]]) && nrow(data_areas_dist_clust[["clustersRes"]]) > 0){       
        cluster_desc <- readClusterResDesc(opts = opts)
        if(all(c("unitcount", "nominalcapacity") %in% colnames(cluster_desc))){
          cluster_desc[, installed.capacity := unitcount * nominalcapacity]
        } else if("nominalcapacity" %in% colnames(cluster_desc)){
          cluster_desc[, installed.capacity := nominalcapacity]
        }
        cluster_desc_class <- sapply(cluster_desc, class)
        keep_col <- c("area", "cluster", "group", names(cluster_desc_class)[cluster_desc_class %in% c("integer", "numeric")])
        cluster_desc <- cluster_desc[, keep_col, with = FALSE]
        
        data_areas_dist_clust[["clustersRes"]] <- merge(data_areas_dist_clust[["clustersRes"]], cluster_desc, c("area", "cluster"))
        
        data_areas_dist_clust[["clustersRes"]]$clustersRes <- sapply(data_areas_dist_clust[["clustersRes"]]$cluster, function(x){
          tmp <- strsplit(as.character(x), "_")[[1]]
          if(length(tmp) > 1){
            tmp <- paste(tmp[1], tmp[2], sep = "_")
          } 
          tmp
        })
        
        tmp_district <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                              data_areas_dist_clust[["clustersRes"]], by = "area", allow.cartesian = TRUE)
        
        
        setnames(tmp_district, "district", "area_tmp")
        tmp_district[, area := NULL]
        setnames(tmp_district, "area_tmp", "area")
        tmp_district <- rbindlist(
          list(
            data_areas_dist_clust[["clustersRes"]][area %in% areas_districts_selections], 
            tmp_district[!is.na(area) & !is.na(cluster), ]
          ), 
          use.names = T, fill = TRUE
        )
        
        num_col <- sapply(tmp_district, class)
        num_col <- names(num_col)[num_col %in% c("integer", "numeric")]
        num_col <- setdiff(num_col, c("mcYear", "time"))
        
        tmp_district <- cube(tmp_district, j = lapply(.SD, sum), 
                             by = c("area", "cluster"), .SDcols = num_col)
        
        data_areas_dist_clust[["clustersRes"]] <- tmp_district[!is.na(area) & !is.na(cluster), ]
        rm(tmp_district)
        gc()
      }
    }, T)
  }
  
  if(!is.null(data_areas_dist_clust$districts) && nrow(data_areas_dist_clust$districts) > 0){
    setnames(data_areas_dist_clust$districts, "district", "area")
    if(!is.null(data_areas_dist_clust$areas) && nrow(data_areas_dist_clust$areas) > 0){
      data_areas_districts <- rbindlist(list(data_areas_dist_clust$areas, data_areas_dist_clust$districts), use.names=TRUE, fill = TRUE)
    } else {
      data_areas_districts <- data_areas_dist_clust$districts
    }
  } else if(!is.null(data_areas_dist_clust$areas) && nrow(data_areas_dist_clust$areas) > 0){
    data_areas_districts <- data_areas_dist_clust$areas
  }
  
  if(!is.null(data_areas_dist_clustH$districts) && nrow(data_areas_dist_clustH$districts) > 0){
    setnames(data_areas_dist_clustH$districts, "district", "area")
    if(!is.null(data_areas_dist_clustH$areas) && nrow(data_areas_dist_clustH$areas) > 0){
      areas_districtsH <- rbindlist(list(data_areas_dist_clustH$areas, data_areas_dist_clustH$districts), use.names=TRUE, fill = TRUE)
    } else {
      areas_districtsH <- data_areas_dist_clustH$districts
    }
  } else if(!is.null(data_areas_dist_clustH$areas) && nrow(data_areas_dist_clustH$areas) > 0){
    areas_districtsH <- data_areas_dist_clustH$areas
  }
  
  if((!is.null(data_areas_districts) && nrow(data_areas_districts) > 0) & !is.null(areas_districtsH) && nrow(areas_districtsH) > 0){
    st_v <- intersect(storage_vars, colnames(areas_districtsH))
    
    expr <- paste0(
      "list(",
      paste(paste0(rep(st_v, each = 2), "_", c("POS", "NEG"), " = sum(", rep(st_v, each = 2), "[", rep(st_v, each = 2), c(">", "<"), "0], na.rm = T)"), collapse = ","),
      ")")
    
    data_PSP <- areas_districtsH[, eval(parse(text = expr)), by = .(area)] 
    
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


# formatage
formatAnnualOutputs <- function(data_areas_dist_clust,
                                data_areas_dist_clustH,
                                dataForSurplus,
                                data_areas_districts,
                                areas_districts_selections,
                                links_selections,
                                opts, 
                                data_intro, 
                                template){
  
  # init :
  # browser()
  yearly_output_short = data.table()
  yearly_welfare = data.table()
  yearly_interconnection = data.table()
  data_long_out = data.table()
  
  if(!(is.null(data_areas_dist_clust))){
    #============================== YEARLY OUTPUT SHORT ========================================
    
    template_gen <- openxlsx::read.xlsx(xlsxFile = template, 
                                        sheet = "Yearly Outputs Short", 
                                        startRow = 7)
    
    compute_expr <- paste0("'", template_gen[[2]], "' = tryCatch({", template_gen[[3]], "}, error = function(e){warning(paste0('Yearly Outputs Short : ', e$message));NA})")
    compute_expr <- paste0("list(", paste(compute_expr, collapse = ", "), ")")
    
    yearly_output_short <- data_areas_districts[, eval(parse(text = compute_expr))]
    
    yearly_output_short <- formater_tab(yearly_output_short, NULL, 
                                        new_names =  as.character(data_areas_districts$area))
    
    # order / missing
    colnames(yearly_output_short) <- toupper(colnames(yearly_output_short))
    template_dt <- data.table()
    template_dt[, c("OUTPUT TYPE") := character(0)]
    template_dt[, c(toupper(areas_districts_selections)) := numeric(0)]
    yearly_output_short <- rbindlist(list(template_dt, yearly_output_short), fill = T)
    
    #=====================================================Welfare System =================================
    if(!is.null(dataForSurplus) && nrow(dataForSurplus) > 0){
      
      surplus  <- dataForSurplus[area %in% tolower(areas_districts_selections)] 
      
      template_surplus <- openxlsx::read.xlsx(xlsxFile = template, 
                                              sheet = "Yearly Welfare", 
                                              startRow = 7)
      
      compute_expr <- paste0("'", template_surplus[[2]], "' = tryCatch({", template_surplus[[3]], "}, error = function(e){warning(paste0('Yearly Welfare : ', e$message));NA})")
      compute_expr <- paste0("list(", paste(compute_expr, collapse = ", "), ")")
      
      yearly_welfare <- surplus[, eval(parse(text = compute_expr))]
      
      yearly_welfare <- formater_tab(yearly_welfare, NULL, 
                                     new_names =  as.character(surplus$area))
      
      # order / missing
      colnames(yearly_welfare) <- toupper(colnames(yearly_welfare))
      template_dt <- data.table()
      template_dt[, c("OUTPUT TYPE") := character(0)]
      template_dt[, c(toupper(areas_districts_selections)) := numeric(0)]
      yearly_welfare <- rbindlist(list(template_dt, yearly_welfare), fill = T)
      
    }
  }
  #===================================================== table interconnections =================================
  
  template_links <- openxlsx::read.xlsx(xlsxFile = template, 
                                        sheet = "Yearly Interconnection", 
                                        startRow = 7, skipEmptyCols = FALSE)
  
  template_links <- t(template_links[, -c(1:2)])
  compute_expr <- paste0("'", template_links[, 1], "' = tryCatch({", template_links[, 2], "}, error = function(e){warning(paste0('Yearly Interconnection : ', e$message));NA})")
  compute_expr <- paste0("list(", paste(compute_expr, collapse = ", "), ")")
  
  try({
    if(!(is.null(data_areas_dist_clustH))){
      if(!is.null(data_areas_dist_clustH$links) && nrow(data_areas_dist_clustH$links) > 0){
        
        yearly_interconnection <- data_areas_dist_clustH$links[, eval(parse(text = compute_expr)), 
                                                               by = list(link = toupper(link))]
        
      } else {
        if("link" %in% colnames(data_areas_dist_clustH)){
          yearly_interconnection <- data_areas_dist_clustH[, eval(parse(text = compute_expr)), 
                                                           by = list(link = toupper(link))]
        }
      }
      
      if(nrow(yearly_interconnection) > 0){
        colnames(yearly_interconnection)[-1] <- paste0(colnames(yearly_interconnection)[-1], 1:(ncol(yearly_interconnection)-1))
        setorder(yearly_interconnection[, .r := match(toupper(links_selections)[toupper(links_selections) %in% link], link)], .r)[, .r := NULL]
      }
    }
  }, T)
  
  # if(nrow(data_links_sums) > 0){
  #   ind_null_ntc <- which(data_links_sums[[3]] == 0 & data_links_sums[[4]] == 0)
  #   if(length(ind_null_ntc) > 0){
  #     data_links_sums[[5]][ind_null_ntc] <- 0
  #     data_links_sums[[6]][ind_null_ntc] <- 0              
  #   }
  # }
  
  if(!(is.null(data_areas_dist_clust))){
    #============================== YEARLY OUTPUT LONG ========================================
    #=====================================Annual generation [GWh] (production / 1000)=================================
    
    data_agg_clust <- NULL
    data_agg_clustRes <- NULL
    data_agg_clust_areas <- NULL
    
    template_long <- openxlsx::read.xlsx(xlsxFile = template, 
                                         sheet = "Yearly Outputs Long", 
                                         startRow = 6, skipEmptyCols = FALSE)
    
    template_long <- as.data.table(template_long)
    colnames(template_long)[1:4] <- c("Out1", "Out2", "Formula", "Type")
    template_long[, order_tmp_ := 1:nrow(template_long)]
    
    template_long[, c("is_cluster", "is_clusterRes", "cluster_name", "is_area") := list(FALSE, FALSE, "", FALSE)]
    template_long[, c("is_cluster", "is_clusterRes", "cluster_name", "is_area") := {
      is_cluster <- grepl("cluster", Type)
      is_clusterRes <- grepl("clusterRes", Type)
      is_cluster[is_clusterRes] <- FALSE
      
      cluster_name <- sapply(
        strsplit(template_long$Type, "-"),
        function(x) {
          if(length(x > 1)){
            paste0(x[-1], collapse = "-")
          } else {
            ""
          }
        }
      )
      
      cluster_name[!(is_cluster | is_clusterRes)] <- ""
      list(is_cluster, is_clusterRes, cluster_name, grepl("^area$", tolower(Type)))
    }]
    
    template_long[, tmp_mathcing_name :=  gsub("[[:space:]]+", "", tolower(Type))]
    
    # cluster ?
    if(any(template_long[["is_cluster"]]) && nrow(data_areas_dist_clust[["clusters"]]) > 0){
      uni_expr <- template_long[is_cluster == TRUE, unique(Formula)]
      if(length(uni_expr) > 0){
        data_agg_clust <- data.table::rbindlist(lapply(uni_expr, function(ex){
          tmp_cluster_var <- tryCatch({
            data_areas_dist_clust[["clusters"]][, .(var = eval(parse(text = ex))), by = .(area, cluster)]
          }, error = function(e){
            warning(paste0('Yearly Outputs Long / ', ex, "' :", e$message))
            NULL
          })
          if(!is.null(tmp_cluster_var)){
            tmp_cluster_var[, Formula := ex]
          }
        }
        ))
        
        data_agg_clust[, cluster := gsub("[[:space:]]+", "", paste0("cluster-", cluster))]
        data_agg_clust <- dcast(data_agg_clust, cluster + Formula ~ area, value.var = "var", fill = 0)
        setnames(data_agg_clust, "cluster", "tmp_mathcing_name")
      }
    }
    
    # clusterRes ?
    if(any(template_long[["is_clusterRes"]]) && nrow(data_areas_dist_clust[["clustersRes"]]) > 0){
      uni_expr <- template_long[is_clusterRes == TRUE, unique(Formula)]
      if(length(uni_expr) > 0){
        data_agg_clustRes <- data.table::rbindlist(lapply(uni_expr, function(ex){
          tmp_cluster_var <- tryCatch({
            data_areas_dist_clust[["clustersRes"]][, .(var = eval(parse(text = ex))), by = .(area, cluster)]
          }, error = function(e){
            warning(paste0('Yearly Outputs Long / ', ex, "' :", e$message))
            NULL
          })
          if(!is.null(tmp_cluster_var)){
            tmp_cluster_var[, Formula := ex]
          }
        }
        ))
        
        data_agg_clustRes[, cluster := gsub("[[:space:]]+", "", paste0("cluster-", cluster))]
        data_agg_clustRes <- dcast(data_agg_clustRes, cluster + Formula ~ area, value.var = "var", fill = 0)
        setnames(data_agg_clustRes, "cluster", "tmp_mathcing_name")
      }
    }
    
    # areas ?
    if(any(template_long[["is_area"]]) && !is.null(data_areas_dist_clust) && nrow(data_areas_districts) > 0){
      uni_expr <- template_long[is_area == TRUE, unique(Formula)]
      if(length(uni_expr) > 0){
        data_agg_clust_areas <- data.table::rbindlist(lapply(uni_expr, function(ex){
          tmp_cluster_var <- tryCatch({
            data_areas_districts[, .(var = eval(parse(text = ex))), by = .(area)]
          }, error = function(e){
            warning(paste0('Yearly Outputs Long / ', ex, "' :", e$message))
            NULL
          })
          if(!is.null(tmp_cluster_var)){
            tmp_cluster_var[, Formula := ex]
          }
        }
        ))
        
        data_agg_clust_areas <- dcast(data_agg_clust_areas, Formula ~ area, value.var = "var", fill = 0)
        data_agg_clust_areas <- data_agg_clust_areas[!is.na(Formula), ]
        data_agg_clust_areas[, tmp_mathcing_name := "area"]
        
      }
    }
    
    # browser()
    full_merge <- rbindlist(list(data_agg_clust, data_agg_clust_areas, data_agg_clustRes), use.names = T, fill = TRUE)
    
    colnames(full_merge)[-c(1:2)] <- tolower(colnames(full_merge)[-c(1:2)])
    setcolorder(full_merge, c("tmp_mathcing_name", "Formula", tolower(areas_districts_selections)))
    
    template_long_final <- merge(template_long, full_merge, 
                                 by = c("tmp_mathcing_name", "Formula"),
                                 all.x = T)
    setorder(template_long_final, order_tmp_)
    data_long_out <- template_long_final[, c((ncol(template_long)+1):ncol(template_long_final)), with = FALSE]
    colnames(data_long_out) <- toupper(colnames(data_long_out))
  }
  
  if(nrow(yearly_output_short) > 0){
    try({
      yearly_output_short[, colnames(yearly_output_short)[-c(1)] := lapply(.SD, function(x) {
        x[is.nan(x)] <- 0 ; 
        x[is.infinite(x)] <- 0;
        x[is.na(x)] <- 0; x}), .SDcols = (colnames(yearly_output_short)[-c(1)])]  }, T)
  }
  
  if(nrow(yearly_welfare) > 0){
    try({
      yearly_welfare[, colnames(yearly_welfare)[-c(1,2)] := lapply(.SD, function(x) {
        x[is.nan(x)] <- 0 ; 
        x[is.infinite(x)] <- 0;
        x[is.na(x)] <- 0; x}), .SDcols = (colnames(yearly_welfare)[-c(1,2)])]}, T)
  }
  
  if(nrow(data_long_out) > 0){
    try({
      data_long_out[, colnames(data_long_out) := lapply(.SD, function(x) {
        x[is.nan(x)] <- 0 ; 
        x[is.infinite(x)] <- 0;
        x[is.na(x)] <- 0; x}), .SDcols = (colnames(data_long_out))]}, T)
  }
  
  list(yearly_output_short = yearly_output_short, 
       yearly_welfare = yearly_welfare, 
       yearly_interconnection = yearly_interconnection,
       data_intro = data_intro, 
       data_long_out = data_long_out, 
       areas_districts_selections = areas_districts_selections,
       links_selections = links_selections) 
}


#export
exportAnnualOutputs <- function(infile_name, outfile_name, annual_outputs){
  
  wb <- loadWorkbook(infile_name)
  options(scipen = 0)
  
  # options(scipen = 10000, digits = 1)
  
  try({
    writeData(wb, "Identification", annual_outputs$data_intro[-4,])
    writeData(wb, "Identification", toupper(annual_outputs$areas_districts_selections), startRow = 14, startCol = 2)
    writeData(wb, "Identification", toupper(annual_outputs$links_selections), startRow = 14, startCol = 3)
    writeData(wb, "Identification", annual_outputs$data_intro[4, get(colnames(annual_outputs$data_intro)[2])], startRow = 14, startCol = 4)
  }, T)
  
  try({
    writeData(wb, "Yearly Outputs Short", annual_outputs$data_intro[1:4, 1:2])
    
    if(nrow(annual_outputs$yearly_output_short) > 0){
      writeData(wb, "Yearly Outputs Short", annual_outputs$yearly_output_short[,-(1)], startRow = 7, borders = "all", startCol = 3)
    } 
    
    if(nrow(annual_outputs$yearly_welfare) > 0){
      
      writeData(wb, "Yearly Welfare", annual_outputs$data_intro[1:4, 1:2])
      
      writeData(wb, "Yearly Welfare", annual_outputs$yearly_welfare[,-(1)], startRow = 7, borders = "all", startCol = 3)
    }
    
  }, T)
  
  try({
    if(!is.null(annual_outputs$yearly_interconnection) && nrow(annual_outputs$yearly_interconnection) > 0){
      writeData(wb, "Yearly Interconnection", annual_outputs$data_intro[1:4, 1:2])
      
      # deleteData(wb, "Yearly Interconnection", cols = 1:9, rows = rep(9, 9))
      
      writeData(wb, "Yearly Interconnection", annual_outputs$yearly_interconnection, 
                startRow = 10, borders = "all", startCol = 2, colNames = FALSE)
    }
  }, T)
  try({
    
    writeData(wb, "Yearly Outputs Long", annual_outputs$data_intro[1:4, 1:2])
    
    if(nrow(annual_outputs$data_long_out) > 0){
      writeData(wb, "Yearly Outputs Long", annual_outputs$data_long_out, startRow = 7, borders = "all", startCol = 3)
      
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
importAntaresDatasHourly <- function(opts, 
                                     areas_districts_selections, 
                                     links_selections, 
                                     mcYears = 1,
                                     removeVirtualAreas = FALSE,
                                     storageFlexibility = NULL, 
                                     production = NULL,
                                     reassignCosts = FALSE, 
                                     newCols = TRUE, 
                                     rowBal = TRUE, 
                                     storage_vars = c("PSP", "PSP_Closed", "BATT", "DSR", "EV", "P2G", "H2")){
  
  #init :
  # browser()
  data_areasH = NULL
  data_linksH = NULL
  
  any_removeVirtualAreas <- FALSE
  check <- lapply(removeVirtualAreas, function(x) if(!is.null(x) && length(x) == 1 && x) {any_removeVirtualAreas <<- TRUE})
  all_storageFlexibility <- NULL
  all_production <- NULL
  if(length(storageFlexibility) > 0) all_storageFlexibility <- tolower(unlist(storageFlexibility))
  if(length(production) > 0) all_production <- tolower(unlist(production))
  
  
  opts$districtsDef[, district := tolower(district)]
  opts$districtsDef[, area := tolower(area)]
  
  if(length(areas_districts_selections) > 0){
    
    if("all" %in% areas_districts_selections){
      areas_districts_selections <- unique(c(tolower(opts$areaList), tolower(opts$districtList)))
      
      if(any_removeVirtualAreas && (length(all_storageFlexibility) > 0 || length(all_production) > 0)){
        ind_rm <- grepl(paste(paste0("(", c(all_storageFlexibility, all_production), ")"), collapse = "|"), 
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
    
    if(opts$antaresVersion >= 810 | length(opts$areasWithResClusters) > 0){
      areas_clusters_res_selection <- intersect(areas_districts_selections_add, tolower(opts$areasWithResClusters))
    } else {
      areas_clusters_res_selection <- NULL
    }
    
  } else {
    areas_districts_selections <- NULL
    areas_districts_selections_add <- NULL
    areas_selection <- NULL
    districts_selection <- NULL
    areas_clusters_selection <- NULL
    areas_clusters_res_selection <- NULL
  }
  
  if(length(links_selections) > 0){
    links_selections <- tolower(links_selections)
  } else {
    links_selections <- NULL
  }
  
  if("all" %in% links_selections){
    links_selections <- unique(tolower(opts$linkList))
    if(any_removeVirtualAreas && (length(all_storageFlexibility) > 0 || length(all_production) > 0)){
      ind_rm <- grepl(paste(paste0("(", c(all_storageFlexibility, all_production), ")"), collapse = "|"), 
                      links_selections)
      links_selections <- links_selections[!ind_rm]
    }
  }
  
  if(!any_removeVirtualAreas){
    data_h <- readAntares(areas = areas_selection, 
                          districts = districts_selection,
                          clusters = areas_clusters_selection,
                          clustersRes = areas_clusters_res_selection,
                          links = links_selections,
                          timeStep = "hourly", select = NULL, mcYears = mcYears)
    
    
  } else {
    
    if(opts$antaresVersion >= 810  | length(opts$areasWithResClusters) > 0){
      clusters_res <- "all"
    } else {
      clusters_res <- NULL
    }
    
    data_h <- readAntares(areas = "all", 
                          districts = "all",
                          clusters = "all",
                          clustersRes = clusters_res,
                          links = "all",
                          timeStep = "hourly", 
                          select = NULL, 
                          linkCapacity = FALSE, mcYears = mcYears)
    
    for(ii in 1:length(removeVirtualAreas)){
      if(!is.null(removeVirtualAreas[[ii]]) && length(removeVirtualAreas[[ii]]) == 1 && removeVirtualAreas[[ii]]){
        data_h <- suppressWarnings({
          removeVirtualAreas(
            data_h, 
            storageFlexibility = storageFlexibility[[ii]], 
            production = production[[ii]],
            reassignCosts = reassignCosts[[ii]], 
            newCols = newCols[[ii]], 
            rowBal = rowBal
          )
        })
      }
    }
    
    if(!is.null(data_h$areas) && nrow(data_h$areas) > 0){
      data_h$areas <- data_h$areas[area %in% areas_selection, ]
    }
    
    if(!is.null(data_h[["clusters"]]) && nrow(data_h[["clusters"]]) > 0){
      data_h[["clusters"]] <- data_h[["clusters"]][area %in% areas_clusters_selection, ]
    }
    
    if(!is.null(data_h[["clustersRes"]]) && nrow(data_h[["clustersRes"]]) > 0){
      data_h[["clustersRes"]] <- data_h[["clustersRes"]][area %in% areas_clusters_res_selection, ]
    }
    
    if(!is.null(data_h$districts) && nrow(data_h$districts) > 0){
      data_h$districts <- data_h$districts[district %in% districts_selection, ]
    }
    
    if(!is.null(data_h$links) && nrow(data_h$links) > 0){
      data_h$links <- data_h$links[link %in% links_selections, ]
    }
    
    gc(reset = T)
    
  }
  
  if(!is.null(data_h$areas) && nrow(data_h$areas) > 0){
    st_v <- intersect(storage_vars, colnames(data_h$areas))
    if(length(st_v) > 0){
      new_cols <- paste0(st_v, "_POS")
      data_h$areas[, c(new_cols) := lapply(.SD, function(x) ifelse(x > 0, x, 0)), .SDcols = st_v]
      
      new_cols <- paste0(st_v, "_NEG")
      data_h$areas[, c(new_cols) := lapply(.SD, function(x) ifelse(x < 0, x, 0)), .SDcols = st_v]
    }
  }
  
  
  if(!is.null(data_h$districts) && nrow(data_h$districts) > 0){
    st_v <- intersect(storage_vars, colnames(data_h$districts))
    if(length(st_v) > 0){
      new_cols <- paste0(st_v, "_POS")
      data_h$districts[, c(new_cols) := lapply(.SD, function(x) ifelse(x > 0, x, 0)), .SDcols = st_v]
      
      new_cols <- paste0(st_v, "_NEG")
      data_h$districts[, c(new_cols) := lapply(.SD, function(x) ifelse(x < 0, x, 0)), .SDcols = st_v]
    }
  }
  
  
  # Enrichissement par la somme des clusters par districts : 
  if(!is.null(data_h[["clusters"]]) && nrow(data_h[["clusters"]]) > 0){
    try({
      tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                   data_h[["clusters"]], by = c("area"), allow.cartesian = TRUE)
      tmp[, c("production", "NP Cost", "NODU") := list(as.numeric(production), as.numeric(`NP Cost`), as.numeric(NODU))]
      tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                          by = c("district", "cluster", "time"), .SDcols = c("production", "NP Cost", "NODU")))
      setnames(tmp, "district", "area")
      data_h[["clusters"]] <- rbindlist(list(data_h[["clusters"]][area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
      rm(tmp)
      gc()
    }, T)
  }
  
  if(!is.null(data_h[["clustersRes"]]) && nrow(data_h[["clustersRes"]]) > 0){
    try({
      tmp <- merge(opts$districtsDef[district %in% areas_districts_selections], 
                   data_h[["clustersRes"]], by = c("area"), allow.cartesian = TRUE)
      tmp[, c("production") := list(as.numeric(production))]
      tmp <- na.omit(cube(tmp, j = lapply(.SD, sum), 
                          by = c("district", "cluster", "time"), .SDcols = c("production")))
      setnames(tmp, "district", "area")
      data_h[["clustersRes"]] <- rbindlist(list(data_h[["clustersRes"]][area %in% areas_districts_selections], tmp), use.names = T, fill = TRUE)
      rm(tmp)
      gc()
    }, T)
  }
  
  list(
    data = data_h, 
    areas_districts_selections = areas_districts_selections, 
    links_selections = links_selections, 
    opts = opts
  )
}

# formatage
formatHourlyOutputs <- function(data_h, 
                                areas_selections,
                                market_data_code, 
                                links_selections, 
                                opts, 
                                dico = dico){
  
  # browser()
  data_out <- NA ; dt_stats <- NA ; areas_districts <- NA
  
  if("data.table" %in% class(data_h) && attr(data_h, "type") == "links"){
    data_links <- data_h
  } else {
    data_links <- data_h$links
  }
  
  try({
    
    areas_selections <- tolower(areas_selections)
    
    if(!is.null(data_h[["clusters"]]) && nrow(data_h[["clusters"]])){
      data_h[["clusters"]] <- aggregateClusters(data_h[["clusters"]], var = "production", opts, hourly = T)
    }
    
    if(!is.null(data_h[["clustersRes"]]) && nrow(data_h[["clustersRes"]])){
      data_h[["clustersRes"]] <- aggregateClusters(data_h[["clustersRes"]], var = "production", opts, hourly = T)
    }
    
    if(!is.null(data_h$districts) && nrow(data_h$districts) > 0){
      setnames(data_h$districts, "district", "area")
      if(!is.null(data_h$areas) && nrow(data_h$areas) > 0){
        data_h_dist <- rbindlist(list(data_h$areas, data_h$districts), use.names = T, fill = T)
      } else {
        data_h_dist <- data_h$districts
      }
    } else if(!is.null(data_h$areas) && nrow(data_h$areas) > 0){
      data_h_dist <- data_h$areas
    }
    
    if("PSP" %in% colnames(data_h_dist) && "PSP_TURB" %in% market_data_code) {
      data_h_dist[, PSP_TURB := ifelse(PSP > 0, PSP, 0)]
    }
    
    if("PSP" %in% colnames(data_h_dist) && "PSP_TURB" %in% market_data_code) {
      data_h_dist[, PSP_PUMP := ifelse(PSP < 0, PSP, 0)]
    }
    
    cols_areas <- intersect(colnames(data_h_dist), market_data_code)
    
    data_all <- data_h_dist[area %in% areas_selections,
                            c("area", "time", cols_areas), with = F]
    
    
    if(!is.null(data_h[["clusters"]]) && nrow(data_h[["clusters"]])){
      temp <- dcast(data_h[["clusters"]], area + time ~ cluster, value.var = "var", fill = 0)
      
      data_all <- merge(temp, data_all, by = c("area", "time"), all = TRUE)
    }
    
    if(!is.null(data_h[["clustersRes"]]) && nrow(data_h[["clustersRes"]])){
      temp <- dcast(data_h[["clustersRes"]], area + time ~ cluster, value.var = "var", fill = 0)
      
      data_all <- merge(temp, data_all, by = c("area", "time"), all = TRUE)
    }
    
    data_all <- data_all[, c("area", "time", intersect(market_data_code, colnames(data_all))), with = F]
    
    # ajout d'éventuelles variables demandées mais qui n'existent pas
    cols_comp <- setdiff(market_data_code, colnames(data_all))
    
    if(length(cols_comp) > 0) data_all[, (cols_comp) :=  0]
    data_all <- data_all[, c("area", "time", market_data_code), with = F]
    
    cols_availables <- intersect(market_data_code, colnames(data_all))
    
    setcolorder(data_all, neworder = c("area", "time", cols_availables))
    
    data_out <- unique(data_all[,.(Date = time)])
    data_out[, Hour := .I]
    setcolorder(data_out, c("Hour", "Date")) 
    
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