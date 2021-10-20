#-----------------
# Importation de nouvelles donnees
#-----------------
observe({
  if(input$import_data > 0){
    isolate({
      if(!is.null(opts())){
        # not a .h5 file, so read data
        if(!opts()$h5){
          # Treat mcYears
          if(input$read_type_mcYears == "synthetic"){
            mcYears <- NULL
          } else if(input$read_type_mcYears == "all"){
            mcYears <- "all"
          } else {
            mcYears <- as.numeric(input$read_mcYears)
          }
          
          # import data
          list_warning <- list() 
          
          data <- withCallingHandlers({
            tryCatch({
              readAntares(areas = input$read_areas, links = input$read_links, 
                          clusters = input$read_clusters, clustersRes = input$read_clusters_res,
                          districts = input$read_districts, misc = input$read_misc, 
                          thermalAvailabilities = input$read_thermalAvailabilities,
                          hydroStorage = input$read_hydroStorage, hydroStorageMaxPower = input$read_hydroStorageMaxPower, 
                          reserve = input$read_reserve, linkCapacity = input$read_linkCapacity, 
                          mustRun = input$read_mustRun, thermalModulation = input$read_thermalModulation,
                          select = input$read_select, mcYears = mcYears, timeStep = input$read_timeStep, 
                          opts = opts(), 
                          # parallel = input$read_parallel,
                          simplify = TRUE, showProgress = FALSE)},
              error = function(e){
                showModal(modalDialog(
                  title = "Error reading data",
                  easyClose = TRUE,
                  footer = NULL,
                  paste("Please update input. Error : ", e$message, sep = "\n")
                ))
                list()
              })}, 
            warning = function(w){
              list_warning[[length(list_warning) + 1]] <<- w$message
            }
          )
          
          if(length(list_warning) > 0 & !is.null(data) && length(data) > 0){
            showModal(modalDialog(
              title = "Warning reading data",
              easyClose = TRUE,
              footer = NULL,
              HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
            ))
          }
          
          # removeVirtualAreas
          if(input$rmva_ctrl){
            if(length(data) > 0){
              
              list_warning <- list() 
              
              data <- withCallingHandlers({
                
                storageFlexibility_list <- build_storage_list(
                  PSP = input$rmva_storageFlexibility,
                  PSP_Closed = input$rmva_PSP_Closed,
                  BATT = input$rmva_BATT,
                  DSR = input$rmva_DSR, 
                  EV = input$rmva_EV, 
                  P2G = input$rmva_P2G, 
                  H2 = input$rmva_H2
                )
                
                tryCatch({
                  removeVirtualAreas(x = data, 
                                     storageFlexibility = storageFlexibility_list, 
                                     production = input$rmva_production,
                                     reassignCosts = input$rmva_reassignCosts, 
                                     newCols = input$rmva_newCols)},
                  error = function(e){
                    showModal(modalDialog(
                      title = "removeVirtualAreas : error",
                      easyClose = TRUE,
                      footer = NULL,
                      paste("Please update input. Error : ", e$message, sep = "\n")
                    ))
                    list()
                  })}, 
                warning = function(w){
                  list_warning[[length(list_warning) + 1]] <<- w$message
                }
              )
              
              if(input$rmva_ctrl_step_2){
                
                data <- withCallingHandlers({
                  
                  storageFlexibility_list <- build_storage_list(
                    PSP = input$rmva_storageFlexibility_2,
                    PSP_Closed = input$rmva_PSP_Closed_2,
                    BATT = input$rmva_BATT_2,
                    DSR = input$rmva_DSR_2, 
                    EV = input$rmva_EV_2, 
                    P2G = input$rmva_P2G_2, 
                    H2 = input$rmva_H2_2
                  )
                  
                  tryCatch({
                    removeVirtualAreas(x = data, 
                                       storageFlexibility = storageFlexibility_list, 
                                       production = input$rmva_production_2,
                                       reassignCosts = input$rmva_reassignCosts_2, 
                                       newCols = input$rmva_newCols_2)},
                    error = function(e){
                      showModal(modalDialog(
                        title = "removeVirtualAreas step 2 : error",
                        easyClose = TRUE,
                        footer = NULL,
                        paste("Please update input. Error : ", e$message, sep = "\n")
                      ))
                      list()
                    })}, 
                  warning = function(w){
                    list_warning[[length(list_warning) + 1]] <<- w$message
                  }
                )
              }
              
              if(input$rmva_ctrl_step_2 & input$rmva_ctrl_step_3){
                
                data <- withCallingHandlers({
                  
                  storageFlexibility_list <- build_storage_list(
                    PSP = input$rmva_storageFlexibility_3,
                    PSP_Closed = input$rmva_PSP_Closed_3,
                    BATT = input$rmva_BATT_3,
                    DSR = input$rmva_DSR_3, 
                    EV = input$rmva_EV_3, 
                    P2G = input$rmva_P2G_3, 
                    H2 = input$rmva_H2_3
                  )
                  
                  tryCatch({
                    removeVirtualAreas(x = data, 
                                       storageFlexibility = storageFlexibility_list, 
                                       production = input$rmva_production_3,
                                       reassignCosts = input$rmva_reassignCosts_3, 
                                       newCols = input$rmva_newCols_3)},
                    error = function(e){
                      showModal(modalDialog(
                        title = "removeVirtualAreas step 3 : error",
                        easyClose = TRUE,
                        footer = NULL,
                        paste("Please update input. Error : ", e$message, sep = "\n")
                      ))
                      list()
                    })}, 
                  warning = function(w){
                    list_warning[[length(list_warning) + 1]] <<- w$message
                  }
                )
              }
              
              if(length(list_warning) > 0 & !is.null(data) && length(data) > 0){
                showModal(modalDialog(
                  title = "removeVirtualAreas : warning",
                  easyClose = TRUE,
                  footer = NULL,
                  HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
                ))
              }
              
            }
          }
          
          if(length(data) > 0){
            # save params
            params <- list(
              areas = input$read_areas, 
              links = input$read_links, 
              clusters = input$read_clusters,
              clustersRes = input$read_clusters_res,
              districts = input$read_districts, 
              misc = input$read_misc, 
              thermalAvailabilities = input$read_thermalAvailabilities,
              hydroStorage = input$read_hydroStorage, 
              hydroStorageMaxPower = input$read_hydroStorageMaxPower, 
              reserve = input$read_reserve, 
              linkCapacity = input$read_linkCapacity, 
              mustRun = input$read_mustRun, 
              thermalModulation = input$read_thermalModulation,
              select = input$read_select, 
              mcYears = mcYears, 
              timeStep = input$read_timeStep, 
              parallel = input$read_parallel
            )
            
            if("antaresDataList" %in% class(data)){
              ind_rm <- names(data)[which(sapply(data, function(x) nrow(x) == 0))]
              if(length(ind_rm) > 0){
                for(i in ind_rm){
                  data[[i]] <- NULL
                }
              }
              
              if(!is.null(data$areas)){

                st_v <- intersect(storage_vars, colnames(data$areas))
                if(length(st_v) > 0){
                  new_cols <- paste0(st_v, "_POS")
                  data$areas[, c(new_cols) := lapply(.SD, function(x) ifelse(x > 0, x, 0)), .SDcols = st_v]
                  
                  new_cols <- paste0(st_v, "_NEG")
                  data$areas[, c(new_cols) := lapply(.SD, function(x) ifelse(x < 0, x, 0)), .SDcols = st_v]
                }

                if(all(c("MIX. FUEL", "MISC. DTG") %in% colnames(data$areas))){
                  data$areas[, c("Others_non-renewable") := `MIX. FUEL` + `MISC. DTG`]
                }      
                if(all(c("H. STOR", "H. ROR") %in% colnames(data$areas))){
                  data$areas[, c("Hydro") := `H. STOR` + `H. ROR`]
                }     
              }
              
              if(!is.null(data$districts)){
                
                st_v <- intersect(storage_vars, colnames(data$districts))
                if(length(st_v) > 0){
                  new_cols <- paste0(st_v, "_POS")
                  data$districts[, c(new_cols) := lapply(.SD, function(x) ifelse(x > 0, x, 0)), .SDcols = st_v]
                  
                  new_cols <- paste0(st_v, "_NEG")
                  data$districts[, c(new_cols) := lapply(.SD, function(x) ifelse(x < 0, x, 0)), .SDcols = st_v]
                }
                
                if(all(c("MIX. FUEL", "MISC. DTG") %in% colnames(data$districts))){
                  data$districts[, c("Others_non-renewable") := `MIX. FUEL` + `MISC. DTG`]
                }      
                if(all(c("H. STOR", "H. ROR") %in% colnames(data$districts))){
                  data$districts[, c("Hydro") := `H. STOR` + `H. ROR`]
                }     
              }
              
              # split FLOW LIN. en NEG et POS
              if(!is.null(data$links)){
                if("FLOW LIN." %in% colnames(data$links)){
                  data$links[, c("FLOW_NEG", "FLOW_POS") := list(
                                   ifelse(`FLOW LIN.` < 0, `FLOW LIN.`, 0),
                                   ifelse(`FLOW LIN.` > 0, `FLOW LIN.`, 0)
                                 )]
                }
              }
            } else if("antaresDataTable" %in% class(data)){
              if(nrow(data) > 0 && "area" %in% colnames(data)){
                
                st_v <- intersect(storage_vars, colnames(data))
                if(length(st_v) > 0){
                  new_cols <- paste0(st_v, "_POS")
                  data[, c(new_cols) := lapply(.SD, function(x) ifelse(x > 0, x, 0)), .SDcols = st_v]
                  
                  new_cols <- paste0(st_v, "_NEG")
                  data[, c(new_cols) := lapply(.SD, function(x) ifelse(x < 0, x, 0)), .SDcols = st_v]
                }
                
                if(all(c("MIX. FUEL", "MISC. DTG") %in% colnames(data))){
                  data[, c("Others_non-renewable") := `MIX. FUEL` + `MISC. DTG`]
                }      
                if(all(c("H. STOR", "H. ROR") %in% colnames(data))){
                  data[, c("Hydro") := `H. STOR` + `H. ROR`]
                }
              }
              
              if(nrow(data) > 0 && "link" %in% colnames(data)){
                if("FLOW LIN." %in% colnames(data)){
                  data[, c("FLOW_NEG", "FLOW_POS") := list(
                    ifelse(`FLOW LIN.` < 0, `FLOW LIN.`, 0),
                    ifelse(`FLOW LIN.` > 0, `FLOW LIN.`, 0)
                  )]
                }
              }
            }
            
 
            n_list <- length(list_data_all$antaresDataList) + 1
            list_data_all$antaresDataList[[n_list]] <- data
            
            # write params and links control
            list_data_all$params[[n_list]] <- params
            list_data_all$opts[[n_list]] <- opts()
            if(!is.null(input$read_links)){
              list_data_all$have_links[n_list] <- TRUE
            } else {
              list_data_all$have_links[n_list] <- FALSE
            }
            have_areas <- is.null(input$read_areas) & is.null(input$read_links) & is.null(input$read_clusters) & 
              is.null(input$read_districts) | !is.null(input$read_areas)
            if(have_areas){
              list_data_all$have_areas[n_list] <- TRUE
            } else {
              list_data_all$have_areas[n_list] <- FALSE
            }
            names(list_data_all$antaresDataList)[[n_list]] <- current_study_path()
          }
          
        } 
        # med TSO : no H5
        # else {
        #   params <- list(
        #     areas = input$read_areas, 
        #     links = input$read_links, 
        #     clusters = input$read_clusters, 
        #     clustersRes = input$read_clusters_res,
        #     districts = input$read_districts,
        #     select = input$read_select
        #   )
        #   
        #   # a .h5 file, so return opts...
        #   n_list <- length(list_data_all$antaresDataList) + 1
        #   list_data_all$antaresDataList[[n_list]] <- opts()
        #   
        #   # write params and links control
        #   list_data_all$params[[n_list]] <- params
        #   list_data_all$opts[[n_list]] <- opts()
        #   if(!is.null(input$read_links)){
        #     list_data_all$have_links[n_list] <- TRUE
        #   } else {
        #     list_data_all$have_links[n_list] <- FALSE
        #   }
        #   have_areas <- is.null(input$read_areas) & is.null(input$read_links) & is.null(input$read_clusters) & 
        #     is.null(input$read_districts) | !is.null(input$read_areas)
        #   if(have_areas){
        #     list_data_all$have_areas[n_list] <- TRUE
        #   } else {
        #     list_data_all$have_areas[n_list] <- FALSE
        #   }
        #   names(list_data_all$antaresDataList)[[n_list]] <- current_study_path()
        # }
      }
    })
  }
})

observe({
  if(input$import_data > 0){
    updateTabsetPanel(session, inputId = "tab_data", selected = "<div id=\"label_tab_analysis\" class=\"shiny-text-output\"></div>")
  }
})

# control : have data
output$have_data <- reactive({
  length(list_data_all$antaresDataList) > 0
})
outputOptions(output, "have_data", suspendWhenHidden = FALSE)