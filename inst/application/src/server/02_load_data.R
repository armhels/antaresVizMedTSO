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
              readAntares(areas = input$read_areas, links = input$read_links, clusters = input$read_clusters,
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
                  paste("Please update input. Error : ", e, sep = "\n")
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
              HTML(paste0(list_warning, collapse  = "<br><br>"))
            ))
          }
          
          # removeVirtualAreas
          if(input$rmva_ctrl){
            if(length(data) > 0){
              list_warning <- list() 
              data <- withCallingHandlers({
                tryCatch({
                  removeVirtualAreas(x = data, 
                                     storageFlexibility = input$rmva_storageFlexibility, 
                                     production = input$rmva_production,
                                     reassignCosts = input$rmva_reassignCosts, 
                                     newCols = input$rmva_newCols)},
                  error = function(e){
                    showModal(modalDialog(
                      title = "removeVirtualAreas : error",
                      easyClose = TRUE,
                      footer = NULL,
                      paste("Please update input. Error : ", e, sep = "\n")
                    ))
                    list()
                  })}, 
                warning = function(w){
                  list_warning[[length(list_warning) + 1]] <<- w$message
                }
              )
              
              if(length(list_warning) > 0 & !is.null(data) && length(data) > 0){
                showModal(modalDialog(
                  title = "removeVirtualAreas : warning",
                  easyClose = TRUE,
                  footer = NULL,
                  HTML(paste0(list_warning, collapse  = "<br><br>"))
                ))
              }
            }
          }
          
          if(length(data) > 0){
            # save params
            params <- list(
              areas = input$read_areas, links = input$read_links, clusters = input$read_clusters,
              districts = input$read_districts, misc = input$read_misc, 
              thermalAvailabilities = input$read_thermalAvailabilities,
              hydroStorage = input$read_hydroStorage, hydroStorageMaxPower = input$read_hydroStorageMaxPower, 
              reserve = input$read_reserve, linkCapacity = input$read_linkCapacity, 
              mustRun = input$read_mustRun, thermalModulation = input$read_thermalModulation,
              select = input$read_select, mcYears = mcYears, timeStep = input$read_timeStep, 
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
                if("PSP" %in% colnames(data$areas)){
                  data$areas[, c("Battery_storage_&_pumping", 
                                 "Battery_discharge_&_turbine") := list(
                                   ifelse(PSP < 0, PSP, 0),
                                   ifelse(PSP > 0, PSP, 0)
                                 )]
                }
                if(all(c("MIX. FUEL", "MISC. DTG") %in% colnames(data$areas))){
                  data$areas[, c("Others_non-renewable") := `MIX. FUEL` + `MISC. DTG`]
                }      
                if(all(c("H. STOR", "H. ROR") %in% colnames(data$areas))){
                  data$areas[, c("Hydro") := `H. STOR` + `H. ROR`]
                }     
              }
            } else if("antaresDataTable" %in% class(data)){
              if(nrow(data) > 0 && "area" %in% colnames(data)){
                if("PSP" %in% colnames(data)){
                  data[, c("Battery_storage_&_pumping", 
                                 "Battery_discharge_&_turbine") := list(
                                   ifelse(PSP < 0, PSP, 0),
                                   ifelse(PSP > 0, PSP, 0)
                                 )]
                }
                if(all(c("MIX. FUEL", "MISC. DTG") %in% colnames(data))){
                  data[, c("Others_non-renewable") := `MIX. FUEL` + `MISC. DTG`]
                }      
                if(all(c("H. STOR", "H. ROR") %in% colnames(data))){
                  data[, c("Hydro") := `H. STOR` + `H. ROR`]
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
          
        } else {
          params <- list(
            areas = input$read_areas, links = input$read_links, 
            clusters = input$read_clusters, districts = input$read_districts,
            select = input$read_select
          )
          
          # a .h5 file, so return opts...
          n_list <- length(list_data_all$antaresDataList) + 1
          list_data_all$antaresDataList[[n_list]] <- opts()
          
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