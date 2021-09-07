function(input, output, session) {

  # shiny files
  volumes <- {
    vol <- getVolumes()()
    # names(vol) <- gsub("/$", "", vol)
    names(vol) <- vol
    
    if(!is.null(study_dir) && study_dir != "" && dir.exists(study_dir)){
      study_path <- strsplit(study_dir, "/")[[1]]
      study_path <- paste0(study_path[-length(study_path)], collapse = "/")
      c(Home = fs::path_home(), vol, Antares = study_path)
    } else {
      c(Home = fs::path_home(), vol)
    }
  }
  
  output$is_manipulate_new_version <- reactive({
    packageVersion("manipulateWidget") >= "0.11"
  })
  
  outputOptions(output, "is_manipulate_new_version", suspendWhenHidden = FALSE)
  
  #----------------
  # Write h5
  #----------------
  # source("src/server/07_write_h5.R", local = T)
  
  #----------------
  # set / read / export data
  #----------------
  source("src/server/01_set_read_data.R", local = T)
  
  source("src/server/01b_infoBox.R", local = T)
  
  source("src/server/09_export_data.R", local = T)
  
  #----------------
  # shared parameters
  #----------------
  
  modules <- reactiveValues(prodStack = NULL, exchangesStack = NULL, plotts = NULL, plotMap = NULL, 
                            id_prodStack = NULL, id_exchangesStack = NULL, id_plotts = NULL, id_plotMap = NULL,
                            init_prodStack = FALSE, init_exchangesStack = FALSE, init_plotts = FALSE, init_plotMap = FALSE)
  
  # all data loaded by user, with informations
  list_data_all <- reactiveValues(antaresDataList = list(), params = list(), 
                                  have_links = c(), have_areas = c(), opts = list())
  
  # set of controls
  list_data_controls <- reactiveValues(have_links = FALSE, have_areas = FALSE, 
                                       n_links = -1, n_areas = -1, n_maps = -1)
  
  
  #-----------------
  # Importation de nouvelles donnees
  #-----------------
  
  source("src/server/02_load_data.R", local = T)
  
  #----------------
  # Dataset selection
  #----------------
  source("src/server/03_data_selection.R", local = T)
  
  #-----------------
  # modules
  #-----------------
  
  # launch when click on ""Launch Analysis" button
  # and get back which opts / data to keep
  ind_keep_list_data <- reactive({
    if(input$update_module > 0){
      isolate({
        names_input <- names(input)
        keep_input <- names_input[grepl("^list_study_check", names_input)]
        keep_input <- keep_input[as.numeric(gsub("list_study_check", "", keep_input)) <= length(list_data_all$antaresDataList)]
        if(length(keep_input) > 0){
          keep_input <- sort(keep_input)
          final_keep <- sapply(keep_input, function(x){
            input[[x]]
          })
          
          # all to keep
          ind_all <- which(final_keep)
          
          # with areas
          ind_areas <- intersect(which(list_data_all$have_areas), ind_all)
          
          # all areas in data
          if(length(ind_areas) > 0){
            all_areas <- unique(do.call("c", lapply(ind_areas, function(x){
              data <- list_data_all$antaresDataList[[x]]
              if("antaresDataTable" %in% class(data)){
                unique(as.character(data$area))            
              } else  if("antaresDataList" %in% class(data)){
                unique(as.character(data$areas$area))
              }
            })))
          } else {
            all_areas <- NULL
          }
          
          # with links
          ind_links <- intersect(which(list_data_all$have_links), ind_all)
          
          # refStudy
          refStudyTrue <- .get_if_output_has_refStudy()
          idRefStudy <- 0
          if(refStudyTrue){
            nameRefStudy <- .get_name_refStudy()
            idRefStudy <- .get_id_refStudy()
            validAreas <- setdiff(ind_areas, idRefStudy)
            validLinks <- setdiff(ind_links, idRefStudy)
          }else{
            validAreas <- ind_areas
            validLinks <- ind_links
          }
          
          if(idRefStudy > 0){
            refStudy <- list_data_all$antaresDataList[idRefStudy][[1]]
          }else{
            refStudy <- NULL
          }
          
          list(ind_all = ind_all, 
               ind_areas = validAreas, 
               ind_links = validLinks,
               refStudy = refStudy, 
               all_areas = all_areas)
        } else {
          NULL
        }
      })
    } else {
      NULL
    }
  })
  
  #------------------
  # prodStack, plotTS & stackExchange
  #------------------
  
  source("src/server/05_modules.R", local = T)
  
  #------------
  # plotMap
  #------------
  
  source("src/server/06_module_map.R", local = T)
  
  #------------
  # MED Tso_map
  #------------
  
  source("src/server/06b_module_medtso_map.R", local = T)
  
  #------------
  # Format excel output
  #------------
  
  source("src/server/11_format_output.R", local = T)
  
  #----------------
  # shared inputs
  #----------------
  
  source("src/server/04_shared_input.R", local = T)
  
  #----------------
  # language
  #----------------
  
  source("src/server/08_language.R", local = T)
  
  #----------------
  # memory options
  #----------------
  observe({
    if(!is.na(input$ram_limit)){
      antaresRead::setRam(input$ram_limit)
    }
  })
  
  observe({
    if(!is.na(input$data_module)){
      limitSizeGraph(input$data_module)
    }
  })
  
  #----------------
  # quit
  #----------------
  buildExe <- FALSE
  
  if(!buildExe)
  {
  # in case of classic use : 
  observe({
    if(input$quit > 0){
      stopApp(returnValue = TRUE)
    }
  })
  }else{
    # in case of Rinno / packaging app for windows
    # (and so comment previous observe....!)
    #
    # in app mod
    observe({
      if(input$quit > 0){
        stopApp()
        q("no")
      }
    })

    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  # if (!interactive()) {
  #   session$onSessionEnded(function() {
  #     stopApp()
  #     q("no")
  #   })
  # }

}
