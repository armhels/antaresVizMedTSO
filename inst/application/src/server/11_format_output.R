# read data parameters ----

# observe directory 
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory_format_output
  },
  handlerExpr = {
    if (input$directory_format_output > 0) {
      # condition prevents handler execution on initial app launch
      path = choose.dir(default = readDirectoryInput(session, 'directory_format_output'))
      updateDirectoryInput(session, 'directory_format_output', value = path)
    }
  }
)

output$directory_message_format_output <- renderText({
  if(length(input$directory_format_output) > 0){
    if(input$directory_format_output == 0){
      antaresVizMedTSO:::.getLabelLanguage("Please first choose a folder with antares output", current_language$language)
    } else {
      antaresVizMedTSO:::.getLabelLanguage("No antares output found in directory", current_language$language)
    }
  }
})

# list files in directory
dir_files_format_output <- reactive({
  path <- readDirectoryInput(session, 'directory_format_output')
  if(!is.null(path)){
    files = list.files(path, full.names = T)
    data.frame(name = basename(files), file.info(files))
  } else {
    NULL
  }
})

# have antares study in directory ?
is_antares_results_format_output <- reactive({
  dir_files <- dir_files_format_output()
  is_h5 <- any(grepl(".h5$", dir_files$name))
  is_study <- all(c("output", "study.antares") %in% dir_files$name)
  list(is_h5 = is_h5, is_study = is_study)
})

output$ctrl_is_antares_study_format_output <- reactive({
  is_antares_results_format_output()$is_study & !is_antares_results_format_output()$is_h5
})

outputOptions(output, "ctrl_is_antares_study_format_output", suspendWhenHidden = FALSE)

# if have study, update selectInput list
observe({
  is_antares_results <- is_antares_results_format_output()
  if(is_antares_results$is_h5 | is_antares_results$is_study){
    isolate({
      if(is_antares_results$is_study){
        files = list.files(paste0(readDirectoryInput(session, 'directory_format_output'), "/output"), full.names = T)
      } 
      if(is_antares_results$is_h5){
        files = list.files(readDirectoryInput(session, 'directory_format_output'), pattern = ".h5$", full.names = T)
      } 
      if(length(files) > 0){
        files <- data.frame(name = basename(files), file.info(files))
        choices <- rownames(files)
        names(choices) <- files$name
      } else {
        choices <- NULL
      }
      updateSelectInput(session, "study_path_format_output", "", choices = choices)
    })
  }
})

# init opts after validation
opts_format_output <- reactive({
  if(input$init_sim_format_output > 0){
    opts <- 
      tryCatch({
        setSimulationPath(isolate(input$study_path_format_output))
      }, error = function(e){
        showModal(modalDialog(
          title = "Error setting file",
          easyClose = TRUE,
          footer = NULL,
          paste("Directory/file is not an Antares study : ", e, sep = "\n")
        ))
        NULL
      })
    if(!is.null(opts)){
      if(is.null(opts$h5)){
        opts$h5 <- FALSE
      }
      # bad h5 control
      if(opts$h5){
        if(length(setdiff(names(opts), c("h5", "h5path"))) == 0){
          showModal(modalDialog(
            easyClose = TRUE,
            footer = NULL,
            "Invalid h5 file : not an Antares study."
          ))
          opts <- NULL
        }
      }
    }
    opts
  } else {
    NULL
  }
})

output$current_opts_h5_format_output <- reactive({
  opts_format_output()$h5
})

outputOptions(output, "current_opts_h5_format_output", suspendWhenHidden = FALSE)

current_study_path_format_output <- reactive({
  if(input$init_sim_format_output > 0){
    rev(unlist(strsplit(isolate(input$study_path_format_output), "/")))[1]
  }
})


# control : have not null opts ?
output$have_study_format_output <- reactive({
  !is.null(opts_format_output())
})

outputOptions(output, "have_study_format_output", suspendWhenHidden = FALSE)

observe({
  if(input$import_data_format_output > 0){
    updateTabsetPanel(session, inputId = "format_output_panel", selected = "Parameters")
  }
})


# update readAntares / opts parameters

observe({
  opts <- opts_format_output()
  current_language <- current_language$language
  if(!is.null(opts)){
    isolate({
      
      areas <- c("all", unique(c(opts$areaList, opts$districtList)))
      updateSelectInput(session, "read_areas_y_format_output", choices = areas, selected = areas[1])
      updateSelectInput(session, "read_areas_h_format_output", choices = areas, selected = areas[1])
      
      # links
      links <- c("all", opts$linkList)
      updateSelectInput(session, "read_links_y_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("Links", current_language), " : "), 
                        choices = links, selected = links[1])
      
      updateSelectInput(session, "read_links_h_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("Links", current_language), " : "), 
                        choices = links, selected = links[1])
      
      # mcYears
      mcy <- c(opts$mcYears)
      updateSelectInput(session, "read_mcYears_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("mcYears", current_language), " : "), 
                        choices = mcy, selected = mcy[1])
      
      # removeVirtualAreas
      updateCheckboxInput(session, "rmva_ctrl_format_output", antaresVizMedTSO:::.getLabelLanguage("Remove virtual Areas", current_language), FALSE)
      
      updateSelectInput(session, "rmva_storageFlexibility_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("storageFlexibility", current_language), " : "), 
                        choices = opts$areaList, selected = NULL)
      updateSelectInput(session, "rmva_production_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("production", current_language), " : "),
                        choices = opts$areaList, selected = NULL)
      
    })
  }
})

output$ui_sel_file_import_format_output <- renderUI({
  current_language <- current_language$language
  input$init_sim # clear if change simulation
  fluidRow(
    column(6, 
           div(fileInput("file_sel_import_format_output", antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language),
                         accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")), align = "center")
    ), 
    column(6, 
           div(br(),
               tags$a(href = "readAntares_selection.xlsx", 
                      antaresVizMedTSO:::.getLabelLanguage("Download selection file template", current_language), 
                      class="btn btn-default", download = "readAntares_selection.xlsx"),
               align = "center"
           )
    )
  )
})


output$ui_file_sel_format_output <- renderUI({
  current_language <- current_language$language
  input$init_sim # clear if change simulation
  fluidRow(
    column(6, 
           div(fileInput("file_sel_format_output", antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language),
                         accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")), align = "center")
    ), 
    column(6, 
           div(br(),
               tags$a(href = "Output_Selection_template.xlsx", 
                      antaresVizMedTSO:::.getLabelLanguage("Download selection file template", current_language), 
                      class="btn btn-default", download = "Output_Selection_template.xlsx"),
               align = "center"
           )
    )
  )
})

observe({
  file_sel <- input$file_sel_format_output
  
  isolate({
    current_language <- current_language$language
    if (!is.null(file_sel)){
      withCallingHandlers({
        list_sel <- tryCatch({ 
          readTemplateFile(file_sel$datapath)},
          error = function(e){
            showModal(modalDialog(
              title = antaresVizMedTSO:::.getLabelLanguage("Error reading selection file", current_language),
              easyClose = TRUE,
              footer = NULL,
              e
            ))
            NULL
          })}, 
        warning = function(w){
          showModal(modalDialog(
            title = antaresVizMedTSO:::.getLabelLanguage("Warning reading selection file", current_language),
            easyClose = TRUE,
            footer = NULL,
            w
          ))
        })
      
      if(!is.null(list_sel)){
        if(!is.null(list_sel$areas_districts_annual)){
          updateSelectInput(session, 'read_areas_y_format_output', selected = list_sel$areas_districts_annual)
        }
        
        if(!is.null(list_sel$links_annual)){
          updateSelectInput(session, 'read_links_y_format_output', selected = list_sel$links_annual)
        }
        
        if(!is.null(list_sel$areas_districts_hourly)){
          updateSelectInput(session, 'read_areas_h_format_output', selected = list_sel$areas_districts_hourly)
        }
        
        if(!is.null(list_sel$links_hourly)){
          updateSelectInput(session, 'read_links_h_format_output', selected = list_sel$links_hourly)
        }
        
        if(!is.null(list_sel$dico)){
          dico(list_sel$dico)
        }
        
        if(!is.null(list_sel$variables_hourly)){
          choices <- unique(c(list_sel$variables_hourly, dico()$ANTARES_naming))
          selected <- list_sel$variables_hourly
          updateSelectInput(session, 'var_h_format_output', choices = choices, selected = selected)
        }
      }
    }
  })
})


dico <- reactiveVal(defaut_output_params$dico)

# sélection à partir d'un fichier
observe({
  file_sel <- input$file_sel_import_format_output
  
  isolate({
    current_language <- current_language$language
    if (!is.null(file_sel)){
      withCallingHandlers({
        list_sel <- tryCatch({ 
          antaresVizMedTSO::readStudyShinySelection(file_sel$datapath)},
          error = function(e){
            showModal(modalDialog(
              title = antaresVizMedTSO:::.getLabelLanguage("Error reading selection file", current_language),
              easyClose = TRUE,
              footer = NULL,
              e
            ))
            NULL
          })}, 
        warning = function(w){
          showModal(modalDialog(
            title = antaresVizMedTSO:::.getLabelLanguage("Warning reading selection file", current_language),
            easyClose = TRUE,
            footer = NULL,
            w
          ))
        })
      
      if(!is.null(list_sel)){
        
        mcy <- list_sel$mcYears
        if(!is.null(mcy)){
          updateRadioButtons(session, "read_type_mcYears_format_output", selected = "custom")
          updateSelectInput(session, "read_mcYears_format_output", selected = as.character(mcy))
        } else {
          updateRadioButtons(session, "read_type_mcYears_format_output", selected = "synthetic")
          updateSelectInput(session, "read_mcYears_format_output", selected = NULL)
        }
        
        updateCheckboxInput(session, "rmva_ctrl_format_output", value = list_sel$removeVirtualAreas)
        updateCheckboxInput(session, "rmva_reassignCosts_format_output", value = list_sel$reassignCost)
        updateCheckboxInput(session, "rmva_newCols_format_output", value = list_sel$newCols)
        
        updateSelectInput(session, "rmva_storageFlexibility_format_output", selected = list_sel$storageFlexibility)
        updateSelectInput(session, "rmva_production_format_output", selected = list_sel$production)
        
      }
    }
  })
})


# import data ----


observe({
  if(input$import_data_format_output > 0){
    updateTabsetPanel(session, inputId = "medtso_map_panel", selected = "Parameters")
  }
})


output$export_annual_format_output <- downloadHandler(
  filename = function() {
    paste('Annual_OutputFile_', format(Sys.time(), format = "%Y%d%m_%H%M%S"), '.zip', sep='')
  },
  content = function(con) {
    
    
    tmp_file <- paste0(tempdir(), "/", "Annual_OutputFile_", format(Sys.time(), format = "%Y%d%m_%H%M%S"), '.xlsx')

    
    # importation des donnees
    if(!is.null(opts_format_output())){
      # not a .h5 file, so read data
      if(!opts_format_output()$h5){
        
        progress <- Progress$new(session, min=0, max=1)
        on.exit(progress$close())
        progress$set(message = 'Annual Output', detail = 'Importing data...')
        
        progress$set(value = 0.1)
        
        # browser()
        # Treat mcYears
        if(input$read_type_mcYears_format_output == "synthetic"){
          mcYears <- NULL
        } else if(input$read_type_mcYears_format_output == "all"){
          mcYears <- NULL
        } else {
          mcYears <- as.numeric(input$read_mcYears_format_output)
        }
        
        # l <- list(opts = opts_format_output(), 
        #           areas_districts_selections = input$read_areas_y_format_output,
        #           links_selections = input$read_links_y_format_output, 
        #           mcYears = mcYears, 
        #           removeVirtualAreas = input$rmva_ctrl_format_output,
        #           storageFlexibility = input$rmva_storageFlexibility_format_output, 
        #           production = input$rmva_production_format_output,
        #           reassignCosts = input$rmva_reassignCosts_format_output, 
        #           newCols = input$rmva_newCols_format_output)
        # saveRDS(l, "l.RDS")
        # import data
        data <- withCallingHandlers({
          tryCatch({
            importAntaresDatasAnnual(opts = opts_format_output(), 
                                     areas_districts_selections = input$read_areas_y_format_output,
                                     links_selections = input$read_links_y_format_output, 
                                     mcYears = mcYears, 
                                     removeVirtualAreas = input$rmva_ctrl_format_output,
                                     storageFlexibility = input$rmva_storageFlexibility_format_output, 
                                     production = input$rmva_production_format_output,
                                     reassignCosts = input$rmva_reassignCosts_format_output, 
                                     newCols = input$rmva_newCols_format_output)
          },
          error = function(e){
            showModal(modalDialog(
              title = "Error reading data",
              easyClose = TRUE,
              footer = NULL,
              paste("Please update input. Error : ", e, sep = "\n")
            ))
            NULL
          })}, 
          warning = function(w){
            showModal(modalDialog(
              title = "Warning reading data",
              easyClose = TRUE,
              footer = NULL,
              w
            ))
          }
        )
        
        progress$set(value = 0.5)
        
        if(length(data) > 0){
          data
        } else {
          NULL
        }
      } else {
        data <- NULL
      }
      
      
      if(!is.null(data)){
        
        progress$set(message = 'Annual Output', detail = 'Formatting data...')
        
        vars <- system.file("application/data/excel_templates/variablesAnnualOutputLong.csv", package = "antaresVizMedTSO")
        vars <- data.table::fread(vars)
        
        sim_name <- unlist(strsplit(opts_format_output()$simPath, "/"))
        sim_name <- sim_name[length(sim_name)]

        mcYears_xlsx <- mcYears
        if(is.null(mcYears_xlsx)) mcYears_xlsx <- "synthetic"

        data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                                 "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", mcYears_xlsx, 
                                                         opts_format_output()$studyName, sim_name))
        
        
        options(scipen = 10000, digits = 1)
        
        
        # l <- list(data_areas_dist_clust = data$data_areas_dist_clust,
        #           data_areas_dist_clustH = data$data_areas_dist_clustH,
        #           dataForSurplus = data$dataForSurplus,
        #           data_areas_districts = data$data_areas_districts,
        #           links_selections = input$read_links_y_format_output,
        #           areas_districts_selections = input$read_areas_y_format_output,
        #           vars = vars, opts = data$opts, data_intro = data_intro)
        # 
        # saveRDS(l, "l2.RDS")
        # 
        # data <- formatAnnualOutputs(data_areas_dist_clust = data$data_areas_dist_clust,
        #                             data_areas_dist_clustH = data$data_areas_dist_clustH,
        #                             dataForSurplus = data$dataForSurplus,
        #                             data_areas_districts = data$data_areas_districts,
        #                             links_selections = input$read_links_y_format_output,
        #                             areas_districts_selections = input$read_areas_y_format_output,
        #                             vars = vars, opts = data$opts, data_intro = data_intro)
        
        data <- withCallingHandlers({
          tryCatch({
            formatAnnualOutputs(data_areas_dist_clust = data$data_areas_dist_clust,
                                data_areas_dist_clustH = data$data_areas_dist_clustH,
                                dataForSurplus = data$dataForSurplus,
                                data_areas_districts = data$data_areas_districts,
                                links_selections = data$links_selections,
                                areas_districts_selections = data$areas_districts_selections,
                                vars = vars, opts = data$opts, data_intro = data_intro)
          },
          error = function(e){
            showModal(modalDialog(
              title = "Error formatting data",
              easyClose = TRUE,
              footer = NULL,
              e
            ))
            NULL
          })},
          warning = function(w){
            showModal(modalDialog(
              title = "Warning formatting data",
              easyClose = TRUE,
              footer = NULL,
              w
            ))
          }
        )
        
        progress$set(value = 0.7)
        
        if(!is.null(data)){
          infile_name <- system.file("application/data/excel_templates/Annual_OutputFile_Template__R.xlsx", package = "antaresVizMedTSO")
          options(scipen = 10000, digits = 1)
          
          progress$set(message = 'Annual Output', detail = 'Writting data...')
          
          data <- withCallingHandlers({
            tryCatch({
              exportAnnualOutputs(infile_name = infile_name, outfile_name = tmp_file,
                                  annual_outputs = data, data_intro = data_intro)
            },
            error = function(e){
              showModal(modalDialog(
                title = "Error writing data",
                easyClose = TRUE,
                footer = NULL,
                e
              ))
              list()
            })}, 
            warning = function(w){
              showModal(modalDialog(
                title = "Warning writing data",
                easyClose = TRUE,
                footer = NULL,
                w
              ))
            }
          )
        }
      }   
      
      progress$set(value = 1)
      
      if(is.null(data)){
        wb <- openxlsx::createWorkbook()
        openxlsx::saveWorkbook(wb, tmp_file, overwrite = TRUE)
      }
      
      # fichier .zip
      zip(con, tmp_file, flags = "-r -j")
      # suppression du .csv
      rm(tmp_file)
    }
  }
)


output$export_hourly_format_output <- downloadHandler(
  filename = function() {
    paste('Hourly_OutputFile_', format(Sys.time(), format = "%Y%d%m_%H%M%S"), '.zip', sep='')
  },
  content = function(con) {
    
    tmp_file <- paste0(tempdir(), "/", "Hourly_OutputFile_", format(Sys.time(), format = "%Y%d%m_%H%M%S"), '.xlsx')
    
    
    # importation des donnees
    if(!is.null(opts_format_output())){
      # not a .h5 file, so read data
      if(!opts_format_output()$h5){
        
        progress <- Progress$new(session, min=0, max=1)
        on.exit(progress$close())
        
        progress$set(message = 'Hourly Output', detail = 'Importing data...')
        progress$set(value = 0.1)
        
        # browser()
        # Treat mcYears
        if(input$read_type_mcYears_format_output == "synthetic"){
          mcYears <- NULL
        } else if(input$read_type_mcYears_format_output == "all"){
          mcYears <- NULL
        } else {
          mcYears <- as.numeric(input$read_mcYears_format_output)
        }
        
        # import data
        data <- withCallingHandlers({
          tryCatch({
            importAntaresDatasHourly(opts = opts_format_output(), 
                                     areas_districts_selections = input$read_areas_h_format_output,
                                     links_selections = input$read_links_h_format_output, 
                                     mcYears = mcYears, 
                                     removeVirtualAreas = input$rmva_ctrl_format_output,
                                     storageFlexibility = input$rmva_storageFlexibility_format_output, 
                                     production = input$rmva_production_format_output,
                                     reassignCosts = input$rmva_reassignCosts_format_output, 
                                     newCols = input$rmva_newCols_format_output)
          },
          error = function(e){
            showModal(modalDialog(
              title = "Error reading data",
              easyClose = TRUE,
              footer = NULL,
              paste("Please update input. Error : ", e, sep = "\n")
            ))
            NULL
          })}, 
          warning = function(w){
            showModal(modalDialog(
              title = "Warning reading data",
              easyClose = TRUE,
              footer = NULL,
              w
            ))
          }
        )
        
        progress$set(value = 0.5)
        
        if(length(data) > 0){
          data
        } else {
          NULL
        }
      } else {
        data <- NULL
      }
      
      
      if(!is.null(data)){
        
        # data_antares <<- data
        # aareas_selections <<- input$read_areas_h_format_output
        # mmarket_data_code <<- input$var_h_format_output
        # llinks_selections <<- input$read_links_h_format_output
        
        progress$set(message = 'Hourly Output', detail = 'Formatting data...')
        
        mcYears_xlsx <- mcYears
        if(is.null(mcYears_xlsx)) mcYears_xlsx <- "synthetic"
        sim_name <- unlist(strsplit(opts_format_output()$simPath, "/"))
        sim_name <- sim_name[length(sim_name)]
        data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                                 "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", mcYears_xlsx, 
                                                         opts_format_output()$studyName, sim_name))
        
        
        options(scipen = 10000, digits = 1)
        
        data <- withCallingHandlers({
          tryCatch({
            formatHourlyOutputs(
              data_h = data$data,
              areas_selections = data$areas_districts_selections,
              market_data_code = input$var_h_format_output,
              links_selections = data$links_selections,
              dico = dico())
          },
          error = function(e){
            showModal(modalDialog(
              title = "Error formatting data",
              easyClose = TRUE,
              footer = NULL,
              e
            ))
            NULL
          })},
          warning = function(w){
            showModal(modalDialog(
              title = "Warning formatting data",
              easyClose = TRUE,
              footer = NULL,
              w
            ))
          }
        )
        
        progress$set(value = 0.7)
        
        if(!is.null(data)){
          # data_format <<- data
          
          infile_name <- system.file("application/data/excel_templates/hourly_OutputFile_Template__R.xlsx", package = "antaresVizMedTSO")
          options(scipen = 10000, digits = 1)
          
          progress$set(message = 'Hourly Output', detail = 'Writting data...')
          
          data <- withCallingHandlers({
            tryCatch({
              exportHourlyOutputs(hourly_outputs = data, infile_name = infile_name, outfile_name = tmp_file,
                                  data_intro = data_intro, market_data_code = input$var_h_format_output)
            },
            error = function(e){
              showModal(modalDialog(
                title = "Error writing data",
                easyClose = TRUE,
                footer = NULL,
                e
              ))
              list()
            })}, 
            warning = function(w){
              showModal(modalDialog(
                title = "Warning writing data",
                easyClose = TRUE,
                footer = NULL,
                w
              ))
            }
          )
        }
      }   
      
      progress$set(value = 1)
      
      if(is.null(data)){
        wb <- openxlsx::createWorkbook()
        openxlsx::saveWorkbook(wb, tmp_file, overwrite = TRUE)
      }
      
      # fichier .zip
      zip(con, tmp_file, flags = "-r -j")
      # suppression du .csv
      rm(tmp_file)
      
    }
  }
)
