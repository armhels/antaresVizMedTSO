# read data parameters ----

shinyDirChoose(input, "directory_format_output", 
               roots = volumes, 
               session = session, 
               defaultRoot = {
                 if(!is.null(study_dir) && study_dir != ""){
                   study_path <- strsplit(study_dir, "/")[[1]]
                   study_path <- paste0(study_path[-length(study_path)], collapse = "/")
                   if(study_path %in% volumes){
                     "Antares"
                   } else if (paste0(strsplit(study_dir, "/")[[1]][1], "/") %in% names(volumes)){
                     paste0(strsplit(study_dir, "/")[[1]][1], "/")
                   } else {
                     NULL
                   }
                 } else {
                   NULL
                 }
               })

rv_directory_format_output <- reactiveVal(study_dir)

observe({
  if (!is.null(input$directory_format_output) && !is.integer(input$directory_format_output)) {
    rv_directory_format_output(as.character(shinyFiles::parseDirPath(volumes, input$directory_format_output)))
  }
})

output$print_directory_format_output <- renderPrint({
  rv_directory_format_output()
})

observe({
  val <- rv_directory_format_output()
  if(!is.null(val) && val != ""){
    if(!isTRUE(all.equal(isolate(rv_directory()), val))){
      rv_directory(val)
    }
    if(!isTRUE(all.equal(isolate(rv_directory_medtso_maps()), val))){
      rv_directory_medtso_maps(val)
    }
  }
})

# # observe directory 
# observeEvent(
#   ignoreNULL = TRUE,
#   eventExpr = {
#     input$directory_format_output
#   },
#   handlerExpr = {
#     if (input$directory_format_output > 0) {
#       # condition prevents handler execution on initial app launch
#       path = choose.dir(default = readDirectoryInput(session, 'directory_format_output'))
#       updateDirectoryInput(session, 'directory_format_output', value = path)
#     }
#   }
# )

# output$directory_message_format_output <- renderText({
#   if(length(input$directory_format_output) > 0){
#     if(input$directory_format_output == 0){
#       antaresVizMedTSO:::.getLabelLanguage("Please first choose a folder with antares output", current_language$language)
#     } else {
#       antaresVizMedTSO:::.getLabelLanguage("No antares output found in directory", current_language$language)
#     }
#   }
# })

output$directory_message_format_output <- renderText({
  if(!is.null(input$directory_format_output) || is.integer(input$directory_format_output)){
    antaresVizMedTSO:::.getLabelLanguage("Please first choose a folder with antares output", current_language$language)
  } else {
    antaresVizMedTSO:::.getLabelLanguage("No antares output found in directory", current_language$language)
  }
})


# list files in directory
dir_files_format_output <- reactive({
  # path <- readDirectoryInput(session, 'directory_format_output')
  path <- rv_directory_format_output()
  if(!is.null(path)){
    # save path in default conf
    conf <- tryCatch(yaml::read_yaml("default_conf.yml"), error = function(e) NULL)
    if(!is.null(conf)){
      conf$study_dir <- path
      tryCatch({
        yaml::write_yaml(conf, file = "default_conf.yml")
      }, error = function(e) NULL)
    }
    
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
        # files = list.files(paste0(readDirectoryInput(session, 'directory_format_output'), "/output"), full.names = T)
        files = list.files(file.path(rv_directory_format_output(), "output"), full.names = T)
      } 
      if(is_antares_results$is_h5){
        # files = list.files(readDirectoryInput(session, 'directory_format_output'), pattern = ".h5$", full.names = T)
        files = list.files(file.path(rv_directory_format_output()), full.names = T)
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

observe({
  val <- input$study_path_format_output
  if(!is.null(val) && val != ""){
    if(!isTRUE(all.equal(isolate(input$study_path), val))){
      updateSelectInput(session, "study_path", selected =  val)
    }
    if(!isTRUE(all.equal(isolate(input$study_path_medtso_maps), val))){
      updateSelectInput(session, "study_path_medtso_maps", selected =  val)
    }
  }
})

# init opts after validation
opts_format_output_tmp <- reactive({
  if(input$init_sim_format_output > 0){
    opts <- 
      tryCatch({
        setSimulationPath(isolate(input$study_path_format_output))
      }, error = function(e){
        showModal(modalDialog(
          title = "Error setting file",
          easyClose = TRUE,
          footer = NULL,
          paste("Directory/file is not an Antares study : ", e$message, sep = "\n")
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
  opts_format_output_tmp()$h5
})

outputOptions(output, "current_opts_h5_format_output", suspendWhenHidden = FALSE)


# control : have not null opts ?
opts_format_output <- reactive({
  if(input$import_data_format_output > 0){
    isolate({
      opts_format_output_tmp()
    })
  } else {
    NULL
  }
})

output$have_study_format_output <- reactive({
  !is.null(opts_format_output())
})
outputOptions(output, "have_study_format_output", suspendWhenHidden = FALSE)

output$have_study_format_output_tmp <- reactive({
  !is.null(opts_format_output_tmp())
})
outputOptions(output, "have_study_format_output_tmp", suspendWhenHidden = FALSE)

observe({
  if(input$import_data_format_output > 0){
    updateTabsetPanel(session, inputId = "format_output_panel", selected = "Parameters")
  }
})


# update readAntares / opts parameters

observe({
  opts <- opts_format_output_tmp()
  current_language <- current_language$language
  if(!is.null(opts)){
    isolate({
      # mcYears
      mcy <- c(opts$mcYears)
      updateSelectInput(session, "read_mcYears_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("mcYears", current_language), " : "), 
                        choices = mcy, selected = mcy)
      

      # removeVirtualAreas
      updateCheckboxInput(session, "rmva_ctrl_format_output", antaresVizMedTSO:::.getLabelLanguage("enabled", current_language), FALSE)
      updateCheckboxInput(session, "rmva_ctrl_format_output_2", value = FALSE)
      updateCheckboxInput(session, "rmva_ctrl_format_output_3", value = FALSE)
      
      for(ii in rm_storage_input_import_format_final){
        updateSelectInput(session, ii, choices = opts$areaList, selected = NULL)
      }
      
      updateSelectInput(session, "rmva_production_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("production", current_language), " : "),
                        choices = opts$areaList, selected = NULL)
      updateSelectInput(session, "rmva_production_format_output_2", paste0(antaresVizMedTSO:::.getLabelLanguage("production", current_language), " : "),
                        choices = opts$areaList, selected = NULL)
      updateSelectInput(session, "rmva_production_format_output_3", paste0(antaresVizMedTSO:::.getLabelLanguage("production", current_language), " : "),
                        choices = opts$areaList, selected = NULL)
      
      updateCheckboxInput(session, "rmva_reassignCosts_format_output", antaresVizMedTSO:::.getLabelLanguage("reassignCosts", current_language), FALSE)
      updateCheckboxInput(session, "rmva_newCols_format_output", antaresVizMedTSO:::.getLabelLanguage("newCols", current_language), FALSE)
      
      updateCheckboxInput(session, "rmva_reassignCosts_format_output_2", antaresVizMedTSO:::.getLabelLanguage("reassignCosts", current_language), FALSE)
      updateCheckboxInput(session, "rmva_newCols_format_output_2", antaresVizMedTSO:::.getLabelLanguage("newCols", current_language), FALSE)
      
      updateCheckboxInput(session, "rmva_reassignCosts_format_output_3", antaresVizMedTSO:::.getLabelLanguage("reassignCosts", current_language), FALSE)
      updateCheckboxInput(session, "rmva_newCols_format_output_3", antaresVizMedTSO:::.getLabelLanguage("newCols", current_language), FALSE)
      
    })
  }
})

observe({
  opts <- opts_format_output()
  current_language <- current_language$language
  if(!is.null(opts)){
    isolate({
      
      areas <- c("all", unique(c(opts$areaList, opts$districtList)))
      if(input$rmva_ctrl_format_output && (length(input$rmva_storageFlexibility_format_output) > 0 || length(input$rmva_production_format_output) > 0)){
        ind_rm <- grepl(paste(paste0("(", c(input$rmva_storageFlexibility_format_output, input$rmva_production_format_output), ")"), collapse = "|"), 
                        areas)
        areas <- areas[!ind_rm]
      }
      
      updateSelectInput(session, "read_areas_y_format_output", choices = areas, selected = areas[1])
      updateSelectInput(session, "read_areas_h_format_output", choices = areas, selected = areas[1])
      
      # links
      links <- c("all", unique(c(opts$linkList)))
      if(input$rmva_ctrl_format_output && (length(input$rmva_storageFlexibility_format_output) > 0 || length(input$rmva_production_format_output) > 0)){
        ind_rm <- grepl(paste(paste0("(", c(input$rmva_storageFlexibility_format_output, input$rmva_production_format_output), ")"), collapse = "|"), 
                        links)
        links <- links[!ind_rm]
      }
      
      updateSelectInput(session, "read_links_y_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("Links", current_language), " : "), 
                        choices = links, selected = links[1])
      
      updateSelectInput(session, "read_links_h_format_output", paste0(antaresVizMedTSO:::.getLabelLanguage("Links", current_language), " : "), 
                        choices = links, selected = links[1])
      
    })
  }
})

output$ui_sel_file_import_format_output <- renderUI({
  current_language <- current_language$language
  input$init_sim # clear if change simulation
  fluidRow(
    column(6, 
           # div(fileInput("file_sel_import_format_output", antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language),
           #               accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")), align = "center")
           
           div(
             shinyFilesButton("file_sel_import_format_output", 
                              label = antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language), 
                              title= NULL, 
                              icon = icon("upload"),
                              multiple = FALSE, viewtype = "detail"),
             align = "center", style = "margin-top:20px")
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

output$ui_file_sel_template_format_output <- renderUI({
  current_language <- current_language$language
  input$init_sim # clear if change simulation
  fluidRow(
    column(8, offset = 2, 
           div(
             tags$br(),
             shinyFilesButton("file_sel_template_format_output", 
                              label = antaresVizMedTSO:::.getLabelLanguage("Import a template output file (.xlsx)", current_language), 
                              title= NULL, 
                              icon = icon("upload"),
                              multiple = FALSE, viewtype = "detail"), align = "center"),
           
           tags$br(),
           div(
             tags$a(href = "Annual_OutputFile_Template_R.xlsx", 
                    antaresVizMedTSO:::.getLabelLanguage("Download template output file example", current_language), 
                    class="btn btn-default", download = "Annual_OutputFile_Template_R.xlsx"), align = "center"
           )
           
    )
  )
})

shinyFileChoose(input, "file_sel_template_format_output", 
                roots = volumes, 
                session = session, 
                filetypes = c("XLS", "xls", "xlsx", "XLSX"), 
                defaultRoot = {
                  if(!is.null(file_sel_template_format_output) && file_sel_template_format_output != "" && paste0(strsplit(file_sel_template_format_output, "/")[[1]][1], "/") %in% names(volumes)){
                    paste0(strsplit(file_sel_template_format_output, "/")[[1]][1], "/")
                  } else {
                    NULL
                  }
                },
                defaultPath = {
                  if(!is.null(file_sel_template_format_output) && file_sel_template_format_output != "" && paste0(strsplit(file_sel_template_format_output, "/")[[1]][1], "/") %in% names(volumes)){
                    if(file.exists(file_sel_template_format_output)){
                      paste0(strsplit(file_sel_template_format_output, "/")[[1]][-1], collapse = "/")
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                })

template_annual_rv <- reactiveVal(file_sel_template_format_output)

output$show_template_annual <- renderText({
  paste0("Template : ", template_annual_rv())
})

observe({
  file_sel <- shinyFiles::parseFilePaths(volumes, input$file_sel_template_format_output)
  if("data.frame" %in% class(file_sel) && nrow(file_sel) == 0) file_sel <- NULL
  isolate({
    current_language <- current_language$language
    if (!is.null(file_sel)){
      # save path in default conf
      conf <- tryCatch(yaml::read_yaml("default_conf.yml"), error = function(e) NULL)
      if(!is.null(conf)){
        conf$file_sel_template_format_output <- file_sel$datapath
        tryCatch({
          yaml::write_yaml(conf, file = "default_conf.yml")
        }, error = function(e) NULL)
      }
      template_annual_rv(file_sel$datapath)
    }
  })
})


output$ui_file_sel_format_output <- renderUI({
  current_language <- current_language$language
  input$init_sim # clear if change simulation
  fluidRow(
    column(6, 
           # div(fileInput("file_sel_format_output", antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language),
           #               accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")), align = "center")
           
           div(
             shinyFilesButton("file_sel_format_output", 
                              label = antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language), 
                              title= NULL, 
                              icon = icon("upload"),
                              multiple = FALSE, viewtype = "detail"),
             align = "center", style = "margin-top:20px")
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

shinyFileChoose(input, "file_sel_format_output", 
                roots = volumes, 
                session = session, 
                filetypes = c("XLS", "xls", "xlsx", "XLSX"), 
                defaultRoot = {
                  if(!is.null(file_sel_format_output) && file_sel_format_output != "" && paste0(strsplit(file_sel_format_output, "/")[[1]][1], "/") %in% names(volumes)){
                    paste0(strsplit(file_sel_format_output, "/")[[1]][1], "/")
                  } else {
                    NULL
                  }
                },
                defaultPath = {
                  if(!is.null(file_sel_format_output) && file_sel_format_output != "" && paste0(strsplit(file_sel_format_output, "/")[[1]][1], "/") %in% names(volumes)){
                    if(file.exists(file_sel_format_output)){
                      paste0(strsplit(file_sel_format_output, "/")[[1]][-1], collapse = "/")
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                })

observe({
  # file_sel <- input$file_sel_format_output
  file_sel <- shinyFiles::parseFilePaths(volumes, input$file_sel_format_output)
  if("data.frame" %in% class(file_sel) && nrow(file_sel) == 0) file_sel <- NULL
  isolate({
    current_language <- current_language$language
    if (!is.null(file_sel)){
      # save path in default conf
      conf <- tryCatch(yaml::read_yaml("default_conf.yml"), error = function(e) NULL)
      if(!is.null(conf)){
        conf$file_sel_format_output <- file_sel$datapath
        tryCatch({
          yaml::write_yaml(conf, file = "default_conf.yml")
        }, error = function(e) NULL)
      }
      withCallingHandlers({
        list_sel <- tryCatch({ 
          readTemplateFile(file_sel$datapath)},
          error = function(e){
            showModal(modalDialog(
              title = antaresVizMedTSO:::.getLabelLanguage("Error reading selection file", current_language),
              easyClose = TRUE,
              footer = NULL,
              e$message
            ))
            NULL
          })}, 
        warning = function(w){
          showModal(modalDialog(
            title = antaresVizMedTSO:::.getLabelLanguage("Warning reading selection file", current_language),
            easyClose = TRUE,
            footer = NULL,
            w$message
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
          choices <- unique(c(dico()$Name))
          selected <- list_sel$variables_hourly
          updateSelectInput(session, 'var_h_format_output', choices = choices, selected = selected)
        }
      }
    }
  })
})


dico <- reactiveVal(defaut_output_params$dico)

shinyFileChoose(input, "file_sel_import_format_output", 
                roots = volumes, 
                session = session, 
                filetypes = c("XLS", "xls", "xlsx", "XLSX"), 
                defaultRoot = {
                  if(!is.null(file_sel_import_format_output) && file_sel_import_format_output != "" && paste0(strsplit(file_sel_import_format_output, "/")[[1]][1], "/") %in% names(volumes)){
                    paste0(strsplit(file_sel_import_format_output, "/")[[1]][1], "/")
                  } else {
                    NULL
                  }
                },
                defaultPath = {
                  if(!is.null(file_sel_import_format_output) && file_sel_import_format_output != "" && paste0(strsplit(file_sel_import_format_output, "/")[[1]][1], "/") %in% names(volumes)){
                    if(file.exists(file_sel_import_format_output)){
                      paste0(strsplit(file_sel_import_format_output, "/")[[1]][-1], collapse = "/")
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                })

# sélection à partir d'un fichier
observe({
  # file_sel <- input$file_sel_import_format_output
  file_sel <- shinyFiles::parseFilePaths(volumes, input$file_sel_import_format_output)
  if("data.frame" %in% class(file_sel) && nrow(file_sel) == 0) file_sel <- NULL
  isolate({
    current_language <- current_language$language
    if (!is.null(file_sel)){
      # save path in default conf
      conf <- tryCatch(yaml::read_yaml("default_conf.yml"), error = function(e) NULL)
      if(!is.null(conf)){
        conf$file_sel_import_format_output <- file_sel$datapath
        tryCatch({
          yaml::write_yaml(conf, file = "default_conf.yml")
        }, error = function(e) NULL)
      }
      withCallingHandlers({
        list_sel <- tryCatch({ 
          antaresVizMedTSO::readStudyShinySelection(file_sel$datapath)},
          error = function(e){
            showModal(modalDialog(
              title = antaresVizMedTSO:::.getLabelLanguage("Error reading selection file", current_language),
              easyClose = TRUE,
              footer = NULL,
              e$message
            ))
            NULL
          })}, 
        warning = function(w){
          showModal(modalDialog(
            title = antaresVizMedTSO:::.getLabelLanguage("Warning reading selection file", current_language),
            easyClose = TRUE,
            footer = NULL,
            w$message
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
        
        # removeVirtualsAreas
        updateCheckboxInput(session, "rmva_ctrl_format_output", value = list_sel$removeVirtualAreas)
        updateCheckboxInput(session, "rmva_ctrl_format_output_2", value = list_sel$removeVirtualAreas_2)
        updateCheckboxInput(session, "rmva_ctrl_format_output_3", value = list_sel$removeVirtualAreas_3)
        
        updateCheckboxInput(session, "rmva_reassignCosts_format_output", value = list_sel$reassignCost)
        updateCheckboxInput(session, "rmva_reassignCosts_format_output_2", value = list_sel$reassignCost_2)
        updateCheckboxInput(session, "rmva_reassignCosts_format_output_3", value = list_sel$reassignCost_3)
        
        updateCheckboxInput(session, "rmva_newCols_format_output", value = list_sel$newCols)
        updateCheckboxInput(session, "rmva_newCols_format_output_2", value = list_sel$newCols_2)
        updateCheckboxInput(session, "rmva_newCols_format_output_3", value = list_sel$newCols_3)
        
        updateSelectInput(session, "rmva_production_format_output", selected = list_sel$production)
        updateSelectInput(session, "rmva_production_format_output_2", selected = list_sel$production_2)
        updateSelectInput(session, "rmva_production_format_output_3", selected = list_sel$production_3)
        
        updateSelectInput(session, "rmva_storageFlexibility_format_output", selected = list_sel$`storageFlexibility (PSP)`)
        updateSelectInput(session, "rmva_PSP_Closed_format_output", selected = list_sel$`Hydro Storage (PSP_Closed)`)
        updateSelectInput(session, "rmva_BATT_format_output", selected = list_sel$`Battery Storage (BATT)`)
        updateSelectInput(session, "rmva_DSR_format_output", selected = list_sel$`Demand Side (DSR)`)
        updateSelectInput(session, "rmva_EV_format_output", selected = list_sel$`Electric Vehicle (EV)`)
        updateSelectInput(session, "rmva_P2G_format_output", selected = list_sel$`Power-to-gas (P2G)`)
        updateSelectInput(session, "rmva_H2_format_output", selected = list_sel$`Hydrogen (H2)`)
        
        updateSelectInput(session, "rmva_storageFlexibility_format_output_2", selected = list_sel$`storageFlexibility (PSP)_2`)
        updateSelectInput(session, "rmva_PSP_Closed_format_output_2", selected = list_sel$`Hydro Storage (PSP_Closed)_2`)
        updateSelectInput(session, "rmva_BATT_format_output_2", selected = list_sel$`Battery Storage (BATT)_2`)
        updateSelectInput(session, "rmva_DSR_format_output_2", selected = list_sel$`Demand Side (DSR)_2`)
        updateSelectInput(session, "rmva_EV_format_output_2", selected = list_sel$`Electric Vehicle (EV)_2`)
        updateSelectInput(session, "rmva_P2G_format_output_2", selected = list_sel$`Power-to-gas (P2G)_2`)
        updateSelectInput(session, "rmva_H2_format_output_2", selected = list_sel$`Hydrogen (H2)_2`)
        
        updateSelectInput(session, "rmva_storageFlexibility_format_output_3", selected = list_sel$`storageFlexibility (PSP)_3`)
        updateSelectInput(session, "rmva_PSP_Closed_format_output_3", selected = list_sel$`Hydro Storage (PSP_Closed)_3`)
        updateSelectInput(session, "rmva_BATT_format_output_3", selected = list_sel$`Battery Storage (BATT)_3`)
        updateSelectInput(session, "rmva_DSR_format_output_3", selected = list_sel$`Demand Side (DSR)_3`)
        updateSelectInput(session, "rmva_EV_format_output_3", selected = list_sel$`Electric Vehicle (EV)_3`)
        updateSelectInput(session, "rmva_P2G_format_output_3", selected = list_sel$`Power-to-gas (P2G)_3`)
        updateSelectInput(session, "rmva_H2_format_output_3", selected = list_sel$`Hydrogen (H2)_3`)
        
      }
    }
  })
})

observe({
  tmp_mcyears <- input$read_mcYears_format_output
  if(!is.null(tmp_mcyears) && length(tmp_mcyears) > 0){
    updateSelectInput(session, "read_mcYears_y_format_output", choices = tmp_mcyears, selected = tmp_mcyears)
    updateSelectInput(session, "read_mcYears_h_format_output", choices = tmp_mcyears, selected = tmp_mcyears[1])
  }
})

rm_output_params <- reactiveValues()

observe({
  if(input$import_data_format_output > 0){
    isolate({
      rm_output_params$removeVirtualAreas = list(
        input$rmva_ctrl_format_output, 
        input$rmva_ctrl_format_output_2, 
        input$rmva_ctrl_format_output_3
      )
      
      rm_output_params$storageFlexibility = list(
        build_storage_list(
          PSP = input$rmva_storageFlexibility_format_output,
          PSP_Closed = input$rmva_PSP_Closed_format_output,
          BATT = input$rmva_BATT_format_output,
          DSR = input$rmva_DSR_format_output, 
          EV = input$rmva_EV_format_output, 
          P2G = input$rmva_P2G_format_output, 
          H2 = input$rmva_H2_format_output
        ),
        build_storage_list(
          PSP = input$rmva_storageFlexibility_format_output_2,
          PSP_Closed = input$rmva_PSP_Closed_format_output_2,
          BATT = input$rmva_BATT_format_output_2,
          DSR = input$rmva_DSR_format_output_2, 
          EV = input$rmva_EV_format_output_2, 
          P2G = input$rmva_P2G_format_output_2, 
          H2 = input$rmva_H2_format_output_2
        ),
        build_storage_list(
          PSP = input$rmva_storageFlexibility_format_output_3,
          PSP_Closed = input$rmva_PSP_Closed_format_output_3,
          BATT = input$rmva_BATT_format_output_3,
          DSR = input$rmva_DSR_format_output_3, 
          EV = input$rmva_EV_format_output_3, 
          P2G = input$rmva_P2G_format_output_3, 
          H2 = input$rmva_H2_format_output_3
        )
      ) 
      
      rm_output_params$production = build_production_list(
        input$rmva_production_format_output,
        input$rmva_production_format_output_2,
        input$rmva_production_format_output_3
      )
      
      rm_output_params$reassignCosts = list(
        input$rmva_reassignCosts_format_output,
        input$rmva_reassignCosts_format_output_2, 
        input$rmva_reassignCosts_format_output_3
      )
      
      rm_output_params$newCols = list(
        input$rmva_newCols_format_output,
        input$rmva_newCols_format_output_2,
        input$rmva_newCols_format_output_3
      )
    })
  }
})

# import data ----
output$export_annual_format_output <- downloadHandler(
  filename = function() {
    paste('Annual_OutputFile_', format(Sys.time(), format = "%Y%d%m_%H%M%S"), '.zip', sep='')
  },
  content = function(con) {
    
    # importation des donnees
    if(!is.null(opts_format_output())){
      
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
        mcYears <- as.numeric(input$read_mcYears_y_format_output)
      }
      
      if(length(mcYears) == 0) mcYears <- NULL
      if(is.null(mcYears)){
        i <- 1
      } else {
        i <- length(mcYears)
      }
      
      tmp_files <- lapply(1:i, function(tmp){
        
        tmp_file <- paste0(tempdir(), "/", "Annual_OutputFile_", format(Sys.time(), format = "%Y%d%m_%H%M%S"), '.xlsx')
        
        if(tmp == 1 & is.null(mcYears)){
          mcy <- NULL
        } else {
          mcy <- mcYears[tmp]
        }
        
        list_warning <- list() 
        data <- withCallingHandlers({
          tryCatch({
            importAntaresDatasAnnual(opts = opts_format_output(), 
                                     areas_districts_selections = input$read_areas_y_format_output,
                                     links_selections = input$read_links_y_format_output, 
                                     mcYears = mcy, 
                                     removeVirtualAreas = rm_output_params$removeVirtualAreas,
                                     storageFlexibility = rm_output_params$storageFlexibility, 
                                     production = rm_output_params$production,
                                     reassignCosts = rm_output_params$reassignCosts,
                                     newCols = rm_output_params$newCols, 
                                     storage_vars = storage_vars
            )
          },
          error = function(e){
            showModal(modalDialog(
              title = "Error reading data",
              easyClose = TRUE,
              footer = NULL,
              paste("Please update input. Error : ", e$message, sep = "\n")
            ))
            NULL
          })}, 
          warning = function(w){
            list_warning[[length(list_warning) + 1]] <<- w$message
          }
        )
        
        # browser()
        if(length(list_warning) > 0 & !is.null(data)){
          showModal(modalDialog(
            title = "Warning reading data",
            easyClose = TRUE,
            footer = NULL,
            HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
          ))
        }
        
        progress$set(value = 0.5)
        
        if(length(data) > 0){
          data <- data
        } else {
          data <- NULL
        }
        
        if(!is.null(data)){
          
          progress$set(message = 'Annual Output', detail = 'Formatting data...')
          
          sim_name <- unlist(strsplit(opts_format_output()$simPath, "/"))
          sim_name <- sim_name[length(sim_name)]
          
          mcYears_xlsx <- mcy
          if(is.null(mcYears_xlsx)) mcYears_xlsx <- "synthetic"
          
          data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                                   "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), input$status_annual, mcYears_xlsx, 
                                                           opts_format_output()$studyName, sim_name))
          
          colnames(data_intro)[2] <- input$scenario_annual
          
          
          options(scipen = 10000, digits = 1)
          
          list_warning <- list()
          
          template <- template_annual_rv()
          
          data <- withCallingHandlers({
            tryCatch({
              
              formatAnnualOutputs(data_areas_dist_clust = data$data_areas_dist_clust,
                                  data_areas_dist_clustH = data$data_areas_dist_clustH,
                                  dataForSurplus = data$dataForSurplus,
                                  data_areas_districts = data$data_areas_districts,
                                  links_selections = data$links_selections,
                                  areas_districts_selections = data$areas_districts_selections,
                                  opts = data$opts, data_intro = data_intro, 
                                  template = template)
            },
            error = function(e){
              showModal(modalDialog(
                title = "Error formatting data",
                easyClose = TRUE,
                footer = NULL,
                e$message
              ))
              NULL
            })},
            warning = function(w){
              list_warning[[length(list_warning) + 1]] <<- w$message
            }
          )
          
          if(length(list_warning) > 0 & !is.null(data)){
            showModal(modalDialog(
              title = "Warning formatting data",
              easyClose = TRUE,
              footer = NULL,
              HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
            ))
          }
          progress$set(value = 0.7)
          
          if(!is.null(data)){
            
            options(scipen = 10000, digits = 1)
            
            progress$set(message = 'Annual Output', detail = 'Writting data...')
            
            list_warning <- list() 
            data <- withCallingHandlers({
              tryCatch({
                exportAnnualOutputs(infile_name = template, 
                                    outfile_name = tmp_file,
                                    annual_outputs = data)
              },
              error = function(e){
                showModal(modalDialog(
                  title = "Error writing data",
                  easyClose = TRUE,
                  footer = NULL,
                  e$message
                ))
                list()
              })}, 
              warning = function(w){
                list_warning[[length(list_warning) + 1]] <<- w$message
              }
            )
            
            if(length(list_warning) > 0 & !is.null(data) && length(data) > 0){
              showModal(modalDialog(
                title = "Warning writing data",
                easyClose = TRUE,
                footer = NULL,
                HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
              ))
            }
          }
        }   
        
        progress$set(value = 1)
        
        if(is.null(data)){
          wb <- openxlsx::createWorkbook()
          openxlsx::saveWorkbook(wb, tmp_file, overwrite = TRUE)
        }
        tmp_file
      })
      
      if(length(tmp_files) > 0){
        # fichier .zip
        zip(con, tmp_files, flags = "-r -j")
        # suppression du .csv
        rm(tmp_files)
        if(is_electron){
          showModal(modalDialog(
            antaresVizMedTSO:::.getLabelLanguage("File automatically downloaded in default folder", current_language),
            easyClose = TRUE,
            footer = NULL
          ))
        }
      } else {
        showModal(modalDialog(
          title = "Error writing data",
          easyClose = TRUE,
          footer = NULL,
          "No able to write any file"
        ))
      }
      
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
          mcYears <- as.numeric(input$read_mcYears_h_format_output)
        }
        
        # import data
        list_warning <- list() 
        data <- withCallingHandlers({
          tryCatch({
            importAntaresDatasHourly(opts = opts_format_output(), 
                                     areas_districts_selections = input$read_areas_h_format_output,
                                     links_selections = input$read_links_h_format_output, 
                                     mcYears = mcYears, 
                                     removeVirtualAreas = rm_output_params$removeVirtualAreas,
                                     storageFlexibility = rm_output_params$storageFlexibility, 
                                     production = rm_output_params$production,
                                     reassignCosts = rm_output_params$reassignCosts,
                                     newCols = rm_output_params$newCols, 
                                     storage_vars = storage_vars)
          },
          error = function(e){
            showModal(modalDialog(
              title = "Error reading data",
              easyClose = TRUE,
              footer = NULL,
              paste("Please update input. Error : ", e$message, sep = "\n")
            ))
            NULL
          })}, 
          warning = function(w){
            list_warning[[length(list_warning) + 1]] <<- w$message
          }
        )
        
        data_hourly <<- data
        if(length(list_warning) > 0 & !is.null(data)){
          showModal(modalDialog(
            title = "Warning reading data",
            easyClose = TRUE,
            footer = NULL,
            HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
          ))
        }
        
        progress$set(value = 0.5)
        
        if(length(data) > 0){
          data <- data
        } else {
          data <- NULL
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
                                 "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), input$status_hourly, mcYears_xlsx, 
                                                         opts_format_output()$studyName, sim_name))
        
        colnames(data_intro)[2] <- input$scenario_hourly
        
        options(scipen = 10000, digits = 1)
        
        list_warning <- list() 
        data <- withCallingHandlers({
          tryCatch({
            formatHourlyOutputs(
              data_h = data$data,
              areas_selections = data$areas_districts_selections,
              market_data_code = input$var_h_format_output,
              links_selections = data$links_selections,
              opts = data$opts,
              dico = dico())
          },
          error = function(e){
            showModal(modalDialog(
              title = "Error formatting data",
              easyClose = TRUE,
              footer = NULL,
              e$message
            ))
            NULL
          })},
          warning = function(w){
            list_warning[[length(list_warning) + 1]] <<- w$message
          }
        )
        
        if(length(list_warning) > 0 & !is.null(data)){
          showModal(modalDialog(
            title = "Warning formatting data",
            easyClose = TRUE,
            footer = NULL,
            HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
          ))
        }
        progress$set(value = 0.7)
        
        data_hourly_frm <<- data
        if(!is.null(data)){
          # data_format <<- data
          
          infile_name <- system.file("application/data/excel_templates/hourly_OutputFile_Template__R.xlsx", package = "antaresVizMedTSO")
          options(scipen = 10000, digits = 1)
          
          progress$set(message = 'Hourly Output', detail = 'Writting data...')
          
          list_warning <- list() 
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
                e$message
              ))
              list()
            })}, 
            warning = function(w){
              list_warning[[length(list_warning) + 1]] <<- w$message
            }
          )
          if(length(list_warning) > 0  & !is.null(data) && length(data) > 0){
            showModal(modalDialog(
              title = "Warning writing data",
              easyClose = TRUE,
              footer = NULL,
              HTML(paste0(unique(list_warning), collapse  = "<br><br>"))
            ))
          }
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
      
      if(is_electron){
        showModal(modalDialog(
          antaresVizMedTSO:::.getLabelLanguage("File automatically downloaded in default folder", current_language),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
    }
  }
)
