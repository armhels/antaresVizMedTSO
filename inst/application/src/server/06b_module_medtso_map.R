# read data parameters ----

# observe directory 
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory_medtso_maps
  },
  handlerExpr = {
    if (input$directory_medtso_maps > 0) {
      # condition prevents handler execution on initial app launch
      path = choose.dir(default = readDirectoryInput(session, 'directory_medtso_maps'))
      updateDirectoryInput(session, 'directory_medtso_maps', value = path)
    }
  }
)

output$directory_message_medtso_maps <- renderText({
  if(length(input$directory_medtso_maps) > 0){
    if(input$directory_medtso_maps == 0){
      antaresVizMedTSO:::.getLabelLanguage("Please first choose a folder with antares output", current_language$language)
    } else {
      antaresVizMedTSO:::.getLabelLanguage("No antares output found in directory", current_language$language)
    }
  }
})

# list files in directory
dir_files_medtso_maps <- reactive({
  path <- readDirectoryInput(session, 'directory_medtso_maps')
  if(!is.null(path)){
    files = list.files(path, full.names = T)
    data.frame(name = basename(files), file.info(files))
  } else {
    NULL
  }
})

# have antares study in directory ?
is_antares_results_medtso_maps <- reactive({
  dir_files <- dir_files_medtso_maps()
  is_h5 <- any(grepl(".h5$", dir_files$name))
  is_study <- all(c("output", "study.antares") %in% dir_files$name)
  list(is_h5 = is_h5, is_study = is_study)
})

output$ctrl_is_antares_study_medtso_maps <- reactive({
  is_antares_results_medtso_maps()$is_study & !is_antares_results_medtso_maps()$is_h5
})

outputOptions(output, "ctrl_is_antares_study_medtso_maps", suspendWhenHidden = FALSE)

# if have study, update selectInput list
observe({
  is_antares_results <- is_antares_results_medtso_maps()
  if(is_antares_results$is_h5 | is_antares_results$is_study){
    isolate({
      if(is_antares_results$is_study){
        files = list.files(paste0(readDirectoryInput(session, 'directory_medtso_maps'), "/output"), full.names = T)
      } 
      if(is_antares_results$is_h5){
        files = list.files(readDirectoryInput(session, 'directory_medtso_maps'), pattern = ".h5$", full.names = T)
      } 
      if(length(files) > 0){
        files <- data.frame(name = basename(files), file.info(files))
        choices <- rownames(files)
        names(choices) <- files$name
      } else {
        choices <- NULL
      }
      updateSelectInput(session, "study_path_medtso_maps", "", choices = choices)
    })
  }
})

# init opts after validation
opts_medtso_maps <- reactive({
  if(input$init_sim_medtso_maps > 0){
    opts <- 
      tryCatch({
        setSimulationPath(isolate(input$study_path_medtso_maps))
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

output$current_opts_h5_medtso_maps <- reactive({
  opts_medtso_maps()$h5
})

outputOptions(output, "current_opts_h5_medtso_maps", suspendWhenHidden = FALSE)

current_study_path_medtso_maps <- reactive({
  if(input$init_sim_medtso_maps > 0){
    rev(unlist(strsplit(isolate(input$study_path_medtso_maps), "/")))[1]
  }
})


# control : have not null opts ?
output$have_study_medtso_maps <- reactive({
  !is.null(opts_medtso_maps())
})

outputOptions(output, "have_study_medtso_maps", suspendWhenHidden = FALSE)


# update readAntares / opts parameters

observe({
  opts <- opts_medtso_maps()
  current_language <- current_language$language
  if(!is.null(opts)){
    isolate({
      # areas
      areas <- c("all", opts$areaList)
      updateSelectInput(session, "read_areas_medtso_maps", paste0(antaresVizMedTSO:::.getLabelLanguage("Areas", current_language), " : "), 
                        choices = areas, selected = areas[1])
      
      # links
      links <- c("all", opts$linkList)
      updateSelectInput(session, "read_links_medtso_maps", paste0(antaresVizMedTSO:::.getLabelLanguage("Links", current_language), " : "), 
                        choices = links, selected = links[1])
      
      # mcYears
      mcy <- c(opts$mcYears)
      updateSelectInput(session, "read_mcYears_medtso_maps", paste0(antaresVizMedTSO:::.getLabelLanguage("mcYears", current_language), " : "), 
                        choices = mcy, selected = mcy[1])
      
      # removeVirtualAreas
      
      updateCheckboxInput(session, "rmva_ctrl_medtso_maps", antaresVizMedTSO:::.getLabelLanguage("Remove virtual Areas", current_language), FALSE)
      
      updateSelectInput(session, "rmva_storageFlexibility_medtso_maps", paste0(antaresVizMedTSO:::.getLabelLanguage("storageFlexibility", current_language), " : "), 
                        choices = opts$areaList, selected = NULL)
      updateSelectInput(session, "rmva_production_medtso_maps", paste0(antaresVizMedTSO:::.getLabelLanguage("production", current_language), " : "),
                        choices = opts$areaList, selected = NULL)
      
    })
  }
})

output$ui_sel_file_import_medtso_maps <- renderUI({
  current_language <- current_language$language
  input$init_sim # clear if change simulation
  fluidRow(
    column(6, 
           div(fileInput("file_sel_import_medtso_maps", antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language),
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


# sélection à partir d'un fichier
observe({
  file_sel <- input$file_sel_import_medtso_maps
  
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
        # areas
        updateSelectInput(session, "read_areas_medtso_maps", selected = list_sel$areas)
        
        # links
        updateSelectInput(session, "read_links_medtso_maps", selected = list_sel$links)
        
        mcy <- list_sel$mcYears
        if(!is.null(mcy)){
          updateSelectInput(session, "read_mcYears_medtso_maps", selected = as.character(mcy)[1])
        } else {
          updateSelectInput(session, "read_mcYears_medtso_maps", selected = "1")
        }
        
        updateCheckboxInput(session, "rmva_ctrl_medtso_maps", value = list_sel$removeVirtualAreas)
        updateCheckboxInput(session, "rmva_reassignCosts_medtso_maps", value = list_sel$reassignCost)
        updateCheckboxInput(session, "rmva_newCols_medtso_maps", value = list_sel$newCols)
        
        updateSelectInput(session, "rmva_storageFlexibility_medtso_maps", selected = list_sel$storageFlexibility)
        updateSelectInput(session, "rmva_production_medtso_maps", selected = list_sel$production)
        
      }
    }
  })
})


# import data ----
# les donnees

#-----------------
# Importation de nouvelles donnees
#-----------------
data_map <- reactive({
  if(input$import_data_medtso_maps > 0){
    isolate({
      if(!is.null(opts_medtso_maps())){
        # not a .h5 file, so read data
        if(!opts_medtso_maps()$h5){
          
          mcYears <- as.numeric(input$read_mcYears_medtso_maps)
          
          # import data
          data <- withCallingHandlers({
            tryCatch({
              
              get_data_map(opts = opts_medtso_maps(), areas = input$read_areas_medtso_maps, 
                           links = input$read_links_medtso_maps, 
                           mcYears = mcYears, 
                           removeVirtualAreas = input$rmva_ctrl_medtso_maps,
                           storageFlexibility = input$rmva_storageFlexibility_medtso_maps, 
                           production = input$rmva_production_medtso_maps,
                           reassignCosts = input$rmva_reassignCosts_medtso_maps, 
                           newCols = input$rmva_newCols_medtso_maps)
            },
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
              showModal(modalDialog(
                title = "Warning reading data",
                easyClose = TRUE,
                footer = NULL,
                w
              ))
            }
          )
          
          if(length(data) > 0){
            data
          } else {
            NULL
          }
        }
      }
    })
  }
})

observe({
  if(input$import_data_medtso_maps > 0){
    updateTabsetPanel(session, inputId = "medtso_map_panel", selected = "Parameters")
  }
})


output$have_data_map_tso <- reactive({
  !is.null(data_map())
})
outputOptions(output, "have_data_map_tso", suspendWhenHidden = FALSE)

# tables avec les positions ----
pos_links <- reactiveVal(data.table(ref_medtsomap_data$links))

output$dt_pos_links <- renderDT({
  datatable(pos_links(), rownames = F, editable = TRUE, selection = "none",
            options = list(dom = 'ftip', paging = F))
})

observeEvent(input$dt_pos_links_cell_edit, {
  info = input$dt_pos_links_cell_edit
  str(info)
  i = info$row
  j = info$col + 1  # column index offset by 1
  v = info$value
  tmp <- as.data.frame(pos_links())
  tmp[i, j] <- DT::coerceValue(v, tmp[i, j])
  # replaceData(dataTableProxy('dt_data_armement'), tmp, resetPaging = FALSE, rownames = FALSE)
  pos_links(as.data.table(tmp))
})


pos_areas <- reactiveVal(data.table(ref_medtsomap_data$areas))

output$dt_pos_areas <- renderDT({
  
  datatable(pos_areas(), rownames = F, editable = TRUE, selection = "none",
            options = list(dom = 'ftip', paging = F))
})

observeEvent(input$dt_pos_areas_cell_edit, {
  info = input$dt_pos_areas_cell_edit
  # str(info)
  i = info$row
  j = info$col + 1  # column index offset by 1
  v = info$value
  tmp <- as.data.frame(pos_areas())
  tmp[i, j] <- DT::coerceValue(v, tmp[i, j])
  # replaceData(dataTableProxy('dt_data_armement'), tmp, resetPaging = FALSE, rownames = FALSE)
  pos_areas(as.data.table(tmp))
})

# country map ----

observe({
  data_map <- data_map()
  if(!is.null(data_map$area)){
    choice <- colnames(data_map$area)[-c(1:4)]
    updateSelectInput(session, inputId = "column_selection", label = "Variable :",
                      choices = choice, selected = "MRG. PRICE")
  }
})


output$have_maptso_data <- reactive({
  data_map <- data_map()
  pos_areas <- pos_areas()
  pos_links <- pos_links()
  !is.null(data_map) && !is.null(pos_areas) && !is.null(pos_links)
})
outputOptions(output, "have_maptso_data", suspendWhenHidden = FALSE)

gg_countries_plots <- reactive({
  data_map <- data_map()
  pos_areas <- pos_areas()
  pos_links <- pos_links()
  input$go_cty
  isolate({
    if(!is.null(data_map) && !is.null(pos_areas) && !is.null(pos_links)){
      
      withProgress(message = 'Map in progress', value = 0.2, {
        
        suppressWarnings({
          tmp <- init_map_sp(sp_object, pos_areas, data_map, var_countries = input$column_selection, 
                             palette_colors  = c(input$col_min, input$col_med, input$col_max), label_size = 4)
          
          incProgress(0.3)
          
          pos_links <- pos_links[pos_links$draw_link %in% 1, ]
          if(nrow(pos_links) > 0){
            tmp <- add_links(tmp$map, pos_links, data_map$links$arrows, 
                             col_value = "value", 
                             color = c(input$col_arrow_1_countries, input$col_arrow_2_countries),  size = input$arrow_width_countries,
                             length = input$arrow_size_countries, lon_gap = 0, lat_gap = 0, 
                             text_size = input$arrow_textsize_countries)
            
            incProgress(0.3)
          }
          
          tmp + ggtitle(input$title_countries) + 
            theme(plot.title = element_text(face = "bold", size = 25, hjust = 0.5))
        })
      })
    } else {
      NULL
    }
  })
})


output$countries_plots <- renderPlot({
  gg <- gg_countries_plots()
  if(!is.null(gg)){
    gg
  } else {
    NULL
  }
})

output$download_countries <- downloadHandler(
  filename <- paste0("Countries_", format(Sys.time(), format = "%Y%m%d_%H%M%s"), '.png'),
  content <- function(file) {
    ggsave(file, plot = gg_countries_plots(), width = 20, height = 20, limitsize = FALSE)
  })

# exchanges and production ----

observe({
  data_map <- data_map()
  if(!is.null(data_map$area)){
    choice <- colnames(data_map$area)[-c(1:4)]
    updateSelectInput(session, inputId = "centers_columns", label = "Variable :",
                      choices = choice, 
                      selected = c("MISC. NDG", "H. ROR" , "WIND", "SOLAR","NUCLEAR", "LIGNITE" , "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR"))
  }
})

gg_centers_plots <- reactive({
  data_map <- data_map()
  pos_areas <- pos_areas()
  pos_links <- pos_links()
  input$go_exch
  isolate({
    if(!is.null(data_map) && !is.null(pos_areas) && !is.null(pos_links)){
      
      withProgress(message = 'Map in progress', value = 0, {
        
        suppressWarnings({
          res_map <- init_map_sp(sp_object, pos_areas, data_map, var_countries = NULL, 
                                 palette_colors  = input$col_sp, label_size = 0)
          
          incProgress(0.2)
          
          data_pos = copy(pos_areas)
          setnames(data_pos, c("lon_pie", "lat_pie"), c("long", "lat"))
          
          if(any(data_pos$draw_pie %in% "1")){
            tmp <- add_pie(res_map$map, ref_map = pos_areas, data_pos, data_map$areas, 
                           id_col = "area",
                           pie_col = input$centers_columns, 
                           r = input$pie_size_centers, text_size = input$pie_textsize_centers, colors = NULL, 
                           legend_position = res_map$legend_position, label_col = "code", alpha = input$pie_alpha_centers) 
          } else {
            tmp <- res_map$map
          }
          
          
          incProgress(0.3)
          
          pos_links <- pos_links[pos_links$draw_link %in% 1, ]
          if(nrow(pos_links) > 0){
            tmp <- add_links(tmp, pos_links, data_map$links$arrows, 
                             col_value = "value", 
                             color = c(input$col_arrow_1, input$col_arrow_2),  size = input$arrow_width_centers,
                             length = input$arrow_size_centers, lon_gap = 0, lat_gap = 0, 
                             text_size = input$arrow_textsize_centers)
            
            incProgress(0.2)
          }
          
          tmp + ggtitle(input$title_centers) + 
            theme(plot.title = element_text(face = "bold", size = 25, hjust = 0.5)) 
        })
      })
    } else {
      NULL
    }
  })
})


output$centers_plots <- renderPlot({
  gg <- gg_centers_plots()
  if(!is.null(gg)){
    gg
  } else {
    NULL
  }
})

output$download_centers <- downloadHandler(
  filename <- paste0("Exch_prod_", format(Sys.time(), format = "%Y%m%d_%H%M%s"), '.png'),
  content <- function(file) {
    ggsave(file, plot = gg_centers_plots(), width = 20, height = 20, limitsize = FALSE)
  })


# interconnexion ----

gg_interco_plots <- reactive({
  data_map <- data_map()
  pos_areas <- pos_areas()
  pos_links <- pos_links()
  input$go_interco
  isolate({
    if(!is.null(data_map) && !is.null(pos_areas) && !is.null(pos_links)){
      
      withProgress(message = 'Map in progress', value = 0, {
        
        suppressWarnings({
          res_map <- init_map_sp(sp_object, pos_areas, data_map, var_countries = NULL, 
                                 palette_colors  = input$col_sp_interco)
          
          incProgress(0.2)
          
          pos_links <- pos_links[pos_links$draw_link %in% 1, ]
          if(nrow(pos_links)){
            pos_links_pie <- copy(pos_links)
            setnames(pos_links_pie, c("lon_pie", "lat_pie"), c("long", "lat"))
            
            if(nrow(data_map$links$centers) > 0){
              data_links <- copy(data_map$links$centers)
              setnames(data_links, c("pie_ab", "pie_ba", "pie_null"), c("% A->B Saturation", "% B->A Saturation", "% Null Saturation"))
              
              tmp <- add_pie(res_map$map, ref_map = NULL, pos_links_pie, data_links, 
                             id_col = "link",
                             pie_col = c("% A->B Saturation", "% B->A Saturation", "% Null Saturation"),
                             colors = c(input$col_arrow_1_interco, input$col_arrow_2_interco, "gray"),
                             r = input$pie_size_interco, text_size = input$pie_textsize_interco, 
                             legend_position = res_map$legend_position, alpha = input$pie_alpha_interco) 
            } else {
              tmp <- res_map$map
            }
            incProgress(0.3)
            
            tmp <- add_links(tmp, pos_links, data_map$links$arrows, 
                             col_value = "pct", 
                             color = c(input$col_arrow_1_interco, input$col_arrow_2_interco),  size = input$arrow_width_interco,
                             length = input$arrow_size_interco, lon_gap = 0, lat_gap = 0, 
                             text_size = input$arrow_textsize_interco)
            
            incProgress(0.2)
          }
          
          tmp + ggtitle(input$title_interco) + 
            theme(plot.title = element_text(face = "bold", size = 25, hjust = 0.5), 
                  plot.subtitle = element_text(hjust = 1)) + 
            labs(
              subtitle="Arrows % refers to loading of interconnections during the year (Energy/limit)\nPie are % saturation hours"
            )
        })
      })
    } else {
      NULL
    }
  })
})


output$interco_plots <- renderPlot({
  gg <- gg_interco_plots()
  if(!is.null(gg)){
    gg
  } else {
    NULL
  }
})

output$download_interco <- downloadHandler(
  filename <- paste0("Interconnexion_", format(Sys.time(), format = "%Y%m%d_%H%M%s"), '.png'),
  content <- function(file) {
    ggsave(file, plot = gg_interco_plots(), width = 20, height = 20, limitsize = FALSE)
  })

# sélection à partir d'un fichier -----

output$ui_file_sel_medtso_map <- renderUI({
  current_language <- current_language$language
  fluidRow(
    column(width = 6, 
           div(fileInput("file_sel_medtso_map", antaresVizMedTSO:::.getLabelLanguage("Import a selection file (.xlsx)", current_language),
                         accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")), align = "center")
    ),
    column(6, 
           div( br(),
                downloadButton("get_sel_file_medtso_map",
                               antaresVizMedTSO:::.getLabelLanguage("Generate current selection file", current_language), 
                               class = NULL),
                align = "center"
           )
    )
  )
})

observe({
  file_sel <- input$file_sel_medtso_map
  
  isolate({
    current_language <- current_language$language
    if (!is.null(file_sel)){
      withCallingHandlers({
        list_sel <- tryCatch({ 
          readMEDTsoMapInput(file_sel$datapath)},
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
        pos_areas(as.data.table(list_sel$areas))
        pos_links(as.data.table(list_sel$links))
        
        # update des inputs
        if(!is.null(list_sel$inputs)){
          ctrl <- lapply(1:nrow(list_sel$inputs), function(x){
            type = list_sel$inputs$type[x]
            id = list_sel$inputs$id[x]
            label = list_sel$inputs$label[x]
            value = list_sel$inputs$value[x]
            if(type %in% "sliderInput"){
              value <- as.numeric(gsub("^([[:space:]]*) | ([[:space:]]*)$", "", gsub(",", ".", value, fixed = TRUE)))
            }
            if(type %in% "selectInput" && id %in% "centers_columns"){
              value <- gsub("^([[:space:]]*) | ([[:space:]]*)$", "", unlist(strsplit(value, ",")))
            }
            
            if(type %in% "selectInput"){
              updateSelectInput(session, inputId = id, label = label, selected = value)
            } else if(type %in% "textInput"){
              updateTextInput(session, inputId = id, label = label, value = value)
            } else if(type %in% "colourInput"){
              updateColourInput(session, inputId = id, label = label, value = value)
            } else if(type %in% "sliderInput"){
              updateSliderInput(session, inputId = id, label = label, value = value)
            }
          })
        }
      }
    }
  })
})


output$get_sel_file_medtso_map <- downloadHandler(
  filename = function() {
    paste('MEDTso_maps_selection_', format(Sys.time(), format = "%Y%d%m_%H%M%S"), '.xlsx', sep='')
  },
  content = function(con) {
    writeMEDTsoMapInput(pos_areas(), pos_links(), 
                        list(title_countries = input$title_countries,
                             column_selection = input$column_selection,
                             col_min = input$col_min,
                             col_med = input$col_med,
                             col_max = input$col_max,
                             arrow_width_countries = input$arrow_width_countries,
                             arrow_size_countries = input$arrow_size_countries,
                             arrow_textsize_countries = input$arrow_textsize_countries,
                             col_arrow_1_countries = input$col_arrow_1_countries,
                             col_arrow_2_countries = input$col_arrow_2_countries,
                             title_centers = input$title_centers,
                             centers_columns = input$centers_columns,
                             col_sp = input$col_sp,
                             pie_size_centers = input$pie_size_centers,
                             pie_textsize_centers = input$pie_textsize_centers,
                             pie_alpha_centers = input$pie_alpha_centers,
                             arrow_width_centers = input$arrow_width_centers,
                             arrow_size_centers = input$arrow_size_centers,
                             arrow_textsize_centers = input$arrow_textsize_centers,
                             col_arrow_1 = input$col_arrow_1,
                             col_arrow_2 = input$col_arrow_2,
                             title_interco = input$title_interco,
                             col_sp_interco = input$col_sp_interco,
                             pie_size_interco = input$pie_size_interco,
                             pie_textsize_interco = input$pie_textsize_interco,
                             pie_alpha_interco = input$pie_alpha_interco,
                             arrow_width_interco = input$arrow_width_interco,
                             arrow_size_interco = input$arrow_size_interco,
                             arrow_textsize_interco = input$arrow_textsize_interco,
                             col_arrow_1_interco = input$col_arrow_1_interco,
                             col_arrow_2_interco = input$col_arrow_2_interco),
                        con)
  }
)

