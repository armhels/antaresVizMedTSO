observe({
  language <-  current_language$language
  updateActionButton(session, 'edit_layout',  antaresVizMedTSO:::.getLabelLanguage("Edit layout", current_language$language))
})

# list of opts for set layout
layout <- reactive({
  ind_keep_list_data <- ind_keep_list_data()
  isolate({
    if(!is.null(ind_keep_list_data)){
      ind_map <- unique(sort(c(ind_keep_list_data$ind_areas, ind_keep_list_data$ind_links)))
      if(length(ind_map) > 0){
        if(packageVersion("antaresRead") <= '2.0.0'){
          readLayout(opts = list_data_all$opts[ind_map][[1]])
        } else {
          tmp <- tryCatch(readLayout(opts = list_data_all$opts[ind_map]), 
                          error = function(e) return(readLayout(opts = list_data_all$opts[ind_map][[1]])))
          tmp
        }
      }else{
        NULL
      }
    } else {
      NULL
    }
  })
})

ml <- reactiveVal(defaut_map_layout)
ml_to_edit <- reactiveVal(NULL)

# module for set and save layout
map_language <- reactive({
  current_language$language
})

ml_builder <- callModule(antaresVizMedTSO:::changeCoordsServer, "ml", layout, 
                         what = reactive("areas"), language = map_language, stopApp = FALSE)

observe({
  tmp_ml <- ml_builder()
  if(!is.null(tmp_ml)){
    ml(tmp_ml)
  }
})

shinyFileChoose(input, "file_import_layout", 
                roots = volumes, 
                session = session, 
                filetypes = c("RDS", "rds", "Rds"), 
                defaultRoot = {
                  if(!is.null(map_layout) && map_layout != "" && paste0(strsplit(map_layout, "/")[[1]][1], "/") %in% names(volumes)){
                    paste0(strsplit(map_layout, "/")[[1]][1], "/")
                  } else {
                    NULL
                  }
                },
                defaultPath = {
                  if(!is.null(map_layout) && map_layout != "" && paste0(strsplit(map_layout, "/")[[1]][1], "/") %in% names(volumes)){
                    if(dir.exists(map_layout)){
                      paste0(strsplit(map_layout, "/")[[1]][-1], collapse = "/")
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                })


observe({
  # ml_file <- input$import_layout
  ml_file <- shinyFiles::parseFilePaths(volumes, input$file_import_layout)
  if("data.frame" %in% class(ml_file) && nrow(ml_file) == 0) ml_file <- NULL
  if (!is.null(ml_file)){
    tmp_ml <- try(readRDS(ml_file$datapath), silent = TRUE)
    if("mapLayout" %in% class(tmp_ml)){
      ml(tmp_ml)
      # save path in default conf
      conf <- tryCatch(yaml::read_yaml("default_conf.yml"), error = function(e) NULL)
      if(!is.null(conf)){
        conf$map_layout <- ml_file$datapath
        tryCatch({
          yaml::write_yaml(conf, file = "default_conf.yml")
        }, error = function(e) NULL)
      }
    } else {
      showModal(modalDialog(
        title = "Invalid map layout file",
        easyClose = TRUE,
        footer = NULL,
        "Must be a valid .RDS file (class 'mapLayout')"
      ))
    }
  }
})

# control : have a not null layout, and so print map module ?
print_map <- reactiveValues(value = FALSE)

observe({
  if(!is.null(ml())){
    print_map$value <- TRUE
  } else {
    print_map$value <- FALSE
  }
})


output$current_layout <- renderLeafletDragPoints({
  if(!is.null(ml())){
    plotMapLayout(ml())
  }
})

output$must_print_map <- reactive({
  print_map$value
})

outputOptions(output, "must_print_map", suspendWhenHidden = FALSE)

observe({
  tmp_ml <- ml()
  all_areas <- ind_keep_list_data()$all_areas
  if(!is.null(tmp_ml)){
    if(!is.null(all_areas) && length(all_areas > 0)){
      if(!all(all_areas %in% tmp_ml$all_coords$area)){
        miss_area <- data.table(area = setdiff(all_areas, tmp_ml$all_coords$area), x = NA, y = NA, color = "#DA3713")
        tmp_ml$all_coords <- data.table::rbindlist(list(tmp_ml$all_coords, miss_area), use.names = T, fill = T)
      }
    }
    ml_to_edit(tmp_ml)
  }
})

# edition du mapLayout
ml_edit <- callModule(antaresVizMedTSO:::changeCoordsServer, "ml_edit", ml_to_edit, 
                      what = reactive("areas"), language = map_language, stopApp = FALSE)

observe({
  if(!is.null(ml_edit())){
    ml(ml_edit())
  }
})

list_params_map <- reactiveVal(NULL)

shinyFileChoose(input, "load_map_params", 
                roots = volumes, 
                session = session, 
                filetypes = c("RDS", "rds", "Rds"), 
                defaultRoot = {
                  if(!is.null(load_map_params) && load_map_params != "" && paste0(strsplit(load_map_params, "/")[[1]][1], "/") %in% names(volumes)){
                    paste0(strsplit(load_map_params, "/")[[1]][1], "/")
                  } else {
                    NULL
                  }
                },
                defaultPath = {
                  if(!is.null(load_map_params) && load_map_params != "" && paste0(strsplit(load_map_params, "/")[[1]][1], "/") %in% names(volumes)){
                    if(dir.exists(load_map_params)){
                      paste0(strsplit(load_map_params, "/")[[1]][-1], collapse = "/")
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                })

observe({
  # load params
  # file_params <- input$load_map_params
  file_params <- shinyFiles::parseFilePaths(volumes, input$load_map_params)
  if("data.frame" %in% class(file_params) && nrow(file_params) == 0) file_params <- NULL
  if(length(file_params) > 0){
    conf <- tryCatch(yaml::read_yaml("default_conf.yml"), error = function(e) NULL)
    if(!is.null(conf)){
      conf$load_map_params <- file_params$datapath
      tryCatch({
        yaml::write_yaml(conf, file = "default_conf.yml")
      }, error = function(e) NULL)
    }
    list_params <- tryCatch(readRDS(file_params$datapath), error = function(e) NULL)
    list_params_map(list_params)
  }
  
})


observe({
  ml <- ml()
  ind_keep_list_data <- ind_keep_list_data()
  language <- current_language$language
  
  # load params
  list_params <- list_params_map()
  
  isolate({
    if(input$update_module > 0){
      if(!is.null(ind_keep_list_data)){
        ind_map <- unique(sort(c(ind_keep_list_data$ind_areas, ind_keep_list_data$ind_links)))
        if(length(ind_map) > 0){
          if(!is.null(ml)){
            refStudy <- ind_keep_list_data$refStudy
            
            # init / re-init module plotMap
            id_plotMap   <- paste0("plotMap_", round(runif(1, 1, 100000000)))
            
            # update shared input table
            input_data$data[grepl("^plotMap", input_id), input_id := paste0(id_plotMap, "-shared_", input)]
            
            output[["plotMap_ui"]] <- renderUI({
              if(packageVersion("manipulateWidget") < "0.11"){
                mwModuleUI(id = id_plotMap, height = "800px")
              } else {
                mwModuleUI(id = id_plotMap, height = 800, updateBtn = TRUE)
              }
            })
            
            if(packageVersion("manipulateWidget") < "0.11"){
              .compare <- input$sel_compare_plotMap
              if(input$sel_compare_mcyear){
                .compare <- unique(c(.compare, "mcYear"))
              }
              
              if(length(.compare) > 0){
                list_compare <- vector("list", length(.compare))
                names(list_compare) <- .compare
                # set main with study names
                # if(length(ind_map) != 1){
                #   list_compare$main <- names(list_data_all$antaresDataList[ind_map])
                # }
                .compare <- list_compare
              } else {
                .compare = NULL
              }
            } else {
              .compare <- NULL
            }
            
            plotMap_args <- list(
              x = list_data_all$antaresDataList[ind_map], 
              mapLayout = ml, 
              interactive = TRUE, 
              .updateBtn = TRUE, 
              compare = .compare,
              language = language, 
              .exportBtn = TRUE, 
              .exportType = "webshot",
              h5requestFiltering = list_data_all$params[ind_map],
              xyCompare = "union", 
              .runApp = FALSE
            )
            
            if(packageVersion("manipulateWidget") < "0.11"){
              plotMap_args$.updateBtnInit <- TRUE
            }
            
            if(!is.null(list_params)){
              # if(packageVersion("manipulateWidget") >= "0.11"){
              #   plotMap_args$compare <- NULL
              # }
              plotMap_args <- c(plotMap_args, list_params)
            }
            
            mod_plotMap <- do.call(antaresVizMedTSO::plotMap, plotMap_args)
            
            if("MWController" %in% class(modules$plotMap)){
              modules$plotMap$clear()
            }
            
            modules$plotMap <- mod_plotMap
            modules$id_plotMap <- id_plotMap
            modules$init_plotMap <- TRUE
            # save data and params
            list_data_controls$n_maps <- length(ind_map)
          }
        }
      }
    }
  })
})

# save map params
output$save_map_params <- downloadHandler(
  filename = function() {
    paste0("plotMap_Inputs_", format(Sys.time(), format = "%Y%m%d_%H%M%s"), ".RDS")
  },
  content = function(file) {
    
    current_id_plotMap <- modules$id_plotMap
    
    list_params <- list(
      colAreaVar = "none", 
      sizeAreaVars = c(),
      areaChartType = c("bar"),
      uniqueScale = FALSE,
      showLabels = FALSE,
      popupAreaVars = c(),
      labelAreaVar = "none",
      colLinkVar = "none", 
      sizeLinkVar = "none", 
      popupLinkVars = c(),
      type = c("detail"),
      mcYear = "average",
      typeSizeAreaVars = FALSE,
      aliasSizeAreaVars = c(),
      sizeMiniPlot = FALSE
    )
    
    input_persit <- c("colAreaVar", "sizeAreaVars", "areaChartType", "uniqueScale", 
                      "showLabels", "popupAreaVars", "labelAreaVar", "colLinkVar", 
                      "sizeLinkVar", "popupLinkVars", "type", "mcYear", "typeSizeAreaVars", 
                      "aliasSizeAreaVars", "dateRange", "sizeMiniPlot")
    
    # list_compare <- list()
    n_inputs <- names(isolate(input))
    is_compare <- isolate(input[[paste0(current_id_plotMap, "-inputarea-compare-compare")]])
    if(length(is_compare) == 0) is_compare <- FALSE
    for(ip in input_persit){
      if(packageVersion("manipulateWidget") < "0.11"){
        tmp <- isolate(input[[paste0(current_id_plotMap, "-shared_", ip)]])
        if(!is.null(tmp)) list_params[[ip]] <- tmp
      } else {
        #   n_grp <- sort(n_inputs[grepl(paste0("(", current_id_plotMap, "-inputarea-output_)([[:digit:]]+)(_", ip, ")$"), n_inputs)])
        #   if(is_compare && length(n_grp) > 0){
        #     m <- regexpr("(output_)([[:digit:]]+)", n_grp)
        #     max_char <- max(as.numeric(gsub("output_", "", regmatches(n_grp, m))))
        #     
        #     n_grp <- paste0(current_id_plotMap, "-inputarea-output_", 1:max_char, "_", ip)
        #     val <- lapply(n_grp, function(x){
        #       tmp <- isolate(input[[x]])
        #       if(is.null(tmp)) tmp <- NA
        #       tmp
        #     })
        #     list_compare[[ip]] <- unname(val)
        #   } else {
        #     tmp <- isolate(input[[paste0(current_id_plotMap, "-inputarea-shared_", ip)]])
        #     if(!is.null(tmp)){
        #       list_params[[ip]] <- tmp
        #     }
        #   }
        # }
        
        if(is_compare){
          showModal(
            modalDialog(
              title = "Warning",
              "It's at moment not possible to save comparison parameters",
              easyClose = TRUE
            )
          )
        }
        tmp <- isolate(input[[paste0(current_id_plotMap, "-inputarea-shared_", ip)]])
        if(!is.null(tmp)){
          list_params[[ip]] <- tmp
        }
      }
    }
    
    # if(length(list_compare) > 0){
    #   list_params$compare <- list_compare
    # }
    
    saveRDS(list_params, file)
  }
)


observe({
  modules$init_plotMap
  if(input[['map_panel']] == "<div id=\"label_tab_map_viz\" class=\"shiny-text-output\"></div>"){
    isolate({
      if("MWController" %in% class(modules$plotMap) & modules$init_plotMap){
        modules$plotMap <- mwModule(id = modules$id_plotMap,  modules$plotMap)
        modules$init_plotMap <- FALSE
      }
    })
  }
})

# download layout
output$download_layout <- downloadHandler(
  filename = function() {
    paste('mapLayout-', Sys.Date(), '.RDS', sep='')
  },
  content = function(con) {
    saveRDS(ml(), file = con)
    
    if(is_electron){
      showModal(modalDialog(
        antaresVizMedTSO:::.getLabelLanguage("File automatically downloaded in default folder", current_language),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  }
)

# change page
observe({
  if(!is.null(ml())){
    updateNavbarPage(session, inputId = "map_panel", selected = "<div id=\"label_tab_map_viz\" class=\"shiny-text-output\"></div>")
    updateTabsetPanel(session, inputId = "tab_layout_view", selected = "Carte")
  }
})

observe({
  if(!is.null(ml())){
    updateNavbarPage(session, inputId = "map_panel", selected = "<div id=\"label_tab_layout_view\" class=\"shiny-text-output\"></div>")
    updateTabsetPanel(session, inputId = "tab_layout_view", selected = "Carte")
  }
})

observe({
  if(!is.null(input[['ml-done']])){
    if(input[['ml-done']] > 0){
      updateNavbarPage(session, inputId = "map_panel", selected = "<div id=\"label_tab_map_viz\" class=\"shiny-text-output\"></div>")
      updateTabsetPanel(session, inputId = "tab_layout_view", selected = "Carte")
    }
  }
})

observe({
  if(!is.null(input[['ml_edit-done']])){
    if(input[['ml_edit-done']] > 0){
      updateNavbarPage(session, inputId = "map_panel", selected = "<div id=\"label_tab_map_viz\" class=\"shiny-text-output\"></div>")
      updateTabsetPanel(session, inputId = "tab_layout_view", selected = "Carte")
    }
  }
})

output$ui_get_set_map_params <- renderUI({
  current_language <- current_language$language
  fluidRow(
    column(6, 
           div(downloadButton("save_map_params", antaresVizMedTSO:::.getLabelLanguage("Download current plotMap configuration", current_language)), align = "center")
    ),
    column(6, 
           # div(
           #   fileInput("load_map_params", antaresVizMedTSO:::.getLabelLanguage("Import a plotMap configuration (.RDS)", current_language), 
           #             accept = c(".RDS", ".rds", ".Rds")), 
           #   align = "center"
           # )
           div(
             shinyFilesButton("load_map_params", 
                              label = antaresVizMedTSO:::.getLabelLanguage("Import a plotMap configuration (.RDS)", current_language), 
                              title= NULL, 
                              icon = icon("upload"),
                              multiple = FALSE, viewtype = "detail"),
             align = "center"
           )
    )
  )
})


##### map colors -----

update_colors <- reactiveVal(0)
output$set_color_vars <- renderUI({
  update_colors()
  language <- current_language$language
  colVars <- getColorsVars()
  colVars <- colVars[lan == "en"]
  if(!is.null(colVars) && nrow(colVars) > 0){
    n <- nrow(colVars)
    middle <- round(n / 2)
    lapply(1:middle, function(x){
      second <- x + middle
      if(second <= n){
        fluidRow(
          column(width = 3,  
                 h5(.getColumnsLanguage(colVars$Column[x], language), style = "font-weight: bold;")
          ),
          column(width = 3,
                 colourInput(paste0("col_var_", x), 
                             label = NULL,
                             value  = colVars$colors[x])
          ),
          column(width = 3,  
                 h5(.getColumnsLanguage(colVars$Column[second], language), style = "font-weight: bold;")
          ),
          column(width = 3,
                 colourInput(paste0("col_var_", second), 
                             label = NULL,
                             value  = colVars$colors[second])
          ),
        )
      } else {
        fluidRow(
          column(width = 3,  
                 h5(.getColumnsLanguage(colVars$Column[x], language), style = "font-weight: bold;")
          ),
          column(width = 3,
                 colourInput(paste0("col_var_", x), 
                             label = NULL,
                             value  = colVars$colors[x])
          )
        )
      }
      
    })
  }
})

observe({
  
  colVars <- getColorsVars()
  colVars <- colVars[lan == "en"]
  
  if(!is.null(colVars) && nrow(colVars) > 0){
    col <- sapply(1:nrow(colVars), function(i){
      input[[paste0("col_var_", i)]]
    })
    
    if(!is.list(col) && length(col) == nrow(colVars)){
      rgb <- data.table(t(grDevices::col2rgb(col)))
      rgb[, Column := colVars$Column]
      rgb[, colors := col]
      
      # info_colors_var_map$colVars <- rgb
      setColorsVars(rgb)
    }
  }
})

output$ui_get_set_map_colors <- renderUI({
  current_language <- current_language$language
  fluidRow(
    column(6, 
           div(downloadButton("save_map_colors", antaresVizMedTSO:::.getLabelLanguage("Download current color settings", current_language)), align = "center")
    ),
    column(6, 
           # div(
           #   fileInput("load_map_colors", antaresVizMedTSO:::.getLabelLanguage("Import color settings (.RDS)", current_language), 
           #             accept = c(".RDS", ".rds", ".Rds")), 
           #   align = "center"
           # )
           div(
             shinyFilesButton("load_map_colors", 
                              label = antaresVizMedTSO:::.getLabelLanguage("Import color settings (.RDS)", current_language), 
                              title= NULL, 
                              icon = icon("upload"),
                              multiple = FALSE, viewtype = "detail"),
             align = "center"
           )
    )
  )
})


shinyFileChoose(input, "load_map_colors", 
                roots = volumes, 
                session = session, 
                filetypes = c("RDS", "rds", "Rds"), 
                defaultRoot = {
                  if(!is.null(load_map_colors) && load_map_colors != "" && paste0(strsplit(load_map_colors, "/")[[1]][1], "/") %in% names(volumes)){
                    paste0(strsplit(load_map_colors, "/")[[1]][1], "/")
                  } else {
                    NULL
                  }
                },
                defaultPath = {
                  if(!is.null(load_map_colors) && load_map_colors != "" && paste0(strsplit(load_map_colors, "/")[[1]][1], "/") %in% names(volumes)){
                    if(dir.exists(load_map_colors)){
                      paste0(strsplit(load_map_colors, "/")[[1]][-1], collapse = "/")
                    } else {
                      NULL
                    }
                  } else {
                    NULL
                  }
                })

observe({
  # load params
  # file_params <- input$load_map_colors
  file_params <- shinyFiles::parseFilePaths(volumes, input$load_map_colors)
  if("data.frame" %in% class(file_params) && nrow(file_params) == 0) file_params <- NULL
  if(length(file_params) > 0){
    conf <- tryCatch(yaml::read_yaml("default_conf.yml"), error = function(e) NULL)
    if(!is.null(conf)){
      conf$load_map_colors <- file_params$datapath
      tryCatch({
        yaml::write_yaml(conf, file = "default_conf.yml")
      }, error = function(e) NULL)
    }
    list_params <- tryCatch(readRDS(file_params$datapath), error = function(e) NULL)
    setColorsVars(list_params)
    update_colors(isolate(update_colors()) + 1)
  }
})


# save map params
output$save_map_colors <- downloadHandler(
  filename = function() {
    paste0("plotMap_Colors_", format(Sys.time(), format = "%Y%m%d_%H%M%s"), ".RDS")
  },
  content = function(file) {
    
    colVars <- getColorsVars()
    colVars <- colVars[lan == "en"]
    
    if(!is.null(colVars) && nrow(colVars) > 0){
      col <- sapply(1:nrow(colVars), function(i){
        input[[paste0("col_var_", i)]]
      })
      
      if(!is.list(col) && length(col) == nrow(colVars)){
        rgb <- data.table(t(grDevices::col2rgb(col)))
        rgb[, Column := colVars$Column]
        rgb[, colors := col]
      }
    } else {
      rgb <- NULL
    }
    saveRDS(rgb, file)
  }
)