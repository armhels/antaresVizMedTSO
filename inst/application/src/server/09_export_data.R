#------------------
# gestion de la liste
#------------------
output$info_list_data_export <- renderUI({
  list_data <- list_data_all$antaresDataList
  if(length(list_data) > 0){
    isolate({
      # affichage du nom de l'etude
      study <- lapply(1:length(list_data), function(i) {
        study_name <- paste0("export_list_study_", i)
        div(
          h4(textOutput(study_name)), style = 'height:24px', align = "center")
      })
      # checkbox de selection
      check_list <- lapply(1:length(list_data), function(i) {
        check_name <- paste0("export_list_study_check", i)
        div(
          checkboxInput(check_name, antaresVizMedTSO:::.getLabelLanguage("Select this study", current_language$language), value = ifelse(i == 1, T, F)), align = "center")
      })

      # bouton pour afficher les parametres
      params_list <- lapply(1:length(list_data), function(i) {
        btn_name <- paste0("export_list_study_params", i)
        div(
          actionButton(btn_name, antaresVizMedTSO:::.getLabelLanguage("View parameters", current_language$language)), align = "center")
      })

      # format et retour
      fluidRow(
        column(4, do.call(tagList, study)),
        column(2, do.call(tagList, params_list)),
        column(2, do.call(tagList, check_list), offset = 0)
      )
    })
  }else {
    # element vide si pas de donnees
    fluidRow()
  }
})

# creation des outputs
# - titre de l'etude
# - print des parametres
observe({
  # lancement lors de la recuperation des donnees formatees
  list_data_tmp <- list_data_all$antaresDataList
  if(length(list_data_tmp) > 0){
    isolate({
      ctrl <- lapply(1:length(list_data_tmp), function(i) {
        study_name <- paste0("export_list_study_", i)
        study_params <- paste0("export_list_study_params", i)
        output[[study_name]] <- renderText({
          names(list_data_tmp)[i]
        })
        
        output[[study_params]] <- renderPrint({
          str(list_data_all$params[[i]])
        })
      })
    })
  }
})

# observe locaux pour l'affichage des parametres
# et pour la suppression des etudes
for(j in 1:16){
  local({
    l_j <- j
    observe({
      if(!is.null(input[[paste0("export_list_study_params", l_j)]])){
        if(input[[paste0("export_list_study_params", l_j)]] > 0){
          showModal(modalDialog(
            easyClose = TRUE,
            footer = NULL,
            verbatimTextOutput(paste0("export_list_study_params", l_j))
          ))
        }
      }
    })
  })
}

# observe locaux pour selectionner une etude de reference

imported_data <- reactiveVal(NULL)

observe({
  list_data <- list_data_all$antaresDataList
  if(length(list_data) == 0){
    imported_data(NULL)
  }
})

for(j in 1:16){
  local({
    l_j <- j
    #on ne peut avoir qu une etude de reference a la fois 
    observe({
      if(!is.null(input[[paste0("export_list_study_check", l_j)]])){
        if(input[[paste0("export_list_study_check", l_j)]] > 0){
          isolate({
            for(k in 1:16){
              if(k != l_j){
                #on ne peut avoir qu une etude de reference a la fois 
                updateCheckboxInput(session, paste0("export_list_study_check", k), 
                                    label = antaresVizMedTSO:::.getLabelLanguage("Select this study", 
                                                                                 current_language$language),
                                    value = FALSE)
              } else {
                list_data <- list_data_all$antaresDataList
                data <- list_data[[k]]
                # dddttt <<- data
                if("list" %in% class(data)){
                  if(length(data) > 0) {
                    ctrl <- sapply(1:length(data), FUN = function(i){
                      data_i <- data[[i]]
                      if("timeId" %in% colnames(data[[i]])) {
                        data[[i]][, timeId := NULL]
                      }
                      changeCols <- colnames(data[[i]])[which(sapply(data[[i]], function(x) {
                        "factor" %in% class(x)
                      }))]
                      if(length(changeCols) > 0){
                        data[[i]][,(changeCols) := lapply(.SD, as.character), .SDcols = changeCols]
                      }
                      invisible(NULL)
                    })
                  }
                } else if("data.frame" %in% class(data)) {
                  if("timeId" %in% colnames(data)) {
                    data[, timeId := NULL]
                  }
                  changeCols <- colnames(data)[which(sapply(data, function(x) {
                    "factor" %in% class(x)
                  }))]
                  if(length(changeCols) > 0){
                    data[,(changeCols):= lapply(.SD, as.character), .SDcols = changeCols]
                  }
                }
                imported_data(data)
              }
            }
          })
        }
      }
    })
  })
}


# ui et initialisation des inputs -----------------------------------------

output$panels_tab <- renderUI({
  imported_data <- imported_data()
  isolate({
    if(!is.null(imported_data)) {
      nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)
      if(nb_panels == 1) {
        panel_name <- paste0(attributes(imported_data)$names[1], "s")
        if(attributes(imported_data)$names[2] == "cluster") panel_name <- "clusters"

        if(panel_name == "clusters") {
          res_list <- tabsetPanel(
            tabPanel(title = panel_name,
                     fluidRow(
                       column(3,
                              selectInput("lig1", label = "Areas", choices = NULL, selected = NULL, multiple = T)),
                       column(3,
                              selectInput("lig1_2", label = "Clusters", choices = NULL, selected = NULL, multiple = T)),
                       column(3,
                              selectInput("col1", label = "Variables", choices = NULL, selected = NULL, multiple = T))
                     ),
                     tags$hr(),
                     fluidRow(column(3,infoBoxOutput("box_tab1_1", 12)),
                              column(3,infoBoxOutput("box_tab1_2", 12)),
                              column(3,infoBoxOutput("box_tab1_3", 12)),
                              column(2, downloadButton("import_tab1", antaresVizMedTSO:::.getLabelLanguage("Export this table", current_language$language))),
                              column(1, tags$div(style="padding-top:0px; display: none", id = "export_busy_1",
                                                 tags$img(src="spinner.gif", height = 40, width = 40),align = 'left'))
                     ),
                     tags$hr(),
                     div(h3(antaresVizMedTSO:::.getLabelLanguage("First 10 rows", current_language$language)), align = "center"),
                     DTOutput("dt1")
            )
          )
        } else {
          res_list <- tabsetPanel(
            tabPanel(title = panel_name,
                     fluidRow(
                       column(3,
                              selectInput("lig1", label = panel_name, choices = NULL, selected = NULL, multiple = T)),
                       column(3,
                              selectInput("col1", label = "Variables", choices = NULL, selected = NULL, multiple = T))
                     ),
                     tags$hr(),
                     fluidRow(column(3,infoBoxOutput("box_tab1_1", 12)),
                              column(3,infoBoxOutput("box_tab1_2", 12)),
                              column(3,infoBoxOutput("box_tab1_3", 12)),
                              column(2, downloadButton("import_tab1", antaresVizMedTSO:::.getLabelLanguage("Export this table", current_language$language))),
                              column(1, tags$div(style="padding-top:0px; display: none", id = "export_busy_1",
                                                 tags$img(src="spinner.gif", height = 40, width = 40),align = 'left'))
                     ),
                     tags$hr(),
                     div(h3(antaresVizMedTSO:::.getLabelLanguage("First 10 rows", current_language$language)), align = "center"),
                     DTOutput("dt1")
            )
          )
        }

      }
      else if(nb_panels > 1) {
        liste_tab <- list()
        sapply(1:nb_panels, FUN = function(nb){
          panel_name <- names(imported_data)[nb]
          if(panel_name == "clusters") {
            liste_tab[[nb]] <<- tabPanel(title = panel_name,
                                         fluidRow(column(3,
                                                         selectInput(paste0("lig", nb), label = "Areas", choices = NULL, selected = NULL, multiple = T)),
                                                  column(3,
                                                         selectInput(paste0("lig", nb, "_2"), label = "Clusters", choices = NULL, selected = NULL, multiple = T)),
                                                  column(3,
                                                         selectInput(paste0("col", nb), label = "Variables", choices = NULL, selected = NULL, multiple = T))
                                         ),
                                         tags$hr(),
                                         fluidRow(column(3,infoBoxOutput(paste0("box_tab", nb, "_1"), 12)),
                                                  column(3,infoBoxOutput(paste0("box_tab", nb, "_2"), 12)),
                                                  column(3,infoBoxOutput(paste0("box_tab", nb, "_3"), 12)),
                                                  column(2, downloadButton(paste0("import_tab", nb), antaresVizMedTSO:::.getLabelLanguage("Export this table", current_language$language))),
                                                  column(1, tags$div(style="padding-top:0px; display: none", id = paste0("export_busy_", nb),
                                                                     tags$img(src="spinner.gif", height = 40, width = 40),align = 'left'))
                                         ),
                                         tags$hr(),
                                         div(h3(antaresVizMedTSO:::.getLabelLanguage("First 10 rows", current_language$language)), align = "center"),
                                         DTOutput(paste0("dt", nb))
            )
          } else {
            liste_tab[[nb]] <<- tabPanel(title = panel_name,
                                         fluidRow(
                                           column(3,
                                                  selectInput(paste0("lig", nb), label = panel_name, choices = NULL, selected = NULL, multiple = T)),
                                           column(3,
                                                  selectInput(paste0("col", nb), label = "Variables", choices = NULL, selected = NULL, multiple = T))
                                         ),
                                         tags$hr(),
                                         fluidRow(column(3,infoBoxOutput(paste0("box_tab", nb, "_1"), 12)),
                                                  column(3,infoBoxOutput(paste0("box_tab", nb, "_2"), 12)),
                                                  column(3,infoBoxOutput(paste0("box_tab", nb, "_3"), 12)),
                                                  column(2, downloadButton(paste0("import_tab", nb), antaresVizMedTSO:::.getLabelLanguage("Export this table", current_language$language))),
                                                  column(1, tags$div(style="padding-top:0px; display: none", id = paste0("export_busy_", nb),
                                                                     tags$img(src="spinner.gif", height = 40, width = 40),align = 'left'))
                                         ),
                                         tags$hr(),
                                         div(h3(antaresVizMedTSO:::.getLabelLanguage("First 10 rows", current_language$language)), align = "center"),
                                         DTOutput(paste0("dt", nb))
            )
          }
        })

        if(nb_panels == 2) {
          res_list <- tabsetPanel(liste_tab[[1]], liste_tab[[2]])
        } else if(nb_panels == 3) {
          res_list <- tabsetPanel(liste_tab[[1]], liste_tab[[2]], liste_tab[[3]])
        } else if(nb_panels == 4) {
          res_list <- tabsetPanel(liste_tab[[1]], liste_tab[[2]], liste_tab[[3]], liste_tab[[4]])
        }

      } else {
        res_list <- NULL
      }
      res_list
    } else {
      list()
    }
  })
})

outputOptions(output, "panels_tab", suspendWhenHidden = FALSE)


# update des inputs -------------------------------------------------------
observe({
  imported_data <- imported_data()
  if(!is.null(imported_data)) {
    isolate({

      nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)
      if(nb_panels == 1) {

        panel_name <- paste0(attributes(imported_data)$names[1], "s")
        if(attributes(imported_data)$names[2] == "cluster") panel_name <- "clusters"

        if(panel_name == "areas") {
          choix_lig1 <- unique(imported_data$area)
          choix_col1 <- colnames(imported_data)[-which(colnames(imported_data) == "area")]
        } else if(panel_name == "links") {
          choix_lig1 <- unique(imported_data$link)
          choix_col1 <- colnames(imported_data)[-which(colnames(imported_data) == "link")]
        } else if(panel_name == "districts") {
          choix_lig1 <- unique(imported_data$district)
          choix_col1 <- colnames(imported_data)[-which(colnames(imported_data) == "district")]
        } else if(panel_name == "clusters") {
          choix_lig1 <- unique(imported_data$area)
          choix_col1 <- colnames(imported_data)[-which(colnames(imported_data) %in% c("area", "cluster"))]
        }

        if(panel_name == "clusters") {
          updateSelectInput(session, "lig1", label = "areas", choices = c("all", choix_lig1),
                            selected = "all")
        } else {
          updateSelectInput(session, "lig1", label = panel_name, choices = c("all", choix_lig1),
                            selected = "all")
        }


        if(any(c("month", "week", "day", "hour", "time") %in% choix_col1)) {
          choix_col1 <- choix_col1[-which(choix_col1 %in% c("month", "week", "day", "hour", "time"))]
        }

        updateSelectInput(session, "col1", label = "Variables", choices = c("all", choix_col1),
                          selected = "all")

      } else if(nb_panels > 0){
        sapply(1:nb_panels, FUN = function(nb) {
          panel_name = names(imported_data)[nb]
          data_nb <- imported_data[[nb]]
          if(panel_name == "areas") {
            choix_lig1 <- unique(data_nb$area)
            choix_col1 <- colnames(data_nb)[-which(colnames(data_nb) %in% c("area","month", "week",
                                                                            "day", "hour", "time"))]
          } else if(panel_name == "links") {
            choix_lig1 <- unique(data_nb$link)
            choix_col1 <- colnames(data_nb)[-which(colnames(data_nb) %in% c("link","month", "week",
                                                                            "day", "hour", "time"))]
          } else if(panel_name == "districts") {
            choix_lig1 <- unique(data_nb$district)
            choix_col1 <- colnames(data_nb)[-which(colnames(data_nb) %in% c("district","month", "week",
                                                                            "day", "hour", "time"))]
          } else if(panel_name == "clusters") {
            choix_lig1 <- unique(data_nb$area)
            choix_col1 <- colnames(data_nb)[-which(colnames(data_nb) %in% c("area", "cluster", "month", "week",
                                                                            "day", "hour", "time"))]
          }

          if(panel_name == "clusters") {
            updateSelectInput(session, paste0("lig", nb), label = "areas",
                              choices = c("all", choix_lig1), selected = "all")
          } else {
            updateSelectInput(session, paste0("lig", nb), label = panel_name,
                              choices = c("all", choix_lig1), selected = "all")
          }

          updateSelectInput(session, paste0("col", nb), label = "Variables", choices = c("all", choix_col1),
                            selected = "all")
        })
      } else {
        NULL
      }

    })
  } else {
    NULL
  }
})

choix_lig1_2 <- reactive({
  imported_data <- imported_data()
  lig1 <- input$lig1
  isolate({
    if(!is.null(lig1) && length(imported_data) > 1) {
      nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)
      if(nb_panels > 1) {
        panel_name = names(imported_data)[1]
        data_nb <- imported_data[[1]]
      } else {
        panel_name <- paste0(attributes(imported_data)$names[1], "s")
        if(attributes(imported_data)$names[2] == "cluster") panel_name <- "clusters"
        data_nb <- imported_data
      }

      if(panel_name == "clusters" & !is.null(input[[paste0("lig", 1)]])) {
        if("all" %in% lig1) {
          lig1 <- unique(data_nb$area)
        }
        data_2 <- data_nb[area %in% lig1]
        choix_lig1_2 <- c("all", unique(data_2$cluster))

        current_sel <- isolate(input$lig1_2)
        selected <- intersect(current_sel, choix_lig1_2)
        if(length(selected) == 0){
          selected = "all"
        }
        choix_lig1_2 <- list(choices = choix_lig1_2, selected = selected)
      } else {
        choix_lig1_2 <- NULL
      }

    } else {
      choix_lig1_2 <- NULL
    }
    return(choix_lig1_2)
  })
})

observe({
  choix_lig1_2 <- choix_lig1_2()
  if(!is.null(choix_lig1_2)) {
    isolate({
      updateSelectInput(session, "lig1_2", label = "Clusters", choices = choix_lig1_2$choices,
                        selected = choix_lig1_2$selected)
    })
  }
})

choix_lig2_2 <- reactive({
  imported_data <- imported_data()
  lig2 <- input$lig2
  isolate({
    if(!is.null(lig2) && length(imported_data) > 1) {
      nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)
      if(nb_panels > 1) {
        panel_name = names(imported_data)[2]
        data_nb <- imported_data[[2]]
        if(panel_name == "clusters" & !is.null(input[[paste0("lig", 2)]])) {
          if("all" %in% lig2) {
            lig2 <- unique(data_nb$area)
          }
          data_2 <- data_nb[area %in% lig2]
          choix_lig2_2 <- c("all", unique(data_2$cluster))

          current_sel <- isolate(input$lig2_2)
          selected <- intersect(current_sel, choix_lig2_2)
          if(length(selected) == 0){
            selected = "all"
          }
          choix_lig2_2 <- list(choices = choix_lig2_2, selected = selected)

        } else {
          choix_lig2_2 <- NULL
        }

      } else {
        choix_lig2_2 <- NULL
      }
      return(choix_lig2_2)
    }

  })
})

observe({
  choix_lig2_2 <- choix_lig2_2()
  if(!is.null(choix_lig2_2)) {
    isolate({
      updateSelectInput(session, "lig2_2", label = "Clusters", choices = choix_lig2_2$choices,
                        selected = choix_lig2_2$selected)

    })
  }
})

choix_lig3_2 <- reactive({
  imported_data <- imported_data()
  lig3 <- input$lig3
  isolate({
    if(!is.null(lig3) & length(imported_data) > 2) {
      nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)
      if(nb_panels > 1) {
        panel_name = names(imported_data)[3]
        data_nb <- imported_data[[3]]
        if(panel_name == "clusters" & !is.null(input[[paste0("lig", 3)]])) {
          if("all" %in% lig3) {
            lig3 <- unique(data_nb$area)
          }
          data_2 <- data_nb[area %in% lig3]
          choix_lig3_2 <- c("all", unique(data_2$cluster))

          current_sel <- isolate(input$lig3_2)
          selected <- intersect(current_sel, choix_lig3_2)
          if(length(selected) == 0){
            selected = "all"
          }
          choix_lig3_2 <- list(choices = choix_lig3_2, selected = selected)
        } else {
          choix_lig3_2 <- NULL
        }

      } else {
        choix_lig3_2 <- NULL
      }
      return(choix_lig3_2)
    }

  })
})

observe({
  choix_lig3_2 <- choix_lig3_2()
  if(!is.null(choix_lig3_2)) {
    isolate({
      updateSelectInput(session, "lig3_2", label = "Clusters", choices = choix_lig3_2$choices,
                        selected = choix_lig3_2$selected)

    })
  }
})

choix_lig4_2 <- reactive({
  imported_data <- imported_data()
  lig4 <- input$lig4
  isolate({
    if(!is.null(lig4) & length(imported_data) > 3) {
      nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)
      if(nb_panels > 1) {
        panel_name = names(imported_data)[4]
        data_nb <- imported_data[[4]]
        if(panel_name == "clusters" & !is.null(input[[paste0("lig", 4)]])) {
          if("all" %in% lig4) {
            lig4 <- unique(data_nb$area)
          }
          data_2 <- data_nb[area %in% lig4]
          choix_lig4_2 <- c("all", unique(data_2$cluster))

          current_sel <- isolate(input$lig4_2)
          selected <- intersect(current_sel, choix_lig4_2)
          if(length(selected) == 0){
            selected = "all"
          }
          choix_lig4_2 <- list(choices = choix_lig4_2, selected = selected)
        } else {
          choix_lig4_2 <- NULL
        }

      } else {
        choix_lig4_2 <- NULL
      }
      return(choix_lig4_2)
    }

  })
})

observe({
  choix_lig4_2 <- choix_lig4_2()
  if(!is.null(choix_lig4_2)) {
    isolate({
      updateSelectInput(session, "lig4_2", label = "Clusters", choices = choix_lig4_2$choices,
                        selected = choix_lig4_2$selected)

    })
  }
})

# donnees et tables -------------------------------------------------------

data_dt1 <- reactive({
  imported_data <- imported_data()
  if(!is.null((imported_data))) {
    nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)
    if(nb_panels == 1) {
      panel_name <- paste0(attributes(imported_data)$names[1], "s")
      if(attributes(imported_data)$names[2] == "cluster") panel_name <- "clusters"

      data2 <- imported_data
    } else if(nb_panels > 1) {
      panel_name <- names(imported_data)[1]
      data2 <- imported_data[[1]]
    }

    data2 <- subsetDataTable(data = data2, panel_name = panel_name,
                             lig1 = input$lig1, lig2 = input$lig1_2, col1 = input$col1)

    data2

  } else {
    data.table("No data")
  }
})

output$dt1 <- renderDT({
  datatable(head(data_dt1(), 10), rownames = FALSE,
            options = list(dom = 't', scrollX = TRUE))
})

output$box_tab1_1 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Rows", current_language$language), value = nrow(data_dt1()), icon = icon("bars"), color = "green", fill = FALSE)
})

output$box_tab1_2 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Cols", current_language$language), value = ncol(data_dt1()), icon = icon("barcode"), color = "green", fill = FALSE)
})

output$box_tab1_3 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("Size", current_language$language), value = format(object.size(data_dt1()), units = "auto"), icon = icon("archive"), color = "green", fill = FALSE)
})

data_dt2 <- reactive({
  imported_data <- imported_data()
  if(!is.null((imported_data))) {
    nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)

    panel_name <- names(imported_data)[2]
    data2 <- imported_data[[2]]

    data2 <- subsetDataTable(data = data2, panel_name = panel_name,
                             lig1 = input$lig2, lig2 = input$lig2_2, col1 = input$col2)

    data2

  } else {
    data.table("No data")
  }
})

output$dt2 <- renderDT({
  datatable(head(data_dt2(), 10), rownames = FALSE,
            options = list(dom = 't', scrollX = TRUE))
})

output$box_tab2_1 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Rows", current_language$language), value = nrow(data_dt2()), icon = icon("bars"), color = "green", fill = FALSE)
})

output$box_tab2_2 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Cols", current_language$language), value = ncol(data_dt2()), icon = icon("barcode"), color = "green", fill = FALSE)
})

output$box_tab2_3 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("Size", current_language$language), value = format(object.size(data_dt2()), units = "auto"), icon = icon("archive"), color = "green", fill = FALSE)
})

data_dt3 <- reactive({
  imported_data <- imported_data()
  if(!is.null((imported_data))) {
    nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)

    panel_name <- names(imported_data)[3]
    data2 <- imported_data[[3]]

    data2 <- subsetDataTable(data = data2, panel_name = panel_name,
                             lig1 = input$lig3, lig2 = input$lig3_2, col1 = input$col3)

    data2

  } else {
    data.table("No data")
  }
})

output$dt3 <- renderDT({
  datatable(head(data_dt3(), 10), rownames = FALSE,
            options = list(dom = 't', scrollX = TRUE))
})

output$box_tab3_1 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Rows", current_language$language), value = nrow(data_dt3()), icon = icon("bars"), color = "green", fill = FALSE)
})

output$box_tab3_2 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Cols", current_language$language), value = ncol(data_dt3()), icon = icon("barcode"), color = "green", fill = FALSE)
})

output$box_tab3_3 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("Size", current_language$language), value = format(object.size(data_dt3()), units = "auto"), icon = icon("archive"), color = "green", fill = FALSE)
})

data_dt4 <- reactive({
  imported_data <- imported_data()
  if(!is.null((imported_data))) {
    nb_panels <- ifelse("list" %in% class(imported_data), length(imported_data), 1)

    panel_name <- names(imported_data)[4]
    data2 <- imported_data[[4]]

    data2 <- subsetDataTable(data = data2, panel_name = panel_name,
                             lig1 = input$lig4, lig2 = input$lig4_2, col1 = input$col4)

    data2

  } else {
    data.table("No data")
  }
})

output$dt4 <- renderDT({
  datatable(head(data_dt4(), 10), rownames = FALSE,
            options = list(dom = 't', scrollX = TRUE))
})

output$box_tab4_1 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Rows", current_language$language), value = nrow(data_dt4()), icon = icon("bars"), color = "green", fill = FALSE)
})

output$box_tab4_2 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("N. Cols", current_language$language), value = ncol(data_dt4()), icon = icon("barcode"), color = "green", fill = FALSE)
})

output$box_tab4_3 <- renderInfoBox({
  infoBox(title = antaresVizMedTSO:::.getLabelLanguage("Size", current_language$language), value = format(object.size(data_dt4()), units = "auto"), icon = icon("archive"), color = "green", fill = FALSE)
})


# Import des donnees ------------------------------------------------------
output$import_data <- downloadHandler(
  filename = function() {
    paste0("Antares_IO_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".xlsx")
  },
  content = function(file) {
    data_res <- imported_data()
    if(!is.null(data_res)){
      session$sendCustomMessage(type = 'show_spinner',
                                message = list(id = "#export_busy"))

      withCallingHandlers({
        tryCatch({openxlsx::write.xlsx(data_res, file, row.names = FALSE)},
                 error = function(e){
                   showModal(modalDialog(
                     title = "Error writing data",
                     easyClose = TRUE,
                     footer = NULL,
                     paste("You can try to reduce size... Error : ", e, sep = "\n")
                   ))
                   openxlsx::write.xlsx(NULL, file, row.names = FALSE)
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

      session$sendCustomMessage(type = 'hide_spinner',
                                message = list(id = "#export_busy"))
    }
  }
)

output$import_data_sel <- downloadHandler(
  filename = function() {
    paste0("Antares_IO_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".xlsx")
  },
  content = function(file) {
    data_res <- imported_data()
    data_import <- NULL
    if("list" %in% class(data_res)) {
      data_import <- lapply(1:length(data_res), FUN = function(i) {
        eval(parse(text = paste0("data_dt", i, "()")))
      })

    } else if("data.frame" %in% class(data_res) | "data.table" %in% class(data_res)){
      data_import <- data_dt1()
    }

    if(!is.null(data_import)){
      session$sendCustomMessage(type = 'show_spinner',
                                message = list(id = "#export_busy"))

      withCallingHandlers({
        tryCatch({openxlsx::write.xlsx(data_import, file, row.names = FALSE)},
                 error = function(e){
                   showModal(modalDialog(
                     title = "Error writing data",
                     easyClose = TRUE,
                     footer = NULL,
                     paste("You can try to reduce size... Error : ", e, sep = "\n")
                   ))
                   openxlsx::write.xlsx(NULL, file, row.names = FALSE)
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

      session$sendCustomMessage(type = 'hide_spinner',
                                message = list(id = "#export_busy"))
    }
  }
)

output$import_tab1 <- downloadHandler(
  filename = function() {
    paste0("Antares_IO_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".xlsx")
  },
  content = function(file) {
    data_res <- data_dt1()
    if(!is.null(data_res)){
      session$sendCustomMessage(type = 'show_spinner',
                                message = list(id = "#export_busy_1"))

      withCallingHandlers({
        tryCatch({openxlsx::write.xlsx(data_res, file, row.names = FALSE)},
                 error = function(e){
                   showModal(modalDialog(
                     title = "Error writing data",
                     easyClose = TRUE,
                     footer = NULL,
                     paste("You can try to reduce size... Error : ", e, sep = "\n")
                   ))
                   openxlsx::write.xlsx(NULL, file, row.names = FALSE)
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

      session$sendCustomMessage(type = 'hide_spinner',
                                message = list(id = "#export_busy_1"))
    }
  }
)

output$import_tab2 <- downloadHandler(
  filename = function() {
    paste0("Antares_IO_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".xlsx")
  },
  content = function(file) {
    data_res <- data_dt2()
    if(!is.null(data_res)){
      session$sendCustomMessage(type = 'show_spinner',
                                message = list(id = "#export_busy_2"))

      withCallingHandlers({
        tryCatch({openxlsx::write.xlsx(data_res, file, row.names = FALSE)},
                 error = function(e){
                   showModal(modalDialog(
                     title = "Error writing data",
                     easyClose = TRUE,
                     footer = NULL,
                     paste("You can try to reduce size... Error : ", e, sep = "\n")
                   ))
                   openxlsx::write.xlsx(NULL, file, row.names = FALSE)
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

      session$sendCustomMessage(type = 'hide_spinner',
                                message = list(id = "#export_busy_2"))
    }
  }
)

output$import_tab3 <- downloadHandler(
  filename = function() {
    paste0("Antares_IO_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".xlsx")
  },
  content = function(file) {
    data_res <- data_dt3()
    if(!is.null(data_res)){
      session$sendCustomMessage(type = 'show_spinner',
                                message = list(id = "#export_busy_3"))

      withCallingHandlers({
        tryCatch({openxlsx::write.xlsx(data_res, file, row.names = FALSE)},
                 error = function(e){
                   showModal(modalDialog(
                     title = "Error writing data",
                     easyClose = TRUE,
                     footer = NULL,
                     paste("You can try to reduce size... Error : ", e, sep = "\n")
                   ))
                   openxlsx::write.xlsx(NULL, file, row.names = FALSE)
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

      session$sendCustomMessage(type = 'hide_spinner',
                                message = list(id = "#export_busy_3"))
    }
  }
)

output$import_tab4 <- downloadHandler(
  filename = function() {
    paste0("Antares_IO_", format(Sys.time(), "%d%m%Y_%H%M%S"), ".xlsx")
  },
  content = function(file) {
    data_res <- data_dt4()
    if(!is.null(data_res)){
      session$sendCustomMessage(type = 'show_spinner',
                                message = list(id = "#export_busy_4"))

      withCallingHandlers({
        tryCatch({openxlsx::write.xlsx(data_res, file, row.names = FALSE)},
                 error = function(e){
                   showModal(modalDialog(
                     title = "Error writing data",
                     easyClose = TRUE,
                     footer = NULL,
                     paste("You can try to reduce size... Error : ", e, sep = "\n")
                   ))
                   openxlsx::write.xlsx(NULL, file, row.names = FALSE)
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

      session$sendCustomMessage(type = 'hide_spinner',
                                message = list(id = "#export_busy_4"))
    }
  }
)

output$export_btn <- renderUI({
  current_language <- current_language$language
  isolate({
    fluidRow(column(6,div(downloadButton("import_data", antaresVizMedTSO:::.getLabelLanguage("Export all tables (without filters)", current_language), icon = icon("upload")),
                          align = "right")),
             column(3,div(downloadButton("import_data_sel", antaresVizMedTSO:::.getLabelLanguage("Export all tables (with filters)", current_language), icon = icon("upload")),
                          align = "left")),
             column(3,
                    tags$div(style="padding-top:0px; display: none", id = "export_busy",
                             tags$img(src = "spinner.gif", height = 40, width = 40),align = 'left')
             )
    )
  })
})

