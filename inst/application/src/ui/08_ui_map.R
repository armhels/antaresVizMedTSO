tabPanel(textOutput("label_tab_map_menu"), 
         tabsetPanel(id = "map_panel",
                     tabPanel(textOutput("label_tab_layout_build"), 
                              fluidRow(
                                column(12,
                                       conditionalPanel(condition = "output.have_data",
                                                        conditionalPanel(condition = "output.have_data_links || output.have_data_areas",
                                                                         antaresVizMedTSO:::changeCoordsUI("ml")
                                                        ),
                                                        conditionalPanel(condition = "output.have_data_links === false && output.have_data_areas === false",
                                                                         h3(textOutput("no_data_4"))
                                                        )
                                                        
                                       ),
                                       conditionalPanel(condition = "output.have_data === false",
                                                        h3(textOutput("no_data_5"))
                                       )
                                       
                                )
                              )
                     ),
                     tabPanel(textOutput("label_tab_layout_view"),
                              tabsetPanel(id = "tab_layout_view",
                                          tabPanel("Carte", 
                                                   fluidRow(
                                                     column(12,
                                                            conditionalPanel(condition = "output.must_print_map",
                                                                             div(h3(textOutput("title_current_layout")), align = "center"),
                                                                             leafletDragPointsOutput("current_layout", height = "700px")
                                                            ),
                                                            conditionalPanel(condition = "output.must_print_map === false",
                                                                             h3(textOutput("no_layout_1"))
                                                            ),
                                                            hr(),
                                                            fluidRow(
                                                              column(6,
                                                                     fluidRow(
                                                                       column(6, h4(textOutput("title_import_layout"))),
                                                                       column(6,   
                                                                              div(
                                                                                # fileInput("import_layout", NULL, 
                                                                                #           accept = c(".RDS", ".rds", ".Rds")),
                                                                                shinyFilesButton("file_import_layout", 
                                                                                                  NULL, 
                                                                                                  NULL, 
                                                                                                  icon = icon("upload"),
                                                                                                  multiple = FALSE, viewtype = "detail"),
                                                                              
                                                                                align = "left")
                                                                       )
                                                                     )
                                                              ),
                                                              column(6,
                                                                     conditionalPanel(condition = "output.must_print_map",
                                                                                      fluidRow(
                                                                                        column(6, h4(textOutput("title_download_layout"))),
                                                                                        column(6,   div(downloadButton('download_layout', NULL), align = "left"))
                                                                                      )
                                                                     )
                                                                     
                                                              )
                                                            )
                                                     )
                                                   )
                                          ),
                                          tabPanel("Edition",
                                                   fluidRow(
                                                     column(12,
                                                            conditionalPanel(condition = "output.must_print_map",
                                                                             antaresVizMedTSO:::changeCoordsUI("ml_edit")
                                                                             
                                                            ),
                                                            conditionalPanel(condition = "output.must_print_map === false",
                                                                             h3(textOutput("no_layout_3")), 
                                                                             hr()
                                                            )
                                                            
                                                     )
                                                   )
                                                   
                                          )
                              )
                              
                              
                     ),
                     tabPanel(textOutput("label_tab_map_viz"), 
                              fluidRow(
                                column(12,
                                       conditionalPanel(condition = "output.have_data",
                                                        conditionalPanel(condition = "output.must_print_map",
                                                                         uiOutput("plotMap_ui"),
                                                                         hr(),
                                                                         uiOutput("ui_get_set_map_params")
                                                        ), 
                                                        conditionalPanel(condition = "output.must_print_map === false", 
                                                                         h3(textOutput("no_layout_2"))
                                                        )
                                       ),
                                       conditionalPanel(condition = "output.have_data === false",
                                                        h3(textOutput("no_data_6"))
                                       )
                                )
                              )
                     ),
                     tabPanel(textOutput("label_tab_map_color"),
                              uiOutput("set_color_vars")
                     )
         )
)