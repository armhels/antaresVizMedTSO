tabPanel(textOutput("label_tab_medtso_map_menu"), 
         tabsetPanel(id = "medtso_map_panel", 
                     tabPanel(textOutput("label_tab_import_data_2"),
                              h3(textOutput("title_import_data_3")),
                              fluidRow(
                                # column(5, 
                                #        directoryInput('directory_medtso_maps', label = '', value = study_dir)
                                # ), 
                                column(1, 
                                       div(
                                         shinyDirButton(id = "directory_medtso_maps", label = "", title = "", icon = icon("folder")), 
                                         style = "margin-top:17px", align = "center"
                                       )
                                ), 
                                column(4, 
                                       div(
                                         verbatimTextOutput("print_directory_medtso_maps"), 
                                         style = "margin-top:15px", align = "left"
                                       )
                                ), 
                                conditionalPanel(condition = "output.ctrl_is_antares_study_medtso_maps", 
                                                 column(1, 
                                                        div(br(), h4("Simulation : "), align = "center")
                                                 ), 
                                                 column(4, 
                                                        selectInput("study_path_medtso_maps", "", choices = NULL, selected = NULL, width = "100%")
                                                 ), 
                                                 column(2, 
                                                        div(br(), 
                                                            actionButton("init_sim_medtso_maps", "Set simulation", icon = icon("check-circle")),
                                                            align = "center"
                                                        )
                                                 )
                                ),
                                conditionalPanel(condition = "output.ctrl_is_antares_study_medtso_maps === false", 
                                                 column(5, 
                                                        h3(textOutput("directory_message_medtso_maps"), style = "color : red")
                                                 )
                                )
                              ), 
                              
                              conditionalPanel(condition = "output.have_study_medtso_maps",
                                               fluidRow(
                                                 column(12,
                                                        hr(), 
                                                        div(h3(textOutput("current_opts_2"), align = "center")),
                                                        
                                                        h3(textOutput("title_readAntares_2")),
                                                        fluidRow(
                                                          column(3, 
                                                                 selectInput("read_areas_medtso_maps", "Areas :", choices = NULL, selected = NULL, multiple = TRUE)
                                                          ), 
                                                          column(3, 
                                                                 selectInput("read_links_medtso_maps", "Links :", choices = NULL, selected = NULL, multiple = TRUE)
                                                          ),
                                                          column(3, 
                                                                 selectInput("read_mcYears_medtso_maps", "Choose mcYears :", choices = NULL, selected = NULL, multiple = FALSE)
                                                          ),
                                                          column(3, 
                                                                 checkboxInput("rmva_ctrl_medtso_maps", "Remove virtual Areas", FALSE)
                                                          )
                                                        ), 
                                                        
                                                        conditionalPanel(condition = "input.rmva_ctrl_medtso_maps",
                                                                         h4(textOutput("title_removeVirtualAreas_2")),
                                                                         fluidRow(
                                                                           column(3, 
                                                                                  selectInput("rmva_storageFlexibility_medtso_maps", "storageFlexibility :", choices = NULL, selected = NULL, multiple = TRUE)
                                                                           ),
                                                                           column(3, 
                                                                                  selectInput("rmva_production_medtso_maps", "production :", choices = NULL, selected = NULL, multiple = TRUE)
                                                                           ), 
                                                                           
                                                                           column(3, 
                                                                                  br(),
                                                                                  checkboxInput("rmva_reassignCosts_medtso_maps", "reassignCosts", FALSE)
                                                                           ),
                                                                           
                                                                           column(3, 
                                                                                  br(),
                                                                                  checkboxInput("rmva_newCols_medtso_maps", "newCols", FALSE)
                                                                           )
                                                                         )
                                                        ),
                                                        
                                                        hr(),
                                                        uiOutput("ui_sel_file_import_medtso_maps"),
                                                        
                                                        div(actionButton("import_data_medtso_maps", "Validate & import data", icon = icon("upload")), align = "center")
                                                 )
                                               )
                              )
                              
                              
                     ),
                     tabPanel("Parameters",
                              conditionalPanel(condition = "output.have_data_map_tso",
                                               br(),
                                               uiOutput("ui_file_sel_medtso_map"),
                                               
                                               fluidRow(
                                                 column(4, includeMarkdown("src/inputs_medtso_maps.md")),
                                                 column(8, tabsetPanel(
                                                                        tabPanel("Links", DTOutput("dt_pos_links")),
                                                                        
                                                                        tabPanel("Areas", DTOutput("dt_pos_areas"))
                                                                        
                                                 ))
                                               )
                                               
                              ), 
                              conditionalPanel(condition = "output.have_data_map_tso === false",
                                               h3(textOutput("no_data_9"))
                              )
                     ),
                     tabPanel("Countries",
                              conditionalPanel(condition = "output.have_data_map_tso",
                                               fluidRow(
                                                 column(4, textInput("title_countries", label = "Title :", value = "Day ahead marginal price", width = "100%")),
                                                 column(width = 2, selectInput(inputId = "column_selection", label = "Variable :",
                                                                               choices = NULL, selected = NULL, multiple = F)
                                                 ),
                                                 column(width = 2, colourInput("col_min", "Min. col", "#F5ED08")),
                                                 column(width = 2, colourInput("col_med", "Middle. col", "#FF4924")),
                                                 column(width = 2, colourInput("col_max", "Max. col", "#5E0202"))
                                               ),
                                               fluidRow(
                                                 column(width = 2, sliderInput("arrow_width_countries", "Arrow width :", min = 0.1, max = 1, value = 0.5, step = 0.1)),
                                                 column(width = 2, sliderInput("arrow_size_countries", "Arrow size :", min = 0.01, max = 0.2, value = 0.08, step = 0.01)),
                                                 column(width = 2, sliderInput("arrow_textsize_countries", "Arrow label :", min = 1, max = 10, value = 4, step = 1)),
                                                 column(width = 3, div(colourInput("col_arrow_1_countries", "Arrow : color 1", "#217314"), align = "center")),
                                                 column(width = 3, div(colourInput("col_arrow_2_countries", "Arrow : color 2", "#9c0636"), align = "center"))
                                               ),
                                               div(actionButton("go_cty", "OK"), align = "center"),
                                               hr(),
                                               fluidRow(plotOutput("countries_plots", height = "1000px")),
                                               hr(),
                                               div(downloadButton("download_countries", ""), align = "center")
                                               
                              ), 
                              conditionalPanel(condition = "output.have_data_map_tso === false",
                                               h3(textOutput("no_data_10"))
                              )
                     ),
                     tabPanel("Exchanges and productions",
                              conditionalPanel(condition = "output.have_data_map_tso",
                                               fluidRow(
                                                 column(3, textInput("title_centers", label = "Title :", value = "Exchanges and productions", width = "100%")),
                                                 column(width = 6, selectInput(inputId = "centers_columns", label = "Variables :", choices = NULL, selected = NULL, multiple = T, width = "100%")),
                                                 column(width = 3, colourInput("col_sp", "Cty : color", "#f3f1f2"))
                                               ),
                                               
                                               
                                               fluidRow(
                                                 column(width = 3, sliderInput("pie_size_centers", "Pie size :", min = 1, max = 6, value = 2.5, step = 0.5)),
                                                 column(width = 3, sliderInput("pie_textsize_centers", "Pie label :", min = 1, max = 10, value = 3, step = 0.5)),
                                                 column(width = 3, sliderInput("pie_alpha_centers", "Pie alpha :", min = 0, max = 1, value = 0.5, step = 0.1)),
                                                 column(width = 3, sliderInput("arrow_width_centers", "Arrow width :", min = 0.1, max = 1, value = 0.5, step = 0.1))
                                               ),
                                               fluidRow(
                                                 column(width = 3, sliderInput("arrow_size_centers", "Arrow size :", min = 0.01, max = 0.2, value = 0.08, step = 0.01)),
                                                 column(width = 3, sliderInput("arrow_textsize_centers", "Arrow label :", min = 1, max = 10, value = 4, step = 1)),
                                                 column(width = 3, colourInput("col_arrow_1", "Arrow : color 1", "#217314")),
                                                 column(width = 3, colourInput("col_arrow_2", "Arrow : color 2", "#9c0636"))
                                                 
                                               ),
                                               div(actionButton("go_exch", "OK"), align = "center"),
                                               hr(),
                                               fluidRow(plotOutput("centers_plots", height = "1000px")),
                                               hr(),
                                               div(downloadButton("download_centers", ""), align = "center")
                                               
                              ), 
                              conditionalPanel(condition = "output.have_data_map_tso === false",
                                               h3(textOutput("no_data_11"))
                              )
                     ),
                     tabPanel("Interconnection usage",
                              conditionalPanel(condition = "output.have_data_map_tso",
                                               fluidRow(
                                                 column(3, textInput("title_interco", label = "Title :", value = "Interconnection usage", width = "100%")),
                                                 column(width = 3, sliderInput("pie_size_interco", "Pie size :", min = 1, max = 6, value = 2, step = 0.5)),
                                                 column(width = 3, sliderInput("pie_textsize_interco", "Pie label :", min = 1, max = 10, value = 3.5, step = 0.5)),
                                                 column(width = 1, sliderInput("pie_alpha_interco", "Pie alpha :", min = 0, max = 1, value = 0.8, step = 0.1)),
                                                 column(width = 1, colourInput("col_sp_interco", "Cty : color", "#f3f1f2"))
                                               ),
                                               fluidRow(
                                                 column(width = 3, sliderInput("arrow_width_interco", "Arrow width :", min = 0.1, max = 1, value = 0.5, step = 0.1)),
                                                 column(width = 3, sliderInput("arrow_size_interco", "Arrow size :", min = 0.01, max = 0.2, value = 0.08, step = 0.01)),
                                                 column(width = 3, sliderInput("arrow_textsize_interco", "Arrow label :", min = 1, max = 10, value = 4, step = 1)),
                                                 column(width = 1, colourInput("col_arrow_1_interco", "Arrow : color 1", "#217314")),
                                                 column(width = 1, colourInput("col_arrow_2_interco", "Arrow : color 2", "#9c0636"))
                                               ),
                                               div(actionButton("go_interco", "OK"), align = "center"),
                                               hr(),
                                               fluidRow(plotOutput("interco_plots", height = "1000px")),
                                               hr(),
                                               div(downloadButton("download_interco", ""), align = "center")
                                               
                              ), 
                              conditionalPanel(condition = "output.have_data_map_tso === false",
                                               h3(textOutput("no_data_12"))
                              )
                              
                     )
         )
)