tabPanel(textOutput("label_tab_format_output"), 
         tabsetPanel(id = "format_output_panel", 
                     tabPanel(textOutput("label_tab_import_data_3"),
                              h3(textOutput("title_import_data_4")),
                              fluidRow(
                                # column(5, 
                                #        directoryInput('directory_format_output', label = '',
                                #                       value = study_dir)
                                # ), 
                                column(1, 
                                       div(
                                         shinyDirButton(id = "directory_format_output", label = "", title = "", icon = icon("folder")), 
                                         style = "margin-top:17px", align = "center"
                                       )
                                ), 
                                column(4, 
                                       div(
                                         verbatimTextOutput("print_directory_format_output"), 
                                         style = "margin-top:15px", align = "left"
                                       )
                                ), 
                                conditionalPanel(condition = "output.ctrl_is_antares_study_format_output", 
                                                 column(1, 
                                                        div(br(), h4("Simulation : "), align = "center")
                                                 ), 
                                                 column(4, 
                                                        selectInput("study_path_format_output", "", choices = NULL, selected = NULL, width = "100%")
                                                 ), 
                                                 column(2, 
                                                        div(br(), 
                                                            actionButton("init_sim_format_output", "Set simulation", icon = icon("check-circle")),
                                                            align = "center"
                                                        )
                                                 )
                                ),
                                conditionalPanel(condition = "output.ctrl_is_antares_study_format_output === false", 
                                                 column(5, 
                                                        h3(textOutput("directory_message_format_output"), style = "color : red")
                                                 )
                                )
                              ), 
                              
                              conditionalPanel(condition = "output.have_study_format_output_tmp",
                                               fluidRow(
                                                 column(12,
                                                        hr(), 
                                                        div(h3(textOutput("current_opts_3"), align = "center")),
                                                        
                                                        h3(textOutput("title_readAntares_3")),
                                                        fluidRow(
                                                          
                                                          column(3, offset = 2, 
                                                                 radioButtons("read_type_mcYears_format_output", "mcYears :",
                                                                              c("synthetic", "custom"), inline = TRUE)
                                                          ), 
                                                          conditionalPanel(condition = "input.read_type_mcYears_format_output === 'custom'", 
                                                                           column(3, 
                                                                                  selectInput("read_mcYears_format_output", "Choose mcYears :", choices = NULL, selected = NULL, multiple = TRUE)
                                                                           )
                                                          )
                                                        ),
                                                        
                                                        fluidRow(
                                                          column(3, 
                                                                 h4(textOutput("title_removeVirtualAreas_3"))
                                                          ),
                                                          column(3, 
                                                                 checkboxInput("rmva_ctrl_format_output", "enabled", FALSE)
                                                          )
                                                        ),
                                                        conditionalPanel(condition = "input.rmva_ctrl_format_output",
                                                                         fluidRow(
                                                                           column(3, 
                                                                                  selectInput("rmva_storageFlexibility_format_output", "storageFlexibility (PSP) :", choices = NULL, 
                                                                                              selected = NULL, multiple = TRUE, width = "100%")
                                                                           ),
                                                                           column(3, 
                                                                                  selectInput("rmva_production_format_output", "production :", choices = NULL, selected = NULL, 
                                                                                              multiple = TRUE, width = "100%")
                                                                           ), 
                                                                           
                                                                           column(3, 
                                                                                  br(),
                                                                                  checkboxInput("rmva_reassignCosts_format_output", "reassignCosts", FALSE)
                                                                           ),
                                                                           
                                                                           column(3, 
                                                                                  br(),
                                                                                  checkboxInput("rmva_newCols_format_output", "newCols", FALSE)
                                                                           )
                                                                         ),
                                                                         fluidRow(
                                                                           column(4, 
                                                                                  selectInput("rmva_PSP_Closed_format_output", "Hydro Storage  (PSP_Closed):", choices = NULL, 
                                                                                              selected = NULL, multiple = TRUE, width = "100%")
                                                                           ),
                                                                           column(4, 
                                                                                  selectInput("rmva_BATT_format_output", "Battery Storage (BATT)  :", choices = NULL, 
                                                                                              selected = NULL, multiple = TRUE, width = "100%")
                                                                           ),
                                                                           column(4, 
                                                                                  selectInput("rmva_DSR_format_output", "Demand Side  (DSR) :", choices = NULL, 
                                                                                              selected = NULL, multiple = TRUE, width = "100%")
                                                                           )
                                                                         ), 
                                                                         fluidRow(
                                                                           column(4, 
                                                                                  selectInput("rmva_EV_format_output", "Electric Vehicle (EV) :", choices = NULL, 
                                                                                              selected = NULL, multiple = TRUE, width = "100%")
                                                                           ),
                                                                           
                                                                           column(4, 
                                                                                  selectInput("rmva_P2G_format_output", "Power-to-gas (P2G) :", choices = NULL, 
                                                                                              selected = NULL, multiple = TRUE, width = "100%")
                                                                           ),
                                                                           column(4, 
                                                                                  selectInput("rmva_H2_format_output", "Hydrogen (H2) :", choices = NULL, 
                                                                                              selected = NULL, multiple = TRUE, width = "100%")
                                                                           )
                                                                         ),
                                                                         checkboxInput("rmva_ctrl_format_output_step_2", "step 2", FALSE),
                                                                         
                                                                         conditionalPanel(condition = "input.rmva_ctrl_format_output_step_2",
                                                                                          fluidRow(
                                                                                            column(3, 
                                                                                                   selectInput("rmva_storageFlexibility_format_output_2", "storageFlexibility (PSP) :", choices = NULL, 
                                                                                                               selected = NULL, multiple = TRUE, width = "100%")
                                                                                            ),
                                                                                            column(3, 
                                                                                                   selectInput("rmva_production_format_output_2", "production :", choices = NULL, selected = NULL, 
                                                                                                               multiple = TRUE, width = "100%")
                                                                                            ), 
                                                                                            
                                                                                            column(3, 
                                                                                                   br(),
                                                                                                   checkboxInput("rmva_reassignCosts_format_output_2", "reassignCosts", FALSE)
                                                                                            ),
                                                                                            
                                                                                            column(3, 
                                                                                                   br(),
                                                                                                   checkboxInput("rmva_newCols_format_output_2", "newCols", FALSE)
                                                                                            )
                                                                                          ),
                                                                                          fluidRow(
                                                                                            column(4, 
                                                                                                   selectInput("rmva_PSP_Closed_format_output_2", "Hydro Storage  (PSP_Closed):", choices = NULL, 
                                                                                                               selected = NULL, multiple = TRUE, width = "100%")
                                                                                            ),
                                                                                            column(4, 
                                                                                                   selectInput("rmva_BATT_format_output_2", "Battery Storage (BATT)  :", choices = NULL, 
                                                                                                               selected = NULL, multiple = TRUE, width = "100%")
                                                                                            ),
                                                                                            column(4, 
                                                                                                   selectInput("rmva_DSR_format_output_2", "Demand Side  (DSR) :", choices = NULL, 
                                                                                                               selected = NULL, multiple = TRUE, width = "100%")
                                                                                            )
                                                                                          ), 
                                                                                          fluidRow(
                                                                                            column(4, 
                                                                                                   selectInput("rmva_EV_format_output_2", "Electric Vehicle (EV) :", choices = NULL, 
                                                                                                               selected = NULL, multiple = TRUE, width = "100%")
                                                                                            ),
                                                                                            
                                                                                            column(4, 
                                                                                                   selectInput("rmva_P2G_format_output_2", "Power-to-gas (P2G) :", choices = NULL, 
                                                                                                               selected = NULL, multiple = TRUE, width = "100%")
                                                                                            ),
                                                                                            column(4, 
                                                                                                   selectInput("rmva_H2_format_output_2", "Hydrogen (H2) :", choices = NULL, 
                                                                                                               selected = NULL, multiple = TRUE, width = "100%")
                                                                                            )
                                                                                          ),
                                                                                          checkboxInput("rmva_ctrl_format_output_step_3", "step 3", FALSE),
                                                                                          conditionalPanel(condition = "input.rmva_ctrl_format_output_step_3",
                                                                                                           fluidRow(
                                                                                                             column(3, 
                                                                                                                    selectInput("rmva_storageFlexibility_format_output_3", "storageFlexibility (PSP) :", choices = NULL, 
                                                                                                                                selected = NULL, multiple = TRUE, width = "100%")
                                                                                                             ),
                                                                                                             column(3, 
                                                                                                                    selectInput("rmva_production_format_output_3", "production :", choices = NULL, selected = NULL, 
                                                                                                                                multiple = TRUE, width = "100%")
                                                                                                             ), 
                                                                                                             
                                                                                                             column(3, 
                                                                                                                    br(),
                                                                                                                    checkboxInput("rmva_reassignCosts_format_output_3", "reassignCosts", FALSE)
                                                                                                             ),
                                                                                                             
                                                                                                             column(3, 
                                                                                                                    br(),
                                                                                                                    checkboxInput("rmva_newCols_format_output_3", "newCols", FALSE)
                                                                                                             )
                                                                                                           ),
                                                                                                           fluidRow(
                                                                                                             column(4, 
                                                                                                                    selectInput("rmva_PSP_Closed_format_output_3", "Hydro Storage  (PSP_Closed):", choices = NULL, 
                                                                                                                                selected = NULL, multiple = TRUE, width = "100%")
                                                                                                             ),
                                                                                                             column(4, 
                                                                                                                    selectInput("rmva_BATT_format_output_3", "Battery Storage (BATT)  :", choices = NULL, 
                                                                                                                                selected = NULL, multiple = TRUE, width = "100%")
                                                                                                             ),
                                                                                                             column(4, 
                                                                                                                    selectInput("rmva_DSR_format_output_3", "Demand Side  (DSR) :", choices = NULL, 
                                                                                                                                selected = NULL, multiple = TRUE, width = "100%")
                                                                                                             )
                                                                                                           ), 
                                                                                                           fluidRow(
                                                                                                             column(4, 
                                                                                                                    selectInput("rmva_EV_format_output_3", "Electric Vehicle (EV) :", choices = NULL, 
                                                                                                                                selected = NULL, multiple = TRUE, width = "100%")
                                                                                                             ),
                                                                                                             
                                                                                                             column(4, 
                                                                                                                    selectInput("rmva_P2G_format_output_3", "Power-to-gas (P2G) :", choices = NULL, 
                                                                                                                                selected = NULL, multiple = TRUE, width = "100%")
                                                                                                             ),
                                                                                                             column(4, 
                                                                                                                    selectInput("rmva_H2_format_output_3", "Hydrogen (H2) :", choices = NULL, 
                                                                                                                                selected = NULL, multiple = TRUE, width = "100%")
                                                                                                             )
                                                                                                           )
                                                                                          )
                                                                         )
                                                                         
                                                                         
                                                        ),
                                                        hr(),
                                                        uiOutput("ui_sel_file_import_format_output"),
                                                        hr(),
                                                        div(actionButton("import_data_format_output", "Validate & import data", icon = icon("upload")), align = "center")
                                                 )
                                               )
                              )
                              
                              
                     ),
                     tabPanel("Parameters",
                              conditionalPanel(condition = "output.have_study_format_output && input.import_data_format_output > 0",
                                               br(),
                                               uiOutput("ui_file_sel_format_output"),
                                               hr(),
                                               fluidRow(
                                                 column(6, 
                                                        div(h4("Annual"), align = "center"),
                                                        
                                                        textInput("scenario_annual", "Scenario : ", "2030 - Scenario 1", width = "100%"),
                                                        
                                                        textInput("status_annual", "Status : ", "Reference", width = "100%"),
                                                        
                                                        selectInput("read_areas_y_format_output", "Areas & Districts :", choices = NULL, 
                                                                    selected = NULL, multiple = TRUE, width = "100%"),
                                                        
                                                        selectInput("read_links_y_format_output", "Links :", choices = NULL, 
                                                                    selected = NULL, multiple = TRUE, width = "100%"),
                                                        conditionalPanel(condition = "input.read_type_mcYears_format_output === 'custom'", 
                                                                         
                                                                         selectInput("read_mcYears_y_format_output", "Choose mcYears :", choices = NULL, 
                                                                                     selected = NULL, multiple = TRUE, width = "100%")
                                                                         
                                                        ),
                                                        div(downloadButton("export_annual_format_output", ""), align = "center"),
                                                        tags$hr(),
                                                        textOutput("show_template_annual"),
                                                        uiOutput("ui_file_sel_template_format_output")
                                                 ),
                                                 column(6, 
                                                        div(h4("Hourly"), align = "center"),
                                                        
                                                        textInput("scenario_hourly", "Scenario : ", "2030 - Scenario 1", width = "100%"),
                                                        
                                                        textInput("status_hourly", "Status : ", "Reference", width = "100%"),
                                                        
                                                        selectInput("read_areas_h_format_output", "Areas & Districts :", choices = NULL, 
                                                                    selected = NULL, multiple = TRUE, width = "100%"),
                                                        
                                                        selectInput("read_links_h_format_output", "Links :", choices = NULL, 
                                                                    selected = NULL, multiple = TRUE, width = "100%"),
                                                        
                                                        selectInput("var_h_format_output", "Variables :", choices = unique(defaut_output_params$dico$ANTARES_naming), 
                                                                    selected = unique(defaut_output_params$dico$ANTARES_naming), multiple = TRUE, width = "100%"),
                                                        conditionalPanel(condition = "input.read_type_mcYears_format_output === 'custom'", 
                                                                         selectInput("read_mcYears_h_format_output", "Choose mcYear :", choices = NULL, 
                                                                                     selected = NULL, multiple = FALSE, width = "100%")
                                                                         
                                                        ),
                                                        div(downloadButton("export_hourly_format_output", ""), align = "center")
                                                 )
                                               )
                                               
                              ), 
                              conditionalPanel(condition = "output.have_study_format_output === false || input.import_data_format_output == 0",
                                               h3(textOutput("no_data_13"))
                              )
                     )
         )
)
