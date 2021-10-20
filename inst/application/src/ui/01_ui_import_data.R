tabPanel(textOutput("label_tab_import_data"),
         h3(textOutput("title_import_data")),
         fluidRow(
           # column(5, 
           #        directoryInput('directory', label = '', value = study_dir)
           # ),
           column(1, 
                  div(
                    shinyDirButton(id = "directory", label = "", title = "", icon = icon("folder")), 
                    style = "margin-top:17px", align = "center"
                  )
           ), 
           column(4, 
                  div(
                    verbatimTextOutput("print_directory"), 
                    style = "margin-top:15px", align = "left"
                  )
           ), 
           conditionalPanel(condition = "output.ctrl_is_antares_study | output.ctrl_is_antares_h5", 
                            column(1, 
                                   div(br(), h4("Simulation : "), align = "center")
                            ), 
                            column(4, 
                                   selectInput("study_path", "", choices = NULL, selected = NULL, width = "100%")
                            ), 
                            column(2, 
                                   div(br(), 
                                       actionButton("init_sim", "Set simulation", icon = icon("check-circle")),
                                       align = "center"
                                   )
                            )
           ),
           conditionalPanel(condition = "output.ctrl_is_antares_study === false & output.ctrl_is_antares_h5 === false", 
                            column(5, 
                                   h3(textOutput("directory_message"), style = "color : red")
                            )
           )
         ), 
         
         conditionalPanel(condition = "output.have_study",
                          source("./src/ui/02_ui_infoBox.R")$value, 
                          
                          fluidRow(
                            column(12,
                                   hr(), 
                                   div(h3(textOutput("current_opts"), align = "center")),
                                   
                                   h3(textOutput("title_readAntares")),
                                   fluidRow(
                                     column(6, 
                                            selectInput("read_areas", "Areas :", choices = NULL, 
                                                        selected = NULL, multiple = TRUE, width = "100%")
                                     ), 
                                     column(6, 
                                            selectInput("read_links", "Links :", choices = NULL, 
                                                        selected = NULL, multiple = TRUE, width = "100%")
                                     )
                                   ), 
                                   fluidRow(
                                     column(4, 
                                            selectInput("read_clusters", "Clusters : ", choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
                                     ), 
                                     column(4, 
                                            selectInput("read_clusters_res", "ClustersRes : ", choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
                                     ),
                                     column(4, 
                                            selectInput("read_districts", "Districts :", choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
                                     )
                                   ), 
                                   uiOutput("ui_sel_file"),
                                   conditionalPanel(condition = "output.current_opts_h5 === false",
                                                    fluidRow(
                                                      column(3, 
                                                             checkboxInput("read_misc", "misc", FALSE),
                                                             checkboxInput("read_reserve", "reserve", FALSE)
                                                      ),
                                                      column(3, 
                                                             checkboxInput("read_thermalAvailabilities", "thermalAvailabilities", FALSE),
                                                             checkboxInput("read_linkCapacity", "linkCapacity", FALSE)
                                                      ),
                                                      column(3, 
                                                             checkboxInput("read_hydroStorage", "hydroStorage", FALSE),
                                                             checkboxInput("read_mustRun", "mustRun", FALSE)
                                                      ),
                                                      column(3, 
                                                             checkboxInput("read_hydroStorageMaxPower", "hydroStorageMaxPower", FALSE),
                                                             checkboxInput("read_thermalModulation", "thermalModulation", FALSE)
                                                      )
                                                    ),
                                                    fluidRow(
                                                      column(3, 
                                                             selectInput("read_timeStep", "timeStep :", choices = c("hourly", "daily", "weekly",
                                                                                                                    "monthly", "annual"))
                                                      ),
                                                      column(3, 
                                                             radioButtons("read_type_mcYears", "mcYears :",
                                                                          c("synthetic", "all", "custom"), inline = TRUE)
                                                      ), 
                                                      conditionalPanel(condition = "input.read_type_mcYears === 'custom'", 
                                                                       column(3, 
                                                                              selectInput("read_mcYears", "Choose mcYears :", choices = NULL, selected = NULL, multiple = TRUE)
                                                                       )
                                                      )
                                                      # ,column(3, 
                                                      #        checkboxInput("read_parallel", "parallel", FALSE)
                                                      # )
                                                    )
                                   ), 
                                   fluidRow(
                                     column(12, 
                                            selectInput("read_select", "Select :", choices = NULL, selected = NULL, 
                                                        width = "100%", multiple = TRUE)
                                     )
                                   ),
                                   conditionalPanel(condition = "output.current_opts_h5 === false",
                                                    fluidRow(
                                                      column(3, 
                                                             h4(textOutput("title_removeVirtualAreas"))
                                                      ),
                                                      column(3, 
                                                             checkboxInput("rmva_ctrl", "enabled", FALSE)
                                                      )
                                                    ),
                                                    conditionalPanel(condition = "input.rmva_ctrl",
                                                                     fluidRow(
                                                                       column(3, 
                                                                              selectInput("rmva_storageFlexibility", "storageFlexibility (PSP) :", choices = NULL, 
                                                                                          selected = NULL, multiple = TRUE, width = "100%")
                                                                       ),
                                                                       column(3, 
                                                                              selectInput("rmva_production", "production :", choices = NULL, selected = NULL, 
                                                                                          multiple = TRUE, width = "100%")
                                                                       ), 
                                                                       
                                                                       column(3, 
                                                                              br(),
                                                                              checkboxInput("rmva_reassignCosts", "reassignCosts", FALSE)
                                                                       ),
                                                                       
                                                                       column(3, 
                                                                              br(),
                                                                              checkboxInput("rmva_newCols", "newCols", FALSE)
                                                                       )
                                                                     ),
                                                                     fluidRow(
                                                                       column(4, 
                                                                              selectInput("rmva_PSP_Closed", "Hydro Storage  (PSP_Closed):", choices = NULL, 
                                                                                          selected = NULL, multiple = TRUE, width = "100%")
                                                                       ),
                                                                       column(4, 
                                                                              selectInput("rmva_BATT", "Battery Storage (BATT)  :", choices = NULL, 
                                                                                          selected = NULL, multiple = TRUE, width = "100%")
                                                                       ),
                                                                       column(4, 
                                                                              selectInput("rmva_DSR", "Demand Side  (DSR) :", choices = NULL, 
                                                                                          selected = NULL, multiple = TRUE, width = "100%")
                                                                       )
                                                                     ), 
                                                                     fluidRow(
                                                                       column(4, 
                                                                              selectInput("rmva_EV", "Electric Vehicle (EV) :", choices = NULL, 
                                                                                          selected = NULL, multiple = TRUE, width = "100%")
                                                                       ),
                                                                       
                                                                       column(4, 
                                                                              selectInput("rmva_P2G", "Power-to-gas (P2G) :", choices = NULL, 
                                                                                          selected = NULL, multiple = TRUE, width = "100%")
                                                                       ),
                                                                       column(4, 
                                                                              selectInput("rmva_H2", "Hydrogen (H2) :", choices = NULL, 
                                                                                          selected = NULL, multiple = TRUE, width = "100%")
                                                                       )
                                                                     ),
                                                                     checkboxInput("rmva_ctrl_step_2", "step 2", FALSE),
                                                                     
                                                                     conditionalPanel(condition = "input.rmva_ctrl_step_2",
                                                                                      fluidRow(
                                                                                        column(3, 
                                                                                               selectInput("rmva_storageFlexibility_2", "storageFlexibility (PSP) :", choices = NULL, 
                                                                                                           selected = NULL, multiple = TRUE, width = "100%")
                                                                                        ),
                                                                                        column(3, 
                                                                                               selectInput("rmva_production_2", "production :", choices = NULL, selected = NULL, 
                                                                                                           multiple = TRUE, width = "100%")
                                                                                        ), 
                                                                                        
                                                                                        column(3, 
                                                                                               br(),
                                                                                               checkboxInput("rmva_reassignCosts_2", "reassignCosts", FALSE)
                                                                                        ),
                                                                                        
                                                                                        column(3, 
                                                                                               br(),
                                                                                               checkboxInput("rmva_newCols_2", "newCols", FALSE)
                                                                                        )
                                                                                      ),
                                                                                      fluidRow(
                                                                                        column(4, 
                                                                                               selectInput("rmva_PSP_Closed_2", "Hydro Storage  (PSP_Closed):", choices = NULL, 
                                                                                                           selected = NULL, multiple = TRUE, width = "100%")
                                                                                        ),
                                                                                        column(4, 
                                                                                               selectInput("rmva_BATT_2", "Battery Storage (BATT)  :", choices = NULL, 
                                                                                                           selected = NULL, multiple = TRUE, width = "100%")
                                                                                        ),
                                                                                        column(4, 
                                                                                               selectInput("rmva_DSR_2", "Demand Side  (DSR) :", choices = NULL, 
                                                                                                           selected = NULL, multiple = TRUE, width = "100%")
                                                                                        )
                                                                                      ), 
                                                                                      fluidRow(
                                                                                        column(4, 
                                                                                               selectInput("rmva_EV_2", "Electric Vehicle (EV) :", choices = NULL, 
                                                                                                           selected = NULL, multiple = TRUE, width = "100%")
                                                                                        ),
                                                                                        
                                                                                        column(4, 
                                                                                               selectInput("rmva_P2G_2", "Power-to-gas (P2G) :", choices = NULL, 
                                                                                                           selected = NULL, multiple = TRUE, width = "100%")
                                                                                        ),
                                                                                        column(4, 
                                                                                               selectInput("rmva_H2_2", "Hydrogen (H2) :", choices = NULL, 
                                                                                                           selected = NULL, multiple = TRUE, width = "100%")
                                                                                        )
                                                                                      ),
                                                                                      checkboxInput("rmva_ctrl_step_3", "step 3", FALSE),
                                                                                      conditionalPanel(condition = "input.rmva_ctrl_step_3",
                                                                                                       fluidRow(
                                                                                                         column(3, 
                                                                                                                selectInput("rmva_storageFlexibility_3", "storageFlexibility (PSP) :", choices = NULL, 
                                                                                                                            selected = NULL, multiple = TRUE, width = "100%")
                                                                                                         ),
                                                                                                         column(3, 
                                                                                                                selectInput("rmva_production_3", "production :", choices = NULL, selected = NULL, 
                                                                                                                            multiple = TRUE, width = "100%")
                                                                                                         ), 
                                                                                                         
                                                                                                         column(3, 
                                                                                                                br(),
                                                                                                                checkboxInput("rmva_reassignCosts_3", "reassignCosts", FALSE)
                                                                                                         ),
                                                                                                         
                                                                                                         column(3, 
                                                                                                                br(),
                                                                                                                checkboxInput("rmva_newCols_3", "newCols", FALSE)
                                                                                                         )
                                                                                                       ),
                                                                                                       fluidRow(
                                                                                                         column(4, 
                                                                                                                selectInput("rmva_PSP_Closed_3", "Hydro Storage  (PSP_Closed):", choices = NULL, 
                                                                                                                            selected = NULL, multiple = TRUE, width = "100%")
                                                                                                         ),
                                                                                                         column(4, 
                                                                                                                selectInput("rmva_BATT_3", "Battery Storage (BATT)  :", choices = NULL, 
                                                                                                                            selected = NULL, multiple = TRUE, width = "100%")
                                                                                                         ),
                                                                                                         column(4, 
                                                                                                                selectInput("rmva_DSR_3", "Demand Side  (DSR) :", choices = NULL, 
                                                                                                                            selected = NULL, multiple = TRUE, width = "100%")
                                                                                                         )
                                                                                                       ), 
                                                                                                       fluidRow(
                                                                                                         column(4, 
                                                                                                                selectInput("rmva_EV_3", "Electric Vehicle (EV) :", choices = NULL, 
                                                                                                                            selected = NULL, multiple = TRUE, width = "100%")
                                                                                                         ),
                                                                                                         
                                                                                                         column(4, 
                                                                                                                selectInput("rmva_P2G_3", "Power-to-gas (P2G) :", choices = NULL, 
                                                                                                                            selected = NULL, multiple = TRUE, width = "100%")
                                                                                                         ),
                                                                                                         column(4, 
                                                                                                                selectInput("rmva_H2_3", "Hydrogen (H2) :", choices = NULL, 
                                                                                                                            selected = NULL, multiple = TRUE, width = "100%")
                                                                                                         )
                                                                                                       )
                                                                                      )
                                                                     )
                                                                     
                                                                     
                                                    )
                                   ),
                                   div(actionButton("import_data", "Validate & import data", icon = icon("upload")), align = "center")
                                   
                            )
                            
                          )
         )
)