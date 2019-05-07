tabPanel("Export",
         conditionalPanel(condition = "output.have_data === true",
                          div(h3(textOutput("title_import_data_2")), align = "center"),
                          h3(textOutput("title_studies_2")),
                          uiOutput("info_list_data_export"),
                          hr(), 
                                   fluidRow(
                                     column(12,
                                            uiOutput("panels_tab"),
                                            conditionalPanel(condition = "output.have_data",
                                                             tags$hr(),
                                                             h1(" "),
                                                             uiOutput("export_btn")
                                            )
                                     )
                                   )
         ),
         conditionalPanel(condition = "output.have_data === false",
                          h3(textOutput("no_data_8"), style = "color : red")
         )
)