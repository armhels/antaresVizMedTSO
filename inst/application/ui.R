# Define UI for antaresViz app
navbarPage(title = HTML('<p style="margin-top: 0.05cm;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbspAntaresVizMedTSO</p>'), id = "nav-id", collapsible = TRUE, 
           position = "fixed-top", theme = "css/custom.css",
           header = div(
             br(), br(), br(), br(),
             a(href = "https://www.med-tso.com/",
               target = "_blank", img(src = "img/Med-TSO logo_0.JPG", class = "ribbon", style = "margin-left: 0.1cm;margin-top: 0.25cm;height: 48px")),
             # a(href = "https://github.com/rte-antares-rpackage/antaresVizMedTSO",
               # target = "_blank", img(src = "img/github.png", class = "ribbon", style = "margin-left: 1.5cm;margin-top: 0.3cm;height: 46px")),
             singleton(tags$script(src = 'events.js')), 
             singleton(tags$script(src = 'is.min.js')),
             # footer
             div(class = "rte_footer", div(actionLink("quit", "Quit application", icon = icon("close"), style = "color:white"), align = "center")),
             # flag : https://www.countries-ofthe-world.com/flags-of-the-world.html
             img(src = "img/flag-of-France.png", 
                 title = "Attention : en cas de changement de langue, les modules de visualisation seront ré-initialisés", 
                 style = "position: fixed;
                        cursor:pointer;
                        right: 0;
                        top: 0;
                        z-index: 100001;
                        margin-right: 1cm;
                        margin-top: 0.6cm;
                        display: block;
                        height: 21px;
                        text-decoration: none;
                        overflow-x: hidden;", onclick="updateShinyLanguage('fr')"),
             img(src = "img/flag-of-United-Kingdom.png", 
                 title = "Warning : To change language can reset the modules of visualization", 
                 style = "position: fixed;
                        cursor:pointer;
                        right: 0;
                        top: 0;
                        z-index: 100001;
                        margin-right: 2.2cm;
                        margin-top: 0.6cm;
                        display: block;
                        height: 21px;
                        text-decoration: none;
                        overflow-x: hidden;", onclick="updateShinyLanguage('en')"),
             tags$script(type="text/javascript", 'if(is.ie()){ alert("This site is not fully compatible with Internet Explorer");};'),
             
             div(id = "import_busy", tags$img(src= "spinner.gif", height = 100, 
                                              style = "position: fixed;top: 50%;z-index:10;left: 48%;"))
             # selectInput("language", "langue:",
             #             c("Français" = "fr",
             #               "English" = "en"))
           ), windowTitle = "antaresVizMedTSO",
           
           tabPanel(textOutput("label_tab_data"),
                    fluidRow(
                      column(12,
                             tabsetPanel(id = "tab_data",
                                         source("src/ui/01_ui_import_data.R", local = T)$value,
                                         source("src/ui/04_ui_analysis.R", local = T)$value,
                                         source("src/ui/03_ui_export_data.R", local = T)$value
                             )
                      )
                    )
           ),
           
           useShinydashboard(),
           
           source("src/ui/05_ui_prodstack.R", local = T)$value,
           
           source("src/ui/06_ui_exchange.R", local = T)$value,
           
           source("src/ui/07_ui_tsplot.R", local = T)$value,
           
           source("src/ui/08_ui_map.R", local = T)$value,
           
           source("src/ui/09_ui_params.R", local = T)$value,
           
           source("src/ui/10_ui_help.R", local = T)$value,
           footer = div(br(), br())
)



