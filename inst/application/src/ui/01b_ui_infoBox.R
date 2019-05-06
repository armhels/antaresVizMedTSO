conditionalPanel(condition = "input.init_sim > 0",
                 fluidRow(
                   infoBoxOutput("mode", 3),
                   infoBoxOutput("synth", 3),
                   infoBoxOutput("yby", 3),
                   infoBoxOutput("MCyears_nb", 3)
                   
                 ),
                 fluidRow(
                   infoBoxOutput("areas_nb", 3),
                   infoBoxOutput("links_nb", 3),
                   infoBoxOutput("districts_nb", 3),
                   infoBoxOutput("areas_clusters", 3)
                   
                 )
)