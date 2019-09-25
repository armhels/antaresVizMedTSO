suppressWarnings({
  suppressMessages({
    suppressPackageStartupMessages({
      require(shiny)
      require(antaresRead)
      require(antaresVizMedTSO)
      require(manipulateWidget)
      require(data.table)
      require(shinydashboard)
      require(shinyWidgets)
      require(DT)
      
      require(colourpicker)
      require(ggplot2)
      require(ggrepel)
      require(ggforce)
      
      require(sp)
      
    })
  })
})

if(file.exists("default_conf.yml")){
  conf <- try(yaml::read_yaml("default_conf.yml"), silent = TRUE)
  study_dir <- conf$study_dir
  map_layout <- conf$map_layout
  if(is.null(study_dir)) study_dir <- ""
  if(is.null(map_layout)) map_layout <- ""
} else {
  map_layout <- ""
  study_dir <- ""
}

defaut_map_layout <- NULL
if(!is.null(map_layout) && file.exists(map_layout)){
  tmp_ml <- try(readRDS(map_layout), silent = TRUE)
  if("mapLayout" %in% class(tmp_ml)){
    defaut_map_layout <- tmp_ml
  }
}

# choose a directory
source("src/scripts/directoryInput.R", encoding = "UTF-8")
source("src/scripts/subsetDataTable.R", encoding = "UTF-8")
source("src/scripts/utils_medtso_maps.R", encoding = "UTF-8")
source("src/scripts/utils_format_output.R", encoding = "UTF-8")

# shared inputs
.global_shared_prodStack <- data.frame(
  module = "prodStack", 
  panel = "prodStack", 
  input = c("dateRange", "unit", "mcYear", "mcYearh", "timeSteph5", "legend", "drawPoints", "stepPlot"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput", "selectInput", 
           "checkboxInput", "checkboxInput", "checkboxInput"), stringsAsFactors = FALSE)

.global_shared_plotts <- data.frame(
  module = "plotts", 
  panel = "tsPlot", 
  input = c("dateRange", "mcYear", "mcYearh", "timeSteph5", "legend", "drawPoints", "stepPlot"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput", 
           "checkboxInput", "checkboxInput", "checkboxInput"), stringsAsFactors = FALSE)


.global_shared_plotMap <- data.frame(
  module = "plotMap", 
  panel = "Map", 
  input = c("dateRange", "mcYear", "mcYearh", "timeSteph5"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput"), stringsAsFactors = FALSE)

.global_shared_exchangesStack <- data.frame(
  module = "exchangesStack", 
  panel = "exchangesStack", 
  input = c("dateRange", "unit", "mcYear", "mcYearh", "timeSteph5", "legend", "drawPoints", "stepPlot"),
  type = c("dateRangeInput", "selectInput", "selectInput", "selectInput", "selectInput", 
           "checkboxInput", "checkboxInput", "checkboxInput"), stringsAsFactors = FALSE)

.global_shared_input <- rbind(.global_shared_prodStack, .global_shared_plotts, .global_shared_plotMap, .global_shared_exchangesStack)


.global_build_input_data <- function(data){
  data$input_id <- paste0(data$module, "-shared_", data$input)
  data$last_update <- NA
  data$update_call <- ""
  class(data$last_update) <- c("character")
  data <- data.table(data)
  data
}

#------------
# compare
#-----------

.global_compare_prodstack <- c("mcYear", "main", "unit", "areas", "legend", 
                               "stack", "stepPlot", "drawPoints")

.global_compare_exchangesStack <- c("mcYear", "main", "unit", "area",
                                    "legend", "stepPlot", "drawPoints")

.global_compare_tsPlot <- c("mcYear", "main", "variable", "type", "confInt", "elements", 
                            "aggregate", "legend", "highlight", "stepPlot", "drawPoints", "secondAxis")

.global_compare_plotMap <- c("mcYear", "type", "colAreaVar", "sizeAreaVars", "areaChartType", "showLabels",
                             "popupAreaVars", "labelAreaVar","colLinkVar", "sizeLinkVar", "popupLinkVars")


#----- generate help for antaresRead function
# library(tools)
# add.html.help <- function(package, func, tempsave = paste0(getwd(), "/temp.html")) {
#   pkgRdDB = tools:::fetchRdDB(file.path(find.package(package), "help", package))
#   topics = names(pkgRdDB)
#   rdfunc <- pkgRdDB[[func]]
#   tools::Rd2HTML(pkgRdDB[[func]], out = tempsave)
# }
# add.html.help("antaresRead", "readAntares", "inst/application/www/readAntares.html")
# add.html.help("antaresRead", "removeVirtualAreas", "inst/application/www/removeVirtualAreas.html")
# add.html.help("antaresRead", "writeAntaresH5", "inst/application/www/writeAntaresH5.html")


#-------- MED-Tso maps

# referentiels des positions
ref_medtsomap_data <- readMEDTsoMapInput("data/MedTSO_map_template.xlsx")

sp_object <- readRDS("data/final_sp_map.RDS")
sp_object@data <- as.data.table(sp_object@data)

#------- format output

defaut_template <- system.file("application/data/excel_templates/Output_Selection_template.xlsx", package = "antaresVizMedTSO")
defaut_output_params <- readTemplateFile(defaut_template)

# new alias on map
setAlias("Total generation", "Total generation", c("areas", "NUCLEAR", 
                                                   "COAL", 
                                                   "LIGNITE", 
                                                   "GAS", 
                                                   "OIL", 
                                                   "Others_non-renewable", 
                                                   "Hydro", 
                                                   "Battery_discharge_&_turbine", 
                                                   "WIND", 
                                                   "SOLAR", 
                                                   "MISC. NDG"))