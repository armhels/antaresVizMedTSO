require(data.table)
require(antaresRead)

path <- "C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Full MedTSO V2"

# path <- "C:\\Users\\Datastorm\\Downloads\\Reference%20May%202019"
opts <- setSimulationPath(path)

areas_districts_selections <- c("fr00", "es00", "fr_all", "medtso")
links_selections <- c("fr00 - ie00", "es00- fr00")
# mcYears = 1
mcYears = NULL
removeVirtualAreas = FALSE
storageFlexibility = NULL
production = NULL
reassignCosts = FALSE
newCols = TRUE
rowBal = TRUE
system.time({antares_datas <- importAntaresDatasAnnual(opts = opts, areas_districts_selections = areas_districts_selections,
                                                       links_selections = links_selections, mcYears = mcYears, 
                                                       removeVirtualAreas = removeVirtualAreas, storageFlexibility =storageFlexibility, 
                                                       newCols = newCols)})

sim_name <- unlist(strsplit(opts$simPath, "/"))
sim_name <- sim_name[length(sim_name)]
data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                         "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", mcYears, 
                                                 opts$studyName, sim_name))

vars <- system.file("application/data/excel_templates/variablesAnnualOutputLong.csv", package = "antaresVizMedTSO")
vars <- fread(vars)

data_areas_dist_clust = antares_datas$data_areas_dist_clust
data_areas_dist_clustH = antares_datas$data_areas_dist_clustH
dataForSurplus = antares_datas$dataForSurplus
data_areas_districts = antares_datas$data_areas_districts
links_selections = links_selections
areas_districts_selections = tolower(areas_districts_selections)
vars = vars
opts = antares_datas$opts
data_intro = data_intro

template <- "C:/Users/BenoitThieurmel/Documents/git/antaresVizMedTSO/inst/application/www/Annual_OutputFile_Template_R.xlsx"

# annual_outputs <- formatAnnualOutputs(data_areas_dist_clust = antares_datas$data_areas_dist_clust,
#                                       data_areas_dist_clustH = antares_datas$data_areas_dist_clustH,
#                                       dataForSurplus = antares_datas$dataForSurplus,
#                                       data_areas_districts = antares_datas$data_areas_districts,
#                                       links_selections = antares_datas$links_selections,
#                                       areas_districts_selections = antares_datas$areas_districts_selections,
#                                       vars = vars, opts = antares_datas$opts, data_intro = data_intro)

annual_outputs_v3 <- formatAnnualOutputs(data_areas_dist_clust = antares_datas$data_areas_dist_clust,
                                      data_areas_dist_clustH = antares_datas$data_areas_dist_clustH,
                                      dataForSurplus = antares_datas$dataForSurplus,
                                      data_areas_districts = antares_datas$data_areas_districts,
                                      links_selections = antares_datas$links_selections,
                                      areas_districts_selections = antares_datas$areas_districts_selections,
                                      opts = antares_datas$opts, data_intro = data_intro, template =template)

all.equal(annual_outputs$data_long_out[, 3:5], annual_outputs_v2$data_long_out)
all.equal(annual_outputs$t_welfare_block, annual_outputs_v2$yearly_welfare)

View(annual_outputs$data_long_out[, c(1:2, 15:20)])
removeVirtualAreas = TRUE

saveRDS(l, "l.RDS")
# import data

opts = opts
areas_districts_selections = NULL
links_selections = c("ba00 - me00")
mcYears = 1
removeVirtualAreas = FALSE
system.time({antares_datas <- importAntaresDatasAnnual(opts = opts, areas_districts_selections = NULL,
                                                       links_selections = c("ba00 - me00"), mcYears = 1, 
                                                       removeVirtualAreas = FALSE, storageFlexibility =storageFlexibility)})


annual_outputs <- formatAnnualOutputs(data_areas_dist_clust = antares_datas$data_areas_dist_clust,
                                      data_areas_dist_clustH = antares_datas$data_areas_dist_clustH,
                                      dataForSurplus = antares_datas$dataForSurplus,
                                      data_areas_districts = antares_datas$data_areas_districts,
                                      links_selections = c("ba00 - me00"),
                                      areas_districts_selections = NULL,
                                      vars = vars, opts = antares_datas$opts, data_intro = data_intro)

sim_name <- unlist(strsplit(opts$simPath, "/"))
sim_name <- sim_name[length(sim_name)]
data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                         "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", mcYears, 
                                                 opts$studyName, sim_name))


vars <- system.file("application/data/excel_templates/variablesAnnualOutputLong.csv", package = "antaresVizMedTSO")
vars <- fread(vars)

data_areas_dist_clust = antares_datas$data_areas_dist_clust
data_areas_dist_clustH = antares_datas$data_areas_dist_clustH
dataForSurplus = antares_datas$dataForSurplus
data_areas_districts = antares_datas$data_areas_districts
links_selections = NULL
areas_districts_selections = tolower(areas_districts_selections)
vars = vars
opts = antares_datas$opts
data_intro = data_intro

annual_outputs <- formatAnnualOutputs(data_areas_dist_clust = antares_datas$data_areas_dist_clust,
                                      data_areas_dist_clustH = antares_datas$data_areas_dist_clustH,
                                      dataForSurplus = antares_datas$dataForSurplus,
                                      data_areas_districts = antares_datas$data_areas_districts,
                                      links_selections = NULL,
                                      areas_districts_selections = tolower(areas_districts_selections),
                                      vars = vars, opts = antares_datas$opts, data_intro = data_intro)

system.time({antares_datas_rm <- importAntaresDatasAnnual(opts = opts, areas_districts_selections = areas_districts_selections,
                                                          links_selections = links_selections, mcYears = mcYears, 
                                                          removeVirtualAreas = TRUE,
                                                          storageFlexibility = c("0_PUMP_Daily", "0_TURB_Daily", "1_PUMP_Weekly", "1_TURB_Weekly", "1_PUMP_MAROC", "1_TURB_MAROC"))})

antares_datas$dataForSurplus
antares_datas_rm$dataForSurplus



require(openxlsx)
options(scipen = 0)

options(scipen = 10000, digits = 1)

data_areas_dist_clust = antares_datas$data_areas_dist_clust
data_areas_dist_clustH = antares_datas$data_areas_dist_clustH
dataForSurplus = antares_datas$dataForSurplus
data_areas_districts = antares_datas$data_areas_districts
links_selections = tolower(links_selections)
areas_districts_selections = tolower(areas_districts_selections)
vars = vars
opts = antares_datas$opts
data_intro = data_intro

l <- readRDS("inst/application/l2.RDS")
l

data_areas_dist_clust = l$data_areas_dist_clust
data_areas_dist_clustH = l$data_areas_dist_clustH
dataForSurplus = l$dataForSurplus
data_areas_districts = l$data_areas_districts
links_selections = l$links_selections
areas_districts_selections = l$areas_districts_selections
vars = l$vars
opts = l$opts
data_intro = l$data_intro

annual_outputs <- formatAnnualOutputs(l$data_areas_dist_clust,
                                      l$data_areas_dist_clustH,
                                      l$dataForSurplus,
                                      l$data_areas_districts,
                                      l$areas_districts_selections,
                                      l$links_selections,
                                      l$vars, l$opts, l$data_intro)

annual_outputs <- formatAnnualOutputs(data_areas_dist_clust = antares_datas$data_areas_dist_clust,
                                      data_areas_dist_clustH = antares_datas$data_areas_dist_clustH,
                                      dataForSurplus = antares_datas$dataForSurplus,
                                      data_areas_districts = antares_datas$data_areas_districts,
                                      links_selections = tolower(links_selections),
                                      areas_districts_selections = tolower(areas_districts_selections),
                                      vars = vars, opts = antares_datas$opts, data_intro = data_intro)

colnames(annual_outputs$t_all)
colnames(annual_outputs$t_welfare_block)


data_areas_dist_clust = antares_datas_rm$data_areas_dist_clust
data_areas_dist_clustH = antares_datas_rm$data_areas_dist_clustH
dataForSurplus = antares_datas_rm$dataForSurplus
data_areas_districts = antares_datas_rm$data_areas_districts
links_selections = tolower(links_selections)
areas_districts_selections = tolower(areas_districts_selections)
vars = vars
opts = antares_datas_rm$opts
data_intro = data_intro

annual_outputs_rm <- formatAnnualOutputs(data_areas_dist_clust = antares_datas_rm$data_areas_dist_clust,
                                         data_areas_dist_clustH = antares_datas_rm$data_areas_dist_clustH,
                                         dataForSurplus = antares_datas_rm$dataForSurplus,
                                         data_areas_districts = antares_datas_rm$data_areas_districts,
                                         links_selections = tolower(links_selections),
                                         areas_districts_selections = tolower(areas_districts_selections),
                                         vars = vars, opts = antares_datas_rm$opts, data_intro = data_intro)

colnames(annual_outputs_rm$t_all)
colnames(annual_outputs_rm$t_welfare_block)

infile_name <- system.file("application/www/Annual_OutputFile_Template_R.xlsx", package = "antaresVizMedTSO")
options(scipen = 10000, digits = 1)


exportAnnualOutputs(infile_name = infile_name, outfile_name = "Annual_OutputFile_Template_not_rm.xlsx",
                    annual_outputs = annual_outputs, links_selections = links_selections, 
                    areas_districts_selections = areas_districts_selections, 
                    data_intro = data_intro)

exportAnnualOutputs(infile_name = infile_name, outfile_name = "Annual_OutputFile_Template_rm.xlsx",
                    annual_outputs = annual_outputs_rm, links_selections = links_selections, 
                    areas_districts_selections = areas_districts_selections, 
                    data_intro = data_intro)

# Hourly ----

source("inst/application/src/scripts/functions.R")

params_h <- readTemplateFile("C:\\Users\\Datastorm\\Desktop\\Med-TSO\\Antares_FULL_V2\\Exportation_Selection_Annual_Hourly_Full perimeter_All.xlsx")
params_h$mcYears <- 1

params <- antaresVizMedTSO::readStudyShinySelection("C:\\Users\\Datastorm\\Desktop\\Med-TSO\\Antares_FULL_V2\\readAntares_selection_Full perimeter_v2.xlsx")
params$mcYears <- 1
mcYears = NULL


# param d'entree des imports
mcYears = 1

dico <- system.file("application/data/excel_templates/dictionnary.csv", package = "antaresVizMedTSO")
dico <- fread(dico)
setkey(dico, "ANTARES_naming")

opts = opts
areas_districts_selections = params_h$areas_districts_hourly
links_selections = params_h$links_hourly
mcYears = 1
market_data_code = params_h$variables_hourly
removeVirtualAreas = params$removeVirtualAreas
storageFlexibility =params$storageFlexibility 
newCols = params$newCols
production = params$production
reassignCosts = params$reassignCosts

system.time(antares_datas <- importAntaresDatasHourly(opts, areas_districts_selections = areas_districts_selections,
                                                      links_selections = links_selections, 
                                                      removeVirtualAreas = removeVirtualAreas, 
                                                      storageFlexibility = storageFlexibility, 
                                                      mcYears = mcYears))

saveRDS(antares_datas, file = "antares_datas_h.RDS", compress = FALSE)

data_h = copy(antares_datas$data)
areas_selections = antares_datas$areas_districts_selections
market_data_code = market_data_code
links_selections = antares_datas$links_selections
dico = dico

hourly_outputs <- formatHourlyOutputs(
  data_h = antares_datas$data,
  areas_selections = antares_datas$areas_districts_selections,
  market_data_code = market_data_code,
  links_selections = antares_datas$links_selections,
  dico = dico)



system.time(antares_datas_rm <- importAntaresDatasHourly(opts, areas_districts_selections = areas_districts_selections,
                                                         links_selections = links_selections, removeVirtualAreas = TRUE,
                                                         storageFlexibility = c("0_PUMP_Daily", "0_TURB_Daily", "1_PUMP_Weekly", "1_TURB_Weekly", "1_PUMP_MAROC", "1_TURB_MAROC")))

areas_districts_selections <- areas_selections
links_selections

system.time(antares_datas <- importAntaresDatasHourly(opts, areas_districts_selections = areas_districts_selections,
                                                      links_selections = links_selections))

data_areas = antares_datas$data_areasH
data_links = antares_datas$data_linksH
areas_selections = aareas_districts_selections
market_data_code = mmarket_data_code
links_selections = llinks_selections
dico = dico

data_h = data_antares$data
areas_selections = data_antares$areas_districts_selections
market_data_code = mmarket_data_code
links_selections = data_antares$links_selections
dico = dico

hourly_outputs <- formatHourlyOutputs(
  data_h = data_antares$data,
  areas_selections = data_antares$areas_districts_selections,
  market_data_code = mmarket_data_code,
  links_selections = data_antares$links_selections,
  dico = dico)

hourly_outputs <- formatHourlyOutputs(
  data_h = antares_datas,
  areas_selections = areas_districts_selections,
  market_data_code = market_data_code,
  links_selections = links_selections,
  dico = dico)

#============================== HOURLY MARKET DATA ========================================

sim_name <- unlist(strsplit(opts$simPath, "/"))
sim_name <- sim_name[length(sim_name)]
data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                         "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", mcYears, 
                                                 opts$studyName, sim_name))

infile_name <- system.file("application/data/excel_templates/hourly_OutputFile_Template__R.xlsx", package = "antaresVizMedTSO")
outfile_name <- "Hourly_OutputFile_TEST.xlsx"

infile_name, outfile_name, hourly_outputs, data_intro, market_data_code

exportHourlyOutputs(hourly_outputs = hourly_outputs, infile_name = infile_name, outfile_name = "Hourly_OutputFile_TEST_not_rm.xlsx",
                    data_intro = data_intro, market_data_code = market_data_code)