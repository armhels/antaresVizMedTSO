source("inst/application/src/scripts/functions.R")


# param d'entree des imports

mcYears = 1
dico <- fread("inst/application/data/excel_templates/dictionnary.csv")
setkey(dico, "ANTARES_naming")

# parametres :

# market_data_code <- c("ROW BAL.", "PSP", "NODU", "gas_ccgt old 1", "gas_conventional old 1", 
#                       "gas_conventional old 2", "gas_ocgt new", "VAR_TEST")
# links_selections = c("AL00 - GR00", "AL00 - ME00", "AL00 - MK00", "AL00 - RS00", "AT00 - CZ00")
# links_selections = NULL

areas_districts_selections = unique(c(as.character(data_areas_dist_clust$areas$area), as.character(data_areas_dist_clust$districts$district)))
links_selections = unique(as.character(data_areas_dist_clust$links$link))

antares_datas <- importAntaresDatas_hourly(areas_districts_selections = areas_districts_selections,
                                           links_selections = links_selections)


hourly_outputs <- format_hourlyOutputs(
  data_areas = antares_datas$data_areasH, 
  data_links = antares_datas$data_linksH, 
  areas_selections = areas_districts_selections,
  market_data_code = market_data_code,
  links_selections = links_selections,
  dico = dico)

#============================== HOURLY MARKET DATA ========================================


data_intro <- data.table("Scenario" = c("Simulator", "Date", "Status", "MC-Year Selection", "Study", "Simulation"), 
                         "2030 - Scenario 1" = c("ANTARES", as.character(Sys.Date()), "Reference", 1, NA, NA))


infile_name <- "inst/application/data/excel_templates/hourly_OutputFile_Template__R.xlsx"
outfile_name <-  "C:/Users/lyesb/Documents/MED-Tso/Hourly_OutputFile_TEST.xlsx"

export_hourlyOutputs(hourly_outputs = hourly_outputs, infile_name = infile_name, outfile_name = outfile_name,
                     data_intro = data_intro, 
                     areas_districts_selections = areas_districts_selections,
                     market_data_code = market_data_code, links_selections = links_selections)





