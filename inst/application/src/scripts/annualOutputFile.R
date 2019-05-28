


areas_districts_selections = c("IT_ALL", "FR_ALL",
                               "gr_all", "medi_all","medi_ne","medi_nw","medi_se","medi_sw","medtso","ue_nomedtso")


data_areas_dist_clust <- readAntares(areas = "all", 
                                     districts = "all",
                                     clusters = "all", links = "all",
                                     timeStep = "annual", select = NULL, mcYears = mcYears)

areas_districts_selections = unique(c(as.character(data_areas_dist_clust$areas$area), as.character(data_areas_dist_clust$districts$district)))
# areas_districts_selections <- NULL
# links_selections = c("AL00 - GR00", "AL00 - ME00", "AL00 - MK00", "AL00 - RS00", "AT00 - CZ00")
# links_selections = NULL
links_selections = unique(as.character(data_areas_dist_clust$links$link))
unique(antares_datas$dataForSurplus$districts$district)

mcYears = 1


source("inst/application/src/scripts/functions.R")

antares_datas <- importAntaresDatas(areas_districts_selections = areas_districts_selections,
                                    links_selections = links_selections)

vars <- fread("~/MED-Tso/variablesAnnualOutputLong.csv")


require(openxlsx)

options(scipen = 0)
annual_outputs <- format_annualOutputs(data_areas_dist_clust = antares_datas$data_areas_dist_clust,
                                       data_areas_dist_clustH = antares_datas$data_areas_dist_clustH,
                                       dataForSurplus = antares_datas$dataForSurplus,
                                       data_areas_districts = antares_datas$data_areas_districts,
                                       links_selections = tolower(links_selections),
                                       areas_districts_selections = tolower(areas_districts_selections),
                                       vars = vars, opts = antares_datas$opts, data_intro = data_intro)


setwd("~/MED-Tso")
infile_name <- "inst/application/data/excel_templates/Annual_OutputFile_Template__R.xlsx"
outfile_name <- "C:/Users/lyesb/Documents/MED-Tso/Annual_OutputFile_TEST.xlsx"


options(scipen = 0)

export_annual_outputs(infile_name = infile_name, outfile_name = outfile_name,
                      annual_outputs = annual_outputs, links_selections = links_selections, 
                      areas_districts_selections = areas_districts_selections, 
                      data_intro = data_intro)


