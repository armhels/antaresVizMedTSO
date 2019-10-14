require(RInno)

Sys.setenv("TAR" = "internal")

cran_pkgs <- c("dygraphs","shiny","plotly","htmltools","htmlwidgets","manipulateWidget",
               "leaflet","sp","rgeos","raster","webshot","data.table","methods","lubridate",
               "geojsonio","graphics","stats","leaflet.minicharts", "bit64", "plyr",
               "assertthat","rAmCharts","utils","openxlsx","shinydashboard","shinyWidgets", 
               "DT","colourpicker","ggplot2","ggrepel","ggforce","stringr", "RCurl")


remotes_pkgs <- c("rte-antares-rpackage/spMaps@med-tso", 
                  "rte-antares-rpackage/antaresRead@master", 
                  "rte-antares-rpackage/antaresProcessing@master",
                  "rte-antares-rpackage/antaresVizMedTSO")


app_name = "antaresVizMedTSO"
app_dir = "create_desktop/"
dir_out = "RInno_installer"
pkgs = cran_pkgs
pkgs_path = "bin" 
repo = "https://cran.rstudio.com"
remotes = remotes_pkgs
locals = NULL
user_browser = "electron" 
include_R = FALSE
include_Pandoc = FALSE 
include_Chrome = FALSE
include_Rtools = FALSE
overwrite = TRUE
force_nativefier = TRUE
nativefier_opts = c('--show-menu-bar')
app_desc = 'Antares Visualizations of Med-TSO studies' 
app_version = "0.0.2"
app_icon = "med_logo_DFT_icon.ico" 
publisher = "Datastorm/RTE"

# 1. copy defaut installation file ----
# copy_installation(app_dir, overwrite)
# 
# file.remove(file.path(app_dir, "infoafter.txt"))
# file.remove(file.path(app_dir, "infobefore.txt"))

# modification du launcher (temps de chargement du global...)
#
# ready <- RCurl::url.exists("http://127.0.0.1:1984")
# while(!ready){
#   Sys.sleep(1)
#   ready <- RCurl::url.exists("http://127.0.0.1:1984")
# }


# Si souhait d'inclure R:
# puis modificaiton du run.js 62
# var Rexe           = oFSO.GetAbsolutePathName(".") + "\\R-3.6.1\\bin\\Rscript.exe";
#
# et modification du launch_app.R
# L87
# R_path <- paste0(getwd(), "\\R-3.6.1\\bin\\R.exe")
# system(sprintf(paste0(R_path, ' -e ".libPaths(c(\'%s\')); shiny::runApp(\'./\', port=1984)"'), applibpath), wait = FALSE)
# 
# et package_manger.R L 32 33
# .libPaths(c(applibpath))

# 2. build electron app ----

nativefier_opts = c("--zoom 0.8", "-m")
nativefy_app(app_name, app_dir, nativefier_opts, 
             app_icon = app_icon, app_port = 1984)

# utilisation du nativefier de Datastorm (fix des menus)
app_port = 1984
system(paste0("R -e ", "\"shiny::runApp(", sprintf("'%s', port=%i)", app_dir, app_port)), wait = FALSE)
oldwd <- getwd()
setwd(app_dir)
nativefier_loc <- "nativefier-app"
local_url <- paste0("http://127.0.0.1:", app_port, "/")
opts_str <- paste(nativefier_opts, collapse = " ")
cmd <- glue::glue("nativefier --name {glue::double_quote(app_name)} --icon {app_icon} {opts_str} {glue::double_quote(local_url)} {glue::double_quote(nativefier_loc)}")
cat("\n", cmd, "\n")
system(cmd)
setwd(oldwd)

# 3. create .bat ----
create_bat(app_name, app_dir)



# 4. create config (package). A refaire si les packages changent ----
?create_config
create_config(app_name, app_dir, pkgs = cran_pkgs, 
              pkgs_path = pkgs_path, 
              remotes = remotes_pkgs, 
              repo = repo, 
              error_log = "error.log", 
              app_repo_url = "none", auth_user = "none", 
              auth_pw = "none", auth_token = "none", 
              user_browser = user_browser)

# 5. create .iss ----

app_name <- start_iss(app_name)
?directives_section

res_iss <- directives_section(app_name, 
                              include_R = FALSE, 
                              R_version = "3.6.1", 
                              include_Pandoc = FALSE, 
                              Pandoc_version = rmarkdown::pandoc_version(), 
                              include_Chrome = FALSE, 
                              include_Rtools = FALSE, 
                              Rtools_version = "3.5", 
                              app_version = app_version, 
                              publisher = publisher, 
                              main_url = "")


?setup_section
res_iss <- setup_section(res_iss, app_dir, dir_out, 
                         app_version = app_version, 
                         default_dir = "userdocs", 
                         privilege = "lowest", 
                         info_before = "infobefore.txt", 
                         info_after = "infoafter.txt", 
                         license_file = "none",
                         setup_icon = "setup.ico", 
                         inst_pw = "none",
                         pub_url = "{#MyAppURL}", 
                         sup_url = "{#MyAppURL}",
                         upd_url = "{#MyAppURL}", 
                         compression = "lzma2/ultra64")

# add version to setup name
res_iss <- gsub("OutputBaseFilename = setup_{#MyAppName}", 
     "OutputBaseFilename = setup_{#MyAppName}_{#MyAppVersion}", 
       res_iss, fixed = TRUE)

# remove infobefore and after
res_iss <- gsub("\nInfoBeforeFile = infobefore.txt", "", 
                res_iss, fixed = TRUE)

res_iss <- gsub("\nInfoAfterFile = infoafter.txt", "", 
     res_iss, fixed = TRUE)

# language
res_iss <- languages_section(res_iss, language = "english")

# task
res_iss <- tasks_section(res_iss, desktop_icon = TRUE)

# icons
res_iss <- icons_section(res_iss, 
                         app_dir, 
                         app_desc = app_desc, 
                         app_icon = app_icon,
                         prog_menu_icon = TRUE, 
                         desktop_icon = TRUE)

# set user rather than common
res_iss <- gsub("{commonprograms}\\{#MyAppName}", "{userprograms}\\{#MyAppName}", 
     res_iss, fixed = TRUE)

res_iss <- gsub("{commondesktop}\\{#MyAppName}", "{userdesktop}\\{#MyAppName}", 
     res_iss, fixed = TRUE)

# file section

res_iss <-  files_section(res_iss, app_name = app_name, 
                app_dir = app_dir, 
                user_browser = user_browser, 
                file_list = character())

# Si souhait d'inclure R:
# modification du .iss à la fin des source
res_iss <- glue::glue('{res_iss}\nSource: "C:\\Program Files\\R\\R-3.6.1\\*"; DestDir: "{{app}}\\R-3.6.1"; Flags: ignoreversion recursesubdirs')

                
# pas besoin                
# res_iss <-  run_section(res_iss, R_flags = "/SILENT")
  

code_section

R_versions <- list("3.6.1")
acceptable_R_versions <- paste0(glue::glue("RVersions.Add('{R_versions}');"), 
                                collapse = "\n  ")

code_file <- paste0(readLines("inst/create_desktop/add_code.iss"), collapse = "\n")
res_iss <- glue::glue("{res_iss}\n  {code_file}\n  // Initialize the values of supported versions\n  RVersions := TStringList.Create; // Make a new TStringList object reference\n  // Add strings to the StringList object\n  {acceptable_R_versions}\n\nend;\n\n// Procedure called by InnoSetup when it is closing\nprocedure DeinitializeSetup();\nbegin\n  RVersions.Free;\nend;\n  ")

writeLines(res_iss, file.path(app_dir, paste0(app_name, ".iss")))


# 6. Compile ----
RInno:::check_app(app_dir, pkgs_path)
options(RInno.app_name = app_name)
options(RInno.app_dir = app_dir)
compile_iss()
