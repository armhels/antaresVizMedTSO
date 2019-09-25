require(RInno)

?create_app
?directives_section
?setup_section
?languages_section
?tasks_section
?files_section
?icons_section
?run_section
?code_section
Sys.setenv("TAR" = "internal")

cran_pkgs <- c("dygraphs","shiny","plotly","htmltools","htmlwidgets","manipulateWidget",
"leaflet","sp","rgeos","raster","webshot","data.table","methods","lubridate",
"geojsonio","graphics","stats","leaflet.minicharts", "bit64", "plyr",
"assertthat","rAmCharts","utils","openxlsx","shinydashboard","shinyWidgets", 
"DT","colourpicker","ggplot2","ggrepel","ggforce","stringr")


remotes_pkgs <- c("rte-antares-rpackage/spMaps@med-tso", 
                  "rte-antares-rpackage/antaresRead@master", 
                  "rte-antares-rpackage/antaresProcessing@master",
                  "rte-antares-rpackage/antaresVizMedTSO")

create_app(app_name = "antaresVizMedTSO", 
           app_dir = "inst/create_desktop/",
           dir_out = "RInno_installer", 
           pkgs = cran_pkgs, 
           pkgs_path = "bin", 
           repo = "https://cran.rstudio.com",
           remotes = remotes_pkgs, 
           locals = NULL, 
           app_repo_url = "none",
           user_browser = "electron", 
           include_R = FALSE,
           include_Pandoc = FALSE, 
           include_Chrome = FALSE,
           include_Rtools = FALSE,
           overwrite = TRUE, 
           force_nativefier = TRUE,
           nativefier_opts = c(), 
           app_desc = 'Antares Visualizations of Med-TSO studies', 
           app_version = "0.0.1",
           app_icon = "med_logo_DFT_icon.ico", 
           publisher = "Datastorm/RTE"
           )

# modification du .iss
# rajout d'une version
# - l42 et 43 :{commondesktop} -> {userdesktop}, {commonprograms} -> {userprograms}

# modification du launcher (temps de chargement du global...)
# L87 avant # start electron
# pb <- winProgressBar(title = "Loading application ...", label = "Initializing ...")
# nb_sec <- 8
# for (i in 1:nb_sec) {
#   setWinProgressBar(pb, value = i / (nb_sec),
#                     label = "Initializing ...")
#   Sys.sleep(1)
# }
# close(pb)

#l 20
# OutputBaseFilename = setup_{#MyAppName}_{#MyAppVersion}
  
# Si souhait d'inclure R:
# modification du .iss à la fin des source
# Source: "C:\Program Files\R\R-3.6.1\*"; DestDir: "{app}\R-3.6.1"; Flags: ignoreversion recursesubdirs
# 
# suppression du control post install : 
# // Pre-installation actions
# if CurStep = ssInstall then
# begin
# #if IncludeR
# #else
# // With `CurStep = ssInstall` we can still `Abort` if R not included but needed
# if RNeeded then
# begin
# SuppressibleMsgBox(Format('Error: R >= %s not found',[RVersions[RVersions.Count - 1]]), mbError, MB_OK, MB_OK);
# Abort;
# end;
# #endif
# end;
# 
# puis modificaiton du run.js 62
# var Rexe           = oFSO.GetAbsolutePathName(".") + "\\R-3.6.1\\bin\\Rscript.exe";
#
# et modification du launch_app.R
# L87
# R_path <- paste0(getwd(), "\\R-3.6.1\\bin\\R.exe")
# system(sprintf(paste0(R_path, ' -e ".libPaths(c(\'%s\', .libPaths())); shiny::runApp(\'./\', port=1984)"'), applibpath), wait = FALSE)
# 
# 
compile_iss()





# Copy installation scripts (JavaScript, icons, infobefore.txt, package_manager.R, launch_app.R)
copy_installation(app_dir = "my/app/path")

# If your users need R installed:
get_R(app_dir = "my/app/path", R_version = "2.2.1")

# Create batch file
create_bat(app_name = "My AppName", app_dir = "my/app/path")

# Create app config file
create_config(app_name = "My AppName", app_dir = "my/app/path",
              pkgs = c("jsonlite", "shiny", "magrittr", "dplyr", "caret", "xkcd"))

# Build the iss script
start_iss(app_name = "My AppName") %>%
  
  # C-like directives
  directives_section(R_version   = "2.2.1", 
                     include_R   = TRUE,
                     app_version = "0.1.2",
                     publisher   = "Your Company", 
                     main_url    = "yourcompany.com") %>%
  
  # Setup Section
  setup_section(output_dir  = "wizard", 
                app_version = "0.1.2",
                default_dir = "pf", 
                privilege   = "high",
                inst_readme = "pre-install instructions.txt", 
                setup_icon  = "myicon.ico",
                pub_url     = "mycompany.com", 
                sup_url     = "mycompany.github.com/issues",
                upd_url     = "mycompany.github.com") %>%
  
  # Languages Section
  languages_section() %>%
  
  # Tasks Section
  tasks_section(desktop_icon = FALSE) %>%
  
  # Files Section
  files_section(app_dir = "my/app/path", file_list = "path/to/extra/files") %>%
  
  # Icons Section
  icons_section(app_desc       = "This is my local shiny app",
                app_icon       = "notdefault.ico",
                prog_menu_icon = FALSE,
                desktop_icon   = FALSE) %>%
  
  # Execution & Pascal code to check registry during installation
  # If the user has R, don't give them an extra copy
  # If the user needs R, give it to them
  run_section() %>%
  code_section() %>%
  
  # Write the Inno Setup script
  writeLines(file.path("my/app/path", "My AppName.iss"))

# Check your files, then
compile_iss()
