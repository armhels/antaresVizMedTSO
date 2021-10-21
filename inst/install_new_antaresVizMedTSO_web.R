# REQUIRED : internet connexion

# REQUIRED : R and Rstudio

#-----------------------------------------------------------------
# Install tools / packages
#-----------------------------------------------------------------

# Install from clean session :

# avant l'installation des packages antaresVizMedTSO
# installer les packages suivants :
# Mettre la ligne en surbrillance ou simplement le curseur sur le code 
# et appuyer sur le boutun "Run" en haut à gauche (ou ctrl + Entree)

# restart from totally cleaned session
rm(list = ls())
otherPkgs <- names(sessionInfo()$otherPkgs)
if(length(otherPkgs) > 0){
  lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE)
}
.rs.restartR( )

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")

# Needed specific spMaps package version :
if(!require(devtools)){
  install.packages("devtools")
}

# udpate some CRAN pck
install.packages("manipulateWidget")

devtools::install_github("rte-antares-rpackage/spMaps", ref = "med-tso")
# And at moment github antaresRead & antaresProcessing
# waiting CRAN update
devtools::install_github("rte-antares-rpackage/antaresRead", ref ="master")
devtools::install_github("rte-antares-rpackage/antaresProcessing", ref ="master")

# Then install the master version of antaresVizMedTSO :
devtools::install_github("rte-antares-rpackage/antaresVizMedTSO", ref ="add_ds_2021")

# To display the help of the package and see all the functions it provides, type:
help(package="antaresVizMedTSO")

#-----------------------------------------------------------------
# Launch App
#-----------------------------------------------------------------

# antaresVizMedTSO is not fully compatible with IE. You can
# copy the adresse from IE to other browser (Firefox, Chrome, ..)
# or change defaut browser if wanted : path to browser .exe :
# options(browser = "C:\\Program Files\\Mozilla Firefox\\firefox.exe")

antaresVizMedTSO::runAppAntaresViz()
