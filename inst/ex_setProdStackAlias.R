# name : nom affiché dans l'applicatio (liste de sélection)
# variables : list nommé de l'empilement
#   nom = nom affiché
#   ensuite la formule de calcul
# colors : couleur en code hexadécimal pour les variables d'empilement  
# lines / linesColors : idem mais pour les courbes
#
# possibilité d'utiliser BATT, BATT_POS, ....
#
setProdStackAlias(
  name = "GRTgaz gaz",
  variables = alist(
    "Gaz conventionnel" = `MISC. DTG`,
    "Gaz renouvelable 1G" = GAS,
    "Gaz renouvelable 2G" = COAL,
    "Gaz renouvelable 3G" = LIGNITE,
    "Gaz renouvelable autre" = OIL,
    "Stockages" = `H. ROR` + `H. STOR`,
    "Import/export" = -(BALANCE + `ROW BAL.`),
    "Défaillance" = `UNSP. ENRG`,
    "Surplus" = `SPIL. ENRG`
  ),
  colors = c(
    "#3c4f69", 
    "#00a984", 
    "#32ffd2", 
    "#007f63", 
    "#77ffe1", 
    "#007bc2", 
    "#154194", 
    "#951b81", 
    "#e5007d"),
  lines = alist(
    "Consommation" = LOAD,
    "Production" = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG` + WIND + SOLAR + `H. ROR` + `H. STOR` + `MISC. NDG` + pmax(0, PSP)
  ),
  lineColors = c("#005542", "#eb9ba6"),
  lineWidth = 2
)

setProdStackAlias(
  name = "GRTgaz gaz/élec",
  variables = alist(
    "Gaz conventionnel" = `MISC. DTG`,
    "Pompage/turbinage" = PSP,
    "Import/export" = -(BALANCE + `ROW BAL.`),
    "Autre renouvelable" = `MISC. NDG`,
    "Eolien" = WIND,
    "Solaire" = SOLAR,
    "Nucléaire" = NUCLEAR,
    "Hydraulique" = `H. ROR` + `H. STOR`,
    "Gaz" = GAS,
    "Charbon" = COAL,
    "Gaz renouvelable" = LIGNITE,
    "Fioul" = OIL,
    "Défaillance" = `UNSP. ENRG`,
    "Surplus" = `SPIL. ENRG`,
    "Effacement" = `MIX. FUEL`
  ),
  colors = c(
    "#3c4f69", 
    "#1147b9", "#154194", 
    "#166a57", "#74cdb9", 
    "#f27406", "#f5b300", 
    "#007bc2", "#f30a0a", 
    "#ac8c35", "#00a984", 
    "#8356a2", "#951b81", 
    "#e5007d", "#adff2f"),
  lines = alist(
    "Consommation" = LOAD,
    "Production" = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG` + WIND + SOLAR + `H. ROR` + `H. STOR` + `MISC. NDG` + pmax(0, PSP) ),
  lineColors = c("#005542", "#eb9ba6"),
  lineWidth = 2
)