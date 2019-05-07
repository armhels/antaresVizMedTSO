subsetDataTable <- function(data, panel_name, lig1, lig2, col1) {
  
  # rows subset
  if(panel_name != "clusters"){
    if(!"all" %in% lig1){
      if(panel_name == "areas") {
        data <- data[area %in% lig1]
      } else if(panel_name == "links") {
        data <- data[link %in% lig1]
      } else if(panel_name == "districts") {
        data <- data[district %in% lig1]
      }
    }
  } else {
    if(!("all" %in% lig1 & "all" %in% lig2)) {
      if("all" %in% lig2) {
        data <- data[area %in% lig1]
      } else if ("all" %in% lig1) {
        data <- data[cluster %in% lig2]
      } else {
        data <- data[area %in% lig1 & cluster %in% lig2]
      }
    }
  }
  
  # column subset
  if(!"all" %in% col1) {
    col_dates <- c("month", "week", "day", "hour", "time")
    col_dates <- col_dates[col_dates %in% colnames(data)]
    
    if(panel_name == "areas") {
      data <- data[, c("area", col_dates, col1),with = FALSE]
    } else if(panel_name == "links") {
      data <- data[, c("link", col_dates, col1),with = FALSE]
    } else if(panel_name == "districts") {
      data <- data[, c("district", col_dates, col1),with = FALSE]
    } else if(panel_name == "clusters") {
      data <- data[, c("area", "cluster", col_dates, col1),with = FALSE]
    }
  }
  data
}