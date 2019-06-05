# path <- "C:\\Users\\Datastorm\\Desktop\\Med-TSO\\Full MedTSO"
# 
# # recuperation des donnees ------
# areas = unique(ref_pos_areas$area)
# links = unique(ref_pos_links$link)
# clusters = NULL
# districts = NULL
# mcYears = 1

get_data_map <- function(opts, areas = NULL, links = NULL, mcYears = 1, 
                         removeVirtualAreas = FALSE,
                         storageFlexibility = NULL, production = NULL,
                         reassignCosts = FALSE, newCols = TRUE, rowBal = TRUE){
  
  if(length(storageFlexibility) > 0) storageFlexibility <- tolower(storageFlexibility)
  if(length(production) > 0) production <- tolower(production)
  if(length(areas) > 0) areas <- tolower(areas)
  if(length(links) > 0) links <- tolower(links)
  
  if(!is.null(areas)){
    if(!removeVirtualAreas){
      data_areas <- readAntares(areas = areas, links = links, 
                                timeStep = "annual", select = NULL, mcYears = mcYears)
    } else {
      data_areas <- readAntares(areas = "all", 
                            links = "all",
                            timeStep = "annual", 
                            select = NULL, 
                            mcYears = mcYears)
      
      data_areas <- suppressWarnings({removeVirtualAreas(data_areas, storageFlexibility = storageFlexibility, production = production,
                                                     reassignCosts = reassignCosts, newCols = newCols, rowBal = rowBal)})
      
      sel_areas <- areas
      if(!"all" %in% sel_areas) data_areas$areas <- data_areas$areas[area %in% sel_areas, ]
      data_areas$links <- NULL
    }

    data_areas <- data_areas$areas
    gc()
    
  } else {
    data_areas <- data.table(area = character(0))
  }
  
  
  if(!is.null(links)){
    
    data_links_h <- readAntares(areas = NULL, links = links, timeStep = "hourly", 
                                select = NULL, mcYears = 1, linkCapacity = TRUE)
    
    # removeVirtualAreas not impact link data
    # if(!removeVirtualAreas){
    # data_links_h <- readAntares(areas = areas, links = links, timeStep = "hourly", 
    #                             select = NULL, mcYears = 1, linkCapacity = TRUE)
    # } else {
    #   
    #   data_links_h <- readAntares(areas = "all", 
    #                             links = "all",
    #                             timeStep = "hourly", 
    #                             select = NULL, 
    #                             mcYears = mcYears, linkCapacity = TRUE)
    #   
    #   data_links_h <- suppressWarnings({removeVirtualAreas(data_links_h, storageFlexibility = storageFlexibility, production = production,
    #                                                      reassignCosts = reassignCosts, newCols = newCols, rowBal = rowBal)})
    #   sel_links <- links
    #   data_links_h$areas <- NULL
    #   data_links_h$links <- data_links_h$links[link %in% sel_links, ]
    #   gc(reset = T)
    # }
    # 
    # data_links_h <- data_links_h$links
    # gc()
    
    data_links <- data_links_h[, list(
      value_ab_center = round(sum(`FLOW LIN.`[`FLOW LIN.` > 0]) / 1000, 0),
      value_ba_center = round(abs(sum(`FLOW LIN.`[`FLOW LIN.` < 0])) / 1000, 0),
      arrow_ab = sum(`FLOW LIN.`[`FLOW LIN.` > 0]) / sum(transCapacityDirect),
      arrow_ba = abs(sum(`FLOW LIN.`[`FLOW LIN.` < 0])) / sum(transCapacityIndirect),
      pie_ab = sum(`CONG. PROB +` > 0) / .N,
      pie_ba = sum(`CONG. PROB -` > 0) / .N
    ), by = link]
    
    data_links[is.na(arrow_ab), arrow_ab := 0]
    data_links[is.na(arrow_ba), arrow_ba := 0]
    
    rm(data_links_h)
    gc()
    
    data_links[, pie_null := 1 - pmin(1, pie_ab + pie_ba)]
    
    data_links_arrows <- data_links[, list(link, value = value_ab_center, pct = paste0(trunc(arrow_ab * 100, 0), "%"))]
    
    data_links_inv <- copy(data_links)
    data_links_inv[, link := sapply(link, function(x)  paste0(rev(unlist(strsplit(x, " - "))), collapse = " - "))]
    data_links_inv <- data_links_inv[, list(link, value = value_ba_center, pct = paste0(trunc(arrow_ba * 100, 0), "%"))]
    
    data_links_arrows <- rbindlist(list(data_links_arrows, data_links_inv))
    
    data_links <- data_links[, list(link, pie_ab, pie_ba, pie_null)]
  } else {
    data_links_arrows <- data.table(link = character(0))
    data_links <- data.table(link = character(0))
  }

  if(nrow(data_areas) > 0 | nrow(data_links) > 0 | nrow(data_links_arrows) > 0){
    list(areas = data_areas, links = list(centers = data_links, 
                                          arrows = data_links_arrows))
  } else {
    NULL
  }

}


# sp_object
# ref_pos_areas
# data_map
# var_countries = "MRG. PRICE" 
# var_countries = NULL
# palette_colors = c("#ff0000", "#0000ff", "#00ff00")
# label_size = 3
# label_color = "black"

# sp_object
# ref_map_areas <- data.table(ref_medtsomap_data$areas)
# data_map <- ddm
# var_countries = "MRG. PRICE"
# var_label = "code"
# palette_colors = c("#ff0000", "#0000ff", "#00ff00")
# label_size = 3
# label_color = "black"
# # 
# init_map_sp(sp_object,
#             data.table(ref_medtsomap_data$areas),
#             ddm,
#             var_countries = "MRG. PRICE",
#             var_label = "code",
#             palette_colors = c("#ff0000", "#0000ff", "#00ff00"),
#             label_size = 3,
#             label_color = "black")$map

init_map_sp <- function(
  sp_object, ref_map_areas, data_map,
  var_countries = "MRG. PRICE", 
  var_label = "code",
  palette_colors = c("#ff0000", "#0000ff", "#00ff00"),
  label_size = 3, label_color = "black"){
  
  ref_map <- copy(ref_map_areas)
  
  ref_pos_areas <- ref_map_areas[ref_map_areas$draw_cty %in% 1, ]
  
  var_countries <- intersect(var_countries, colnames(data_map$areas))
  if(length(var_countries) == 0) var_countries <- NULL
  # browser()
  kt_geom <- as.data.table(ggplot2::fortify(sp_object, region = "OBJECTID"))
  kt_geom[, id := as.integer(id)]
  kt_geom <- merge(kt_geom, ref_pos_areas[, .(OBJECTID, area, name, code)], 
                   by.x = "id", by.y = "OBJECTID", sort = FALSE)
  kt_geom <- merge(kt_geom, data_map$areas[, c("area", var_countries), with = F], by = "area", 
                   sort = FALSE, all.x = TRUE)
  
  map_lon_limit <- range(kt_geom$long, na.rm = T)
  map_lat_limit <- range(kt_geom$lat, na.rm = T)
  
  if(map_lon_limit[1] < -20) map_lon_limit[1] <- -20
  if(map_lon_limit[2] > 50) map_lon_limit[2] <- 50
  
  if(map_lat_limit[1] < 20) map_lat_limit[1] <- 20
  if(map_lat_limit[2] > 58) map_lat_limit[2] <- 58
  
  setorder(kt_geom, order)
  
  if(is.null(var_countries)){
    default_col <- 1
    res <- ggplot() + coord_quickmap(xlim = map_lon_limit, ylim = map_lat_limit) +
      geom_polygon(aes(long, lat, group = group, fill = default_col), data = kt_geom) + 
      geom_path(aes(long, lat, group = group), data = kt_geom, color="black") +
      scale_fill_gradientn(colours = palette_colors) + theme(legend.position = "right") +
      theme_classic() +
      theme(legend.position = "none")
  } else {
    breaks <- waiver()
    if(var_countries %in% c("prix_marginal", "MRG. PRICE")){
      breaks <- c(0, 30, seq(40, 100, 5), 110, 120, 130, Inf)
    }
    res <- ggplot() + coord_quickmap(xlim = map_lon_limit, ylim = map_lat_limit) + 
      geom_polygon(aes(long, lat, group = group, fill = get(var_countries)), data = kt_geom) + 
      geom_path(aes(long, lat, group = group), data = kt_geom, color="black") +
      scale_fill_gradientn(colours = palette_colors, breaks = breaks, minor_breaks = breaks, limits = c(NA, NA)) + 
      theme(legend.position = "right") + guides(fill = guide_legend(keyheight=3, reverse = TRUE, label.position = "right", label.vjust = 0)) +
      theme_classic() +
      labs(fill = paste(var_countries))
  }
  
  if(label_size > 0){
    res <- res + 
      geom_text(data = ref_map[code %in% ref_pos_areas$code, ], aes(x = lon_label, y = lat_label, label = get(var_label), ), 
                size = label_size, color = label_color)
  }
  
  list(map = res, legend_position = c(map_lon_limit[2] - ((map_lon_limit[2] - map_lon_limit[1])/10), map_lat_limit[2] - ((map_lat_limit[2] - map_lat_limit[1])/10)))
  
}


# data_links_arrows <- copy(data_map$links$arrows)
# col_value = "value"
# color = "black"
# size = 0.9
# length = 0.05
# lon_gap = 0
# lat_gap = 0

add_links <- function(res_map, ref_pos_links, data_links_arrows, col_value, 
                      color = c("green", "red"),  size = 0.9, text_size = 4,
                      length = 0.05, lon_gap = 0, lat_gap = 0){
  
  # carte avec fleche
  data_links <- copy(ref_pos_links)
  
  if(nrow(data_links_arrows) > 0){
    
    data_links[, id_link := 1:nrow(data_links)]
    
    data_links[, head := "last"]
    data_links[, col := color[1]]
    data_links[lat_end == lat_start, horiz := T]
    data_links[lat_end != lat_start, horiz := F]
    
    data_links_inv <- copy(data_links)
    data_links_inv[, link := sapply(link, function(x)  paste0(rev(unlist(strsplit(x, " - "))), collapse = " - "))]
    data_links_inv[horiz == T, c("lat_start", "lat_end", "head", "col") := list(lat_start + 0.5, lat_end + 0.5, "first", color[2])]
    data_links_inv[horiz == F, c("lon_start", "lon_end", "head", "col") := list(lon_start + 0.5, lon_end + 0.5, "first", color[2])]
    
    data_links <- rbindlist(list(data_links, data_links_inv))
    
    data_links <- merge(data_links, data_links_arrows[, list(link, value = get(col_value))], by = "link", sort = FALSE)
    
    if(nrow(data_links) > 0){
      # lat_gap <- (sp_object@bbox[2, 2] - sp_object@bbox[2, 1]) / 50
      # lon_gap <- (sp_object@bbox[1, 2] - sp_object@bbox[1, 1]) / 50
      
      data_links[lat_end == lat_start & lon_start > lon_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        ifelse(head %in% "last", lon_start + lon_gap, lon_end - lon_gap),
        lat_end,
        "center",
        ifelse(head %in% "last", "left", "right")
      )]
      
      data_links[lat_end == lat_start & lon_start < lon_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        ifelse(head %in% "last", lon_start - lon_gap, lon_end + lon_gap),
        lat_end,
        "center",
        ifelse(head %in% "last", "right", "left")
      )]
      
      data_links[lon_end == lon_start & lat_start > lat_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        lon_end,
        ifelse(head %in% "last", lat_start + lat_gap, lat_end - lat_gap),
        ifelse(head %in% "last", "bottom", "top"),
        "center"
      )]
      
      data_links[lon_end == lon_start & lat_start < lat_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        lon_end,
        ifelse(head %in% "last", lat_start - lat_gap, lat_end + lat_gap),
        ifelse(head %in% "last", "top", "bottom"),
        "center"
      )]
      
      data_links[lat_start > lat_end & lon_start > lon_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        ifelse(head %in% "last", lon_start + lon_gap, lon_end - lon_gap),
        ifelse(head %in% "last", lat_start + lat_gap, lat_end - lat_gap),
        ifelse(head %in% "last", "bottom", "top"),
        ifelse(head %in% "last", "left", "right")
      )]
      
      
      data_links[lat_start > lat_end & lon_start < lon_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        ifelse(head %in% "last", lon_start - lon_gap, lon_end + lon_gap),
        ifelse(head %in% "last", lat_start + lat_gap, lat_end - lat_gap),
        ifelse(head %in% "last", "bottom", "top"),
        ifelse(head %in% "last", "right", "left")
      )]
      
      data_links[lat_start < lat_end & lon_start > lon_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        ifelse(head %in% "last", lon_start + lon_gap, lon_end - lon_gap),
        ifelse(head %in% "last", lat_start - lat_gap, lat_end + lat_gap),
        ifelse(head %in% "last", "top", "bottom"),
        ifelse(head %in% "last", "left", "right")
      )]
      
      
      data_links[lat_start < lat_end & lon_start < lon_end,  c("lon_lab", "lat_lab", "vjust", "hjust") := list(
        ifelse(head %in% "last", lon_start - lon_gap, lon_end + lon_gap),
        ifelse(head %in% "last", lat_start - lat_gap, lat_end + lat_gap),
        ifelse(head %in% "last", "top", "bottom"),
        ifelse(head %in% "last", "right", "left")
      )]  
      
      res_map + 
        # coord_fixed() + 
        geom_segment(data = data_links, mapping = aes(x = lon_start, y = lat_start, xend = lon_end, 
                                                      yend = lat_end, group = id_link), 
                     arrow = arrow(type = "closed", length = unit(length, "inches"), ends = data_links$head), 
                     size = size, color = data_links$col) +
        geom_label(data_links, mapping = aes(x = lon_lab, y = lat_lab, vjust = vjust, hjust = hjust, label = value), 
                   size = text_size, color = data_links$col)
    } else {
      res_map
    }
  } else {
    res_map
  }
}


add_pie <- function(base_ggmap, ref_map, data_pos, data_pie, 
                    id_col = colnames(data_pie)[1],
                    pie_col = c("WIND","SOLAR","NUCLEAR","LIGNITE","COAL","GAS","OIL"), 
                    r = 2, text_size = 2, colors = NULL, legend_position = NULL, 
                    label_col = NULL, alpha = 0.5){
  
  
  if("draw_pie" %in% colnames(data_pos)){
    label_only <- as.character(data_pos[draw_pie == 0, code])
    label_only <- intersect(unique(ref_map$code), label_only)
    data_pos <- data_pos[data_pos$draw_pie %in% 1, ]
  } else {
    label_only <- NULL
  }
  draw_id <- intersect(unique(data_pos[[id_col]]), unique(data_pie[[id_col]]))
  
  if(length(label_only) > 0 && !is.null(label_col)){
    base_ggmap <- base_ggmap + 
      geom_text(data = ref_map[code%in% label_only, ], aes(x = lon_label, y = lat_label, label = get(label_col)), 
                size = text_size, color = "black")
  }
  # browser()
  # id <- draw_id[1]
  # print(draw_id)
  legend <- NULL
  
  if(length(draw_id) > 0){
    data_pie <- data_pie[get(id_col) %in% draw_id, c(id_col, pie_col), with = FALSE]
    data_pie <- melt(data_pie, id.vars = id_col, measure.vars = pie_col)
    
    data_pie[, value := as.numeric(value)]

    for(id in draw_id){
      df <- data_pie[get(id_col) %in% id, ]
      
      if(T){
        lon_ano <- data_pos[get(id_col) %in% id, long]
        lat_ano <- data_pos[get(id_col) %in% id, lat]
        
        lon <- 0
        lat <- 0
        df[, c("xlab", "ylab", "label", "hjust", "vjust", "lon", "lat", "r") := {
          end = 2 * pi * cumsum(value)/sum(value)
          start = shift(end, 1, fill = 0)
          middle = 0.5 * (start + end)
          hjust = ifelse(middle > pi, 1, 0)
          vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1)
          x_lab = sin(middle)
          y_lab = cos(middle)
          label = paste0(round(value/sum(value) * 100, 0), "%")
          label[label %in% "0%"] <- NA
          list((r/1.5 + 0.05) * x_lab + lon, (r/1.5 +0.05) * y_lab + lat, label, hjust, vjust, lon, lat, r)
        }]
        
        # print(df)
        df[value == 0,value := 0.00001]
        legend <- ggplot(df) + geom_arc_bar(
          aes(x0 = 0, y0 = 0 , r0 = r/2, r = r, amount = value,fill = variable),
          stat = 'pie', alpha = alpha)  +
          geom_text(aes(x = xlab , 
                        y = ylab, 
                        label = label), size = text_size) + coord_fixed() + labs(x = NULL, y = NULL) + 
          theme(legend.position = "right") + guides(fill = guide_legend(title = "Pie"))
        
        if(!is.null(colors) && length(colors) == length(pie_col)){
          legend <- legend + 
            scale_fill_manual(values = colors)
        }
        
        legend <- ggplotGrob(legend)
        
        leg <- which(sapply(legend$grobs, function(x) x$name) == "guide-box") 
        
        legend <- legend$grobs[[leg]] 
        
        # df[value == 0.00001,value := 0]
        if(!all(is.na(df$hjust))){
          sub_plot <- ggplot(df) + geom_arc_bar(
            aes(x0 = 0, y0 = 0 , r0 = r/2, r = r, amount = value,fill = variable),
            stat = 'pie', alpha = alpha)  +
            geom_text(aes(x = xlab , 
                          y = ylab, 
                          label = label), size = text_size) + coord_fixed() + labs(x = NULL, y = NULL) + 
            theme(legend.position = "none", rect = element_blank(),
                  line = element_blank(), text = element_blank())
          
          if(!is.null(label_col)){
            label_pie <- data_pos[get(id_col) %in% id, get(label_col)]
            sub_plot <- sub_plot + geom_text(aes(x = 0, y = 0, label = label_pie))
          }
          if(!is.null(colors) && length(colors) == length(pie_col)){
            sub_plot <- sub_plot + 
              scale_fill_manual(values = colors)
          }
          
          gt_plot <- ggplotGrob(sub_plot)
          
          base_ggmap <- base_ggmap + annotation_custom(gt_plot, 
                                                       xmin = lon_ano -r,
                                                       xmax = lon_ano + r,
                                                       ymin = lat_ano - r,
                                                       ymax = lat_ano + r)
        }
      }
    }
  }
  
  if(!is.null(legend_position) && !is.null(legend)){
    base_ggmap <- base_ggmap + annotation_custom(legend, 
                                                 xmin = legend_position[1],
                                                 xmax = legend_position[1],
                                                 ymin = legend_position[2],
                                                 ymax = legend_position[2])
  }
  base_ggmap
  
}


# input_path <- "C:\\Users\\Datastorm\\Desktop\\MED-Tso_app\\final_shiny\\MedTSO_map_template.xlsx"
readMEDTsoMapInput <- function(input_path){
  
  sel <- list(areas = NULL, links = NULL, inputs = NULL)
  
  if(!file.exists(input_path)){
    stop("Le fichier '", input_path, "' est introuvable")
  }
  
  # areas
  sel_areas <- suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Areas", check.names = FALSE, colNames = TRUE),
                                         error = function(e) {
                                           stop("Error reading sheet 'Areas' : ", e)
                                         }))
  
  if(!is.null(sel_areas) && nrow(sel_areas) > 0){
    stopifnot(all(c("OBJECTID", "name", "area", "draw_pie", "draw_cty", "code", "lon_label", "lat_label", "lon_pie", "lat_pie") %in% colnames(sel_areas)))
    sel_areas$area <- tolower(as.character(sel_areas$area))
    sel$areas <- sel_areas
  }
  
  # links
  sel_links <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Links", check.names = FALSE, colNames = TRUE),
                                          error = function(e) {
                                            stop("Error reading sheet 'Links' : ", e)
                                          }))
  
  if(!is.null(sel_links) && nrow(sel_links) > 0){
    stopifnot(all(c("link", "lon_start", "lon_end", "lat_start", "lat_end", "lon_pie", "lat_pie", "draw_link") %in% colnames(sel_links)))
    sel_links$link <- tolower(as.character(sel_links$link))
    sel$links <- sel_links
  }
  
  # inputs
  sel_inputs <-  suppressWarnings(tryCatch(openxlsx::read.xlsx(input_path, sheet = "Graphical Parameters", check.names = FALSE, colNames = TRUE),
                                          error = function(e) {
                                            stop("Error reading sheet 'Graphical Parameters' : ", e)
                                          }))
  
  if(!is.null(sel_inputs) && nrow(sel_inputs) > 0){
    stopifnot(all(c("type",	"id",	"label",	"value") %in% colnames(sel_inputs)))
    sel_links$link <- tolower(as.character(sel_links$link))
    sel$inputs <- sel_inputs
  }
  
  sel
  
}



#' @export
writeMEDTsoMapInput <- function(areas, links, inputs, output_path){
  
  ## Create a new workbook
  wb <- openxlsx::createWorkbook("antaresVizMedTSO_maps")
  
  ## init worksheets
  openxlsx::addWorksheet(wb, "Areas")
  openxlsx::addWorksheet(wb, "Links")
  openxlsx::addWorksheet(wb, "Graphical Parameters")
  
  ## Need data on worksheet to see all headers and footers
  if(!is.null(areas) && nrow(areas) > 0){
    openxlsx::writeData(wb, sheet = "Areas", data.frame(areas), 
                        colNames = TRUE, rowNames = FALSE)
  }
  
  if(!is.null(links) && nrow(links) > 0){
    openxlsx::writeData(wb, sheet = "Links", data.frame(links), 
                        colNames = TRUE, rowNames = FALSE)
  }
  
  default_link <- read.csv(system.file("application/data/MedTSO_map_default_graphicalu_parameters.csv", package = "antaresVizMedTSO"), 
                           header = TRUE, sep = ";", stringsAsFactors = FALSE)
  
  if(!is.null(inputs)){
    
    keep_input <- which(sapply(inputs, function(x) !is.null(x)))
    if(length(keep_input) > 0){
      inputs <- inputs[keep_input]
      match_names <- match(names(inputs), default_link$id)
      if(!all(is.na(match_names))){
        inputs <- inputs[which(!is.na(match_names))]
        match_names <- setdiff(match_names, NA)
        
        value <- sapply(inputs, function(x){
          if(class(x) %in% c("numeric", "integer")){
            x <- gsub(".", ",", as.character(x), fixed = TRUE)
          } else {
            if(length(x) > 0) x <- paste(x, collapse = ",")
          }
          x
        })
        
        default_link$value[match_names] <- value
      }
    }
  }
  
  openxlsx::writeData(wb, sheet = "Graphical Parameters", data.frame(default_link), 
                      colNames = TRUE, rowNames = FALSE)
   
  ## Save workbook
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
}
