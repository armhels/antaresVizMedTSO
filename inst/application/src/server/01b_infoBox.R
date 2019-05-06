output$mode <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      infoBox(title = "Mode", value = opts$parameters$general$mode, icon = icon("codepen"), color = "green", fill = FALSE)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})

output$synth <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      value <- opts$parameters$output$synthesis
      icone <- ifelse(value, "check-circle", "times")
      couleur <- ifelse(value, "green", "orange")
      infoBox(title = "Synthesis", value = value, icon = icon(icone), color = couleur, fill = FALSE)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})

output$yby <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      value <- opts$parameters$general$`year-by-year`
      icone <- ifelse(value, "check-circle", "times")
      couleur <- ifelse(value, "green", "orange")
      infoBox(title = "Year by year", value = value, icon = icon(icone), color = couleur, fill = F)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})

output$areas_clusters <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      value <- length(opts$areasWithClusters)
      couleur <- ifelse(value > 0, "green", "orange")
      infoBox(title = "Areas with clusters", value = value, icon = icon("object-group"), color = couleur, fill = F)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})

output$areas_nb <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      value <- length(opts$areaList)
      couleur <- ifelse(value > 0, "green", "orange")
      infoBox(title = "Number of areas", value = value, icon = icon("map-marker"), color = couleur, fill = F)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})

output$districts_nb <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      value <- length(opts$districtList)
      couleur <- ifelse(value > 0, "green", "orange")
      infoBox(title = "Number of districts", value = value, icon = icon("globe"),
              color = couleur, fill = F)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})

output$links_nb <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      value <- length(opts$linkList)
      couleur <- ifelse(value > 0, "green", "orange")
      infoBox(title = "Number of links", value = value, icon = icon("link"), color = couleur, fill = F)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})

output$MCyears_nb <- renderInfoBox({
  if(input$init_sim > 0){
    opts <- opts()
    if(!is.null(opts)) {
      infoBox(title = "Monte-Carlo years", value = opts$parameters$general$nbyears, icon = icon("cube"), color = "green", fill = F)
    } else {
      infoBox(" ", color = "green")
    }
  } else {
    infoBox(" ", color = "green")
  }
})