server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  
  readFile <- reactive({
    # req(input$file)
    
    if (!is.null(input$file)) {
      # browser()
      tryCatch(
        return(read_sf(input$file$datapath)),
        error = function(e) {
          from <- input$file$datapath
          to <- file.path(dirname(from), basename(input$file$name))
          file.rename(from, to)
          # input$file$datapath <- to
          return(read_sf(unique(dirname(from))))
        }
      )
      
    } else {
      DatosEjemplo
    }
  })
  
  output$tgtVariable <- renderUI({
    req(readFile(), input$file)
    # browser()
    colOptions <- colnames(readFile())
    selectInput("tgtVar", label = targetVariableText, 
                choices = colOptions, 
                selected = 1)
  })

  
  output$histTgtVar <- renderPlot({
    req(readFile(), input$tgtVar)
    
    
    ggplot(readFile(), aes(x = !!rlang::sym(input$tgtVar))) +
      geom_histogram()
  })
  
  output$histTgtVarUI <- renderUI({
    req(readFile(), input$tgtVar)
    box(width = NULL, status = "info",
        plotOutput("histTgtVar"))
  })
  
  # datasetMag <- reactive({
  #   req(readFile(), datasetInput(), input$tgtVar)
  #   readFile()
  # })

  #generate the leaflet map
  #set the namespace for the map
  # ns <- shiny::NS("editor")

  observe({
    req(input$tgtVar)
    df <- readFile()
    # browser()
    if (nrow(df) > 2000) {
      df <- df[sample(nrow(df), 150),]
    }
    lf.quakes <<- leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.OceanBasemap",group="OceanBasemap") %>%
      addCircleMarkers(data = df,
                       color = "lightblue",
                       weight = 1,
                       fillOpacity = 0.7)
    # lf.quakes <- mapview(df)

    edits <<- callModule(editMod, "editor", lf.quakes)

  })


# generate the reactive dataset based on what is drawn on the leaflet map
  
  datasetInput <- reactive({

    req(edits()$finished)
  # browser()
    Tratamientos_sf <- edits()$finished
    Tratamientos_sf$Tratamiento <- LETTERS[seq_len(nrow(Tratamientos_sf))]
    Tratamientos_sf$Area <- st_area(edits()$finished)
    quake_intersect <- st_intersection(Tratamientos_sf, readFile()) #intersection between what is drawn and the reactive dataframe
    quake_intersect <- st_transform(quake_intersect, 32720)
    list("trat" = Tratamientos_sf,
         "sf" = quake_intersect)

  })
# 
#   #render a histogram with the reactive dataset as the output
# 
  output$plot <- renderPlot({
    req(datasetInput(), input$tgtVar)
    # browser()

    ggplot(datasetInput()$sf, aes(x = !!rlang::sym(input$tgtVar))) +
      geom_histogram(aes(y = stat(count) / sum(count))) +
      ylab("Freq") +
      facet_grid(. ~ Tratamiento)

  })


  output$tablaAreas <- renderTable({
    req(datasetInput(), input$tgtVar)
    # browser()
    TablaFreq <- data.frame(
      "Treatment" = datasetInput()$trat$Tratamiento,
      "Area (m^2)" = datasetInput()$trat$Area,
      check.names = FALSE
)

    },rownames = T)




  Depuracion_dp_mv <- reactive({
    req(datasetInput(), input$tgtVar)
    # browser()

    Depurado <-
      datasetInput()$sf %>%
      group_split(Tratamiento) %>%
      lapply(.,function(x) Depuracion(x, input$tgtVar))

  Depuracion_mv <- do.call(rbind, lapply(Depurado, "[[", "CondicionDeDepuracion"))
  Depuracion_dp <- do.call(rbind, lapply(Depurado, "[[", "Datos"))

  list("Depurados" = Depuracion_dp,
       "Motivo" = Depuracion_mv)
  })


  Descriptiva  <- reactive({
    req(Depuracion_dp_mv(), input$tgtVar)

    TablaDescriptiva <- st_drop_geometry(Depuracion_dp_mv()$Depurados) %>%
      group_by(Tratamiento) %>%
      summarise(Media = mean( !!rlang::sym(paste0(input$tgtVar,".Dep"))),
                DE = sd(!!rlang::sym(paste0(input$tgtVar,".Dep"))),
                CV = DE/Media * 100)

    right_join(TablaDescriptiva,
          zoneValidTables()$Diferencias[[1]][,c("Tratamiento", "stat")])


  })

  output$TablaDescr <- renderTable(Descriptiva())

  VarKrigDescrReac <- reactive({
    req(Depuracion_dp_mv(), input$tgtVar)
    VarKrigDescr(
      ClustersFM =  Depuracion_dp_mv()$Depurados,
      datosAValid =  paste0(input$tgtVar,".Dep"),
      crs = st_crs(Depuracion_dp_mv()$Depurados)
    )
    # iris
  })

  zoneValidTables <- reactive({
    req(Depuracion_dp_mv(), input$tgtVar)

    ValidVarKrig(
    ClustersFM =  Depuracion_dp_mv()$Depurados,
    datosAValid = paste0(input$tgtVar,".Dep"),
    numCluster = "Tratamiento",
    EstDesc = VarKrigDescrReac(),
    crs =  st_crs(Depuracion_dp_mv()$Depurados)
  )
    # iris
  })
#
  output$CompMedias <- renderPlot({
req(zoneValidTables(), input$tgtVar)
    # browser()
    ggplot(zoneValidTables()$Diferencias[[paste0(input$tgtVar,".Dep")]], aes(x = Tratamiento, y = !!rlang::sym(paste0(input$tgtVar,".Dep")))) +
    geom_col(width = 0.15) +
    geom_text(aes(label = stat, y = !!rlang::sym(paste0(input$tgtVar,".Dep"))), nudge_y = 0.10) +
    labs(y = "Rendimiento")


    # ggplot(mpg) + geom_bar(aes(y = class))
 })


  makeTmapDepurated <- reactive({
      req(Depuracion_dp_mv(), VarKrigDescrReac(),zoneValidTables(), input$tgtVar)
   tm_shape(datasetInput()$trat) +
        tm_sf(col = "Tratamiento")+
      tm_shape(Depuracion_dp_mv()$Depurados) +
        tm_sf(col = paste0(input$tgtVar,".Dep"))

  })

  output$depuratedData = renderLeaflet({
    req(makeTmapDepurated())
    # makeTmapDepurated()
    tmap_leaflet(makeTmapDepurated())
  })



  TablaFreq <- reactive({
    req(Depuracion_dp_mv())

    Depuracion_mv <- Depuracion_dp_mv()$Motivo
    Tabla <- round(prop.table(
      table(
        Depuracion_mv$Condition,
        Depuracion_mv$Tratamiento,
        useNA = "ifany"
      ),
      margin = 2
    ) * 100, 2)

    rownames(Tabla)[nrow(Tabla)] <- "Normal"
    Tabla
  })

  output$tablaDepur <- renderTable({as.data.frame.matrix(TablaFreq())},rownames = T)

  makeTmapConditionDepurated <- reactive({
    req(Depuracion_dp_mv(), VarKrigDescrReac(),zoneValidTables())
    tm_shape(datasetInput()$trat) +
      tm_sf(col = "Tratamiento") +
      tm_shape(Depuracion_dp_mv()$Motivo) +
      tm_sf(col = "Condition")

  })

  output$conditionDepuratedData <-renderLeaflet({
    req(makeTmapConditionDepurated())
    # makeTmapConditionDepurated()
    tmap_leaflet(makeTmapConditionDepurated())
  })
}
