server <- function(input, output, session){
  options(shiny.maxRequestSize=30*1024^2)

  readFile <- reactive({
    if (!is.null(input$file)) {
      tryCatch(return(read_sf(input$file$datapath)), error=function(e) {
        if(nrow(input$file>1)) {
          
          MisArchivos <- sapply(input$file$datapath, function(x) {
            # browser()
            MyDirs <- strsplit(x, "/")[[1]]
            MyDir <- paste0(MyDirs[-length(MyDirs)], collapse = "/", paste = "/")
            MyFile <- MyDirs[length(MyDirs)] 
            MyExt <- tail(strsplit(MyFile,"[.]")[[1]], 1)
            MiCapa <- paste0(MyDir,paste0("Capa.",MyExt))
            
            file.rename(x, MiCapa)
            MiCapa
          })
          MyDirs <- strsplit(input$file$datapath, "/")[[1]]
          MyDir <- paste0(MyDirs[-length(MyDirs)], collapse = "/")#, paste = "/")
           ##### SACAR CRS #####
          return(read_sf(dsn=MyDir, crs = 4326))  
          ###### FIN SACAR CRS #####
        }
      })
      
    } else {  quake_sf <- DatosRto
    quake_sf}
  })
  
  output$tgtVariable <- renderUI({
    req(readFile(), input$file)
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
  
  datasetMag <- reactive({
    readFile()
  })

  #generate the leaflet map
  #set the namespace for the map
  ns <- shiny::NS("editor")

  readFile_cnt <- isolate(readFile())
  
  # qpal <- reactive({
  #   req(readFile(), input$tgtVar)
  #   colorQuantile("Greens", datasetMag()[[input$tgtVar]], n = 7)
  #   })
  # qpal_cnt <- isolate(qpal())
  
  lf.quakes <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 30)) %>%
    addTiles() %>%
    addProviderTiles("Esri.OceanBasemap",group="OceanBasemap") %>%
    addProviderTiles("Esri.WorldImagery",group="WorldImagery") %>%

    addCircleMarkers(data = readFile_cnt,  #use the non-reactive dataset
                     color = "lightblue",
                     weight = 1,
                     fillOpacity = 0.7)

 edits <- callModule(editMod, "editor", leafmap = lf.quakes)


#now we need to create an observer so the leaflet map can 'observe' the reactive dataset and be redrawn based on the input

observeEvent(c(input$tgtVar),{
  proxy.lf <- leafletProxy(ns("map"))
  req(nrow(datasetMag())>0)

  proxy.lf %>%
    clearMarkers() %>%
    addCircleMarkers(data = datasetMag(),  #use the reactive dataset generated above
                     color = "red",
                     weight = 1,
                     fillOpacity = 0.7,
                     popup = popupTable(datasetMag(), zcol = input$tgtVar))
})


# generate the reactive dataset based on what is drawn on the leaflet map
  datasetInput <- reactive({

    req(edits()$finished)
    Tratamientos_sf <- edits()$finished
    Tratamientos_sf$Tratamiento <- LETTERS[seq_len(nrow(Tratamientos_sf))]
    Tratamientos_sf$Area <- st_area(edits()$finished)
    quake_intersect <- st_intersection(Tratamientos_sf, datasetMag()) #intersection between what is drawn and the reactive dataframe
    quake_intersect <- st_transform(quake_intersect, 32720)
    list("trat" = Tratamientos_sf,
         "sf" = quake_intersect)

  })

  #render a histogram with the reactive dataset as the output

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
