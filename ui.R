
library(shiny)
library(shinydashboard)

DataUplText <- "Data upload"
ZoneEditorText <- "Map Editor"
MeanCompText <- "Mean comparison"
DataExtractedText <- "Data extracted"
uploadFileText <- "Upload file"



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(DataUplText, tabName = "dataUpl", icon = icon("upload")),
    menuItem(ZoneEditorText, tabName = "zoneEditr", icon = icon("dashboard")),
    menuItem(MeanCompText, icon = icon("th"), tabName = "meanCompars",
             badgeLabel = "new", badgeColor = "green"),
    menuItem(DataExtractedText, icon = icon("broom"), tabName = "depuratedDatamen")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dataUpl",
            h2(DataUplText),
            mainPanel(
            fluidRow(
            fileInput(inputId = "file", label = uploadFileText, multiple = TRUE),
            column(4, 
            uiOutput("tgtVariable")),
            column(8,
                   uiOutput("histTgtVarUI")
                   )
            )
            )
            
    ),
    tabItem(tabName = "zoneEditr",
            h2(ZoneEditorText),
            mainPanel(
              fluidRow(
                
                editModUI("editor", height = 800),
                
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
                              width = 400, height = "auto",
                              
                              h4("Summary data"),
                              
                              plotOutput("plot"),
                              tableOutput("tablaAreas"))))) ,
    
    
    
    tabItem(tabName = "meanCompars",
            h2(MeanCompText),
            fluidRow(
              
              column(4,
                     box(width = NULL, status = "success",
                         tableOutput("TablaDescr")),
                     box(width = NULL, status = "success",
                         plotOutput("CompMedias")
                     )),
              column(8,
                     box(width = NULL, status = "info",
                         leafletOutput("depuratedData")
                     ))
              
            )
    ),
    tabItem(tabName = "depuratedDatamen",
            h2(DataExtractedText),
            fluidRow(
              column(4,
                     box(width = NULL, status = "success",
                         tableOutput("tablaDepur")),
                     ),
              column(8,
                     box(width = NULL, status = "info",
                         leafletOutput("conditionDepuratedData")
                     ))
              
            )
  )
)
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "ZoneCompare"),
  sidebar,
  body
)