library(shiny)
library(maps)
library(readxl)
library(tidyverse)
library(mapview)
library(pillar)
library(sf)
library(dplyr)
library(tmaptools)
library(leaflet)
library(htmltools)
library(shinydashboard)

crime <- read_excel("crime.xls", sheet = "egesz")
df <- data.frame(crime)

df$Lat = as.numeric(df$Lat)
df$Lon = as.numeric(df$Lon)
options(digits = 9)

markericon <- makeIcon(
  iconUrl = "location.png",
  iconWidth = 50, iconHeight = 50,
  iconAnchorX = 25, iconAnchorY = 55
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(selectInput("selectstate", "Válassz államot:", unique(df$State)),
                   selectizeInput("selectcity", "Válassz települést: ", choices = c(df$City))
  ),
  dashboardBody(
    leafletOutput("mymap"),
    br(),
    infoBoxOutput("box", width = 12)
  )
  
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ss <- reactive({
    df %>% filter(State == input$selectstate)
  })
  observeEvent(ss(),({
    updateSelectizeInput(session = session, "selectcity", choices = ss()$City)
  }))
  xx <- reactive({
    df %>% filter(State == input$selectstate, City == input$selectcity)
  })
  
  output$mymap <- renderLeaflet({
    
    leaflet(df %>%
              dplyr::filter(
                City == input$selectcity
              )) %>%
      addTiles() %>%
      addMarkers(lat = ~Lat, lng = ~Lon, icon = markericon, label = ~City, group = "markers", popup = ~paste(City, "<br>State:", State,
                                                                                                             "<br>Population", Population))
  })
  
  output$box <- renderInfoBox({
    infoBox("Adatok: ", input$selectcity, HTML(paste(
      "Népesség:", xx()$Population,  
      "<br>Erőszakos bűncselekmény:",  xx()$Violent, 
      "<br>Gyilkosság:", xx()$Murder, 
      "<br>Nemi erőszak:", xx()$Rape, 
      "<br>Rablás:", xx()$Robbery, 
      "<br>Testisértés:", xx()$Assault,
      "<br>Vagyon elleni bűncselekmény:", xx()$Property, 
      "<br>Betöréses lopás:", xx()$Burglary, 
      "<br>Lopás:", xx()$Theft,
      "<br>Gépjárműlopás:", xx()$Motortheft, 
      "<br>Gyújtogatás:", xx()$Arson)))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
