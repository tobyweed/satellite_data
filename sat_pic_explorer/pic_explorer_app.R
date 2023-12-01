# The following tool generates the footprint of a random image from the dataset and puts it on a map:
  
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)

sat <- read_csv("sat.csv")

sat_sf <- st_as_sf(sat, wkt = "geometry")
sat_sf$area <- st_area(sat_sf)

sat_sf_trimmed <- sat_sf %>%
  filter(area != 0)

# Define UI for the Shiny app
ui <- fluidPage(
  actionButton("sampleButton", "See a random image"),
  leafletOutput("map"),
  uiOutput("link")
)

generate_sample <- function(n) {
  return(sample_n(sat_sf_trimmed, n))
}

get_link <- function(sample) {
  pic_url <-""
  
  if(sample$`Data Source` == "declass1") {
    pic_url <- paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e839febdccb64b3/",
                     sample$`Display ID`, sep = "")
  } else if(sample$`Data Source` == "declass2") {
    pic_url <- paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e839ff7d71d4811/",
                     sample$`Display ID`, sep = "")
  } else if(sample$`Data Source` == "declass3") {
    pic_url <- paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e7c41f3ffaaf662/",
                     sample$`Display ID`, sep = "")
  }
  
  link <- a("Click to See Image and Metadata", href = pic_url, target="_blank" )
  
  return(link)
}

# Define server logic
server <- function(input, output) {
  
  # Update the sampled data when the button is clicked
  observeEvent(input$sampleButton, {
    sample <- generate_sample(1)
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = sample)
    })
    
    # render link
    output$link <- renderUI({
      get_link(sample)
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)