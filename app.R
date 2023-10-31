library(tidyverse)
library(shiny)
library(shinyjs)
library(leaflet)


## SETUP 
facs <- read_csv("facilities.csv")[-1]
missiles <- read_csv("missiles.csv")[-1]
fac_caps <- read_csv("fac_captures.csv")[-1]
miss_caps <- read_csv("miss_captures.csv")[-1]

# get date range
min_date = min(append(facs$start_date, missiles$start_date), na.rm = TRUE)
max_date = max(append(facs$start_date, missiles$start_date), na.rm = TRUE)



## CLIENT
ui <- fluidPage(
  tags$style(type = "text/css", "
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { }
      .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, .js-irs-1 .irs-from, .js-irs-1 .irs-to {background: red;
                                                  border-top: 1px solid red ;
                                                  border-bottom: 1px solid red ;}
      "),
  titlePanel("Coverage of Nuclear Targets by the CORONA Recon Satellite Program"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("showUnknown","Show Facilities with Unknown Start Dates", value = FALSE),
      checkboxInput("showCaptures","Show Spotted Facilities and Missiles", value = FALSE),
      sliderInput("dateSlider", "Adjust Date:", 
                  min = min_date, 
                  max = max_date, 
                  value = min_date, 
                  step = 49,
                  animate = animationOptions(interval = 250))
    ),
    mainPanel(
      leafletOutput("map", height = "600px", width = "800px"),
      plotOutput("plot")
    )
  )
)





##SERVER

server <- function(input, output, session) {
  state <- reactiveValues(fac = facs,
                          miss = missiles)
  
  # handle unknown dates
  adjust_nas <- function() {
    state$fac <- facs
    
    if(input$showUnknown) {
      state$fac$start_date <- replace(state$fac$start_date, is.na(state$fac$start_date), min_date) # replace NA values with the min slider date
    } else {
      state$fac <- state$fac %>% filter(!is.na(start_date)) #filter them out
    }
  }
  
  # filter facilities by date range shown on slider
  fac_filtered_by_dates <- function() {
    state$fac[state$fac$start_date <= input$dateSlider,]
  }
  
  # filter missiles by date range shown on slider
  miss_filtered_by_dates <- function() {
    miss_not_expired <- ifelse(!is.na(state$miss$end_date), state$miss$end_date >= input$dateSlider, TRUE)
    state$miss[(state$miss$start_date <= input$dateSlider) & miss_not_expired,]
  }
  
  # filter for facilities which have been captured bf date
  captured_facs <- function() {
    fac_caps %>% filter(`Acquisition Date` <= input$dateSlider) %>% # filter for fac_caps bf current date
      filter(facility_name %in% state$fac$facility_name) %>% # filter facs which aren't in current state (e.g. NA)
      group_by(facility_name) %>% # group by facility
      filter(`Acquisition Date` == max(`Acquisition Date`)) %>% # keep the most recent fac_caps
      distinct(facility_name, .keep_all = TRUE) # make sure only one capture of each facility is present.
  }
  
  # filter for missiles which have been captured
  captured_miss <- function() {
    miss_caps %>% filter(`Acquisition Date` <= input$dateSlider) %>% # filter for miss_caps bf current date
      group_by(address_found) %>% # group by facility
      filter(`Acquisition Date` == max(`Acquisition Date`)) %>% # keep the most recent fac_caps
      distinct(address_found, .keep_all = TRUE) # make sure only one capture of each facility is present.
  }
  
  get_pic_URL <- function(declass_n, mission, direc_flag, cam_type, disp_ID) {
    # declass1
    url <- "broken link"
    # https://ims.cr.usgs.gov/browse/DIT/1117-2/086D/A/DS1117-2086DA001.jpg
    # https://ims.cr.usgs.gov/browse/DIT/Mission/`Direction Flag`/`Camera Type`(V/F/A)/`Display ID`.jpg
    print (disp_ID)
    # if (declass_n == "declass1") {
    
      cam_letter <- recode(cam_type,
                           "Vertical" = "V",
                           "Aft" = "A",
                           "Cartographic" = "C",
                           "Forward" = "F"
      )
      
      url <- paste("https://ims.cr.usgs.gov/browse/DIT/", mission, "/", direc_flag, "/", cam_letter,"/", disp_ID, ".jpg", sep = "")
    # }
    
    url
  }
  
  # add markers where there should be markers
  create_markers <- function() {
    leafletProxy(mapId = 'map') %>%
      clearMarkers() %>%
      addCircleMarkers(data = fac_filtered_by_dates(),
                       lng = ~lng, lat = ~lat,
                       radius = 5,
                       weight = 1,
                       color = "blue",
                       opacity = 1,
                       fillOpacity = 0.5,
                       popup = ~paste(facility_name, "<br>",
                                      "Facility Start Date: ", start_date,
                                      ifelse(input$showCaptures, "<br>Not Yet Photographed", "")),
      ) %>%
      # addMarkers(data = miss_filtered_by_dates(),
      #            lng = ~lng, lat = ~lat,
      #            icon = makeIcon(
      #              iconUrl = "img/plain-triangle.png",
      #              iconWidth = 10, iconHeight = 10,
      #              iconAnchorX = 10, iconAnchorY = 10
      #            ),
      addCircleMarkers(data = miss_filtered_by_dates(),
                       lng = ~lng, lat = ~lat,
                       radius = 5,
                       weight = 1,
                       color = "yellow",
                       opacity = 1,
                       fillOpacity = 0.5,
                 popup = ~paste("Missile Site",
                                "<br>Start Date: ", start_date,
                                ifelse(input$showCaptures, "<br>Not Yet Photographed", "")),
      )
    
    if (input$showCaptures) {
      
      cap_facs <- captured_facs()
      lngs <- cap_facs$lng
      lats <- cap_facs$lat
      popups <- c()
      
      # fill in fields of popup windows for markers
      if(length(cap_facs) >= 1){
        for (i in 1:length(cap_facs)) {
          # convert Camera Type field
          cam_letter <- recode(cap_facs$`Camera Type`[i],
                              "Vertical" = "V",
                              "Aft" = "A",
                              "Cartographic" = "C",
                              "Forward" = "F"
          )
          
          # build URL
          pic_url <- paste("https://ims.cr.usgs.gov/browse/DIT/", 
                           cap_facs$Mission[i], "/", 
                           cap_facs$`Direction Flag`[i], "/", 
                           cam_letter, "/",
                           cap_facs$`Display ID`[i], ".jpg", sep = "")
          
          popup <- paste(cap_facs$facility_name[i], "<br>",
                         "Facility Start Date: ", cap_facs$start_date[i], "<br>",
                         "Most Recently Photographed: ", cap_facs$`Acquisition Date`[i], "<br>",
                         a("Click to See Photo", href = pic_url, target="_blank" ))
          
          popups <- c(popups, popup)
        }
      }
      
      leafletProxy(mapId = 'map') %>%
        addCircleMarkers(data = cap_facs,
                         lng = lngs, lat = lats,
                         radius = 5,
                         weight = 1,
                         color = "red",
                         opacity = 1,
                         fillOpacity = 0.7,
                         popup = popups
        ) %>%
        addCircleMarkers(data = captured_miss(),
                         lng = ~lng, lat = ~lat,
                         radius = 5,
                         weight = 1,
                         color = "purple",
                         opacity = 1,
                         fillOpacity = 0.7,
                         popup = ~paste("Missile Site",
                                        "<br>Start Date: ", start_date,"<br>",
                                        "Most Recently Photographed: ", `Acquisition Date`),
        )
    }
  }
  
  # run on startup. Adjust NAs and filter by date
  output$map <- renderLeaflet({
    map <- leaflet(leafletOptions( minZoom = 0 )) %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 1)
    map
  })
  
  # re-create map whenever date slider is changed
  observeEvent(eventExpr = { input$dateSlider }, handlerExpr = {
    adjust_nas()
    create_markers()
  })
  
  # re-create map when showUnkown checkbox is toggled
  observeEvent(eventExpr = { input$showUnknown }, handlerExpr = {
    adjust_nas()
    create_markers()
  })
  
  # re-create map when showUnkown checkbox is toggled
  observeEvent(eventExpr = { input$showCaptures }, handlerExpr = {
    adjust_nas()
    create_markers()
  })
}

shinyApp(ui, server)