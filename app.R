
library(tidyverse)
library(shiny)
library(shinyjs)
library(leaflet)
library(DT)

## SETUP 
facs <- read_csv("facilities.csv")[-1]
missiles <- read_csv("missiles.csv")[-1]
fac_caps <- read_csv("fac_caps_with_unknown.csv")[-1]
miss_caps <- read_csv("miss_captures.csv")[-1]

# get date range
min_date = min(append(facs$start_date, missiles$start_date), na.rm = TRUE)
max_date = max(append(facs$start_date, missiles$start_date), na.rm = TRUE)



## CLIENT
ui <- navbarPage("Nuclear Recon Explorer",
  # custom styling
  tags$style(type = "text/css", "
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { }
        .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, .js-irs-1 .irs-from, .js-irs-1 .irs-to {background: red;
                                                    border-top: 1px solid red ;
                                                    border-bottom: 1px solid red ;}
        .subtitle { padding-left: 15px; 
                    color: #808080; }
      "),
  # titlePanel("Explore U.S. Satellite Reconnaissance of Nuclear Targets from 1941-1985"),
  # fluidRow(
  #   h3("Explore U.S. Satellite Reconnaissance of Nuclear Targets from 1941-1985", class = "subtitle"),
  #   br()
  # ),
  tabPanel("Map",
    sidebarLayout(
      sidebarPanel(HTML("<strong>Filter Facilities and Missiles</strong>"),
        checkboxInput("showUnknown","Show Facilities with Unknown Start Dates", value = FALSE),
        checkboxInput("showCaptures","Show Spotted Facilities and Missiles", value = FALSE),
        sliderInput("dateSlider", "Adjust Date:", 
                    min = min_date, 
                    max = max_date, 
                    value = min_date, 
                    step = 49)
                    # animate = animationOptions(interval = 250))
      ),
      mainPanel(
        leafletOutput("map1", height = "600px", width = "800px")
      )
    )
  ),
  
  tabPanel("Search",
       sidebarLayout(
           sidebarPanel(
             # ngens: number slider
             selectInput(inputId = "facility", 
                         label = "Select a facility:", 
                         choices = unique(facs$facility_name)),
             actionButton(inputId = "search",
                          label = "Search")
           ), 
           mainPanel(
             column(6,
                uiOutput(outputId = "fac_name"),
                uiOutput(outputId = "map2label"),
                leafletOutput("map2", height = "300px", width = "400px"),
                # uiOutput(outputId = "capture_plot_label"),
                # plotOutput(outputId = "capture_plot")
              ), 
             column(6,
                uiOutput(outputId = "capture_list")
             )
           )
       )
    )
)





##  ------------------------------- SERVER ---------------------------------- ##

server <- function(input, output, session) {
  state <- reactiveValues(fac = facs,
                          miss = missiles)
  
  # -------------------------------- UTILS -------------------------------------
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
  
  # return a vector of popup windows for captured facilities or missiles
  get_popups <- function(caps, is_facs){
    popups <- c()
    
    # fill in fields of popup windows for markers
    if(nrow(caps) >= 1){
      for (i in 1:nrow(caps)) {
        popup <- paste(ifelse(is_facs, paste(caps$facility_name[i], "<br>"), "Missile Site <br>"),
                       "Start Date: ", caps$start_date[i], "<br>",
                       "Most Recently Photographed: ", caps$`Acquisition Date`[i], "<br>",
                       a("Click to See Image and Metadata", href = caps$pic_URL[i], target="_blank" ))
        
        popups <- c(popups, popup)
      }
    }
    
    popups
  }
  
  # add markers where there should be markers
  create_markers <- function() {
    leafletProxy(mapId = 'map1') %>%
      clearMarkers() %>%
      addCircleMarkers(data = fac_filtered_by_dates(),
                       lng = ~lng, lat = ~lat,
                       radius = 2.8,
                       weight = 1,
                       color = "#2a297b",
                       opacity = 1,
                       fillOpacity = 1,
                       popup = ~paste(facility_name, "<br>",
                                      "Facility Start Date: ", start_date,
                                      ifelse(input$showCaptures, "<br>Not Yet Photographed", ""))
      ) %>%
      addMarkers(data = miss_filtered_by_dates(),
                 lng = ~lng, lat = ~lat,
                 icon = makeIcon(
                   iconUrl = "img/tri_#2a297b.png",
                   iconWidth = 7.5, iconHeight = 7.5
                 ),
                 popup = ~paste("Missile Site",
                                "<br>Start Date: ", start_date,
                                ifelse(input$showCaptures, "<br>Not Yet Photographed", "")))
    
    if (input$showCaptures) {
      fac_caps <- captured_facs()
      fac_lngs <- fac_caps$lng
      fac_lats <- fac_caps$lat
      fac_popups <- get_popups(caps = fac_caps, is_facs = TRUE)
      
      miss_caps <- captured_miss()
      miss_lngs <- miss_caps$lng
      miss_lats <- miss_caps$lat
      miss_popups <- get_popups(caps = miss_caps, is_facs = FALSE)
      
      
      leafletProxy(mapId = 'map1') %>%
        addCircleMarkers(data = fac_caps,
                         lng = fac_lngs, lat = fac_lats,
                         radius = 2.8,
                         weight = 1,
                         color = "#dd2119",
                         opacity = 1,
                         fillOpacity = 1,
                         popup = fac_popups
        ) %>%
        addMarkers(data = miss_caps,
                         lng = miss_lngs, lat = miss_lats,
                         icon = makeIcon(
                           iconUrl = "img/tri_#dd2119.png",
                           iconWidth = 7.5, iconHeight = 7.5
                         ),
                         popup = miss_popups
        )
    }
  }
  
  # map for Map page. run on startup. Adjust NAs and filter by date
  output$map1 <- renderLeaflet({
    map <- leaflet(leafletOptions( minZoom = 0 )) %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 1)
    map
  })
  
  # map for Search page.
  output$map2 <- renderLeaflet({
    map <- leaflet(leafletOptions( minZoom = 0 )) %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 1)
    map
  })
  
  
  
  # --------------------------- EVENT HANDLERS ---------------------------------
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
  
  # search for a facility
  observeEvent(eventExpr = { input$search }, handlerExpr = {
    fac_name <-  input$facility
    facility <- facs %>% filter(facility_name == fac_name)
    
    output$fac_name <- renderUI({
      HTML(paste("<strong>Facility Name:</strong>", fac_name, "<br><br>"))
    })
      
    output$map2label <- renderUI({
      HTML("<strong>Map:</strong>")})
    
    # create map marker, adjust map zoom
    leafletProxy(mapId = 'map2') %>%
      clearMarkers() %>%
      addCircleMarkers(data = facility,
                       lng = facility$lng, lat = facility$lat,
                       radius = 2.8,
                       weight = 1,
                       color = "#2a297b",
                       opacity = 1,
                       fillOpacity = 1
                       # popup = fac_popups
      ) %>%
      setView(lng = facility$lng, lat = facility$lat, zoom = 5)
    
    # output$capture_plot_label <- renderUI({
    #   HTML("<br><strong>Capture Plot: </strong>")
    # })
    
    # # create capture plot
    # capture_counts <- fac_caps %>%
    #   mutate(`Abbreviated Mission` = substr(Mission,1,4)) %>%
    #   filter(facility_name == fac_name) %>%
    #   group_by(facility_name, `Acquisition Date`, `Abbreviated Mission`, start_date) %>%
    #   summarise(n_caps = n()) 
    # 
    # yearly_counts <- capture_counts %>%
    #   mutate(`Year` = as.numeric(substr(`Acquisition Date`,1,4))) %>%
    #   group_by(facility_name, Year) %>%
    #   summarise(n_caps = sum(n_caps)) %>%
    #   ungroup()
    # 
    # yearly_counts %>% 
    #   ggplot(aes(x = Year, y = n_caps, group = 1)) +
    #   geom_line()  
    # 
    # output$capture_plot <- renderPlot({
    #   yearly_counts %>% 
    #     ggplot(aes(x = Year, y = n_caps, group = 1)) +
    #     geom_col()  
    # })
    
    output$capture_list_label <- renderUI({
      HTML("<strong>List of Capture Occurences: </strong>")
    })
    
    cap_table <- fac_caps %>%
      filter(facility_name == fac_name) %>%
      select(`Acquisition Date`, Mission, `Camera Resolution`, `Data Source`) %>%
      mutate()
    
    output$capture_list <- renderUI({
      HTML("<br><strong>Capture Plot: </strong>")
    })
    
  })
}

shinyApp(ui, server)




# # convert Camera Type field
# cam_letter <- recode(caps$`Camera Type`[i],
#                      "Vertical" = "V",
#                      "Aft" = "A",
#                      "Cartographic" = "C",
#                      "Forward" = "F"
# )
# 
# # build URL
# pic_url <- ""
# 
# if(caps$`Data Source`[i] == "declass1") {
#   pic_url <- paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e839febdccb64b3/",
#                    caps$`Display ID`[i], sep = "")
#   
#   #5e839febdccb64b3? 
# } else if(caps$`Data Source`[i] == "declass2") {
#   pic_url <- paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e839ff7d71d4811/",
#                    caps$`Display ID`[i], sep = "")
# } else if(caps$`Data Source`[i] == "declass3") {
#   pic_url <- paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e7c41f3ffaaf662/",
#                    caps$`Display ID`[i], sep = "")
# }