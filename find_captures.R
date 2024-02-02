# -----------------------------------------------------------------------------
# R Script: Identify Potential Capture Occurences
# Run this script to create a dataset of (potential) capture occurences of facilities and missiles.
# "Capture occurences" are any time an extant facility or missile site's coordinates fall within the coordinates
# of a satellite photo, regardless of whether the photo is low quality, covered with clouds, or otherwise unusable.
# Author: Toby Weed
# Date: 2024-02-02
# -----------------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinyjs)
library(leaflet)
library(sf)

facs <- read_csv("data/facilities.csv")[-1] # facility coordinates
missiles <- read_csv("data/missiles.csv")[-1] # missile coordinates
sat <- read_csv("data/sat.csv")[-1] # satellite photo polygons

# create Simple Features DFs
facs_sf <- st_as_sf(facs, coords = c("lng", "lat"), remove = FALSE)
miss_sf <- st_as_sf(missiles, coords = c("lng", "lat"), remove = FALSE)
sat_sf <- st_as_sf(sat, wkt = "geometry")
sat_sf <- sat_sf %>% mutate(polygon_geometry = geometry) # duplicate polygons because otherwise they'll be removed


# ------------------ Identify Facilities Capture Occurrences ---------------- ##

# Perform a spatial join to find points within polygons
fac_captures <- st_join(x = facs_sf, y = sat_sf, join = st_within)

# convert SF geometry columns to characters to avoid CSV file formatting issues
fac_captures$geometry <- st_as_text(fac_captures$geometry)
fac_captures$polygon_geometry <- st_as_text(fac_captures$polygon_geometry)

fac_captures <- fac_captures %>%
  mutate(pic_URL = ifelse(`Data Source` == "declass1",
                          paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e839febdccb64b3/", 
                                `Display ID`, sep = ""),
                          ifelse(`Data Source` == "declass2",
                                 paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e839ff7d71d4811/",
                                       `Display ID`, sep = ""),
                                 ifelse(`Data Source` == "declass3",
                                        paste("https://earthexplorer.usgs.gov/scene/metadata/full/5e7c41f3ffaaf662/",
                                              `Display ID`, sep = ""),
                                        "No URL available.")
                          )
  ))

# filter the dataset to remove cases where the photo was taken before construction started, INCLUDING unknown start dates
fac_caps_with_unknown <- fac_captures %>% 
  filter(ifelse(is.na(start_date), TRUE, start_date <= `Acquisition Date`))%>%
  as.data.frame() # convert from SF back to regular DF

write.csv(fac_caps_with_unknown, "data/fac_caps_with_unknown.csv")

# filter the dataset to remove cases where the photo was taken before construction started, EXCLUDING unknown start dates
fac_caps_no_unknown <- fac_captures %>% 
  filter(start_date <= `Acquisition Date`) %>%
  as.data.frame() # convert from SF back to regular DF


write.csv(fac_caps_no_unknown, "data/fac_caps_no_unknown.csv")




# ------------------ Identify Missiles Capture Occurrences ---------------- ##

# Perform a spatial join to find points within polygons
miss_captures <- st_join(x = miss_sf, y = sat_sf, join = st_within)

# convert SF geometry columns to characters to avoid CSV file formatting issues
miss_captures$geometry <- st_as_text(miss_captures$geometry)
miss_captures$polygon_geometry <- st_as_text(miss_captures$polygon_geometry)

# filter the dataset to remove cases where the photo was taken before construction started
# note on dates: it's not clear how accurate/complete dates are. Are NA end dates b/c data wasn't available or b/c the site never stopped being used? Are start dates accurate?
miss_captures <- miss_captures %>% 
  filter(start_date <= `Acquisition Date`,
         ifelse(!is.na(end_date), end_date >= `Acquisition Date`, TRUE)) %>%
  as.data.frame() # convert from SF back to regular DF

# miss_captures$`Display ID` <- as.character(miss_captures$`Display ID`) # this should be unec.

write.csv(miss_captures, "data/miss_captures.csv")

