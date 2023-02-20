#load all the usual libraries
library(skimr)
library(janitor)
library(tidyverse)
library(lubridate)

#let's load both csv files
current_holdings <- read.csv("latest_as_of_feb2023_csv.csv")
annual_changes <- read.csv("annual_changes_csv.csv")

#preview current_holdings because we want to start with this one
str(current_holdings)

#I found that percentage_of_reserves column is in chr, time to change it to num
current_holdings$percentage_of_reserves <- as.numeric(current_holdings$percentage_of_reserves)

#let's clean all n/a values
current_holdings <- drop_na(current_holdings)

#let's see if there's any trailing/leading whitespaces
current_holdings$country <- gsub("^\\s+|\\s+$", "", current_holdings$country)

#let's visualize it with leaflet
install.packages("leaflet")
install.packages("rgdal")

library(leaflet)
library(rgdal)

#I have csv file with lat and lon
country_lat_lon <- read.csv("country_list_with_lat_and_lon.csv")

#come on now...this is political. and country_lat_lon put it as "Taiwan" anyway
current_holdings['country'][current_holdings['country'] == 'Taiwan Province of China'] <- 'Taiwan'

#let's use full_join to combine them
current_holdings_lat_lon <- full_join(current_holdings, country_lat_lon, by = c("country"="Country"))

#let's create palette and text for leaflet
mypalette <- colorBin(
  palette ="viridis",
  domain = current_holdings_lat_lon$tonnes,
  na.color = "transparent", 
  bins = 8
)


mytext <- paste(
  "<b>Country:</b>", current_holdings_lat_lon$country, "<br />",
  "<b>Metric Tonnes:</b>", current_holdings_lat_lon$tonnes, "<br/>") %>%
  lapply(htmltools::HTML)

#let's create leaflet
leaflet_current_holdings <- leaflet(current_holdings_lat_lon) %>%
  addTiles() %>%
  setView(
    lng = -8, lat = 38, zoom = 1
  ) %>%
  
  addCircleMarkers(
    ~ Longitude..generated., ~ Latitude..generated., 
    fillColor = ~ mypalette(tonnes), 
    fillOpacity = 0.6, 
    color = "white", 
    radius = 4, 
    stroke = FALSE,
    label = mytext,
    labelOptions = labelOptions(
      style = list( 
        "font-weight" = "normal", 
        padding = "3px 8px"
      ), 
      textsize = "13px", 
      direction = "auto"
    ) 
  ) %>%
  
  addLegend( 
    pal = mypalette, 
    values = ~ tonnes, 
    opacity = 0.9,
    title = "Gold Holdings (Tonnes)", 
    position = "bottomright"
  )

#save leaflet file
library(htmlwidgets)
saveWidget(leaflet_current_holdings, file="leaflet_current_holdings.html")