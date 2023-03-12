#load all the usual libraries
library(skimr)
library(janitor)
library(tidyverse)
library(lubridate)

#let's import the first dataset
int_arrivals <- read.csv("arr.csv")


#now we clean up columns we don't use
int_arrivals_cleaned <- int_arrivals %>% select(Country.Name, Country.Code, X2019, X2018, X2017, X2016, X2015)
int_arrivals_cleaned <- int_arrivals_cleaned %>% 
  mutate(arrivals = case_when(
    !is.na(X2019) ~ X2019,
    !is.na(X2018) & is.na(X2019) ~ X2018,
    !is.na(X2017) & is.na(X2019) & is.na(X2018) ~ X2017,
    !is.na(X2016) & is.na(X2019) & is.na(X2018) & is.na(X2017) ~ X2016,
    TRUE ~ X2015
  )
  )

#let's visualize it with leaflet, install packages as needed

#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("geojsonio")

library(leaflet)
library(rgdal)
library(geojsonio)

#let's get world_geojson for our beautiful map
world_geojson <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")

#let's merge int_arrivals to the world_geojson
world_geojson <- sp::merge(world_geojson, int_arrivals_cleaned, by.x = "id", by.y = "Country.Code")

#let's create the text label for leaflet
mytext <- paste(
  "<b>Country:</b>", world_geojson$name, "<br />",
  "<b>Arrivals:</b>", format(world_geojson$arrivals, big.mark = ","), "<br/>") %>%
  lapply(htmltools::HTML)

#let's create the palette for the legend. we use viridis for the consistency of the website
library(viridis)

viridis_colors <- viridis(100)
mypalette <- colorBin(viridis_colors, world_geojson$arrivals, bins = 7, na.color = "grey")

#let's create the map using leaflet
leaflet_int_arrivals <- leaflet(world_geojson) %>%
    setView(lng = 0, lat = 42, zoom = 2) %>%
    addPolygons(
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.5,
      fillColor = ~mypalette(arrivals),
      label = mytext,
      highlightOptions = highlightOptions(
        color = "white", weight = 2, bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = mypalette,
      values = ~arrivals,
      position = "bottomright",
      opacity = 0.8,
      title = "International Arrivals"
    )