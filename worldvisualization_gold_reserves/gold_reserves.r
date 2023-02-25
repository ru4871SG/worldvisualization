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

#let's visualize it with leaflet, install packages as needed

##install.packages("leaflet")
##install.packages("rgdal")
##install.packages("geojsonio")

library(leaflet)
library(rgdal)
library(geojsonio)

#let's get world_geojson for our beautiful map
#data source: https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json , I saved it locally just in case the original file gets deleted. 
world_geojson <- geojson_read("countries.geo.json", what = "sp")

#looks like some country names are inconsistent, we need to change them
current_holdings['country'][current_holdings['country'] == 'United States'] <- 'United States of America'
current_holdings['country'][current_holdings['country'] == 'Belarus, Rep. of'] <- 'Belarus'
current_holdings['country'][current_holdings['country'] == 'China, P.R.: Mainland'] <- 'China'
current_holdings['country'][current_holdings['country'] == 'Croatia, Rep. of'] <- 'Croatia'
current_holdings['country'][current_holdings['country'] == 'Czech Rep.'] <- 'Czech Republic'
current_holdings['country'][current_holdings['country'] == 'Egypt, Arab Rep. of'] <- 'Egypt'
current_holdings['country'][current_holdings['country'] == 'Russian Federation'] <- 'Russia'
current_holdings['country'][current_holdings['country'] == 'Kazakhstan, Rep. of'] <- 'Kazakhstan'
current_holdings['country'][current_holdings['country'] == 'Korea, Rep. of'] <- 'South Korea'
current_holdings['country'][current_holdings['country'] == 'Kyrgyz Rep.'] <- 'Kyrgyzstan'
current_holdings['country'][current_holdings['country'] == 'Mauritania, Islamic Rep. of'] <- 'Mauritania'
current_holdings['country'][current_holdings['country'] == 'Mozambique, Rep. of'] <- 'Mozambique'
current_holdings['country'][current_holdings['country'] == 'Netherlands, The'] <- 'Netherlands'
current_holdings['country'][current_holdings['country'] == 'North Macedonia, Republic of'] <- 'Macedonia'
current_holdings['country'][current_holdings['country'] == 'Poland, Rep. of'] <- 'Poland'
current_holdings['country'][current_holdings['country'] == 'Serbia, Rep. of'] <- 'Republic of Serbia'
current_holdings['country'][current_holdings['country'] == 'Slovenia, Rep. of'] <- 'Slovenia'
current_holdings['country'][current_holdings['country'] == 'Syrian Arab Republic'] <- 'Syria'
current_holdings['country'][current_holdings['country'] == 'Tajikistan, Rep. of'] <- 'Tajikistan'
current_holdings['country'][current_holdings['country'] == 'Uzbekistan, Rep. of'] <- 'Uzbekistan'
current_holdings['country'][current_holdings['country'] == 'Venezuela, Republica Bolivariana de'] <- 'Venezuela'
current_holdings['country'][current_holdings['country'] == 'Yemen, Republic of'] <- 'Yemen'

#let's merge them to our world_geojson
world_geojson <- sp::merge(world_geojson, current_holdings, by.x = "name", by.y = "country")

#let's create text label for leaflet
mytext <- paste(
  "<b>Country:</b>", world_geojson$name, "<br />",
  "<b>Metric Tonnes:</b>", world_geojson$tonnes, "<br/>") %>%
  lapply(htmltools::HTML)

#let's create the palette for the legend
mypalette <- colorNumeric(
  palette = "YlGnBu",
  domain = world_geojson$tonnes
)

#let's create the map now using leaflet
leaflet_current_holdings <- leaflet(world_geojson) %>%
              setView(
                lng = 0, lat = 38, zoom = 2 
              ) %>%
              addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.5,
                          fillColor = ~colorQuantile("YlGnBu", tonnes)(tonnes),
                          label = mytext,
                          highlightOptions = highlightOptions(color = "white", weight = 2,
                                                              bringToFront = TRUE)) %>%
              addLegend(
                pal = mypalette,
                values = ~tonnes,
                position = "topright"
              )

#save leaflet file, if needed
#library(htmlwidgets)
#saveWidget(leaflet_current_holdings, file="leaflet_current_holdings.html")