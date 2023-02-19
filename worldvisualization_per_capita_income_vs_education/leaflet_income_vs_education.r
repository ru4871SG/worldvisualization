library(tidyverse)
library(leaflet)
library(rgdal)

education_vs_income <- read.csv("education_vs_income.csv")

mypalette <- colorBin(
  palette ="viridis",
  domain = education_vs_income$per_capita_income,
  na.color = "transparent", 
  bins = 8
)

mytext <- paste(
  "<b>County:</b>", education_vs_income$county, "<br />",
  "<b>Per Capita Income:</b> $", education_vs_income$per_capita_income, "<br/>",
  "<b>Bachelor's Degree Percentage:</b> ", education_vs_income$bachelor_degree, "%") %>%
  lapply(htmltools::HTML)

leaflet(education_vs_income) %>%
  addTiles() %>%
  setView(
    lng = -98, lat = 38, zoom = 3.5 
  ) %>%
  
  addCircleMarkers(
    ~ lon, ~ lat, 
    fillColor = ~ mypalette(per_capita_income), 
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
    values = ~ per_capita_income, 
    opacity = 0.9,
    title = "Per Capita Income", 
    position = "bottomright"
  )
