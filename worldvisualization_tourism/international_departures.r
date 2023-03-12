#load all the usual libraries
library(skimr)
library(janitor)
library(tidyverse)
library(lubridate)

#let's import the datasets
int_departures <- read.csv("dep.csv")
gdp <- read.csv("gdp.csv")
population <- read.csv("total_population.csv")

#notes: departures for CHN is combined between china mainland, hong kong, and macao. This may lead to slight inaccuracy for our "departures vs. population" and "GDP" correlation later. The reason why we use the departures SUM of the three countries is because the geojson map data does combine them all into one polygon shape.

#now we clean up columns we don't use
int_departures <- int_departures %>% select(Country.Name, Country.Code, X2019, X2018, X2017, X2016, X2015)
gdp <- gdp %>% select(Country.Name, Country.Code, X2019, X2018, X2017, X2016, X2015)
population <- population %>% select(Country.Name, Country.Code, X2019, X2018, X2017, X2016, X2015)

#let's put them all together into one data frame
dfs <- list(int_departures_cleaned,gdp,population)

int_departures_correlation <- dfs %>% reduce(inner_join, by = "Country.Code")

#let's clean up the new data frame
int_departures_correlation <- int_departures_correlation %>% 
  mutate(departures = case_when(
    !is.na(X2019.x) ~ X2019.x,
    !is.na(X2018.x) & is.na(X2019.x) ~ X2018.x,
    !is.na(X2017.x) & is.na(X2019.x) & is.na(X2018.x) ~ X2017.x,
    !is.na(X2016.x) & is.na(X2019.x) & is.na(X2018.x) & is.na(X2017.x) ~ X2016.x,
    TRUE ~ X2015.x
  )
  )

int_departures_correlation <- int_departures_correlation %>% 
  mutate(population = case_when(
    !is.na(X2019) ~ X2019,
    !is.na(X2018) & is.na(X2019) ~ X2018,
    !is.na(X2017) & is.na(X2019) & is.na(X2018) ~ X2017,
    !is.na(X2016) & is.na(X2019) & is.na(X2018) & is.na(X2017) ~ X2016,
    TRUE ~ X2015
  )
  )

int_departures_correlation <- int_departures_correlation %>% 
  mutate(gdp = case_when(
    !is.na(X2019.y) ~ X2019.y,
    !is.na(X2018.y) & is.na(X2019.y) ~ X2018.y,
    !is.na(X2017.y) & is.na(X2019.y) & is.na(X2018.y) ~ X2017.y,
    !is.na(X2016.y) & is.na(X2019.y) & is.na(X2018.y) & is.na(X2017.y) ~ X2016.y,
    TRUE ~ X2015.y
  )
  )

int_departures_correlation <- int_departures_correlation %>% mutate(country = Country.Name, code = Country.Code)

int_departures_correlation <- int_departures_correlation %>% select(country, code, departures, gdp, population)

#I just realized there are non-countries here..thank God I have cleaned up country and continent list before. Let's use the same CSV
country_and_continent <- read.csv("continent_and_country_list.csv")

#let's use left join to filter them
int_departures_correlation_cleaned <- left_join(country_and_continent, int_departures_correlation, by = c("ISO"="code")) %>% mutate(country = country.y) %>% select(country, ISO, region_1, continent, departures, gdp, population)

#let's divide departures by total population
int_departures_correlation_cleaned <- int_departures_correlation_cleaned %>% mutate(departures_vs_population = departures / population)

#let's create the correlation
library(plotly)

plotly_departures_correlation <- plot_ly(data = int_departures_correlation_cleaned) %>%
  add_trace(x = ~gdp, 
            y = ~departures_vs_population, 
            mode = 'markers', 
            type = 'scatter',
            text = ~country,
            color = ~continent,
            hovertemplate = paste(
              "<b>%{text}</b><br>",
              "%{xaxis.title.text}: %{x:$,.0f}<br>",
              "%{yaxis.title.text}: %{y:,.2f}"
            ),
            showlegend = TRUE
  ) %>% layout(yaxis = list(title = "departures vs. population"), xaxis = list(title = "GDP"), dragmode = FALSE)

#let's find the Pearson coefficient as well
cor.test(int_departures_correlation_cleaned$gdp, int_departures_correlation_cleaned$departures_vs_population)

#above test shows cor = 0.6944, which means there is a strong association between GDP as compared to departures divided by population

#let's visualize the departures vs. population with leaflet

library(leaflet)
library(rgdal)
library(geojsonio)

#let's get world_geojson for our beautiful map
world_geojson_dep <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")

#let's merge them
world_geojson_dep <- sp::merge(world_geojson_dep, int_departures_correlation_cleaned, by.x = "id", by.y = "ISO")

#let's create the text label for leaflet
mytext_dep <- paste(
  "<b>Country:</b>", world_geojson_dep$name, "<br />",
  "<b>Departures vs. Population:</b>", format(world_geojson_dep$departures_vs_population, big.mark = ","), "<br/>") %>%
  lapply(htmltools::HTML)

#let's create the palette for the legend. we use viridis for the consistency of the website
library(viridis)

viridis_colors <- viridis(100)
mypalette_dep <- colorBin(viridis_colors, world_geojson_dep$departures_vs_population, bins = 7, na.color = "grey")

#let's create the map using leaflet
leaflet_int_departures <- leaflet(world_geojson_dep) %>%
  setView(lng = 0, lat = 42, zoom = 2) %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    fillColor = ~mypalette_dep(departures_vs_population),
    label = mytext_dep,
    highlightOptions = highlightOptions(
      color = "white", weight = 2, bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = mypalette_dep,
    values = ~departures_vs_population,
    position = "bottomright",
    opacity = 0.8,
    title = "Departures vs. Population"
  )