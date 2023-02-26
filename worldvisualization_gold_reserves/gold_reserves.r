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

#let's create the palette for the legend. we use viridis for the consistency of the website
library(viridis)

viridis_colors <- viridis(100)
mypalette <- colorBin(viridis_colors, world_geojson$tonnes, bins = 8, na.color = "grey")

#let's create the map now using leaflet
leaflet_current_holdings <- leaflet(world_geojson) %>%
              setView(lng = 0, lat = 38, zoom = 2) %>%
              addPolygons(
                color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~mypalette(tonnes),
                label = mytext,
                highlightOptions = highlightOptions(
                  color = "white", weight = 2, bringToFront = TRUE
                )
              ) %>%
              addLegend(
                pal = mypalette,
                values = ~tonnes,
                position = "topright",
                opacity = 0.8,
                title = "in metric tonnes"
              )

#save leaflet file, if needed
#library(htmlwidgets)
#saveWidget(leaflet_current_holdings, file="gold_reserves_current_holdings.html")

#let's work with annual_changes and see if we can change the data frame to include all the numbers
#let's change the country names just like before
annual_changes['country'][annual_changes['country'] == 'United States'] <- 'United States of America'
annual_changes['country'][annual_changes['country'] == 'Belarus, Rep. of'] <- 'Belarus'
annual_changes['country'][annual_changes['country'] == 'China, P.R.: Mainland'] <- 'China'
annual_changes['country'][annual_changes['country'] == 'Croatia, Rep. of'] <- 'Croatia'
annual_changes['country'][annual_changes['country'] == 'Czech Rep.'] <- 'Czech Republic'
annual_changes['country'][annual_changes['country'] == 'Egypt, Arab Rep. of'] <- 'Egypt'
annual_changes['country'][annual_changes['country'] == 'Russian Federation'] <- 'Russia'
annual_changes['country'][annual_changes['country'] == 'Kazakhstan, Rep. of'] <- 'Kazakhstan'
annual_changes['country'][annual_changes['country'] == 'Korea, Rep. of'] <- 'South Korea'
annual_changes['country'][annual_changes['country'] == 'Kyrgyz Rep.'] <- 'Kyrgyzstan'
annual_changes['country'][annual_changes['country'] == 'Mauritania, Islamic Rep. of'] <- 'Mauritania'
annual_changes['country'][annual_changes['country'] == 'Mozambique, Rep. of'] <- 'Mozambique'
annual_changes['country'][annual_changes['country'] == 'Netherlands, The'] <- 'Netherlands'
annual_changes['country'][annual_changes['country'] == 'North Macedonia, Republic of'] <- 'Macedonia'
annual_changes['country'][annual_changes['country'] == 'Poland, Rep. of'] <- 'Poland'
annual_changes['country'][annual_changes['country'] == 'Serbia, Rep. of'] <- 'Republic of Serbia'
annual_changes['country'][annual_changes['country'] == 'Slovenia, Rep. of'] <- 'Slovenia'
annual_changes['country'][annual_changes['country'] == 'Syrian Arab Republic'] <- 'Syria'
annual_changes['country'][annual_changes['country'] == 'Tajikistan, Rep. of'] <- 'Tajikistan'
annual_changes['country'][annual_changes['country'] == 'Uzbekistan, Rep. of'] <- 'Uzbekistan'
annual_changes['country'][annual_changes['country'] == 'Venezuela, Republica Bolivariana de'] <- 'Venezuela'
annual_changes['country'][annual_changes['country'] == 'Yemen, Republic of'] <- 'Yemen'

#let's include top 10 countries by current_holdings, and join them with annual_changes
top_ten <- arrange(current_holdings, -tonnes) %>% head(10)
top_ten <- left_join(top_ten, annual_changes, by = c("country"="country"))

#let's get all the metric tonnes for the last 20 years
top_ten <- top_ten %>% mutate(tonnes_2021 = tonnes - X2022)
top_ten <- top_ten %>% mutate(tonnes_2020 = tonnes_2021 - X2021)
top_ten <- top_ten %>% mutate(tonnes_2019 = tonnes_2020 - X2020)
top_ten <- top_ten %>% mutate(tonnes_2018 = tonnes_2019 - X2019)
top_ten <- top_ten %>% mutate(tonnes_2017 = tonnes_2018 - X2018)
top_ten <- top_ten %>% mutate(tonnes_2016 = tonnes_2017 - X2017)
top_ten <- top_ten %>% mutate(tonnes_2015 = tonnes_2016 - X2016)
top_ten <- top_ten %>% mutate(tonnes_2014 = tonnes_2015 - X2015)
top_ten <- top_ten %>% mutate(tonnes_2013 = tonnes_2014 - X2014)
top_ten <- top_ten %>% mutate(tonnes_2012 = tonnes_2013 - X2013)
top_ten <- top_ten %>% mutate(tonnes_2011 = tonnes_2012 - X2012)
top_ten <- top_ten %>% mutate(tonnes_2010 = tonnes_2011 - X2011)
top_ten <- top_ten %>% mutate(tonnes_2009 = tonnes_2010 - X2010)
top_ten <- top_ten %>% mutate(tonnes_2008 = tonnes_2009 - X2009)
top_ten <- top_ten %>% mutate(tonnes_2007 = tonnes_2008 - X2008)
top_ten <- top_ten %>% mutate(tonnes_2006 = tonnes_2007 - X2007)
top_ten <- top_ten %>% mutate(tonnes_2005 = tonnes_2006 - X2006)
top_ten <- top_ten %>% mutate(tonnes_2004 = tonnes_2005 - X2005)
top_ten <- top_ten %>% mutate(tonnes_2003 = tonnes_2004 - X2004)

#for naming consistencies, let's change the tonnes column name
top_ten <- top_ten %>% rename(tonnes_2022 = tonnes)

#let's clean our data before we do pivot_longer
top_ten <- select(top_ten, country, tonnes_2003, tonnes_2004, tonnes_2005, tonnes_2006, tonnes_2007, tonnes_2008, tonnes_2009, tonnes_2010, tonnes_2011, tonnes_2012, tonnes_2013, tonnes_2014, tonnes_2015, tonnes_2016, tonnes_2017, tonnes_2018, tonnes_2019, tonnes_2020, tonnes_2021, tonnes_2022)

#now let's do pivot_longer
top_ten_pivot_longer <- top_ten %>% pivot_longer(names_to = "year", values_to = "tonnes", tonnes_2003:tonnes_2022)

#let's use stringr and ifelse to clean up the year names
library(stringr)
top_ten_pivot_longer$new_year <- str_extract(top_ten_pivot_longer$year, "\\d+")
top_ten_pivot_longer$year <- ifelse(str_detect(top_ten_pivot_longer$year, "^tonnes_"), paste0("", top_ten_pivot_longer$new_year), top_ten_pivot_longer$year)
top_ten_pivot_longer$new_year <- NULL

#let's change the data type of the year column
top_ten_pivot_longer$year <- as.numeric(top_ten_pivot_longer$year)

#let's use plotly to create interactive stacked bar
library(plotly)

plotly_top_ten_by_year <- plot_ly(top_ten_pivot_longer, x = ~year, y = ~tonnes, type = 'bar', color = ~country, text = ~country, hovertemplate = paste(
  "<b>%{text}</b><br>",
  "%{yaxis.title.text}: %{y:,.0f}<br>",
  "%{xaxis.title.text}: %{x:.}",
  "<extra></extra>"
)) %>% layout(dragmode = FALSE, barmode = "stack", xaxis = list(dtick = 1, tickformat = ".", title = "year"), yaxis = list(tickformat = ",.0f", title = "metric tonnes"), annotations = list(x = 0.5, y = 0.65, text = "worldvisualization.com", showarrow = F, opacity = 0.4, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=13)))