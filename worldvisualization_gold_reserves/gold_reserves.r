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