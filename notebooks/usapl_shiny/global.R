library(shiny)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(shiny.fluent)

## Load data
usapl = read_csv("data/usapl_PROJ.csv")
us_states = read_csv("data/us_states")
all_competitions = st_read("data/comp_types_PROJ.shp")


## Choices 
type_choices <- all_competitions |> 
  pull(MeetTyp) |> 
  unique() |> 
  sort()
  
state_choices <- all_competitions |> 
  pull(MetLctn) |> 
  unique() |> 
  sort()

year_choices <- all_competitions |>
  mutate(Year = year(MeetDat)) |> 
  pull(Year) |> 
  unique() |> 
  sort() 


## All competitionas variable
meet_counts <- all_competitions |>
  group_by(MetLctn) |>
  summarise(meet_count = n(), .groups = "drop")

meet_counts_change <- all_competitions |>
  mutate(MeetYear = as.integer(format(as.Date(MeetDat), "%Y")))  |>
  group_by(MetLctn, MeetYear)  |>
  summarise(meet_count = n(), .groups = "drop")

all_competitions_by_month <- all_competitions |> 
  mutate(Month = format(MeetDat, "%m")) |> 
  group_by(MetLctn, Month) |> 
  slice_head() |> 
  st_drop_geometry() |> 
  select("State" = MetLctn, "Meet Name" = MeetNam, 
         "Month of the year" = Month, "Meet Type" = MeetTyp)

meet_counts_by_year <- all_competitions |> 
  st_drop_geometry() |> 
  mutate(Year = as.integer(format(MeetDat, format = "%Y"))) |> 
  group_by(MetLctn, Year) |> 
  summarize(meet_count = n(), .groups = "drop")

## Labels in chloropleth map
labels <- sprintf(
  "<strong>%s</strong><br/>Total Number of Meets: %s",
  meet_counts$MetLctn, meet_counts$meet_count) |>
  lapply(htmltools::HTML)

labels_change <- sprintf(
  "<strong>%s</strong><br/>Change in Meets (Year %s): %s",
  meet_counts_change$MetLctn, meet_counts_change$MeetYear, meet_counts_change$meet_count) |>
  lapply(htmltools::HTML)
