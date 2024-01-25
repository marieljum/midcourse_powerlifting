library(shiny)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(highcharter)
library(plotly)
library(bslib)
library(ggplot2)

## Load data
usapl = read_csv("data/usapl_PROJ.csv")
us_states = read_csv("data/us_states")
all_competitions = st_read("data/comp_types_PROJ.shp")


## Choices ##################################################
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

meetname_choices <- all_competitions |> 
  pull(MeetNam) |> 
  unique() |> 
  sort()


## All competitions variable ###################################

# All_comps DF 
all_comps <- all_competitions |> 
  st_drop_geometry()

meets_in_state <- all_comps |> 
  group_by(MetLctn) |> 
  mutate(Year = year(MeetDat)) |> 
  select(MetLctn, MeetNam, MeetTyp, Year) |> 
  arrange(MetLctn, Year) |> 
  ungroup()

meet_counts_by_year <- all_comps |>
  mutate(Year = year(MeetDat)) |> 
  group_by(MetLctn, Year) |> 
  summarise(Count = n()) |> 
  ungroup()

meetcounts_wider <- meet_counts_by_year |>
  ungroup() |> 
  arrange(Year) |>
  pivot_wider(names_from = Year, values_from = Count, values_fill = 0) |>
  arrange(MetLctn) |>
  rename(State = MetLctn) |> 
  mutate(Total = rowSums(across(-State)))

meet_counts <- all_competitions |>
  group_by(MetLctn) |>
  summarise(meet_count = n()) |> 
  ungroup()

all_competitions_change_thrutime <- all_competitions |> 
  mutate(Year = format(MeetDat, "%Y")) |> 
  group_by(MetLctn, Year) |> 
  arrange(Year) |> 
  summarize(meet_count = n()) |> 
  summarize("Avg Growth Per Year" = ((last(meet_count)-first(meet_count)))/n())

all_competitions_bymonth_count <- all_comps |> 
  mutate(Month = format(MeetDat, "%m")) |> 
  group_by(MetLctn, Month, MeetNam) |> 
  summarize(meet_count = n(), MeetDate = max(MeetDat)) |> 
  arrange(desc(MeetDate)) |> 
  slice_max(meet_count, with_ties = FALSE) |> 
  select("Meet Name" = MeetNam, Month, Count = meet_count, State = MetLctn) |> 
  ungroup()

# meet_counts_by_year <- all_comps |> 
#   mutate(Year = as.integer(format(MeetDat, format = "%Y"))) |> 
#   group_by(MetLctn, Year) |> 
#   summarize(meet_count = n(), .groups = "drop")

# Pie chart info 
all_competitions_bytype <- all_comps |> 
  group_by(MetLctn, MeetTyp) |>   
  select(MetLctn, MeetNam, MeetTyp) |> 
  summarise(Count = n()) |> 
  ungroup()

## Meet results variables #################################
usapl_meets <- usapl |> 
  select(`Meet Date`, `Meet Location`, `Meet Name`, `Meet Type`, c(7:29)) |> 
  arrange(`Meet Location`, `Meet Date`, `Meet Name`) |> 
  mutate(Year = format(`Date Format`, "%Y"))


## Labels in choropleth map ###############################

labels <- sprintf(
  "<strong>%s</strong><br/>Total Number of Meets: %s",
  meet_counts$MetLctn, meet_counts$meet_count) |>
  lapply(htmltools::HTML)

# # Create labels for each row
# year_columns <- names(meetcounts_wider)[-1]
# 
#
# labels <- meetcounts_wider |> 
#   rowwise() |> 
#   mutate(
#     label = list(
#       select(across(-State)) |> 
#         filter(across(everything(), ~ . > 0)) |> 
#         mutate(
#           label = sprintf("<strong>%s</strong><br/>Total Number of Meets: %s<br/>Meets in %s: %s",
#                           State, Total, cur_column(), get(cur_column()))
#         ) |> 
#         pull(label)
#     ) |> 
#       paste(collapse = "<br/>")
#   ) |> 
#   pull(label)

labels_change <- sprintf(
  "<strong>%s</strong><br/>Change in Meets : %s",
  all_competitions_change_thrutime$MetLctn, all_competitions_change_thrutime$"Avg Growth Per Year") |>
  lapply(htmltools::HTML)
