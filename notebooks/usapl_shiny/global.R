library(shiny)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(highcharter)
library(plotly)
library(bslib)
library(ggplot2)
library(shinydashboard)
library(bs4Dash)

## Load data
usapl = read_csv("data/usapl_PROJ.csv")
us_states = read_csv("data/us_states")
all_competitions = st_read("data/comp_types_PROJ.shp")
membercount = read_csv("data/membercount.csv")
membercountSF = st_read("data/membercount_SF.shp")


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

## Member Count
membercounttotal <- membercountSF |>
  group_by(MetLctn) |>
  summarize(total = sum(MmbrCnt)) |>
  ungroup()

# Change in Meet Count MAP
meetcountchange <- all_competitions |>
  mutate(Year = format(MeetDat, "%Y")) |>
  group_by(MetLctn, Year) |>
  arrange(Year) |>
  summarize(meet_count = n()) |>
  filter(as.numeric(Year) <= 2023) |>
  summarize("Avg Change Per Year" = ((last(meet_count)-first(meet_count)))/n()) |>
  ungroup()

# Change in Members Count MAP
membercountchange <- membercountSF |>
  group_by(MetLctn) |>
  arrange(Year) |>
  filter(Year <= 2023) |>
  slice(c(1, n())) |>
  mutate(Year = as.numeric(Year)) |>
  summarize("Avg Change Per Year" = ((last(MmbrCnt)-first(MmbrCnt)))/(last(Year)-first(Year)))

all_competitions_bymonth_count <- all_comps |>
  mutate(Month = format(MeetDat, "%b")) |> 
  group_by(MetLctn, Month, MeetNam) |>
  summarize(meet_count = n(), MeetDate = max(MeetDat)) |>
  arrange(desc(MeetDate)) |>
  slice_max(meet_count, with_ties = FALSE) |>
  select("Meet Name" = MeetNam, Month, Count = meet_count, State = MetLctn) |>
  ungroup()


# Pie chart info
all_competitions_bytype <- all_comps |>
  group_by(MetLctn, MeetTyp) |>
  select(MetLctn, MeetNam, MeetTyp) |>
  summarise(Count = n()) |>
  ungroup()

## USAPL DF
usapl_meets <- usapl |>
  select(`Meet Date`, `Meet Location`, `Meet Name`, `Meet Type`, c(7:29)) |>
  arrange(`Meet Location`, `Meet Date`, `Meet Name`) |>
  mutate(Year = format(`Date Format`, "%Y"))


## Labels in choropleth map ###############################

labels <- sprintf(
  "<strong>%s</strong><br/>Total Number of Meets: %s",
  meet_counts$MetLctn, meet_counts$meet_count) |>
  lapply(htmltools::HTML)

labels_member <- sprintf(
  "<strong>%s</strong><br/>Total Number of Members: %s",
  membercounttotal$MetLctn, membercounttotal$total) |>
  lapply(htmltools::HTML)


labels_change <- sprintf(
  "<strong>%s</strong><br/>Change in Meets : %s",
  meetcountchange$MetLctn,
  round(meetcountchange$"Avg Change Per Year", 4)
  ) |>
  lapply(htmltools::HTML)

labels_memberchange <- sprintf(
  "<strong>%s</strong><br/>Change in Members : %s",
  membercountchange$MetLctn,
  round(membercountchange$"Avg Change Per Year", 4)
  ) |>
  lapply(htmltools::HTML)
