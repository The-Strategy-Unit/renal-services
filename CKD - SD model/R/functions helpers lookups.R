###

library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(openxlsx)
library(stringr)
library(readr)
library(ggplot2)
library(ggtrendline)
library(purrr)
library(extrafont)
library(StrategyUnitTheme)
library(cvdprevent)
library(lubridate)
library(zoo)


### extract data from CVD Prevent using Craig's package ---- 

# Regions are system level id 6, CKD prevalence is indicator 8
ckd_ind_sys_fn <- function(time) {
  result <- cvd_indicator_raw_data(indicator_id = 8, time_period_id = time, system_level_id = 6) |> 
    clean_names() |> 
    filter(category_attribute == "Persons", metric_category_name == "Persons", metric_category_type_name == "Sex") |>
    select(time_period_name, area_name, numerator, denominator, indicator_name)
}


# function to pull all-cause CVD mortality metric id 970 
ckd_mort_fn <- function(area) {
  result <- cvd_indicator_metric_timeseries(metric_id = 970, area_id = area) |> 
    clean_names() |> 
    dplyr::select(time_period_name, area_name, numerator, denominator, value) 
}

### functions to generate counts of occupancy for each time period ----
# used with UKRR patient-level data

#### occupancy/residency count ----

occupancy_count <- function(df,          # episode-level data
                            start_date,  # first date in time period being considered. enter as lubridate or force to date 
                            # if running in weeks, ensure start date is the day of the week you wish the week to start with (i.e. Monday)
                            end_date,    # last date in time period being considered
                            unit,        # time period  - "day", "week", "month"
                            ep_start_var, # variable name of start (admissions, episode_start_date etc)
                            ep_end_var,
                            ...           # additional variables for grouping by e.g. provider code, TFC 
) {
  
  run_len = length(seq(from = start_date, to = end_date, by = {{unit}}))
  
  keydates <- data.frame(unit_start = c(seq(start_date,
                                            by = {{unit}},
                                            length.out = run_len))) |> 
    mutate(unit_end = case_when(
      {{unit}} == "day" ~ unit_start,
      {{unit}} == "week" ~ unit_start + 6,
      {{unit}} == "month" ~ ceiling_date(unit_start, "month") - days(1),
      {{unit}} == "quarter" ~ ceiling_date(unit_start, "quarter") - days(1),
      {{unit}} == "year" ~ ceiling_date(unit_start, "year") - days(1)
    )
    )
  
  # rename columns of start and end of activity
  df <- df |> 
    rename(date_activity_start = {{ep_start_var}},
           date_activity_end = {{ep_end_var}})
  
  # pre-allocate tibble size to speed up iteration in loop
  activity_all <- tibble(nrow = nrow(df))  |>  
    select()
  #  activity_period <- tibble(nrow = nrow(df))
  
  for (i in 1:run_len) {
    
    activity_period <-  case_when(
      
      # creates 1 flag if resident for complete day
      df$date_activity_start < keydates$unit_start[i] & df$date_activity_end > keydates$unit_end[i] ~ 1,
      TRUE ~ 0)
    
    # column bind this day's flags to previous
    activity_all <- bind_cols(activity_all, activity_period)
    
  }
  
  # rename columns to match the day being counted
  activity_all <- activity_all |> 
    setNames(paste0("d_", keydates$unit_start))
  
  # bind flags columns to patient data then make long
  daily_adm <- bind_cols(df, activity_all) |> 
    pivot_longer(
      cols = starts_with("d_"),
      names_to = "date",
      values_to = "count"
    ) |> 
    
    group_by(date, ...) |> 
    summarise(resident = sum(count)) |> 
    ungroup() |> 
    mutate(date = str_remove(date, "d_"))
  
}

#### alternative - count on census day, first day of each quarter ----


census_count <- function(df,          # episode-level data
                         start_date,  # first date in time period being considered. enter as lubridate or force to date 
                         # if running in weeks, ensure start date is the day of the week you wish the week to start with (i.e. Monday)
                         end_date,    # last date in time period being considered
                         unit,        # time period  - "day", "week", "month"
                         ep_start_var, # variable name of start (admissions, episode_start_date etc)
                         ep_end_var,
                         ...           # additional variables for grouping by e.g. provider code, TFC 
) {
  
  run_len <- length(seq(from = start_date, to = end_date, by = "quarter"))
  
  keydates <- data.frame(keydate = c(seq(start_date,
                                         by = "quarter",
                                         length.out = run_len))) # |> 
  # mutate(unit_end = case_when(
  #   {{unit}} == "day" ~ unit_start,
  #   {{unit}} == "week" ~ unit_start + 6,
  #   {{unit}} == "month" ~ ceiling_date(unit_start, "month") - days(1),
  #   {{unit}} == "quarter" ~ ceiling_date(unit_start, "quarter") - days(1),
  #   {{unit}} == "year" ~ ceiling_date(unit_start, "year") - days(1)
  # )
  # )
  
  # rename columns of start and end of activity
  df <- df |> 
    rename(date_activity_start = {{ep_start_var}},
           date_activity_end = {{ep_end_var}})
  
  # pre-allocate tibble size to speed up iteration in loop
  activity_all <- tibble(nrow = nrow(df))  |>  
    select()
  #  activity_period <- tibble(nrow = nrow(df))
  
  for (i in 1:run_len) {
    
    activity_period <-  case_when(
      
      # creates 1 flag if resident for complete day
      df$date_activity_start < keydates$keydate[i] & df$date_activity_end > keydates$keydate[i] ~ 1,
      TRUE ~ 0)
    
    # column bind this day's flags to previous
    activity_all <- bind_cols(activity_all, activity_period)
    
  }
  
  # rename columns to match the day being counted
  activity_all <- activity_all |> 
    setNames(paste0("d_", keydates$keydate))
  
  # bind flags columns to patient data then make long
  daily_adm <- bind_cols(df, activity_all) |> 
    pivot_longer(
      cols = starts_with("d_"),
      names_to = "date",
      values_to = "count"
    ) |> 
    
    group_by(date, ...) |> 
    summarise(prev = sum(count)) |> 
    ungroup() |> 
    mutate(date = str_remove(date, "d_"))
  
}

