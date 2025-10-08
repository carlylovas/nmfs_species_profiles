# Distribution metrics

## Libraries ----
library(here)
library(tidyverse)
library(gmRi)
library(matrixStats)
library(Hmisc)

## Trawl and species data ----
clean_survey <- read_rds(here("data", "clean_survey.rds"))
sppclass <- read_csv(here("data", "sppclass.csv"))

## Weighted centers 
center_bio <- function(x, ...){
  x |>
    group_by(comname, ...) %>%
    summarise(
      # Un-weighted averages
      total_biomass   = sum(total_biomass_kg),
      avg_biomass     = mean(total_biomass_kg),
      biomass_sd      = sd(total_biomass_kg),
      # Weighted averages
      avg_lat         = weightedMean(lat, w = total_biomass_kg, na.rm = T),  
      avg_lon         = weightedMean(lon, w = total_biomass_kg, na.rm = T),
      avg_sst         = weightedMean(surftemp, w = total_biomass_kg, na.rm = T),
      avg_bot         = weightedMean(bottemp,  w = total_biomass_kg, na.rm = T),
      avg_depth       = weightedMean(depth, w = total_biomass_kg, na.rm = T),
      .groups = "drop") |>
    mutate(decade = 10*year%/%10)
}

## Percentiles ----
percentiles <- function(x, ...){
  x |> 
    pivot_longer(cols = c("lat", "lon", "surftemp", "bottemp", "depth"), 
                 names_to = "variable", values_to = "measurement") |>
    group_by(comname,variable, ...) |>
    summarise(
      "5%"   = wtd.quantile(measurement, weights = total_biomass_kg, probs = 0.05),
      "25%"  = wtd.quantile(measurement, weights = total_biomass_kg, probs = 0.25),
      "50%"  = wtd.quantile(measurement, weights = total_biomass_kg, probs = 0.50),
      "75%"  = wtd.quantile(measurement, weights = total_biomass_kg, probs = 0.75),
      "95%"  = wtd.quantile(measurement, weights = total_biomass_kg, probs = 0.95),
      .groups = "drop") |>
    pivot_longer(cols = "5%":"95%", names_to = "percentiles", values_to = "measurement") |>
    pivot_wider(names_from = "variable", values_from = "measurement") |>
    mutate(decade = 10*year%/%10,
           percentiles = factor(percentiles)) 
    # mutate(roll_mean =  zoo::rollapplyr(measurement, width = 5, FUN = mean, align = "center", partial = T))
}


## Annual ----
annual_centers <- center_bio(clean_survey, year)
annual_percentiles <- percentiles(clean_survey, year)

## Seasonal ----
seasonal_centers <- center_bio(clean_survey, year, season)
seasonal_percentiles <- percentiles(clean_survey, year, season)

## Save out ---- 
write.csv(annual_centers, here("data", "annual_centers.csv"))
write.csv(annual_percentiles, here("data", "annual_percentiles.csv"))
write.csv(seasonal_centers, here("data", "seasonal_centers.csv"))
write.csv(seasonal_percentiles, here("data", "seasonal_percentiles.csv"))

