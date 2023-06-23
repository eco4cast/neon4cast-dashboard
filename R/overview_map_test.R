## create a map with the NEON sites that contains info about number of forecasts and the skill score for AQUATICS

library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(maptools)
library(maps)
library(tmap)




# extract NEON aquatic site info
sites_map <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv")

US_map <- sites_map %>%
  select(field_site_id, field_longitude, field_latitude, field_site_type, field_site_subtype, field_site_name) %>%
  mutate(Type = str_extract(field_site_type, 'Terrestrial'),
         Type = factor(ifelse(is.na(Type), field_site_subtype, Type),
                       levels = c('Lake', 'Wadeable Stream', 'Non-wadeable River', 'Terrestrial')),
         field_site_name = str_replace(field_site_name, 'NEON', '')) %>%
  filter(Type != 'Terrestrial') |>
  rename(site_id = field_site_id)


### Use 7 day forecasts for sites over 30 day period

# extract forecast info for aquatic sites
cutoff <- as.character(Sys.Date() - 60)
end_cutoff <- as.character(Sys.Date() - 30)


# identify a 30 day period
combined <- arrow::open_dataset("cache/parquet/aquatics") |>
  filter(reference_datetime >= cutoff,
         reference_datetime <= end_cutoff) |>
         collect()


# only use 7 day forecasts
first_week_forecasts <- combined |>
  mutate(horizon = as.numeric(datetime - as_date(reference_datetime))) |>
  filter(horizon == 7)

clim_score_deduplicate_weekly <- first_week_forecasts |>
  distinct(model_id, site_id, reference_datetime, datetime, variable, .keep_all = TRUE)

# calculate difference of climatology

site_ids <- unique(clim_score_deduplicate_weekly$site_id)

site_model_id <- map_dfr(site_ids,
                         function(site, clim_score_deduplicate_weekly){
                           clim_score_deduplicate_weekly |>
                             filter(site_id == site) |>
                             summarise(n_mod = n_distinct(model_id)) |>
                             mutate(site_id = site)},
                         clim_score_deduplicate_weekly)

clim_score_df_weekly <- clim_score_deduplicate_weekly |>
  mutate(horizon = as.numeric(datetime - as_date(reference_datetime))) |>
  filter(variable == 'oxygen') |>
  select(reference_datetime, datetime, site_id, variable, crps, model_id, horizon) |>
  pivot_wider(names_from = model_id,
              values_from = crps) |>
  pivot_longer(cols = -c(reference_datetime, datetime, horizon, variable, climatology, site_id),
               names_to = 'model_id') |>
  mutate(difference_crps = climatology - value) |>
  group_by(model_id, site_id) |>
  summarise(med_diff_crps = median(difference_crps, na.rm = T), .groups = 'drop') |>
  mutate(model_skill = ifelse(med_diff_crps > 0, T, F)) |>
  ungroup() |>
  group_by(site_id) |>
  summarise(n_skilled = sum(model_skill, na.rm = T),
            crps_median = median(med_diff_crps, na.rm = TRUE)) |>
  full_join(site_model_id) |>
  mutate(perc_skilled = 100*(n_skilled/n_mod)) |>
  drop_na(crps_median) |> ## might remove this later
  full_join(US_map, by = c('site_id'))

# create spatial object and plot
us_sf <- st_as_sf(clim_score_df_weekly, coords = c("field_longitude", "field_latitude"), crs = 4326)

