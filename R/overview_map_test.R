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
  select(field_site_id, field_longitude, field_latitude, field_site_type, field_site_subtype) %>%
  mutate(Type = str_extract(field_site_type, 'Terrestrial'),
         Type = factor(ifelse(is.na(Type), field_site_subtype, Type),
                       levels = c('Lake', 'Wadeable Stream', 'Non-wadeable River', 'Terrestrial'))) %>%
  filter(Type != 'Terrestrial') |>
  rename(site_id = field_site_id)



# extract forecast info for aquatic sites
cutoff <- as.character(Sys.Date() - 30)
combined <- arrow::open_dataset("cache/parquet/aquatics") |>
  filter(reference_datetime == cutoff) |> collect()

## summarize site crps scores
site_crps <- combined |>
  group_by(site_id) |>
  mutate(crps_median = round(median(crps, na.rm = TRUE), digits = 2)) |>
  mutate(crps_iqr = round(IQR(crps, na.rm = TRUE), digits = 2)) |>
  mutate(num_model = n_distinct(model_id)) |>
  ungroup() |>
  distinct(site_id, .keep_all = TRUE) |>
  select(site_id, crps_median, crps_iqr, num_model) |>
  drop_na(crps_median)


us_sites_crps <- US_map |>
  left_join(site_crps, by = c('site_id')) |>
  drop_na(crps_median)
#
# test_map <- ggplot(us_sites_crps) +
#   borders('usa', fill="white") +
#   geom_point(aes(x = field_longitude, y = field_latitude, colour = crps_median, size = crps_iqr)) +
#   coord_sf(xlim = c(-163, -60), ylim = c(15, 70), expand = T) +
#   theme_minimal(base_size = 24) +
#   labs(x= 'Longitude', y = 'Latitude') +
#   scale_colour_gradient(name = 'CRPS Median') +
#   scale_size_continuous(name = 'CRPS IQR') +
#   theme(axis.title = element_blank(),
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         legend.position = c(0.2,0.4))
#
# test_map


us_sf <- st_as_sf(us_sites_crps, coords = c("field_longitude", "field_latitude"), crs = 4326)

tmap_mode("view")
tm_shape(us_sf) +
  tm_bubbles(col ="crps_median", size = 'crps_iqr', alpha = 0.5, xmod = 1, popup.vars = c('num_model', 'crps_median', 'crps_iqr'))



### Use 7 day forecasts for sites over 30 day period

# extract forecast info for aquatic sites
cutoff <- as.character(Sys.Date() - 60)
end_cutoff <- as.character(Sys.Date() - 30)

# s3 <- arrow::s3_bucket("neon4cast-scores/parquet/aquatics",
#                        endpoint_override = 'data.ecoforecast.org',
#                        anonymous = TRUE)


# identify a 30 day period
combined <- arrow::open_dataset("cache/parquet/aquatics") |>
  filter(reference_datetime >= cutoff,
         reference_datetime <= end_cutoff) |>
         collect()


clim_score_deduplicate <- combined |>
  distinct(model_id, site_id, reference_datetime, datetime, variable, .keep_all = TRUE)

clim_score_df <- clim_score_deduplicate |>
  mutate(horizon = as.numeric(datetime - as_date(reference_datetime))) |>
  # filter(model_id %in% top_river,
  #        !(site_id %in% lake_sites),
  filter(horizon <= 30,
         horizon >= 0,
         variable == 'temperature') |>
  select(reference_datetime, datetime, site_id, variable, crps, model_id, horizon) |>
  pivot_wider(names_from = model_id,
              values_from = crps) |>
  pivot_longer(cols = -c(reference_datetime, datetime, horizon, variable, climatology, site_id),
               names_to = 'model_id') |>
  mutate(difference_crps = climatology - value) |>
  mutate(model_skill = ifelse(difference_crps > 0, TRUE, FALSE))

clim_skill_df <- clim_score_df |>
  group_by(site_id) |>
  summarise()

# only use 7 day forecasts
weekly_crps <- combined |>
  mutate(horizon = as.numeric(datetime - as_date(reference_datetime))) |>
  filter(horizon <= 7,
         horizon >= 0)

clim_score_deduplicate_weekly <- weekly_crps |>
  distinct(model_id, site_id, reference_datetime, datetime, variable, .keep_all = TRUE)

# calculate difference of climatology
clim_score_df_weekly <- clim_score_deduplicate_weekly |>
  mutate(horizon = as.numeric(datetime - as_date(reference_datetime))) |>
  filter(variable == 'temperature') |>
  select(reference_datetime, datetime, site_id, variable, crps, model_id, horizon) |>
  pivot_wider(names_from = model_id,
              values_from = crps) |>
  pivot_longer(cols = -c(reference_datetime, datetime, horizon, variable, climatology, site_id),
               names_to = 'model_id') |>
  mutate(difference_crps = climatology - value) |>
  mutate(model_skill = ifelse(difference_crps > 0, TRUE, FALSE)) |>
  group_by(site_id) |>
  summarise(median(difference_crps, na.rm = TRUE),

            n_distinct(model_id),
            sum(model_skill, na.rm = TRUE))

#
# weekly_crps_df <- clim_score_deduplicate_weekly |>
#   select(model_id, site_id, reference_datetime, datetime, variable, crps)
#
# weekly_rejoin_df <- clim_score_df_weekly |>
#   left_join(weekly_crps)
#
# crps_grouping <- weekly_rejoin_df |>
#   filter(model_id != 'climatology') |>
#   group_by(site_id) |>
#   #mutate(crps_median = round(median(crps, na.rm = TRUE), digits = 2)) |>
#   #mutate(crps_iqr = round(IQR(crps, na.rm = TRUE), digits = 2)) |>
#   mutate(num_model = n_distinct(model_id)) |>
#   summarise(sum(model_skill, na.rm = TRUE)) |>
#   #mutate(clim_better = sum(model_skill, na.rm = TRUE)) |>
#   #mutate(clim_worse = count())
#   ungroup()|>
#   distinct(site_id, .keep_all = TRUE) |>
#   select(site_id, crps_median, crps_iqr, num_model) |>
#   drop_na(crps_median)


us_weekly_sites_crps <- US_map |>
  left_join(site_crps, by = c('site_id')) |>
  drop_na(crps_median)

us_sf <- st_as_sf(us_weekly_sites_crps, coords = c("field_longitude", "field_latitude"), crs = 4326)

tmap_mode("view")
tm_shape(us_sf) +
  tm_bubbles(col ="crps_median", size = 'crps_iqr', alpha = 0.5, xmod = 1, popup.vars = c('num_model', 'crps_median', 'crps_iqr'))
