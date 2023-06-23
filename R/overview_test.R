## create a map with the NEON sites that contains info about number of forecasts and the skill score for AQUATICS

library(sf)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(maptools)
library(maps)


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
  mutate(crps_median = median(crps, na.rm = TRUE)) |>
  mutate(crps_iqr = IQR(crps, na.rm = TRUE)) |>
  ungroup() |>
  distinct(site_id, .keep_all = TRUE) |>
  select(site_id, crps_median, crps_iqr) |>
  drop_na(crps_median)


us_sites_crps <- US_map |>
  left_join(site_crps, by = c('site_id')) |>
  drop_na(crps_median)

test_map <- ggplot(us_sites_crps) +
  borders('world', fill="white") +
  geom_point(aes(x = field_longitude, y = field_latitude, colour = crps_median, size = crps_iqr)) +
  coord_sf(xlim = c(-163, -60), ylim = c(15, 70), expand = T) +
  theme_minimal(base_size = 24) +
  labs(x= 'Longitude', y = 'Latitude') +
  #scale_colour_manual(values = c('slateblue', 'steelblue', 'mediumblue', 'forestgreen')) +
  #scale_size_manual(values = c(2.1,2.1,2.1,1.5)) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.2,0.4),
        legend.title = element_blank())

test_map
