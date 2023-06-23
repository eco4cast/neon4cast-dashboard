# Synthesis plots
library(tidyverse)

#grab from the local cache
site_data <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |>
  dplyr::filter(aquatics == 1)

lake_sites <- site_data |>
  filter(field_site_subtype == 'Lake')

cutoff <- as.character(Sys.Date() - 90)
combined <- arrow::open_dataset("cache/parquet/aquatics") |>
  filter(reference_datetime >= cutoff) |> collect() |>
  mutate(Type = ifelse(site_id %in% lake_sites$field_site_id, 'Lake', 'River'))



site_ids <- unique(combined$site_id)

site_model_id <- map_dfr(site_ids,
                         function(site, combined){
                           combined |>
                             filter(site_id == site) |>
                             summarise(n_mod = n_distinct(model_id)) |>
                             mutate(site_id = site)},
                         combined)

type_model_id <- combined |>
  distinct(model_id, Type) |>
  group_by(Type) |>
  summarise(n_mod = n())

lake_plot <- combined |>
  mutate(horizon = as.numeric(datetime - as_date(reference_datetime))) |>
  filter(site_id %in% lake_sites$field_site_id,
         horizon <= 30,
         horizon > 0,
         variable == 'temperature') |>
  select(reference_datetime, datetime, site_id, variable, crps, model_id, horizon) |>
  distinct(reference_datetime, datetime, site_id, variable,model_id, horizon, .keep_all = T) |>
  pivot_wider(names_from = model_id,
              values_from = crps) |>
  pivot_longer(cols = -c(reference_datetime, datetime, horizon, variable, climatology, site_id),
               names_to = 'model_id') |>
  mutate(difference_crps = climatology - value) |> # negative values, forecast worse than climatology
  select(horizon, model_id, difference_crps, site_id, variable) |>

  group_by(horizon, site_id, model_id) |>
  summarise(median_diff = median(difference_crps, na.rm = T)) |>
  mutate(skilled = ifelse(median_diff > 0, 1, 0)) |>
  drop_na() |>
  group_by(site_id, horizon) |>
  summarise(diff_crps = mean(median_diff, na.rm = T),
            n_skilled = sum(skilled, na.rm = T)) |>
  left_join(site_model_id) |>
  mutate(perc_skilled = 100*(n_skilled/n_mod),
         Type = ifelse(site_id %in% lake_sites$field_site_id, 'Lake', 'River'))

  # ggplot(aes(x=horizon, y = perc_skilled, group = site_id, colour = site_id)) +
  # geom_line(linewidth = 1.2) +
  # theme_bw(base_size = 20) +
  # scale_colour_viridis_d(option = 'turbo', name = 'Site') +
  # scale_fill_viridis_d(option = 'turbo', name = 'Site') +
  # labs(y='% skillful forecasts', title = 'Water temperature forecasts') +
  # theme(legend.position = 'right') +
  # guides(color = guide_legend(nrow = 5)) +
  # facet_wrap(~Type)




lakes_rivers_plot <- combined |>
  mutate(horizon = as.numeric(datetime - as_date(reference_datetime))) |>
  filter(#site_id %in% lake_sites$field_site_id,
         horizon <= 30,
         horizon > 0,
         variable == 'temperature') |>
  select(reference_datetime, datetime, site_id, variable, crps, model_id, horizon) |>
  distinct(reference_datetime, datetime, site_id, variable,model_id, horizon, .keep_all = T) |>
  pivot_wider(names_from = model_id,
              values_from = crps) |>
  pivot_longer(cols = -c(reference_datetime, datetime, horizon, variable, climatology, site_id),
               names_to = 'model_id') |>
  mutate(difference_crps = climatology - value,
         Type = ifelse(site_id %in% lake_sites$field_site_id, 'Lake', 'River')) |> # negative values, forecast worse than climatology
  select(horizon, model_id, difference_crps, site_id, variable, Type) |>

  group_by(horizon, Type, model_id) |>
  summarise(median_diff = median(difference_crps, na.rm = T)) |>
  mutate(skilled = ifelse(median_diff > 0, 1, 0)) |>
  drop_na() |>
  group_by(Type, horizon) |>
  summarise(diff_crps = mean(median_diff, na.rm = T),
            n_skilled = sum(skilled, na.rm = T)) |>
  left_join(type_model_id) |>
  mutate(perc_skilled = 100*(n_skilled/n_mod))
#
  # ggplot(aes(x=horizon, y = perc_skilled, group = Type, colour = Type)) +
  # geom_line(linewidth = 1.2) +
  # theme_bw(base_size = 20) +
  # scale_colour_viridis_d(option = 'turbo', name = 'Type') +
  # scale_fill_viridis_d(option = 'turbo', name = 'Type') +
  # labs(y='% skillful forecasts', title = 'Water temperature forecasts') +
  # theme(legend.position = 'right') +
  # guides(color = guide_legend(nrow = 5))

