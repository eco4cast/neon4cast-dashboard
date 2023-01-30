library(ggiraph)
library(patchwork)
library(tidyverse)
library(neon4cast)
library(score4cast)
library(vis4cast)
library(glue)
library(reactable)
library(httr)
library(reactablefmtr)

# df<- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")
#
# #forecast_sites <- c('BARC','CRAM','LIRO','PRLA','PRPO','SUGG')
#
# df_interest <- df %>%
#   filter(site_id %in% forecast_sites) %>%
#   drop_na(observation) %>%
#   group_by(site_id,variable) %>%
#   summarize(max = max(lubridate::as_date(datetime)))
#
# reactable(df_interest)
#

################################
combined_scores <- function(theme, collect = TRUE){

  vars <- neon4cast:::arrow_env_vars()

  #GENERALIZATION: THIS IS A SPECIFIC ENDPOINT
  s3 <- arrow::s3_bucket(bucket = paste0("neon4cast-scores/parquet/", theme),
                         endpoint_override = "data.ecoforecast.org",
                         anonymous = TRUE)
  ds <- arrow::open_dataset(s3, schema=score4cast::score_schema())
  if (collect) {
    ds <- dplyr::collect(ds)
  }
  on.exit(neon4cast:::unset_arrow_vars(vars))
  ds
}


theme_statistics <- function(themes){

  theme_stats <- purrr::map_dfr(themes, function(theme){

    message(theme)

    theme_scores <- combined_scores(theme = theme, collect = FALSE)

    message('data collected...starting calculations')

    teams <- theme_scores |>
      dplyr::summarise(n = dplyr::n_distinct(model_id)) |>
      dplyr::collect() |>
      dplyr::pull(n)

    forecasts <- theme_scores |>
      dplyr::select(model_id,reference_datetime, variable) |>
      dplyr::distinct() |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::summarise(total = sum(n)) |>
      dplyr::collect() |>
      dplyr::pull(total)

    # forecast_obs <- theme_scores |>
    #   dplyr::filter(!is.na(crps)) |>
    #   dplyr::summarise(n = n(), .groups = "drop") |>
    #   dplyr::summarise(total = sum(n)) |>
    #   dplyr::collect() |>
    #   dplyr::pull(total)

    output <- tibble::tibble(theme = theme,
                             n_teams = teams,
                             n_submissions = forecasts)#,
                             #n_obs_forecasts_pairs = forecast_obs)

    return(output)
  })

  return(theme_stats)

}


## this is for testing the processing time for statistics generation -- need to only call collect() once
themes_list <- c('aquatics','beetles','phenology','terrestrial_30min','terrestrial_daily','ticks')

test_stat <- theme_statistics(themes_list)


theme_aquatic <- combined_scores(theme = 'aquatics', collect = FALSE)

theme_aq2 <- theme_aquatic %>%
  dplyr::summarise(n = dplyr::n_distinct(model_id)) %>%
  dplyr::collect()

theme_aq3 <- theme_aquatic %>%
  dplyr::select(model_id,reference_datetime, variable) %>%
  dplyr::distinct() %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::summarise(total = sum(n)) %>%
  dplyr::collect() %>%
  dplyr::pull(total)

# df_test <- neon4cast::noaa_stage2()
#
# df_test1 <- neon4cast::noaa_stage1()
#
# df2 <- df_test1 %>%
#   filter(site_id == 'SUGG', start_date >= as.character('2023-01-22'), variable == 'TMP') %>%
#   group_by(parameter, start_date) %>%
#   summarise(max = max(horizon)) %>%
#   collect()



#### TEST OUT COLLECTING AND SUMMARIZING FORECAST DATA (used for informing theme stats for each tab)
#aquatic_4cast <- arrow::open_dataset("cache/parquet/aquatics")

themes_list <- c('aquatics','beetles','phenology','terrestrial_30min','terrestrial_daily','ticks')

theme_stats <- theme_statistics(themes_list)

#summed table of summaries
df_totals <- data.frame(theme = nrow(theme_stats),
                        n_teams = sum(theme_stats$n_teams),
                        n_submissions = sum(theme_stats$n_submissions),
                        n_obs_forecasts_pairs = sum(theme_stats$n_obs_forecasts_pairs))

bound_summary_table <- rbind(theme_stats, df_totals)

reactable(bound_summary_table,
          defaultColDef = colDef(
            align = "center"),
          columns = list(theme = colDef(name='Theme'),
                         n_teams = colDef(name='Number of Teams'),
                         n_submissions = colDef(name='Forecasts'),
                         n_obs_forecasts_pairs = colDef(name='Forecast/Observation Pairs')),
          highlight = TRUE,
          bordered = TRUE)

# reactable(df_totals,
#           defaultColDef = colDef(
#             align = "center"),
#           columns = list(theme_total = colDef(name='Total Themes'),
#                          team_total = colDef(name='Total Teams'),
#                          submission_total = colDef(name='Total Submissions'),
#                          forecast_obs_total = colDef(name='Total Forecast/Obs Pairs')),
#           bordered = TRUE)



#### TEST OUT COLLECTING DATA FROM SPECIFIC TIME RANGES
cutoff_week <- as.character(Sys.Date() - 7)
cutoff_month <- as.character(Sys.Date() - 30)
cutoff_year <- as.character(Sys.Date() - 365)

forecast_time_summary <- function(time_cutoff, forecast_theme){
  cache_path <- paste0('cache/parquet/',forecast_theme)
  forecasts_df <- arrow::open_dataset(cache_path) %>%
    filter(reference_datetime >= time_cutoff) %>%
    distinct(model_id, reference_datetime) %>%
    count(model_id) %>%
    collect()

  n_4cast <- sum(forecasts_df$n)
  return(n_4cast)
}

aquatic_forecasts_week <- forecast_time_summary(cutoff_week,'aquatics')
aquatic_forecasts_month <- forecast_time_summary(cutoff_month,'aquatics')
aquatic_forecasts_year <- forecast_time_summary(cutoff_year,'aquatics')



## TEST MAKING FORECAST TABLE FOR EACH THEME

forecast_recent_dates <- arrow::open_dataset("cache/parquet/aquatics") %>%
  group_by(model_id, variable) %>%
  summarise(recent_submission = max(reference_datetime)) %>%
  collect()

forecast_team_submissions <- arrow::open_dataset("cache/parquet/aquatics") %>%
  group_by(model_id, variable) %>%
  summarise(n_submissions = n_distinct(reference_datetime)) %>%
  collect()

forecast_info <- forecast_recent_dates %>%
  left_join(forecast_team_submissions, by=c('model_id','variable'))

df_test5 <- reactable(forecast_info,
          defaultSorted = list(recent_submission = 'desc'),
          columns = list(model_id = colDef(name='Model ID (team name)'),
                         variable = colDef(name='Variable'),
                        recent_submission  = colDef(name='Most Recent Submission'),
                        n_submissions  = colDef(name='Number of Submissions')),
          defaultPageSize = 20,
          filterable = TRUE,
          highlight = TRUE)

df_test5 %>% reactablefmtr::add_title('Forecasts',
                                      align = 'center',
                                      font_size = '30',
                                      margin = margin(t=40,r=0.5,l=0.5,b=0.5))
