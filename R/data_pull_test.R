df<- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")

#forecast_sites <- c('BARC','CRAM','LIRO','PRLA','PRPO','SUGG')

df_interest <- df %>%
  filter(site_id %in% forecast_sites) %>%
  drop_na(observation) %>%
  group_by(site_id,variable) %>%
  summarize(max = max(lubridate::as_date(datetime)))

reactable(df_interest)




df_test <- neon4cast::noaa_stage2()

df_test1 <- neon4cast::noaa_stage1()

df2 <- df_test1 %>%
  filter(site_id == 'SUGG', start_date >= as.character('2023-01-22'), variable == 'TMP') %>%
  group_by(parameter, start_date) %>%
  summarise(max = max(horizon)) %>%
  collect()



#### TEST OUT COLLECTING AND SUMMARIZING FORECAST DATA (used for informing theme stats for each tab)
#aquatic_4cast <- arrow::open_dataset("cache/parquet/aquatics")

themes_list <- c('aquatics','phenology','ticks','beetles') #terrestrial name is different (need to ask)

theme_stats <- theme_statistics(themes_list)

reactable(theme_stats,
          columns = list(theme = colDef(name='Theme'),
                         n_teams = colDef(name='Number of Teams'),
                         n_submissions = colDef(name='Forecasts')),
          highlight = TRUE)


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

forecast_info <- arrow::open_dataset("cache/parquet/aquatics") %>%
  group_by(model_id, variable) %>%
  count(reference_datetime) %>%
  summarise(recent_submission = max(reference_datetime)) %>%
  ungroup() %>%
  #distinct(model_id,variable, .keep_all = TRUE) %>%
  collect()

