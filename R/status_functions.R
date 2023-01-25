
## function for creating statistics for submissions over time
# also need to time variables for subsetting
cutoff_week <- as.character(Sys.Date() - 7)
cutoff_month <- as.character(Sys.Date() - 30)
cutoff_year <- as.character(Sys.Date() - 365)

forecast_time_summary <- function(time_cutoff, forecast_theme){
  cache_path <- paste0('cache/parquet/',forecast_theme)
  forecasts_df <- arrow::open_dataset("cache/parquet/aquatics") %>%
    filter(reference_datetime >= time_cutoff) %>%
    distinct(model_id, reference_datetime) %>%
    count(model_id) %>%
    collect()

  n_4cast <- sum(forecasts_df$n)
  return(n_4cast)
}


## function for creating targets table for each theme
theme_targets_table <- function(targets_df){
  df_summary <- targets_df %>%
    drop_na(observation) %>%
    group_by(site_id,variable) %>%
    summarize(max_date = max(lubridate::as_date(datetime)))

  targets_table <- reactable(df_summary,
            columns = list(max_date = colDef(name='max date')),
            defaultPageSize = 10,
            filterable = TRUE,
            highlight = TRUE)

  return(targets_table)
}


## function for creating forecast table for each theme
theme_forecast_table <- function(forecast_theme){
  cache_path <- paste0('cache/parquet/',forecast_theme)

  forecast_recent_dates <- arrow::open_dataset(cache_path) %>%
    group_by(model_id, variable) %>%
    summarise(recent_submission = max(reference_datetime)) %>%
    collect()

  forecast_team_submissions <- arrow::open_dataset(cache_path) %>%
    group_by(model_id, variable) %>%
    summarise(n_submissions = n_distinct(reference_datetime)) %>%
    collect()

  forecast_info <- forecast_recent_dates %>%
    left_join(forecast_team_submissions, by=c('model_id','variable'))

 final_table <- reactable(forecast_info,
                          columns = list(model_id = colDef(name='Model ID (team name)'),
                                         variable = colDef(name='Variable'),
                                         recent_submission  = colDef(name='Most Recent Submission'),
                                         n_submissions  = colDef(name='Number of Submissions')),
                          defaultPageSize = 10,
                          filterable = TRUE,
                          highlight = TRUE)

 return(final_table)
}
