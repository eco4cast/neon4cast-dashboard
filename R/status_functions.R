
## function for creating statistics for submissions over time
# also need to time variables for subsetting
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

forecast_all_summary <- function(forecast_theme){
  cache_path <- paste0('cache/parquet/',forecast_theme)
  forecasts_df <- arrow::open_dataset(cache_path) %>%
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
            defaultColDef = colDef(
              align = "left"),
            columns = list(max_date = colDef(name='max date')),
            defaultPageSize = 10,
            filterable = TRUE,
            highlight = TRUE) %>%
    reactablefmtr::add_title('Targets',
                             align = 'center',
                             font_size = '30',
                             margin = margin(t=35,r=1,l=1,b=5))

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
                          defaultColDef = colDef(
                            align = "left"),
                          defaultSorted = list(recent_submission = 'desc'),
                          columns = list(model_id = colDef(name='Model ID (team name)'),
                                         variable = colDef(name='Variable'),
                                         recent_submission  = colDef(name='Most Recent Submission'),
                                         n_submissions  = colDef(name='Number of Submissions')),
                          defaultPageSize = 10,
                          filterable = TRUE,
                          highlight = TRUE) %>%
   reactablefmtr::add_title('Forecasts',
                            align = 'center',
                            font_size = '30',
                            margin = margin(t=35,r=1,l=1,b=5))

 return(final_table)
}


## function for health check table

health_check_table <- function(context){

  health_check <- GET(
    "https://healthchecks.io/api/v1/checks/",
    accept_json(),
    add_headers('X-Api-Key' = 'YKpvNOQWGtcGtyKu4rgC0Z7_0AQFo4kj')
  )

  check_parsed <- content(health_check,as='parsed')$checks

  hc_names <- c()
  hc_status <- c()
  hc_latest <- c()

  for (i in seq.int(1,length(check_parsed))){
    hc_names[i] <- check_parsed[[i]][[1]] # json objects 1,7,9 are name, status, and last ping
    hc_status[i] <- check_parsed[[i]][[7]]
    hc_latest[i] <- check_parsed[[i]][[9]]
  }

  check_df <- data.frame(hc_names,hc_status,hc_latest)
  check_df$hc_latest <- as.character(lubridate::as_datetime(check_df$hc_latest))

  ## this is used for making a NOAA specific process table
  if (context == 'NOAA'){
    noaa_processes <- c('NOAA GEFS: stage 1','NOAA GEFS: stage 2','NOAA GEFS: stage 3')

    checks_noaa <- check_df %>%
      filter(hc_names %in% noaa_processes)

    hc_table_noaa <- reactable(checks_noaa,
                               defaultColDef = colDef(
                                 align = "left"),
                          columns = list(hc_names = colDef(name='Process Name'),
                                         hc_status = colDef(name='Status'),
                                         hc_latest = colDef(name = 'Updated (UTC)')),
                          defaultPageSize = 10,
                          filterable = TRUE,
                          highlight = TRUE)
    return(hc_table_noaa)

    ## this is for making a table of ALL processes
  } else if (context == 'ALL'){

  hc_table <- reactable(check_df,
                        defaultColDef = colDef(
                          align = "left"),
            columns = list(hc_names = colDef(name='Process Name'),
                           hc_status = colDef(name='Status'),
                           hc_latest = colDef(name = 'Updated (UTC)')),
            defaultPageSize = 10,
            filterable = TRUE,
            highlight = TRUE)

  return(hc_table)

  } else{

  print('Please specify conext for processes (ALL or NOAA)')

    }

}
