library(arrow)
library(glue)
library(fs)
library(magrittr)
library(dplyr)
library(tidyr)

combined_scores <- function(theme = "", year = "", collect = TRUE,
                            endpoint =  "minio.carlboettiger.info"
                          #  endpoint = "data.ecoforecast.org"
                              ){
  Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
  Sys.unsetenv("AWS_ACCESS_KEY_ID")
  Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_S3_ENDPOINT")

  s3 <- arrow::s3_bucket(bucket = "scores", endpoint_override = endpoint)

  path <- glue::glue("parquet/{theme}/{year}", theme = theme, year = year)
  files <- s3$path(fs::path_norm(path))
  ds <- arrow::open_dataset(files)

  if (collect) {
    ds <- ds %>% collect()
  }
  ds
}


fill_scores <- function(df, null_team = NA) {
  df <- df %>% filter(!is.na(observed)) %>% collect()

  team <- distinct(df,team)
  if (is.na(null_team)) {
    x <- pull(team,team)
    null_team <- x[grepl("null", x)]
  }

  null <- df %>%
    filter(team == null_team) %>%
    select("theme", "target", "x","y","z", "site", "time",
           "forecast_start_time", "crps", "logs")
  all <- tidyr::expand_grid(null, team)
  na_filled <- left_join(all, df,
                         by = c("theme", "team", "target", "x","y","z",
                                "site", "time", "forecast_start_time"),
                         suffix = c("_null", "_team"))
  null_filled <- na_filled %>% mutate(
    crps = case_when(is.na(crps_team) ~ crps_null,
                     !is.na(crps_team) ~ crps_team),
    logs = case_when(is.na(logs_team) ~ logs_null,
                     !is.na(logs_team) ~ logs_team)) %>%
    select(-crps_null, -logs_null)

  ## express difftimes in days, not seconds
  null_filled %>% mutate(interval = as.numeric(interval, units="days"),
                         horizon = as.numeric(horizon, units="days"))

}




