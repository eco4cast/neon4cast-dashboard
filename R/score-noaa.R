library(tidyverse)
library(neon4cast)
library(score4cast)
library(neonstore)

# CURRENTLY only does RH, starting at 2022-04-20.  should be easy to customize


# Unlike other dashboards, this is currently scoring on the fly, rather than reading pre-scored files.

## Read NOAA's weather forecasts pre-extracted at NEON coordinates:
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")

s3 <- arrow::s3_bucket("neon4cast-drivers/noaa/gefs-v12/stage1",
                       endpoint_override =  "data.ecoforecast.org",
                       anonymous=TRUE)
df <- arrow::open_dataset(s3, partitioning = c("start_date", "cycle"))

## this is super-huge, ~ 9 GB.  consider lazy / local disk serialization first?
fc <- df |>
  filter(reference_datetime >= as.Date("2022-05-20"),
         variable == "RH") |>
  mutate(model_id = "noaa_gefs") |>
  collect()



## Read weather measurements pre-extracted from NEON database
# uses the data.ecoforecast.org server by default
library(neonstore)
neon <- arrow::s3_bucket("neon4cast-targets/neon",
                          endpoint_override = "data.ecoforecast.org",
                          anonymous=TRUE)
db <- neon_remote_db(neon)
rh <- arrow::open_dataset(neon$path("RH_30min-basic-DP1.00098.001"))

# Reformat the NEON data to EFI standard
target <- rh |>
  mutate(startDateTime = lubridate::as_datetime(startDateTime)) |>
  filter(startDateTime >= lubridate::as_datetime("2022-05-20")) |>
  select(startDateTime, siteID, RHMean, horizontalPosition, verticalPosition) |>
  group_by(siteID, startDateTime) |>
  summarise(observation = mean(RHMean)) |>
  mutate(variable = "RH") |>
  rename(site_id = siteID, time = startDateTime) |>
  arrange(site_id,time) |>
  collect()


## We're ready to score -- this slow and memory-intensive filtering join could def be improved by some pre-filtering...

scores <- score4cast::crps_logs_score(fc, target)

s3 <- arrow::s3_bucket("neon4cast-scores/noaa", endpoint_override="data.ecoforecast.org")
arrow::write_parquet(scores, s3$path("RH"))

