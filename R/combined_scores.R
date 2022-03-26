library(score4cast)
library(arrow)
library(glue)
library(fs)
library(magrittr)
library(dplyr)
library(tidyr)


combined_scores <- function(theme = NA,
                            collect = TRUE,
                            endpoint = "data.ecoforecast.org"){
  Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
  Sys.unsetenv("AWS_ACCESS_KEY_ID")
  Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_S3_ENDPOINT")

  #GENERALIZATION: THIS IS A SPECIFIC ENDPOINT
  s3 <- arrow::s3_bucket(bucket = "scores/parquet",
                         endpoint_override = endpoint)
  ds <- arrow::open_dataset(s3, partition=c("theme", "year"))
  if (!is.na(theme)) {
    ds <- dplyr::filter(ds, theme == {{theme}})
  }
  if (collect) {
    ds <- dplyr::collect(ds)
  }
  ds
}

