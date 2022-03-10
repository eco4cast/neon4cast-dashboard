library(glue)
library(fs)
library(magrittr)

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

  if(collect) {
    ds <- ds %>% collect()
  }
  ds
}
