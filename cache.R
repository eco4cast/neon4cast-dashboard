Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")

library(arrow)
s3 <- s3_bucket("scores/parquet", endpoint_override="data.ecoforecast.org")
all_scores <- open_dataset(s3, partitioning = c("target_id", "year"))
write_dataset(all_scores,
              "cache",
              partitioning = c("target_id", "year"),
              hive_style = FALSE)
