
library(minio)
install_mc()
mc_alias_set("efi",  endpoint="data.ecoforecast.org",
             access_key = "", secret_key = "")
mc("mirror efi/neon4cast-scores cache/")





# Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
# Sys.unsetenv("AWS_ACCESS_KEY_ID")
# Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
# Sys.unsetenv("AWS_DEFAULT_REGION")
# Sys.unsetenv("AWS_S3_ENDPOINT")
#
# library(arrow)
# library(glue)
# themes <- c("aquatics", "ticks",
#             "beetles", "terrestrial_daily",
#             "phenology", "terrestrial_30min")
#
# for(theme in themes) {
#   glue("neon4cast-scores/parquet/{theme}") |>
#   s3_bucket(endpoint_override="data.ecoforecast.org") |>
#   open_dataset() |>
#   write_dataset(glue("cache/{theme}"),
#                 partitioning = c("model_id"))
# }

# faster to mirror, exploits cache & avoids re-downloading
