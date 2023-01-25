## want to recreate command below
#curl -- header "X-Api-Key: YKpvNOQWGtcGtyKu4rgC0Z7_0AQFo4kj" https://healthchecks.io/api/v1/checks/

## resources
#https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/
#https://healthchecks.io/docs/api/


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

reactable(check_df,
          columns = list(hc_names = colDef(name='Process Name'),
                         hc_status = colDef(name='Status'),
                         hc_latest = colDef(name = 'Updated (UTC)')),
          defaultPageSize = 20,
          filterable = TRUE,
          highlight = TRUE)
