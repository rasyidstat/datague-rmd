library(googlesheets)
library(stringr)
library(rvest)
library(lubridate)
library(tidyverse)
Sys.setlocale("LC_TIME", "English")

# read data from google sheet
gs_ls()
tt <- gs_title("GO-JEK Receipt")
gs_ws_ls(tt)
df <- gs_read(ss=tt, ws="Sheet1", col_names=FALSE)

# write in local and re-read
write_rds(df, paste0("data/gojek_", gsub("-", "", today()), ".rds"))
# df <- read_rds("data/gojek_20170714.rds")
df <- df %>%
  select(dt_created=X1, dt_order=X4, content=X5) %>%
  mutate(dt_created=mdy_hm(dt_created),
         dt_order=dmy(dt_order))

# function to extract html
clean_gojek <- function(h) {
  output <- list()
  h <- read_html(h)
  a <- h %>%
    html_nodes("p span") %>%
    html_text()
  b <- h %>%
    html_nodes("h2") %>%
    html_text() %>%
    trimws()
  c <- h %>%
    html_nodes("strong") %>%
    html_text()
  d <- h %>%
    html_nodes("td span") %>%
    html_text()
  e <- h %>%
    html_nodes("tr td") %>%
    html_text() %>%
    trimws()
  img <- h %>%
    html_nodes("img") %>%
    html_attr("src") %>%
    subset(grepl("imgix", .))
  output$order_id <- gsub("ORDER ID: ", "", b[4])
  output$time_dep <- ymd_hm(paste(dmy(b[3]), c[1]))
  output$time_arrive <- ymd_hm(paste(dmy(b[3]), c[2]))
  output$from <- a[1]
  output$to <- a[2]
  output$distance <- as.numeric(str_extract(d[12], "\\d+\\.\\d+"))
  output$duration <- as.numeric(hms(d[13]))
  output$price <- str_extract(gsub("[[:punct:]]", "", e[23]), "\\d+")
  output$discount <- str_extract(gsub("[[:punct:]]", "", e[21]), "\\d+")
  output$driver <- a[3]
  output$driver_url <- if (length(img) > 0) img else NA
  return(output)
}

# apply to the data
df_clean <- df %>%
  mutate(content_all = map(content, clean_gojek),
         content_all = map(content_all, data.frame)) %>%
  unnest() %>%
  select(-content)

# write back in local for future use
write.csv(df_clean, paste0("data/gojek_clean_", gsub("-", "", today()), ".csv"), row.names = FALSE)
