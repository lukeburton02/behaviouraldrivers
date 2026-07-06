# Set up and inspect YouGov data

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

yougov_raw <- read_csv("data-raw/yougov/united-kingdom.csv",
                       show_col_types = FALSE,
                       locale = locale(encoding = "latin1"))
problems(yougov_raw)

# Summary counts
nrow(yougov_raw) # 64532 total responses
n_distinct(yougov_raw$qweek) # 63 distinct survey weeks
yougov_raw |>
  dplyr::mutate(date = dmy_hm(endtime)) |>
  dplyr::summarise(start_date = min(date, na.rm = TRUE), # 2020-04-01
                   end_date = max(date, na.rm = TRUE)) # 2022-03-29
