# Script to process raw YouGov data
# Selects key demographic variables for analysis, alongside drivers of interest
# Includes perceived severity, fear of contracting COVID-19, confidence in NHS etc
# Saves weighted survey-wave averages by population-level, age, region and gender
#
# Aggregation strategy: group by qweek (native survey wave ID) rather than ISO week.
# Fieldwork spans ~7 days and straddles ISO week boundaries, so floor_date() splits
# a single wave across two ISO weeks — the minority stub week (n=1-90) produces
# extreme weighted means and visible spikes in plots. Each wave is assigned a
# canonical week_start = the Monday of the ISO week containing the most respondents.

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Load raw data -------
yougov_raw <- read_csv("data-raw/yougov/united-kingdom.csv",
                       show_col_types = FALSE,
                       locale = locale(encoding = "latin1"))
problems(yougov_raw) # Identify unexpected data entry formats (drops entire row?)

# Parse dates and select key variables -----
# Weight is post-stratification survey weight based on age, gender and region (with mean 1)
# Bear in mind 1 can be 'low' or 'high' depending on the question, and some options are don't know. Not all drivers cover the same dates

yougov <- yougov_raw |>
  dplyr::mutate(date = dmy_hm(endtime)) |>
  dplyr::select(
    qweek,
    date,
    weight,
    age,
    gender,
    region,
    employment_status,
    perceived_severity = r1_1, # "Coronavirus (COVID-19) is very dangerous for me" 1 = Disagree, 7 = Agree
    perceived_likelihood = r1_2, # "It is likely that I will get coronavirus (COVID-19) in the future" 1 = Disagree, 7 = Agree
    fear_contracting = WCRV_4, # "Which, if any, of the following statements BEST describes your feelings towards contracting the Coronavirus (COVID-19)?" 1 = Very scared, 4 = Not scared, 977 = Don't know, 988 = NA - already contracted
    govt_handling = WCRex1, # "How well or badly do you think the Government are handling the issue of the Coronavirus (COVID-19)?" 1 = Very well, 4 = Very badly, 5 = Don't know
    nhs_confidense = WCRex2, # "And how much confidence do you have in the NHS to respond to a Coronavirus (COVID-19) outbreak in the UK?" 1 = A lot of confidence, 4 = No confidence, 5 = Don't know
    willing_isolate = i11_health, # "If you were advised to do so by a healthcare professional or public health authority to what extent are you willing or not to self-isolate for 7 days?" 1 = Very willing, 5 = Very unwilling, 99 = Not sure
  )

# Recode categoricals to ordered numerics -----
yougov <- yougov |>
  mutate(
    # r1_1 and r1_2 stored as labelled strings sometimes e.g. "1 – Disagree", "7 - Agree" — extract leading digit
    perceived_severity   = as.numeric(str_extract(perceived_severity, "^\\d+")),
    perceived_likelihood = as.numeric(str_extract(perceived_likelihood, "^\\d+")),
    iso_week = floor_date(date, "week", week_start = 1),
    age_group = cut(age,
      breaks = c(17, 24, 34, 44, 54, 64, Inf),
      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ),
    govt_handling_num = case_when(
      govt_handling == "Very badly"     ~ 1,
      govt_handling == "Somewhat badly" ~ 2,
      govt_handling == "Somewhat well"  ~ 3,
      govt_handling == "Very well"      ~ 4
    ),
    nhs_confidence_num = case_when(
      nhs_confidense == "No confidence at all"        ~ 1,
      nhs_confidense == "Not very much confidence"    ~ 2,
      nhs_confidense == "A fair amount of confidence" ~ 3,
      nhs_confidense == "A lot of confidence"         ~ 4
    ),
    willing_isolate_num = case_when(
      willing_isolate == "Very unwilling"                ~ 1,
      willing_isolate == "Somewhat unwilling"            ~ 2,
      willing_isolate == "Neither willing nor unwilling" ~ 3,
      willing_isolate == "Somewhat willing"              ~ 4,
      willing_isolate == "Very willing"                  ~ 5
    ),
    # Binary: 1 = fairly or very scared, 0 = not very or not at all scared
    fear_scared = case_when(
      fear_contracting %in% c(
        "I am fairly scared that I will contract the Coronavirus (COVID-19)",
        "I am very scared that I will contract the Coronavirus (COVID-19)"
      ) ~ 1L,
      fear_contracting %in% c(
        "I am not at all scared that I will contract the Coronavirus (COVID-19)",
        "I am not very scared that I will contract the Coronavirus (COVID-19)"
      ) ~ 0L
    )
  )

# Assign canonical week_start per qweek -----
# Use the Monday of whichever ISO week contains the most respondents for that wave.
wave_dates <- yougov |>
  count(qweek, iso_week) |>
  group_by(qweek) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  select(qweek, week_start = iso_week)

yougov <- yougov |>
  left_join(wave_dates, by = "qweek")

# Helper: weighted mean dropping NAs -----
wmean <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (sum(keep) == 0) return(NA_real_)
  weighted.mean(x[keep], w[keep])
}

# Survey-wave aggregates -----
yougov_weekly <- yougov |>
  group_by(week_start) |>
  summarise(
    n                    = n(),
    perceived_severity   = wmean(perceived_severity, weight),
    perceived_likelihood = wmean(perceived_likelihood, weight),
    fear_scared_prop     = wmean(fear_scared, weight),
    govt_handling        = wmean(govt_handling_num, weight),
    nhs_confidence       = wmean(nhs_confidence_num, weight),
    willing_isolate      = wmean(willing_isolate_num, weight),
    .groups = "drop"
  ) |>
  arrange(week_start)

yougov_weekly_age <- yougov |>
  group_by(week_start, age_group) |>
  summarise(
    n                    = n(),
    perceived_severity   = wmean(perceived_severity, weight),
    perceived_likelihood = wmean(perceived_likelihood, weight),
    fear_scared_prop     = wmean(fear_scared, weight),
    govt_handling        = wmean(govt_handling_num, weight),
    nhs_confidence       = wmean(nhs_confidence_num, weight),
    willing_isolate      = wmean(willing_isolate_num, weight),
    .groups = "drop"
  ) |>
  arrange(week_start, age_group)

yougov_weekly_region <- yougov |>
  group_by(week_start, region) |>
  summarise(
    n                    = n(),
    perceived_severity   = wmean(perceived_severity, weight),
    perceived_likelihood = wmean(perceived_likelihood, weight),
    fear_scared_prop     = wmean(fear_scared, weight),
    govt_handling        = wmean(govt_handling_num, weight),
    nhs_confidence       = wmean(nhs_confidence_num, weight),
    willing_isolate      = wmean(willing_isolate_num, weight),
    .groups = "drop"
  ) |>
  arrange(week_start, region)

yougov_weekly_gender <- yougov |>
  filter(gender %in% c("Male", "Female")) |>
  group_by(week_start, gender) |>
  summarise(
    n                    = n(),
    perceived_severity   = wmean(perceived_severity, weight),
    perceived_likelihood = wmean(perceived_likelihood, weight),
    fear_scared_prop     = wmean(fear_scared, weight),
    govt_handling        = wmean(govt_handling_num, weight),
    nhs_confidence       = wmean(nhs_confidence_num, weight),
    willing_isolate      = wmean(willing_isolate_num, weight),
    .groups = "drop"
  ) |>
  arrange(week_start, gender)

# Save -----
write_csv(yougov_weekly,        "data-processed/yougov_weekly.csv")
write_csv(yougov_weekly_age,    "data-processed/yougov_weekly_age.csv")
write_csv(yougov_weekly_region, "data-processed/yougov_weekly_region.csv")
write_csv(yougov_weekly_gender, "data-processed/yougov_weekly_gender.csv")

message("Done. Survey waves in aggregate: ", nrow(yougov_weekly))