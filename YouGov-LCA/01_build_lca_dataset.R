# Stage 1 -- build the individual-level LCA analysis dataset.
# See initial_plan.md Section 3/Stage 1 for the variable
# set and recode rationale.

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

yougov_raw <- read_csv(here::here("data-raw/yougov/united-kingdom.csv"),
                        show_col_types = FALSE,
                        locale = locale(encoding = "latin1")) |>
  mutate(date = dmy_hm(endtime))

not_missing <- function(x) {
  !is.na(x) & !(as.character(x) %in% c("Not sure", "Don't know", "Not applicable"))
}

# Recode helpers ----------------------------------------------------------

recode_behaviour_binary <- function(x) {
  case_when(
    x %in% c("Always", "Frequently")            ~ 2L,
    x %in% c("Sometimes", "Rarely", "Not at all") ~ 1L,
    TRUE                                          ~ NA_integer_
  )
}

band_contacts <- function(x) {
  case_when(
    x == 0               ~ 1L,
    x >= 1  & x <= 3      ~ 2L,
    x >= 4  & x <= 9      ~ 3L,
    x >= 10               ~ 4L,
    TRUE                  ~ NA_integer_
  )
}

band_left_home <- function(x) {
  case_when(
    x == 0     ~ 1L,
    x == 1     ~ 2L,
    x >= 2 & x <= 3 ~ 3L,
    x >= 4     ~ 4L,
    TRUE       ~ NA_integer_
  )
}

collapse_risk_scale <- function(raw_1to7) {
  digit <- as.numeric(str_extract(raw_1to7, "^\\d+"))
  case_when(
    digit %in% c(1, 2)    ~ 1L,
    digit %in% c(3, 4, 5) ~ 2L,
    digit %in% c(6, 7)    ~ 3L,
    TRUE                  ~ NA_integer_
  )
}

recode_govt_trust <- function(x) {
  case_when(
    x == "Very badly"     ~ 1L,
    x == "Somewhat badly" ~ 2L,
    x == "Somewhat well"  ~ 3L,
    x == "Very well"      ~ 4L,
    TRUE                  ~ NA_integer_
  )
}

recode_nhs_confidence <- function(x) {
  case_when(
    x == "No confidence at all"        ~ 1L,
    x == "Not very much confidence"    ~ 2L,
    x == "A fair amount of confidence" ~ 3L,
    x == "A lot of confidence"         ~ 4L,
    TRUE                                ~ NA_integer_
  )
}

recode_willing_isolate <- function(x) {
  case_when(
    x == "Very unwilling"                 ~ 1L,
    x == "Somewhat unwilling"             ~ 2L,
    x == "Neither willing nor unwilling"  ~ 3L,
    x == "Somewhat willing"               ~ 4L,
    x == "Very willing"                   ~ 5L,
    TRUE                                   ~ NA_integer_
  )
}

# Analysis window: widest range over which the reduced-set indicators are
# jointly available (confirmed in Stage 0: 24 Jun 2020 - 27 Feb 2022).
reduced_raw_cols <- c("i12_health_1", "i12_health_14", "i12_health_15", "i2_health",
                      "r1_1", "WCRex1", "WCRex2")
window_bounds <- lapply(reduced_raw_cols, function(col) {
  as.numeric(range(yougov_raw$date[not_missing(yougov_raw[[col]])], na.rm = TRUE))
})
window_start <- as.POSIXct(max(sapply(window_bounds, `[`, 1)), origin = "1970-01-01", tz = "UTC")
window_end   <- as.POSIXct(min(sapply(window_bounds, `[`, 2)), origin = "1970-01-01", tz = "UTC")
cat("Analysis window:", as.character(as.Date(window_start)), "to",
    as.character(as.Date(window_end)), "\n")

yougov <- yougov_raw |>
  filter(date >= window_start, date <= window_end)
cat("Rows in window:", nrow(yougov), "of", nrow(yougov_raw), "total\n")

# Recode reduced + full-set indicators ------------------------------------
yougov <- yougov |>
  mutate(
    mask_outside        = recode_behaviour_binary(i12_health_1),
    avoid_large_gather  = recode_behaviour_binary(i12_health_14),
    avoid_crowds        = recode_behaviour_binary(i12_health_15),
    contacts_nonhh_cat  = band_contacts(i2_health),
    severity_cat        = collapse_risk_scale(r1_1),
    govt_trust_cat      = recode_govt_trust(WCRex1),
    nhs_confidence_cat  = recode_nhs_confidence(WCRex2),

    wash_hands           = recode_behaviour_binary(i12_health_2),
    sanitiser             = recode_behaviour_binary(i12_health_3),
    cover_cough            = recode_behaviour_binary(i12_health_4),
    avoid_symptomatic       = recode_behaviour_binary(i12_health_5),
    avoid_guests             = recode_behaviour_binary(i12_health_11),
    avoid_small_gather        = recode_behaviour_binary(i12_health_12),
    avoid_med_gather            = recode_behaviour_binary(i12_health_13),
    avoid_public_objects          = recode_behaviour_binary(i12_health_20),
    left_home_cat                   = band_left_home(i7a_health),
    likelihood_cat                    = collapse_risk_scale(r1_2),
    willing_isolate_cat                 = recode_willing_isolate(i11_health),

    age_group = cut(age,
      breaks = c(17, 24, 34, 44, 54, 64, Inf),
      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    )
  )

reduced_indicators <- c("mask_outside", "avoid_large_gather", "avoid_crowds",
                         "contacts_nonhh_cat", "severity_cat", "govt_trust_cat",
                         "nhs_confidence_cat")
full_indicators <- c(reduced_indicators,
                      "wash_hands", "sanitiser", "cover_cough", "avoid_symptomatic",
                      "avoid_guests", "avoid_small_gather", "avoid_med_gather",
                      "avoid_public_objects", "left_home_cat",
                      "likelihood_cat", "willing_isolate_cat")

structural_constraints <- c("employment_status", "household_size", "household_children", "i1_health")
demographics <- c("age", "age_group", "gender", "region", "weight")

# Complete cases on reduced-set indicators only ---------------------------
n_before <- nrow(yougov)
yougov_lca <- yougov |>
  filter(if_all(all_of(reduced_indicators), ~ !is.na(.)))
n_after <- nrow(yougov_lca)
cat("Complete cases on reduced-set indicators:", n_after, "of", n_before,
    "(", n_before - n_after, "dropped )\n")

yougov_lca <- yougov_lca |>
  select(qweek, date, all_of(full_indicators),
         all_of(structural_constraints), all_of(demographics)) |>
  rename(hh_contacts = i1_health)

write_rds(yougov_lca, here::here("YouGov-LCA/data/yougov_lca_individual.rds"))

# Recode dictionary --------------------------------------------------------
recode_dictionary <- tribble(
  ~new_name,             ~raw_column,      ~set,     ~raw_values,                                                          ~recoded_values,                                            ~description,
  "mask_outside",        "i12_health_1",   "reduced", "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Worn a face mask outside home",
  "avoid_large_gather",  "i12_health_14",  "reduced", "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Avoided large gatherings (>10)",
  "avoid_crowds",        "i12_health_15",  "reduced", "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Avoided crowded areas",
  "contacts_nonhh_cat",  "i2_health",      "reduced", "numeric count (heavy round-number heaping, e.g. 100/1000)",         "1=0, 2=1-3, 3=4-9, 4=10+",                                "Non-household contacts within 2m",
  "severity_cat",        "r1_1",           "reduced", "1-7 scale, leading-digit extraction",                               "1=low(1-2), 2=medium(3-5), 3=high(6-7)",                 "COVID perceived severity",
  "govt_trust_cat",      "WCRex1",         "reduced", "Very badly/Somewhat badly/Somewhat well/Very well",                 "1=Very badly ... 4=Very well",                           "Government handling rating",
  "nhs_confidence_cat",  "WCRex2",         "reduced", "No confidence/Not very much/Fair amount/A lot",                     "1=No confidence ... 4=A lot",                            "Confidence in NHS",
  "wash_hands",          "i12_health_2",   "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Washed hands with soap and water",
  "sanitiser",           "i12_health_3",   "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Used hand sanitiser",
  "cover_cough",         "i12_health_4",   "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Covered nose/mouth when coughing",
  "avoid_symptomatic",   "i12_health_5",   "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Avoided contact with symptomatic people",
  "avoid_guests",        "i12_health_11",  "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Avoided having guests at home",
  "avoid_small_gather",  "i12_health_12",  "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Avoided small gatherings (<=2)",
  "avoid_med_gather",    "i12_health_13",  "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Avoided medium gatherings (3-10)",
  "avoid_public_objects","i12_health_20",  "full",    "Always/Frequently/Sometimes/Rarely/Not at all",                     "2=Always/Frequently, 1=Sometimes/Rarely/Not at all",     "Avoided touching objects in public (coverage ends Dec 2020)",
  "left_home_cat",       "i7a_health",     "full",    "numeric count",                                                     "1=0, 2=1, 3=2-3, 4=4+",                                  "Times left home yesterday (coverage ends Jul 2021)",
  "likelihood_cat",      "r1_2",           "full",    "1-7 scale, leading-digit extraction",                               "1=low(1-2), 2=medium(3-5), 3=high(6-7)",                 "Perceived likelihood of contracting COVID",
  "willing_isolate_cat", "i11_health",     "full",    "Very unwilling/Somewhat unwilling/Neither/Somewhat willing/Very willing", "1=Very unwilling ... 5=Very willing",             "Willingness to self-isolate"
)
write_csv(recode_dictionary, here::here("YouGov-LCA/data/yougov_lca_recode_dictionary.csv"))

cat("\nSaved YouGov-LCA/data/yougov_lca_individual.rds (", n_after, "rows,",
    length(full_indicators), "indicator columns )\n")
cat("Saved YouGov-LCA/data/yougov_lca_recode_dictionary.csv (", nrow(recode_dictionary), "rows )\n")
