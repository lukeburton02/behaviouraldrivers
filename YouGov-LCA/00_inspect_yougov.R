# Stage 0 -- inspect raw YouGov data before building the LCA dataset.
# Nothing is recoded here. See initial_plan.md for the
# variable set and the reasoning behind each choice.

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

yougov_raw <- read_csv(here::here("data-raw/yougov/united-kingdom.csv"),
                        show_col_types = FALSE,
                        locale = locale(encoding = "latin1"))
problems(yougov_raw)

yougov_raw <- yougov_raw |>
  mutate(date = dmy_hm(endtime))

cat("Total rows:", nrow(yougov_raw), "\n")
cat("Distinct survey waves (qweek):", n_distinct(yougov_raw$qweek), "\n")
cat("Date range:", as.character(min(yougov_raw$date, na.rm = TRUE)), "to",
    as.character(max(yougov_raw$date, na.rm = TRUE)), "\n\n")

reduced_behaviour <- c(
  mask_outside        = "i12_health_1",
  avoid_large_gather  = "i12_health_14",
  avoid_crowds        = "i12_health_15",
  contacts_nonhh_cat  = "i2_health"
)

reduced_attitude <- c(
  severity_cat        = "r1_1",
  govt_trust_cat      = "WCRex1",
  nhs_confidence_cat  = "WCRex2"
)

full_extra_behaviour <- c(
  wash_hands           = "i12_health_2",
  sanitiser            = "i12_health_3",
  cover_cough          = "i12_health_4",
  avoid_symptomatic    = "i12_health_5",
  avoid_guests         = "i12_health_11",
  avoid_small_gather   = "i12_health_12",
  avoid_med_gather     = "i12_health_13",
  avoid_public_objects = "i12_health_20",
  left_home_cat        = "i7a_health"
)

full_extra_attitude <- c(
  likelihood_cat      = "r1_2",
  willing_isolate_cat = "i11_health"
)

structural_constraints <- c(
  employment_status  = "employment_status",
  household_size     = "household_size",
  household_children = "household_children",
  hh_contacts        = "i1_health"
)

demographics <- c(
  age    = "age",
  gender = "gender",
  region = "region",
  weight = "weight"
)

all_vars <- c(reduced_behaviour, reduced_attitude,
              full_extra_behaviour, full_extra_attitude,
              structural_constraints, demographics)

cat("=== Column existence + coverage window ===\n")
for (i in seq_along(all_vars)) {
  new_name <- names(all_vars)[i]
  raw_col  <- all_vars[i]
  present  <- raw_col %in% names(yougov_raw)
  status   <- if (present) "PASS" else "MISSING"

  if (present) {
    x <- yougov_raw[[raw_col]]
    nonmiss <- !is.na(x) & !(as.character(x) %in% c("Not sure", "Don't know", "Not applicable"))
    first_date <- suppressWarnings(min(yougov_raw$date[nonmiss], na.rm = TRUE))
    last_date  <- suppressWarnings(max(yougov_raw$date[nonmiss], na.rm = TRUE))
    cat(sprintf("%-22s <- %-20s %-7s  n=%6d  %s to %s\n",
                new_name, raw_col, status, sum(nonmiss),
                as.character(as.Date(first_date)), as.character(as.Date(last_date))))
  } else {
    cat(sprintf("%-22s <- %-20s %-7s\n", new_name, raw_col, status))
  }
}

cat("\n=== Raw value counts: REDUCED SET indicators ===\n")
reduced_cols <- c(reduced_behaviour, reduced_attitude)
for (i in seq_along(reduced_cols)) {
  new_name <- names(reduced_cols)[i]
  raw_col  <- reduced_cols[i]
  cat("\n--", new_name, "(", raw_col, ") --\n")
  print(dplyr::count(yougov_raw, .data[[raw_col]], sort = TRUE))
}

cat("\n=== Raw value counts: FULL-SET EXTRA indicators ===\n")
full_extra_cols <- c(full_extra_behaviour, full_extra_attitude)
for (i in seq_along(full_extra_cols)) {
  new_name <- names(full_extra_cols)[i]
  raw_col  <- full_extra_cols[i]
  cat("\n--", new_name, "(", raw_col, ") --\n")
  print(dplyr::count(yougov_raw, .data[[raw_col]], sort = TRUE))
}

# sapply() over a list of POSIXct ranges drops the POSIXct class and returns
# raw epoch-seconds numerics, so min/max must happen before converting back.
cat("\n=== Reduced-set joint coverage window ===\n")
window_bounds <- lapply(reduced_cols, function(raw_col) {
  x <- yougov_raw[[raw_col]]
  nonmiss <- !is.na(x) & !(as.character(x) %in% c("Not sure", "Don't know", "Not applicable"))
  as.numeric(range(yougov_raw$date[nonmiss], na.rm = TRUE))
})
starts <- sapply(window_bounds, `[`, 1)
ends   <- sapply(window_bounds, `[`, 2)
joint_start <- as.POSIXct(max(starts), origin = "1970-01-01", tz = "UTC")
joint_end   <- as.POSIXct(min(ends),   origin = "1970-01-01", tz = "UTC")
cat("Joint start:", as.character(as.Date(joint_start)), "\n")
cat("Joint end:  ", as.character(as.Date(joint_end)), "\n")

cat("\nStage 0 complete. Confirm before Stage 1 is written.\n")
