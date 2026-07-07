# Stage 4 -- how are demographics/structural constraints distributed across
# classes, and do they predict membership? Modal class assignment (predclass)
# ignores classification uncertainty -- see Stage 3 entropy (0.653) and the
# class 1/3 caveat in outputs/lca_reduced_k3_findings.md.

library(readr)
library(dplyr)
library(tidyr)
library(poLCA)
library(ggplot2)

K_CHOSEN <- 3
fit <- readRDS(here::here(sprintf("YouGov-LCA/models/lca_reduced_fit_k%d.rds", K_CHOSEN)))
yougov_lca <- readRDS(here::here("YouGov-LCA/data/yougov_lca_individual.rds")) |>
  mutate(class = factor(fit$predclass))

# 4a -- descriptive distributions ------------------------------------------
band_household_size <- function(x) {
  case_when(
    x %in% c("Don't know", "Prefer not to say") ~ "Not stated",
    x == "8 or more"                             ~ "8+",
    TRUE                                          ~ x
  )
}
band_household_children <- function(x) {
  case_when(
    x %in% c("Don't know", "Prefer not to say") ~ "Not stated",
    x %in% c("5 or more", "7", "9")               ~ "5+",
    TRUE                                           ~ x
  )
}

yougov_lca <- yougov_lca |>
  mutate(
    household_size     = band_household_size(household_size),
    household_children = band_household_children(household_children)
  )

descriptive_vars <- c("age_group", "gender", "region", "employment_status",
                       "household_size", "household_children")

dir.create(here::here("YouGov-LCA/outputs/lca_distributions"), showWarnings = FALSE)

descriptive_var_labels <- c(
  age_group           = "Age group",
  gender              = "Gender",
  region              = "Region",
  employment_status   = "Employment status",
  household_size      = "Household size",
  household_children  = "Household children"
)

for (v in descriptive_vars) {
  v_label <- descriptive_var_labels[[v]]
  dist_tbl <- yougov_lca |>
    filter(!is.na(.data[[v]])) |>
    group_by(class, level = .data[[v]]) |>
    summarise(weighted_n = sum(weight), .groups = "drop_last") |>
    mutate(pct = 100 * weighted_n / sum(weighted_n)) |>
    ungroup()

  write_csv(dist_tbl, here::here(sprintf("YouGov-LCA/outputs/lca_distributions/dist_%s.csv", v)))

  p <- ggplot(dist_tbl, aes(x = class, y = pct, fill = level)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Class", y = "Weighted % within class", fill = v_label,
         title = paste(v_label, "(by modal class assignment)")) +
    theme_minimal()
  ggsave(here::here(sprintf("YouGov-LCA/outputs/lca_distributions/dist_%s.png", v)),
         p, width = 8, height = 5, dpi = 200)
}
cat("4a: saved distribution CSV/PNG for", length(descriptive_vars), "variables to YouGov-LCA/outputs/lca_distributions/\n\n")

# 4b -- does a covariate predict class membership? --------------------------
# Baseline (no covariates) is the Stage 3 model already fit -- reused here
# rather than refit, since it was already fit thoroughly (nrep=50).
# Covariate models use a lighter nrep/maxiter: they only need to beat the
# already-converged baseline log-likelihood for the LR test, not find the
# single best possible fit themselves.
f_reduced <- cbind(mask_outside, avoid_large_gather, avoid_crowds, contacts_nonhh_cat,
                   severity_cat, govt_trust_cat, nhs_confidence_cat) ~ 1

f_age      <- update(f_reduced, . ~ age)
f_occ      <- update(f_reduced, . ~ employment_status)
f_full_cov <- update(f_reduced, . ~ age + gender + employment_status + household_size)

covariate_data <- yougov_lca |>
  mutate(household_size = na_if(household_size, "Not stated")) |>
  filter(!is.na(age), !is.na(gender), !is.na(employment_status), !is.na(household_size))
cat("Covariate models fit on", nrow(covariate_data), "of", nrow(yougov_lca),
    "rows (complete cases on age/gender/employment/household_size)\n")

fit_covariate <- function(formula) {
  set.seed(2024)
  poLCA(formula, data = covariate_data, nclass = K_CHOSEN, nrep = 20,
        maxiter = 2000, verbose = FALSE)
}

fit_age  <- fit_covariate(f_age)
fit_occ  <- fit_covariate(f_occ)
fit_full <- fit_covariate(f_full_cov)

lr_test <- function(fit_cov, name) {
  lr_stat <- 2 * (fit_cov$llik - fit$llik)
  df <- fit_cov$npar - fit$npar
  tibble(model = name, npar = fit_cov$npar, log_likelihood = fit_cov$llik,
         lr_stat = lr_stat, df = df, p_value = pchisq(lr_stat, df, lower.tail = FALSE))
}

covariate_tests <- bind_rows(
  lr_test(fit_age,  "~ age"),
  lr_test(fit_occ,  "~ employment_status"),
  lr_test(fit_full, "~ age + gender + employment_status + household_size")
)
write_csv(covariate_tests, here::here("YouGov-LCA/outputs/lca_reduced_covariate_tests.csv"))

cat("\n4b: covariate LR tests (vs baseline llik =", round(fit$llik, 1), ", npar =", fit$npar, "):\n")
print(covariate_tests)
cat("\nage coefficients (~ age model, reference class 1):\n")
print(fit_age$coeff)
cat("\nemployment_status coefficients (~ employment_status model, reference class 1):\n")
print(fit_occ$coeff)

# 4c -- driver/wellbeing profiles not used to define classes -----------------
# PHQ4 items and cantril_ladder are not in the Stage 1 saved dataset (only
# the reduced/full LCA indicators + Table B/C columns were kept), so the
# window filter + reduced-indicator recode + complete-case steps from Stage 1
# are reproduced here to reconstruct the identical row set before attaching
# predclass. This must stay in sync with 01_build_lca_dataset.R.
recode_behaviour_binary <- function(x) {
  case_when(
    x %in% c("Always", "Frequently")             ~ 2L,
    x %in% c("Sometimes", "Rarely", "Not at all") ~ 1L,
    TRUE                                          ~ NA_integer_
  )
}
band_contacts <- function(x) {
  case_when(x == 0 ~ 1L, x >= 1 & x <= 3 ~ 2L, x >= 4 & x <= 9 ~ 3L, x >= 10 ~ 4L, TRUE ~ NA_integer_)
}
collapse_risk_scale <- function(raw_1to7) {
  digit <- as.numeric(stringr::str_extract(raw_1to7, "^\\d+"))
  case_when(digit %in% c(1, 2) ~ 1L, digit %in% c(3, 4, 5) ~ 2L, digit %in% c(6, 7) ~ 3L, TRUE ~ NA_integer_)
}
recode_govt_trust <- function(x) {
  case_when(x == "Very badly" ~ 1L, x == "Somewhat badly" ~ 2L,
            x == "Somewhat well" ~ 3L, x == "Very well" ~ 4L, TRUE ~ NA_integer_)
}
recode_nhs_confidence <- function(x) {
  case_when(x == "No confidence at all" ~ 1L, x == "Not very much confidence" ~ 2L,
            x == "A fair amount of confidence" ~ 3L, x == "A lot of confidence" ~ 4L, TRUE ~ NA_integer_)
}
recode_phq4 <- function(x) {
  case_when(x == "Not at all" ~ 0L, x == "Several days" ~ 1L,
            x == "More than half the days" ~ 2L, x == "Nearly every day" ~ 3L, TRUE ~ NA_integer_)
}

yougov_raw <- read_csv(here::here("data-raw/yougov/united-kingdom.csv"),
                        show_col_types = FALSE, locale = locale(encoding = "latin1")) |>
  mutate(date = lubridate::dmy_hm(endtime))

reduced_raw_cols <- c("i12_health_1", "i12_health_14", "i12_health_15", "i2_health",
                      "r1_1", "WCRex1", "WCRex2")
window_bounds <- lapply(reduced_raw_cols, function(col) {
  x <- yougov_raw[[col]]
  nonmiss <- !is.na(x) & !(as.character(x) %in% c("Not sure", "Don't know", "Not applicable"))
  as.numeric(range(yougov_raw$date[nonmiss], na.rm = TRUE))
})
window_start <- as.POSIXct(max(sapply(window_bounds, `[`, 1)), origin = "1970-01-01", tz = "UTC")
window_end   <- as.POSIXct(min(sapply(window_bounds, `[`, 2)), origin = "1970-01-01", tz = "UTC")

driver_data <- yougov_raw |>
  filter(date >= window_start, date <= window_end) |>
  mutate(
    mask_outside        = recode_behaviour_binary(i12_health_1),
    avoid_large_gather  = recode_behaviour_binary(i12_health_14),
    avoid_crowds        = recode_behaviour_binary(i12_health_15),
    contacts_nonhh_cat  = band_contacts(i2_health),
    severity_cat        = collapse_risk_scale(r1_1),
    govt_trust_cat      = recode_govt_trust(WCRex1),
    nhs_confidence_cat  = recode_nhs_confidence(WCRex2),
    phq4_1 = recode_phq4(PHQ4_1), phq4_2 = recode_phq4(PHQ4_2),
    phq4_3 = recode_phq4(PHQ4_3), phq4_4 = recode_phq4(PHQ4_4)
  ) |>
  filter(if_all(c(mask_outside, avoid_large_gather, avoid_crowds, contacts_nonhh_cat,
                   severity_cat, govt_trust_cat, nhs_confidence_cat), ~ !is.na(.)))

stopifnot(nrow(driver_data) == nrow(yougov_lca))  # must match Stage 1's row set exactly

driver_data <- driver_data |>
  mutate(
    class = yougov_lca$class,
    weight = yougov_lca$weight,
    phq4_total = phq4_1 + phq4_2 + phq4_3 + phq4_4
  )

driver_summary <- driver_data |>
  dplyr::select(class, weight, phq4_total, cantril_ladder) |>
  pivot_longer(c(phq4_total, cantril_ladder), names_to = "measure", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(class, measure) |>
  summarise(weighted_mean = weighted.mean(value, weight), .groups = "drop")

write_csv(driver_summary, here::here("YouGov-LCA/outputs/lca_distributions/driver_wellbeing_profiles.csv"))

measure_labels <- c(phq4_total = "PHQ-4 total (anxiety/depression)", cantril_ladder = "Cantril ladder (wellbeing)")

p_driver <- ggplot(driver_summary, aes(x = class, y = weighted_mean, fill = class)) +
  geom_col() +
  facet_wrap(~measure, scales = "free_y", labeller = as_labeller(measure_labels)) +
  labs(x = "Class", y = "Weighted mean", fill = "Class",
       title = "Auxiliary measures not used to define classes",
       subtitle = "PHQ-4 total (0-12, higher = more anxiety/depression symptoms); Cantril ladder (0-10, higher = better wellbeing)") +
  theme_minimal()
ggsave(here::here("YouGov-LCA/outputs/lca_distributions/driver_wellbeing_profiles.png"),
       p_driver, width = 8, height = 5, dpi = 200)

cat("\n4c: driver/wellbeing profiles by class:\n")
print(driver_summary)

cat("\nStage 4 complete.\n")
