# IMPORTANT: This shows COMPOSITIONAL CHANGE -- how the share of the population
# in each archetype shifts week to week. It does NOT show individuals moving
# between archetypes. YouGov is a repeated cross-section (different respondents
# each wave). Individual transitions require panel data (e.g. CoMix individual-
# level data). This is explicitly out of scope for this preliminary analysis.

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

K_CHOSEN <- 3
class_labels <- c(`1` = "Trusting compliers", `2` = "Disengaged/low-compliance", `3` = "Compliant sceptics")

fit <- readRDS(here::here(sprintf("YouGov-LCA/models/lca_reduced_fit_k%d.rds", K_CHOSEN)))
yougov_lca <- readRDS(here::here("YouGov-LCA/data/yougov_lca_individual.rds")) |>
  mutate(class = factor(class_labels[as.character(fit$predclass)], levels = class_labels))

window_start <- min(yougov_lca$date)
window_end   <- max(yougov_lca$date)

# Canonical week_start per qweek (Monday of the ISO week with most respondents
# for that wave) -- same convention as analysis/process_yougov.R.
wave_dates <- yougov_lca |>
  mutate(iso_week = floor_date(date, "week", week_start = 1)) |>
  count(qweek, iso_week) |>
  group_by(qweek) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  select(qweek, week_start = iso_week)

yougov_lca <- yougov_lca |> left_join(wave_dates, by = "qweek")

# Class prevalence, weekly and monthly ---------------------------------------
prevalence_weekly <- yougov_lca |>
  group_by(week_start, class) |>
  summarise(weighted_n = sum(weight), .groups = "drop_last") |>
  mutate(prevalence = weighted_n / sum(weighted_n)) |>
  ungroup()

prevalence_monthly <- yougov_lca |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month, class) |>
  summarise(weighted_n = sum(weight), .groups = "drop_last") |>
  mutate(prevalence = weighted_n / sum(weighted_n)) |>
  ungroup()

write_csv(prevalence_weekly, here::here("YouGov-LCA/outputs/lca_reduced_prevalence_weekly.csv"))
write_csv(prevalence_monthly, here::here("YouGov-LCA/outputs/lca_reduced_prevalence_monthly.csv"))

# Context data: OxCGRT stringency + vaccination, Google Mobility -------------
cat("Loading data-raw/OxCGRT/OxCGRT_simplified_v1.csv and data-raw/google_mobility/google_mobility_UK.csv\n")

oxcgrt_uk <- read_csv(here::here("data-raw/OxCGRT/OxCGRT_simplified_v1.csv"), show_col_types = FALSE) |>
  filter(CountryName == "United Kingdom", Jurisdiction == "NAT_TOTAL") |>
  mutate(
    date = as.Date(as.character(Date), format = "%Y%m%d"),
    population_vaccinated = as.numeric(PopulationVaccinated),
    week_start = floor_date(date, "week", week_start = 1)
  ) |>
  filter(date >= window_start, date <= window_end)

oxcgrt_weekly <- oxcgrt_uk |>
  group_by(week_start) |>
  summarise(
    stringency_index       = mean(StringencyIndex_Average, na.rm = TRUE),
    population_vaccinated  = mean(population_vaccinated, na.rm = TRUE),
    .groups = "drop"
  )

mobility_weekly <- read_csv(here::here("data-raw/google_mobility/google_mobility_UK.csv"), show_col_types = FALSE) |>
  filter(is.na(sub_region_1) | sub_region_1 == "") |>
  mutate(week_start = floor_date(date, "week", week_start = 1)) |>
  filter(week_start >= window_start, week_start <= window_end) |>
  group_by(week_start) |>
  summarise(
    retail_recreation = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
    workplaces        = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(mobility_weekly, here::here("YouGov-LCA/outputs/lca_context_mobility_weekly.csv"))

# Policy periods -- cross-checked against C6M_combined_numeric (UK stay-at-home
# sub-indicator): C6M=1 5 Nov-2 Dec 2020 (matches Lockdown 2); C6M=2 5 Jan-
# 12 Mar 2021 (matches Lockdown 3 start, eases slightly before our stated end).
# Lockdown 1 (23 Mar-Jun 2020) predates the analysis window (starts 24 Jun
# 2020) and cannot be shown.
policy_periods <- tribble(
  ~period,                ~start,                    ~end,
  "Lockdown 2",            as.Date("2020-11-05"),     as.Date("2020-12-02"),
  "Lockdown 3",            as.Date("2021-01-06"),     as.Date("2021-03-29"),
  "Omicron restrictions",  as.Date("2021-12-08"),     as.Date("2022-01-27")
)

# Prevalence + context plot ---------------------------------------------------
# Class prevalence, stringency and vaccination all happen to share a genuine
# 0-100 range (prevalence as %, stringency 0-100, vaccination 0-100%), so
# putting them on one shared axis is an honest comparison, not a dual-axis
# trick. Context lines use grey + linetype (not colour) so they get their own
# "Context" legend separate from the archetype colour legend, and are labelled
# directly at their line ends rather than relying on the legend alone.
caption_txt <- sprintf(
  "Analysis window: %s to %s (reduced 7-indicator model, k=%d). Lockdown 1 (23 Mar-Jun 2020) predates this window and cannot be shown.",
  format(window_start, "%d %b %Y"), format(window_end, "%d %b %Y"), K_CHOSEN
)

archetype_data <- prevalence_weekly |>
  transmute(week_start, class, value = 100 * prevalence)

context_data <- bind_rows(
  oxcgrt_weekly |> transmute(week_start, series = "OxCGRT Stringency", value = stringency_index),
  oxcgrt_weekly |> transmute(week_start, series = "Proportion Vaccinated", value = population_vaccinated)
) |>
  mutate(series = factor(series, levels = c("OxCGRT Stringency", "Proportion Vaccinated")))

# Direct end-of-line labels, positioned just before each line's last point so
# they stay inside the panel without needing extra right-margin space.
context_labels <- context_data |>
  group_by(series) |>
  filter(week_start == max(week_start)) |>
  ungroup()

archetype_colours <- c(
  "Trusting compliers"         = "#F8766D",
  "Disengaged/low-compliance"  = "#00BA38",
  "Compliant sceptics"         = "#619CFF"
)

p <- ggplot() +
  annotate("rect", xmin = policy_periods$start, xmax = policy_periods$end,
           ymin = 0, ymax = 100, fill = "grey85", alpha = 0.5) +
  geom_line(data = context_data, aes(x = week_start, y = value, linetype = series),
            colour = "grey40", linewidth = 0.9, alpha = 0.55) +
  geom_text(data = context_labels, aes(x = week_start, y = value, label = series),
            colour = "black", size = 3, hjust = 1, vjust = -0.6) +
  geom_line(data = archetype_data, aes(x = week_start, y = value, colour = class), linewidth = 1) +
  scale_colour_manual(values = archetype_colours, name = "Archetype") +
  scale_linetype_manual(values = c("OxCGRT Stringency" = "solid", "Proportion Vaccinated" = "42"), name = "Context") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       title = "Behavioural archetype prevalence over time",
       subtitle = "Compositional change, not individual transitions. Grey bands = Lockdown 2/3 and Omicron interventions",
       caption = caption_txt) +
  theme_classic()

ggsave(here::here("YouGov-LCA/outputs/lca_reduced_prevalence_over_time.png"),
       p, width = 11, height = 7, dpi = 200)

cat("\nStage 5 complete. Saved:\n")
cat("- YouGov-LCA/outputs/lca_reduced_prevalence_weekly.csv\n")
cat("- YouGov-LCA/outputs/lca_reduced_prevalence_monthly.csv\n")
cat("- YouGov-LCA/outputs/lca_context_mobility_weekly.csv\n")
cat("- YouGov-LCA/outputs/lca_reduced_prevalence_over_time.png\n")
