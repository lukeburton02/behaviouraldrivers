# Plot YouGov drivers per week for overall population

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Load data -----
yougov_weekly <- read_csv("data-processed/yougov_weekly.csv", show_col_types = FALSE) |>
  mutate(week_start = as_date(week_start))

# Reshape to long format for plotting and label ------
driver_labels <- c(
  perceived_severity   = "Perceived severity\n(1–7, higher = more dangerous)",
  perceived_likelihood = "Perceived likelihood\n(1–7, higher = more likely)",
  fear_scared_prop     = "Proportion fairly/very scared\nof contracting COVID-19",
  govt_handling        = "Government handling rating\n(1 = very badly, 4 = very well)",
  nhs_confidence       = "NHS confidence\n(1 = not at all, 4 = very confident)",
  willing_isolate      = "Willingness to self-isolate\n(1 = very unwilling, 5 = very willing)"
)

yougov_long <- yougov_weekly |>
  select(
    week_start,
    n,
    all_of(names(driver_labels))) |>
      pivot_longer(
        cols = all_of(names(driver_labels)),
        names_to = "driver",
        values_to = "value"
      ) |>
      mutate(driver = factor(
        driver,
        levels = names(driver_labels),
        labels = driver_labels))
    
# Key pandemic period markers -----
lockdowns <- data.frame(
  xmin  = as_date(c("2020-03-23", "2020-11-05", "2021-01-05")),
  xmax  = as_date(c("2020-06-01", "2020-12-02", "2021-03-08")),
  label = c("LD1", "LD2", "LD3")
)

# Plot -----
p <- ggplot(yougov_long, aes(x = week_start, y = value)) +
  geom_rect(
    data = lockdowns,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "steelblue", alpha = 0.1
  ) +
  geom_line(linewidth = 0.7, colour = "#2c3e50") +
  geom_point(size = 0.8, colour = "#2c3e50") +
  facet_wrap(~ driver, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title = "YouGov COVID-19 Behavioural Tracker - UK weekly driver time series",
    subtitle = "Weighted weekly means, April 2020-March 2022. Shaded regions = UK national lockdowns",
    x = NULL,
    y = NULL,
    caption = "Source: Imperial College London / YouGov COVID-19 Behacioural Tracker"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    panel.grid.minor = element_blank()
  )

# Save plot -------
ggsave("outputs/yougov_overall_drivers.png", p,
       width = 12, height = 10, dpi = 300, bg = "white")
message("Saved: outputs/yougov_overall_drivers.png")