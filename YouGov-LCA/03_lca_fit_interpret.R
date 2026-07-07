# ASSUMPTION: fixed classes across time
# The model is fitted on all respondents pooled across the analysis window.
# The archetype PROFILES (what each class looks like) are assumed stable over time.
# What changes over time is the CLASS PREVALENCE (share of population in each class).
# This is an explicit preliminary assumption, to be tested via measurement invariance
# in future work. It means we cannot say whether "being cautious" meant the same
# thing in July 2020 as in January 2021 -- only that across the whole period, there
# is a stable group of people who report both high risk perception and high
# protective behaviour.

library(readr)
library(dplyr)
library(tidyr)
library(poLCA)
library(ggplot2)

K_CHOSEN <- 3

yougov_lca <- readRDS(here::here("YouGov-LCA/data/yougov_lca_individual.rds"))

f_reduced <- cbind(mask_outside, avoid_large_gather, avoid_crowds, contacts_nonhh_cat,
                   severity_cat, govt_trust_cat, nhs_confidence_cat) ~ 1

set.seed(2024)
fit <- poLCA(f_reduced, data = yougov_lca, nclass = K_CHOSEN, nrep = 50,
             maxiter = 5000, verbose = FALSE)

write_rds(fit, here::here(sprintf("YouGov-LCA/models/lca_reduced_fit_k%d.rds", K_CHOSEN)))

entropy_r2 <- function(posterior, k) {
  if (k == 1) return(NA_real_)
  1 - mean(rowSums(-posterior * log(posterior + 1e-12)) / log(k))
}

cat("Mixing proportions (% of sample per class):\n")
print(round(100 * fit$P, 1))
cat("Entropy:", round(entropy_r2(fit$posterior, K_CHOSEN), 3), "\n\n")

# Tidy long-format table of every class x item x category probability -------
tidy_probs <- bind_rows(lapply(names(fit$probs), function(item) {
  m <- fit$probs[[item]]
  colnames(m) <- seq_len(ncol(m))
  as_tibble(m) |>
    mutate(class = row_number()) |>
    pivot_longer(-class, names_to = "category", values_to = "probability") |>
    mutate(item = item, category = as.integer(category))
}))
write_csv(tidy_probs, here::here("YouGov-LCA/outputs/lca_reduced_class_profiles.csv"))

# Profile plot ---------------------------------------------------------------
# Binary items: probability of "yes" (category 2). Ordered items: mean
# expected category (sum(category * probability)) -- a single-number summary
# per class per item so all 7 indicators can share one axis.
binary_items  <- c("mask_outside", "avoid_large_gather", "avoid_crowds")
ordinal_items <- c("contacts_nonhh_cat", "severity_cat", "govt_trust_cat", "nhs_confidence_cat")
item_order    <- c(binary_items, ordinal_items)
item_labels <- c(
  mask_outside       = "Wore a mask outside",
  avoid_large_gather = "Avoided large\ngatherings",
  avoid_crowds       = "Avoided crowded\nareas",
  contacts_nonhh_cat = "Non-household\ncontacts",
  severity_cat       = "Perceived severity",
  govt_trust_cat     = "Trust in\ngovernment",
  nhs_confidence_cat = "Confidence in NHS"
)

# Panels are split by what's actually plotted, not by conceptual domain:
# contacts_nonhh_cat is a behaviour but its recoded form is a mean category
# (1-4) like the attitude items, not a P(yes) -- grouping it with the 3 true
# binary items would make the "Behaviour: P(yes)" panel label false for it.
domain_levels <- c("Behaviour (yes/no)", "Attitude & contact frequency (mean category)")

profile_summary <- tidy_probs |>
  group_by(class, item) |>
  summarise(
    value = if (first(item) %in% binary_items) probability[category == 2]
            else sum(category * probability),
    .groups = "drop"
  ) |>
  mutate(
    domain = if_else(item %in% binary_items, domain_levels[1], domain_levels[2]),
    domain = factor(domain, levels = domain_levels),
    item   = factor(item_labels[item], levels = item_labels[item_order]),
    class  = factor(class)
  )

p <- ggplot(profile_summary, aes(x = item, y = value, colour = class, group = class)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~domain, scales = "free", nrow = 1) +
  labs(x = NULL, y = "P(yes) / mean category", colour = "Class",
       title = sprintf("Reduced-set LCA class profiles (k=%d)", K_CHOSEN),
       subtitle = "Behaviour items: probability of 'yes'. Attitude and contact-frequency items: mean expected category.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(here::here("YouGov-LCA/outputs/lca_reduced_class_profiles.png"), p,
       width = 10, height = 6, dpi = 200)

cat("\nProfile summary (for labelling):\n")
print(profile_summary |> arrange(item, class), n = Inf)

cat("\nStage 3 complete. Review YouGov-LCA/outputs/lca_reduced_class_profiles.{csv,png}\n")
cat("and YouGov-LCA/models/lca_reduced_fit_k", K_CHOSEN, ".rds\n", sep = "")
