# Stage 2 -- select the number of classes (reduced set). HARD STOP 2: after
# this runs, review the outputs and set K_CHOSEN in 03_lca_fit_interpret.R.

library(readr)
library(dplyr)
library(poLCA)
library(ggplot2)
library(patchwork)

yougov_lca <- readRDS(here::here("YouGov-LCA/data/yougov_lca_individual.rds"))

f_reduced <- cbind(mask_outside, avoid_large_gather, avoid_crowds, contacts_nonhh_cat,
                   severity_cat, govt_trust_cat, nhs_confidence_cat) ~ 1

# aBIC (sample-size adjusted BIC) penalises complexity less harshly than BIC
# for large N -- included alongside BIC/AIC since no single criterion is
# universally "correct" for choosing k.
abic <- function(logLik, npar, n) -2 * logLik + log((n + 2) / 24) * npar

# Entropy: how confidently each person is assigned to a single class, given
# their posterior class-membership probabilities. 1 = every person is a
# near-certain member of one class; 0 = classes are indistinguishable.
# Undefined for k=1 (nothing to separate), left as NA.
entropy_r2 <- function(posterior, k) {
  if (k == 1) return(NA_real_)
  1 - mean(rowSums(-posterior * log(posterior + 1e-12)) / log(k))
}

K_RANGE <- 1:7

# Exploratory k-scan only: nrep/maxiter reduced from the ground-rule default
# (30/5000) to 10/1000 purely for speed -- this stage only needs a stable
# relative BIC/entropy trend across k to find the elbow, not the single best
# possible likelihood at every k. The final chosen-k model in Stage 3 still
# uses nrep=50 at the full maxiter, since that is the model actually reported.
NREP_SCAN <- 10
MAXITER_SCAN <- 1000

results <- list()
for (k in K_RANGE) {
  t0 <- Sys.time()
  set.seed(2024)
  fit <- poLCA(f_reduced, data = yougov_lca, nclass = k, nrep = NREP_SCAN,
               maxiter = MAXITER_SCAN, verbose = FALSE)
  results[[k]] <- tibble(
    k             = k,
    log_likelihood = fit$llik,
    npar          = fit$npar,
    bic           = fit$bic,
    aic           = fit$aic,
    abic          = abic(fit$llik, fit$npar, nrow(yougov_lca)),
    entropy       = entropy_r2(fit$posterior, k),
    smallest_class_pct = if (k == 1) 100 else round(100 * min(fit$P), 1)
  )
  cat("k =", k, "fitted in", round(difftime(Sys.time(), t0, units = "secs"), 1),
      "s. BIC =", round(fit$bic, 1),
      " entropy =", round(results[[k]]$entropy, 3), "\n")
}

model_selection <- bind_rows(results)
write_csv(model_selection, here::here("YouGov-LCA/outputs/lca_reduced_model_selection.csv"))

p_fit <- ggplot(model_selection, aes(x = k)) +
  geom_line(aes(y = bic, colour = "BIC")) +
  geom_point(aes(y = bic, colour = "BIC")) +
  geom_line(aes(y = abic, colour = "aBIC")) +
  geom_point(aes(y = abic, colour = "aBIC")) +
  scale_x_continuous(breaks = model_selection$k) +
  labs(x = "Number of classes (k)", y = "Information criterion", colour = NULL,
       title = "Model fit vs number of classes") +
  theme_minimal()

p_entropy <- ggplot(model_selection, aes(x = k, y = entropy)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = model_selection$k) +
  labs(x = "Number of classes (k)", y = "Entropy",
       title = "Class separation vs number of classes") +
  theme_minimal()

ggsave(here::here("YouGov-LCA/outputs/lca_reduced_model_selection.png"),
       p_fit + p_entropy, width = 12, height = 5, dpi = 200)

cat("\n")
print(model_selection)

cat("
How to choose k:
- Lower BIC/aBIC is better. Look for an elbow (bend) rather than the absolute minimum.
- Entropy closer to 1 means cleaner class separation.
- No class should be smaller than ~5% of the sample.
- The solution should be interpretable -- after Stage 3, ask: do the classes tell a coherent story?
- Wright et al. (2022) found k=4 on six binary behaviour items (entropy 0.82) -- a loose prior
  expectation only. Our 7-item mixed binary/multi-level set has more free parameters per class,
  so this is not a like-for-like comparison.

Stage 2 complete. Inspect YouGov-LCA/outputs/lca_reduced_model_selection.csv and .png.
Set K_CHOSEN at the top of YouGov-LCA/03_lca_fit_interpret.R, then continue.
")
