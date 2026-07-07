# Notes for future extensions

Written after the first-pass k=3 reduced-set run. Read before changing the
variable set, k, or swapping datasets.

## If you change the reduced variable set

- **Update in 4 places, not 1**: the `cbind(...) ~ 1` formula and recode
  functions are independently redefined in `01_build_lca_dataset.R`,
  `02_lca_select_k.R`, `03_lca_fit_interpret.R`, and again inside
  `04_lca_distributions.R`'s 4b covariate section. Nothing shares a single
  source of truth — miss one and it'll fit silently on the old variable set.
- **`04_lca_distributions.R` (Stage 4c) is the most fragile point.** It
  reconstructs Stage 1's entire window-filter + recode pipeline a second time
  from raw data, purely to join `PHQ4_*`/`cantril_ladder` back onto the
  dataset, then asserts `nrow(driver_data) == nrow(yougov_lca)`. If Stage 1's
  recode logic changes and this copy isn't updated in lockstep, the assertion
  either fails loudly (best case) or — if row counts coincidentally still
  match — silently misaligns `class` labels to the wrong people.
- **Stage 3's profile plot has hardcoded item lists** (`binary_items`,
  `item_labels`, the "Behaviour (yes/no)" vs "Attitude & contact frequency"
  domain split). A new variable set means manually deciding which new items
  are binary vs ordinal and re-picking plain-English labels — this doesn't
  generalise automatically.
- Rerun Stage 0 first — coverage dates and the joint analysis window will
  likely shift with different indicators.

## If you change k

- `K_CHOSEN` is hardcoded separately in Stage 3, Stage 4, and Stage 5 (not
  read from one place).
- `class_labels` in Stage 5 (`Trusting compliers` / `Disengaged` / `Compliant
  sceptics`) are this run's substantive interpretation of k=3 — meaningless
  for a different k until re-derived from the new Stage 3 profile output.

## If you switch to CoMix, REACT, or other individual-level data

- Stages 0–1 (raw loading + recoding) are YouGov-specific from the ground up
  and would need rewriting, not adapting.
- Stage 5's OxCGRT/mobility context-loading code is dataset-agnostic and
  should carry over as-is, provided the new individual dataset still has a
  `date`/`week_start`-alignable column.
- Bigger analytical point, not just a coding one: YouGov is a repeated
  cross-section, which is why Stage 5 carries the "compositional change, not
  individual transitions" caveat throughout. Individual-level panel data
  (CoMix, REACT) removes that limitation — you could track actual archetype
  transitions per person, which is currently listed as a "next step" precisely
  because YouGov can't do it.

## General: worth overhauling repo structure toward more modular/reproducible research code

The numbered-script-per-stage approach (each self-contained, some deliberate
duplication) was the right call for a fast first pass with manual checkpoints
between stages. It stops paying off once the variable set/k/dataset are being
iterated on repeatedly — the duplication above becomes a real source of silent
bugs. Given this is already scaffolded as an R package (`DESCRIPTION`,
`NAMESPACE`, `R/`, `Roxygen`), the natural fix is to move the shared pieces
(recode functions, the poLCA formula, `K_CHOSEN`, class labels/entropy helper)
into documented functions in `R/`, sourced by every stage script instead of
copy-pasted — standard practice for this to remain reproducible as it grows,
not a stylistic preference.
