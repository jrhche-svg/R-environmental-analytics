## ============================================================
## Appendix 12–style sampling curves for ACM screening
## Unit = residential dwelling (apartment or home)
## Curves shown: 1%, 5%, 10% prevalence bounds
##
## Interpretation:
##   If 0 positives are observed in n sampled units,
##   we are 95% confident true prevalence is below the curve.
##
## Plot behavior:
##   Stepwise (hold-then-jump) and monotone non-decreasing n via cummax().
## ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# Hypergeometric probability: P(0 positives)
# ------------------------------------------------------------
p_zero_hits <- function(N, K, n) {
  if (K == 0 || n == 0) return(1)
  phyper(q = 0, m = K, n = N - K, k = n)
}

# ------------------------------------------------------------
# Smallest n such that P(0 hits | K positives) < alpha
# ------------------------------------------------------------
min_n_for_alpha <- function(N, K, alpha = 0.05) {
  for (n in 1:N) {
    if (p_zero_hits(N, K, n) < alpha) return(n)
  }
  N
}

# ------------------------------------------------------------
# Parameterized Appendix-12–style sampler
# IMPORTANT: returns ONLY new columns (n, k, K, method)
# to avoid unnest() duplicating names like N and prevalence.
# ------------------------------------------------------------
appendix12_sampler <- function(N,
                               prevalence,
                               cap_units,
                               largeN_pct,
                               alpha = 0.05,
                               largeN_threshold = 1040,
                               use_largeN_pct_rule = TRUE) {
  
  # Largest acceptable number of positives
  k <- min(ceiling(prevalence * N) - 1, cap_units - 1)
  k <- max(k, 0)
  
  # Smallest unacceptable case (Appendix 12 Note 2 logic)
  K <- k + 1
  
  if (N <= largeN_threshold || !use_largeN_pct_rule) {
    n <- min_n_for_alpha(N, K, alpha)
    method <- "Exact hypergeometric"
  } else {
    n <- round(largeN_pct * N)
    n <- max(1, min(n, N))
    method <- "Large-N percentage rule"
  }
  
  tibble(
    n = n,
    k = k,
    K = K,
    method = method
  )
}

# ------------------------------------------------------------
# Build sampling curves using tidyr::expand_grid
# ------------------------------------------------------------
build_curves <- function(N_min = 10, N_max = 2000) {
  
  prevalence_defs <- tibble(
    prevalence = c(0.01, 0.05, 0.10),
    cap_units  = c(10, 50, 100),
    largeN_pct = c(0.012, 0.058, 0.029),
    label      = c("1% bound", "5% bound", "10% bound")
  )
  
  expand_grid(
    N = N_min:N_max,
    prevalence_defs
  ) |>
    rowwise() |>
    mutate(result = list(
      appendix12_sampler(
        N = N,
        prevalence = prevalence,
        cap_units = cap_units,
        largeN_pct = largeN_pct
      )
    )) |>
    unnest(result) |>
    ungroup()
}

# ------------------------------------------------------------
# Generate raw curve data
# ------------------------------------------------------------
df <- build_curves(10, 2000)

# ------------------------------------------------------------
# Enforce monotone, stepwise sample sizes for plotting
# ------------------------------------------------------------
df_step <- df |>
  group_by(label) |>
  arrange(N, .by_group = TRUE) |>
  mutate(n_step = cummax(n)) |>
  ungroup()


# ------------------------------------------------------------
# Step-range table: collapse constant n_step segments into N ranges
# Output columns: label, n_step, N_min, N_max
# ------------------------------------------------------------
make_step_table <- function(df_step) {
  
  df_step %>%
    group_by(label) %>%
    arrange(N, .by_group = TRUE) %>%
    mutate(segment_id = cumsum(N == first(N) | n_step != lag(n_step, default = first(n_step)))) %>%
    group_by(label, segment_id) %>%
    summarise(
      n_step = first(n_step),
      N_min  = min(N),
      N_max  = max(N),
      .groups = "drop"
    ) %>%
    select(label, n_step, N_min, N_max) %>%
    arrange(label, N_min)
}

step_table <- make_step_table(df_step)

# Print to console
print(step_table, n = 200)

# Optional: write to CSV for reporting
# write.csv(step_table, "ACM_sampling_step_table.csv", row.names = FALSE)


# ------------------------------------------------------------
# Plot (step function)
# ------------------------------------------------------------
ggplot(df_step, aes(x = N, y = n_step, color = label)) +
  geom_step(linewidth = 1) +
  geom_vline(
    xintercept = 1040,
    linetype = "dashed",
    color = "grey40"
  ) +
  labs(
    title = "Appendix 12–Style Sampling Curves for ACM Screening",
    subtitle = "Stepwise (hold-then-jump) sample sizes; 95% confidence, zero-detects design",
    x = "Number of residential units in group (N)",
    y = "Required sample size (n)",
    color = "Prevalence bound"
  ) +
  scale_color_manual(
    values = c(
      "1% bound"  = "#7570b3",
      "5% bound"  = "#1b9e77",
      "10% bound" = "#d95f02"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )
