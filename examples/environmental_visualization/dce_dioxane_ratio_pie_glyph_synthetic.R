# ============================
# Synthetic DCE–Dioxane Example (targeted ratio behavior)
# Ratio = DCE / Dioxane  (0–500 ft ≈ 1–5; ~1500 ft ≈ 1; far field ≪ 1)
# Three plots: ratio-linear, ratio-log, screen depth
#
# REVISION (requested):
#   - Plot 3 (Screen Depth vs Distance): pie sizes decrease with distance
#   - Depth pattern now has a smooth "down-then-flatten" curve:
#       rapid deepening near source -> approaches ~80–85 ft bgs -> trends laterally
#   - Remove pie_r_depth legend
#   - Add grid lines aligned to major x/y breaks
# ============================

library(dplyr)
library(ggplot2)
library(PieGlyph)
library(scales)
library(tibble)

set.seed(42)

# ----------------------------
# 1) Build synthetic dataset
# ----------------------------
n        <- 36
Dist_FT  <- seq(0, 3500, length.out = n)
Well_ID  <- sprintf("W-%03d", seq_len(n))

# ---- REVISED Depth geometry (curved & flattening) ----
# Rapid deepening near source; asymptotes around ~80–85 ft bgs
base_depth <- 38      # near-source TOS (ft bgs)
delta_dep  <- 45      # total deepening magnitude
L_curve    <- 900     # curvature scale (smaller = earlier flattening)

TOS_dep <- base_depth +
  delta_dep * (1 - exp(-Dist_FT / L_curve)) +
  rnorm(n, 0, 1.5)

Screen_LEN   <- 18 + 6 * runif(n)   # ft
Screen_depth <- TOS_dep + Screen_LEN

# ---- Target ratio curve: r(d) = DCE/Dioxane ----
r0      <- 5.0
r_floor <- 0.03
tau     <- 917

r_clean <- r_floor + (r0 - r_floor) * exp(-Dist_FT / tau)
r_noise <- exp(rnorm(n, mean = 0, sd = 0.12))
r_obs   <- pmax(1e-4, r_clean * r_noise)

# Convert ratio to fractions
f_dioxane <- 1 / (1 + r_obs)
f_dce     <- r_obs / (1 + r_obs)

# Total concentration profile
Total_ppb <- pmax(2, 18 * exp(-Dist_FT / 5000) + 6 + rnorm(n, 0, 0.5))

Dioxane_ppb_l <- Total_ppb * f_dioxane
DCE_ppb_l     <- Total_ppb * f_dce

# Final ratio and crossover
DCE_DX_Ratio <- DCE_ppb_l / pmax(Dioxane_ppb_l, 1e-6)
cross_idx    <- which.min(abs(DCE_DX_Ratio - 1))
cross_x      <- Dist_FT[cross_idx]
cross_y      <- DCE_DX_Ratio[cross_idx]

# Slight jitter for plotting
dfp <- tibble(
  Well_ID, Dist_FT, TOS_dep, Screen_LEN, Screen_depth,
  DCE_ppb_l, Dioxane_ppb_l, DCE_DX_Ratio
) %>%
  mutate(
    Dist_FT_jit      = Dist_FT + rnorm(n(), 0, 10),
    DCE_DX_Ratio_jit = pmax(DCE_DX_Ratio + rnorm(n(), 0, 0.05), 1e-4),
    Screen_depth_jit = Screen_depth + rnorm(n(), 0, 0.5)
  )

# ----------------------------
# 2) Common aesthetics
# ----------------------------
fill_scale <- scale_fill_manual(
  name   = "Constituents",
  values = c("DCE_ppb_l" = "#1b9e77", "Dioxane_ppb_l" = "#d95f02"),
  labels = c("DCE (µg/L)", "1,4-Dioxane (µg/L)")
)

cap_common <- "Synthetic data with targeted ratio decay: near-source DCE-dominant → far-field Dioxane-dominant."

# Axis breaks (used for gridlines too)
x_breaks       <- pretty(dfp$Dist_FT, n = 8)
y_breaks_ratio <- pretty(dfp$DCE_DX_Ratio_jit, n = 8)
y_breaks_depth <- pretty(dfp$Screen_depth_jit, n = 8)

# ----------------------------
# 2A) Pie size schedule for Plot 3 (gradual decrease with distance)
# ----------------------------
pie_r_near <- 0.28
pie_r_far  <- 0.12

dfp <- dfp %>%
  mutate(
    pie_r_depth = scales::rescale(
      Dist_FT,
      to   = c(pie_r_near, pie_r_far),   # decreases as distance increases
      from = range(Dist_FT, na.rm = TRUE)
    )
  )

# Theme with grid lines at major breaks
theme_grid <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray93"),
    plot.title = element_text(face = "bold")
  )

# ----------------------------
# 3) Plot 1: Ratio vs Distance (linear)
# ----------------------------
pie_radius_ratio <- 0.18

p_ratio_lin <- ggplot(dfp, aes(x = Dist_FT_jit, y = DCE_DX_Ratio_jit)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4, color = "grey40") +
  annotate("label", x = cross_x, y = cross_y,
           label = "Crossover (DCE/Dioxane ≈ 1)",
           size = 3, label.size = 0.25, alpha = 0.9) +
  geom_pie_glyph(
    slices    = c("DCE_ppb_l", "Dioxane_ppb_l"),
    colour    = "black", linewidth = 0.3,
    radius    = pie_radius_ratio
  ) +
  fill_scale +
  scale_x_continuous("Distance from source (ft)", breaks = x_breaks, labels = label_comma()) +
  scale_y_continuous("DCE / Dioxane (unitless)",
                     breaks = y_breaks_ratio,
                     labels = label_number(accuracy = 0.01)) +
  labs(
    title = "DCE/Dioxane Ratio vs Distance (Linear Scale)",
    subtitle = "0–500 ft: ~1–5 → ~1500 ft: ~1 → >3000 ft: ≪1",
    caption = cap_common
  ) +
  theme_grid

# ----------------------------
# 4) Plot 2: Ratio vs Distance (log10)
# ----------------------------
p_ratio_log <- ggplot(dfp %>% filter(is.finite(DCE_DX_Ratio_jit), DCE_DX_Ratio_jit > 0),
                      aes(x = Dist_FT_jit, y = DCE_DX_Ratio_jit)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4, color = "grey40") +
  geom_pie_glyph(
    slices    = c("DCE_ppb_l", "Dioxane_ppb_l"),
    colour    = "black", linewidth = 0.3,
    radius    = pie_radius_ratio
  ) +
  fill_scale +
  scale_x_continuous("Distance from source (ft)", breaks = x_breaks, labels = label_comma()) +
  scale_y_log10("DCE / Dioxane (log scale)",
                breaks = 10^seq(-2, 2),
                labels = label_number(accuracy = 0.01)) +
  labs(
    title = "DCE/Dioxane Ratio vs Distance (Log Scale)",
    subtitle = "Dashed line at ratio = 1 (equal contributions)",
    caption = cap_common
  ) +
  theme_grid

# ----------------------------
# 5) Plot 3: Screen Depth vs Distance
#     - Curved depth geometry + decreasing pie size with distance
#     - Remove pie_r_depth legend
#     - Gridlines at major breaks
# ----------------------------
p_depth <- ggplot(dfp, aes(x = Dist_FT_jit, y = Screen_depth_jit)) +
  geom_pie_glyph(
    aes(radius = pie_r_depth),
    slices    = c("DCE_ppb_l", "Dioxane_ppb_l"),
    colour    = "black", linewidth = 0.3
  ) +
  fill_scale +
  scale_x_continuous("Distance from source (ft)", breaks = x_breaks, labels = label_comma()) +
  scale_y_reverse("Top of Screen Depth (ft bgs)", breaks = y_breaks_depth) +
  guides(radius = "none") +   # <-- removes pie_r_depth / radius legend
  labs(
    title = "Screen Depth vs Distance (Synthetic Data Set)",
    caption = "BASIS: Single Source-Diving Plume Pattern; pie size/1,4-Dioxane decreases with distance."
  ) +
  theme_grid

# ----------------------------
# 6) Save and print
# ----------------------------
ggsave("synthetic_ratio_linear_nolabels.png", p_ratio_lin, width = 12, height = 6, dpi = 300)
ggsave("synthetic_ratio_log_nolabels.png",    p_ratio_log, width = 12, height = 6, dpi = 300)
ggsave("synthetic_depth_nolabels.png",        p_depth,     width = 17, height = 10, dpi = 300)

print(p_ratio_lin)
print(p_ratio_log)
print(p_depth)
