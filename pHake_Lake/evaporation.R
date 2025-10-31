# Phake Lake Evaporation Estimation Model
# This model estimates lake evaporation using multiple methods

# Required libraries
library(ggplot2)
library(dplyr)

# ============================================================================
# EVAPORATION ESTIMATION FUNCTIONS
# ============================================================================

#' Penman Method for Lake Evaporation
#' @param Rs Solar radiation (MJ/m²/day)
#' @param Ta Air temperature (°C)
#' @param Tw Water temperature (°C)
#' @param RH Relative humidity (%)
#' @param u Wind speed at 2m height (m/s)
#' @return Evaporation rate (mm/day)
penman_evaporation <- function(Rs, Ta, Tw, RH, u) {
  # Constants
  lambda <- 2.45  # Latent heat of vaporization (MJ/kg)
  gamma <- 0.067  # Psychrometric constant (kPa/°C)
  
  # Saturation vapor pressure (kPa) - Tetens equation
  es_air <- 0.6108 * exp((17.27 * Ta) / (Ta + 237.3))
  es_water <- 0.6108 * exp((17.27 * Tw) / (Tw + 237.3))
  
  # Actual vapor pressure (kPa)
  ea <- es_air * (RH / 100)
  
  # Slope of saturation vapor pressure curve (kPa/°C)
  delta <- 4098 * es_air / (Ta + 237.3)^2
  
  # Aerodynamic term
  f_u <- 2.6 * (1 + 0.54 * u)  # Wind function
  E_aero <- f_u * (es_water - ea)
  
  # Radiation term (simplified, assuming net radiation ≈ 0.7 * Rs)
  Rn <- 0.7 * Rs  # Net radiation approximation
  E_rad <- delta * Rn / lambda
  
  # Combined Penman equation
  E <- (E_rad + gamma * E_aero) / (delta + gamma)
  
  return(pmax(E, 0))  # Ensure non-negative
}

#' Dalton-type Mass Transfer Method
#' @param Ta Air temperature (°C)
#' @param Tw Water temperature (°C)
#' @param RH Relative humidity (%)
#' @param u Wind speed (m/s)
#' @return Evaporation rate (mm/day)
dalton_evaporation <- function(Ta, Tw, RH, u) {
  # Saturation vapor pressure (kPa)
  es_water <- 0.6108 * exp((17.27 * Tw) / (Tw + 237.3))
  es_air <- 0.6108 * exp((17.27 * Ta) / (Ta + 237.3))
  
  # Actual vapor pressure
  ea <- es_air * (RH / 100)
  
  # Mass transfer coefficient
  k <- 0.13 + 0.14 * u  # Empirical coefficient
  
  # Evaporation (mm/day)
  E <- k * (es_water - ea)
  
  return(pmax(E, 0))
}

#' Priestley-Taylor Method (simplified)
#' @param Rs Solar radiation (MJ/m²/day)
#' @param Ta Air temperature (°C)
#' @return Evaporation rate (mm/day)
priestley_taylor <- function(Rs, Ta) {
  lambda <- 2.45
  gamma <- 0.067
  alpha <- 1.26  # Priestley-Taylor coefficient for water bodies
  
  # Saturation vapor pressure slope
  es <- 0.6108 * exp((17.27 * Ta) / (Ta + 237.3))
  delta <- 4098 * es / (Ta + 237.3)^2
  
  # Net radiation (approximation)
  Rn <- 0.7 * Rs
  
  # Evaporation
  E <- alpha * (delta / (delta + gamma)) * (Rn / lambda)
  
  return(pmax(E, 0))
}

# ============================================================================
# MODEL APPLICATION
# ============================================================================

#' Estimate Lake Evaporation
#' @param data Data frame with columns: Rs, Ta, Tw, RH, u
#' @param method Method to use: "penman", "dalton", "priestley_taylor", or "ensemble"
#' @return Data frame with evaporation estimates
estimate_evaporation <- function(data, method = "ensemble") {
  results <- data %>%
    mutate(
      E_penman = penman_evaporation(Rs, Ta, Tw, RH, u),
      E_dalton = dalton_evaporation(Ta, Tw, RH, u),
      E_priestley = priestley_taylor(Rs, Ta)
    )
  
  # Calculate ensemble mean
  results$E_ensemble <- rowMeans(results[, c("E_penman", "E_dalton", "E_priestley")])
  
  # Select primary estimate based on method
  results$E_estimate <- switch(method,
                               "penman" = results$E_penman,
                               "dalton" = results$E_dalton,
                               "priestley_taylor" = results$E_priestley,
                               "ensemble" = results$E_ensemble
  )
  
  return(results)
}

# ============================================================================
# EXAMPLE DATA AND APPLICATION
# ============================================================================

# Create example dataset for Phake Lake
set.seed(123)
n_days <- 365

phake_lake_data <- data.frame(
  date = seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days),
  doy = 1:n_days
) %>%
  mutate(
    # Solar radiation (MJ/m²/day) - seasonal variation
    Rs = 15 + 10 * sin(2 * pi * (doy - 80) / 365) + rnorm(n_days, 0, 2),
    # Air temperature (°C) - seasonal variation
    Ta = 15 + 10 * sin(2 * pi * (doy - 80) / 365) + rnorm(n_days, 0, 2),
    # Water temperature (°C) - lags air temperature slightly
    Tw = 16 + 9 * sin(2 * pi * (doy - 95) / 365) + rnorm(n_days, 0, 1.5),
    # Relative humidity (%) - inverse seasonal pattern
    RH = 65 - 15 * sin(2 * pi * (doy - 80) / 365) + rnorm(n_days, 0, 5),
    # Wind speed (m/s)
    u = abs(3 + 1.5 * sin(2 * pi * doy / 365) + rnorm(n_days, 0, 0.8))
  ) %>%
  mutate(
    Rs = pmax(Rs, 0),
    RH = pmax(pmin(RH, 100), 0)
  )

# Estimate evaporation using all methods
results <- estimate_evaporation(phake_lake_data, method = "ensemble")

# Print summary statistics
cat("=== Phake Lake Evaporation Model Results ===\n\n")
cat("Annual Evaporation Estimates (mm):\n")
cat(sprintf("  Penman Method:         %.1f mm\n", sum(results$E_penman)))
cat(sprintf("  Dalton Method:         %.1f mm\n", sum(results$E_dalton)))
cat(sprintf("  Priestley-Taylor:      %.1f mm\n", sum(results$E_priestley)))
cat(sprintf("  Ensemble Mean:         %.1f mm\n", sum(results$E_ensemble)))

cat("\nDaily Statistics (mm/day):\n")
cat(sprintf("  Mean:   %.2f\n", mean(results$E_ensemble)))
cat(sprintf("  Median: %.2f\n", median(results$E_ensemble)))
cat(sprintf("  Min:    %.2f\n", min(results$E_ensemble)))
cat(sprintf("  Max:    %.2f\n", max(results$E_ensemble)))

# ============================================================================
# VISUALIZATION
# ============================================================================

# Plot 1: Daily evaporation time series
p1 <- ggplot(results, aes(x = date, y = E_ensemble)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE, alpha = 0.2) +
  labs(title = "Phake Lake Daily Evaporation Estimate",
       subtitle = "Ensemble method (mean of Penman, Dalton, and Priestley-Taylor)",
       x = "Date",
       y = "Evaporation (mm/day)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p1)

# Plot 2: Comparison of methods
p2 <- results %>%
  select(date, E_penman, E_dalton, E_priestley, E_ensemble) %>%
  tidyr::pivot_longer(cols = -date, names_to = "Method", values_to = "Evaporation") %>%
  mutate(Method = recode(Method,
                         "E_penman" = "Penman",
                         "E_dalton" = "Dalton",
                         "E_priestley" = "Priestley-Taylor",
                         "E_ensemble" = "Ensemble"
  )) %>%
  ggplot(aes(x = date, y = Evaporation, color = Method)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  labs(title = "Comparison of Evaporation Estimation Methods",
       x = "Date",
       y = "Evaporation (mm/day)") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

print(p2)

# Plot 3: Monthly aggregation
monthly_results <- results %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(
    monthly_evap = sum(E_ensemble),
    avg_Rs = mean(Rs),
    avg_Ta = mean(Ta),
    avg_RH = mean(RH),
    .groups = "drop"
  )

p3 <- ggplot(monthly_results, aes(x = as.Date(paste0(month, "-01")), y = monthly_evap)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(title = "Phake Lake Monthly Evaporation",
       x = "Month",
       y = "Total Evaporation (mm)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p3)

# Return results for further analysis
results