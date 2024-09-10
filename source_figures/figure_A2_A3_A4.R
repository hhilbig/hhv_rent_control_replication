# Clear environment and load required packages
rm(list = ls())
pacman::p_load(readr, tidyverse, ggplot2)

# Load and preprocess data
df <- read_rds("data/data_main.rds") %>%
  filter(
    !is.na(treated_rd_relative),
    rent_sqm_final_relative <= quantile(rent_sqm_final_relative, 0.99, na.rm = TRUE),
    year_constr_mode < 2014
  ) %>%
  select(rent_sqm_final_relative, apartment_size, year_constr_mode, rent_sqm_final) %>%
  mutate(
    pct_reduc = rent_sqm_final_relative * 100 / rent_sqm_final,
    annual_reduction = rent_sqm_final_relative * apartment_size * 12
  )

# Helper function to create histogram with mean and median lines
create_histogram <- function(data, x, xlab, binwidth = NULL) {
  mean_val <- mean(data[[x]], na.rm = TRUE)
  median_val <- median(data[[x]], na.rm = TRUE)

  ggplot(data, aes_string(x = x)) +
    geom_histogram(fill = "grey95", color = "black", binwidth = binwidth) +
    geom_vline(xintercept = mean_val, linetype = "solid", size = 1.5) +
    geom_vline(xintercept = median_val, linetype = "dashed", size = 1.5) +
    xlab(xlab) +
    ylab("Number of apartments / respondents") +
    theme_bw()
}

# Figure A2: Distribution of rent decrease per sqm
p1 <- create_histogram(df, "rent_sqm_final_relative", "Rent reduction due to policy (Euro/sqm)")
print(p1)

# Figure A4: Distribution of annual total rent reduction
p2 <- create_histogram(df, "annual_reduction", "Rent reduction due to policy (Euro/year)")
print(p2)

# Figure A3: Distribution of percentage rent reduction
p3 <- create_histogram(df, "pct_reduc", "Per-sqm rent reduction in percent")
print(p3)

# Calculate and print summary statistics
summary_stats <- df %>%
  summarise(
    mean_reduction_sqm = mean(rent_sqm_final_relative, na.rm = TRUE),
    median_reduction_sqm = median(rent_sqm_final_relative, na.rm = TRUE),
    mean_reduction_annual = mean(annual_reduction, na.rm = TRUE),
    median_reduction_annual = median(annual_reduction, na.rm = TRUE),
    mean_reduction_pct = mean(pct_reduc, na.rm = TRUE),
    median_reduction_pct = median(pct_reduc, na.rm = TRUE),
    q75_reduction_pct = quantile(pct_reduc, 0.75, na.rm = TRUE)
  )

print(summary_stats)
