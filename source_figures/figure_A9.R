# Clear environment and load packages
rm(list = ls())
pacman::p_load(readr, tidyverse, stringr, purrr, ggplot2, rddensity, rdd)

# Load and prepare data
df <- read_rds("data/data_main.rds") %>%
  select(
    matches("sample"), rent_sqm_final_relative,
    year_constr_mode, index_nimbyism
  )

# Create dataset based on sample dummies
dummy_list <- df %>%
  colnames() %>%
  str_subset("sample")

out <- dummy_list %>%
  map_dfr(~ df %>%
    filter(!!sym(.x) == 1) %>%
    mutate(group = .x)) %>%
  select(-one_of(dummy_list))

# Prepare data for kink plots
df_k <- out %>%
  filter(str_detect(group, "kink"), abs(rent_sqm_final_relative) <= 5) %>%
  mutate(group = if_else(str_detect(group, "owners_kink"), "Owners", "Tenants"))

# Figure A9: Histogram for kink design
p1 <- ggplot(df_k, aes(x = rent_sqm_final_relative)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_histogram(color = "black", fill = "grey95", binwidth = 0.5, center = 0.75) +
  theme_bw() +
  labs(y = "Number of respondents", x = "Rent relative to threshold (prior to policy)")
print(p1)

# Density tests
m1 <- rddensity(X = df$rent_sqm_final_relative[df$sample_tenants_kink == 1], c = 0, h = 5)
print(paste("Kink tenants p-value:", m1$test$p_jk))

m3 <- rddensity(X = df$year_constr_mode[df$sample_tenants_rd == 1], c = 2013.5, h = 2)
print(paste("RD tenants p-value:", m3$test$p_jk))
