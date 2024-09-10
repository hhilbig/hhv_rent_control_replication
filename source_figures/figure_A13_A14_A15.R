# Clear environment
rm(list = ls())
# Load required packages
pacman::p_load(readr, tidyverse, ggplot2, readxl)

# Load and prepare data
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_kink_relative), rent_sqm_final_relative < 100)

source("source_results/functions.R")

# Define outcomes
outcome_list <- c(
  "y_financial_diff", "y_move_5years", "y_immig_berlin", "y_immig_kiez",
  "y_constr_kiez", "y_diff_constr", "party_left_hte", "y_constr_kiez_bin",
  "y_immig_kiez_bin"
)

outcome_labs <- c(
  "Financial situation difficult", "Propensity to move", "Support more immigration to Berlin",
  "Support local-level immigration", "Support local-level construction",
  "Difference: local-level - Berlin construction", "Support for left-wing parties",
  "Support local-level construction (binary)", "Support local-level immigration (binary)"
)


# Add indices to outcomes
index_list <- c("index_wellbeing", "index_redistribution", "index_nimbyism")
index_labs <- c("Wellbeing index", "Redistribution index", "NIMBYism index")

# Combine outcomes and indices
outcome_list <- c(outcome_list, index_list)
outcome_labs <- c(outcome_labs, index_labs)

# Define NIMBYism outcomes
o_nimby <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")

# Set plot width
w <- 5.5

# Define plotting function
pf <- function(o) {
  df[, "o"] <- df[, o]

  ggplot(data = df, aes(rent_sqm_final_relative, y_constr_kiez)) +
    geom_smooth(
      aes(x = rent_sqm_final_relative, y = o, group = rent_sqm_final_relative > 0),
      span = 5, color = "grey60", method = "loess"
    ) +
    stat_summary_bin(
      aes(x = rent_sqm_final_relative, y = o, group = rent_sqm_final_relative > 0),
      fun = "mean", geom = "point", fill = "white", shape = 21,
      orientation = "x", bins = 10, size = 2
    ) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    theme_bw() +
    labs(
      x = "Rent relative to cutoff",
      y = outcome_labs[outcome_list == o] %>% add_linebreak(8)
    ) +
    theme(axis.text.x = element_text(
      angle = 90, vjust = 0.5,
      hjust = 1
    )) +
    theme(legend.position = "right")
}

# Define covariates
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size")

# Remove missing values
df <- df %>%
  drop_na(one_of(c(outcome_list, "rent_sqm_final_relative", cov_cont, cov_cat)))

# Select relevant columns
df <- df %>%
  select(one_of(c(outcome_list, "rent_sqm_final_relative", cov_cont, cov_cat)))

# Residualize outcomes
for (y in outcome_list) {
  formula <- as.formula(paste0(y, "~", paste0(c(cov_cont, cov_cat), collapse = "+")))
  df[, paste0(y, "_resid")] <- lm(formula, data = df) %>% residuals()
}

# Update outcome lists with residualized versions
outcome_list <- paste0(outcome_list, "_resid")
o_nimby <- paste0(o_nimby, "_resid")
outcome_labs <- paste0(outcome_labs, " (residualized)")

# Generate plots for Figures A13, A14, A15
p1 <- pf(o = o_nimby[1])
p2 <- pf(o = o_nimby[2])
p3 <- pf(o = o_nimby[3])

# Print plots
print(p1)
print(p2)
print(p3)
