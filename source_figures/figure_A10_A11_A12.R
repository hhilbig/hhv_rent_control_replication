# Clear environment
rm(list = ls())

# Load required packages
pacman::p_load(readr, tidyverse, tidyr, ggplot2, readxl)

# Load and prepare data
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_rd_relative))

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

# Calculate conditional means
cmeans <- df %>%
  filter(!is.na(years_rel_2013)) %>%
  select(years_rel_2013, one_of(outcome_list)) %>%
  pivot_longer(cols = -years_rel_2013, names_to = "outcome", values_to = "value") %>%
  group_by(years_rel_2013, outcome) %>%
  summarise(
    m = mean(value, na.rm = TRUE),
    sd_y = sd(value, na.rm = TRUE),
    n = n(),
    sd_m = sd_y / sqrt(n),
    conf.low = m - sd_m * qnorm(0.975),
    conf.high = m + sd_m * qnorm(0.975)
  ) %>%
  left_join(data.frame(outcome = outcome_list, lab = outcome_labs)) %>%
  filter(outcome %in% o_nimby)

# Define plotting function
pf <- function(o) {
  cmeans_constr <- cmeans %>%
    filter(outcome == o)

  df[, "o"] <- df[, o]

  ggplot(data = df, aes(-years_rel_2013 + 2013, y_constr_kiez)) +
    geom_smooth(
      aes(x = -years_rel_2013 + 2013, y = o, group = years_rel_2013 > -1),
      span = 5, color = "grey60", method = "loess"
    ) +
    geom_errorbar(
      data = cmeans_constr,
      aes(x = -years_rel_2013 + 2013, y = m, ymin = conf.low, ymax = conf.high),
      width = 0
    ) +
    geom_point(
      data = cmeans_constr,
      aes(x = -years_rel_2013 + 2013, y = m, size = n),
      shape = 21, fill = "white"
    ) +
    geom_vline(xintercept = 2013.5, linetype = "dotted", color = "grey50") +
    theme_bw() +
    labs(
      x = "Construction year",
      y = outcome_labs[outcome_list == o] %>% add_linebreak(8)
    ) +
    scale_x_continuous(breaks = 2010:2017) +
    x_axis_90deg() +
    theme(legend.position = "right") +
    scale_size_continuous(
      trans = "log",
      name = "Number of\nrespondents",
      breaks = c(50, 100, 150),
      range = c(2, 4)
    )
}

# Define covariates
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size")

# Remove missing values and select relevant columns
df <- df %>%
  drop_na(one_of(c(outcome_list, "years_rel_2013", cov_cont, cov_cat))) %>%
  select(one_of(c(outcome_list, "years_rel_2013", cov_cont, cov_cat)))

# Residualize outcomes
for (y in outcome_list) {
  formula <- as.formula(paste0(y, "~", paste0(c(cov_cont, cov_cat), collapse = "+")))
  df[, paste0(y, "_resid")] <- lm(formula, data = df) %>% residuals()
}

# Update outcome lists with residualized versions
outcome_list <- paste0(outcome_list, "_resid")
o_nimby <- paste0(o_nimby, "_resid")
outcome_labs <- paste0(outcome_labs, " (residualized)")

# Recalculate conditional means for residualized outcomes
cmeans <- df %>%
  filter(!is.na(years_rel_2013)) %>%
  select(years_rel_2013, one_of(outcome_list)) %>%
  pivot_longer(cols = -years_rel_2013, names_to = "outcome", values_to = "value") %>%
  group_by(years_rel_2013, outcome) %>%
  summarise(
    m = mean(value, na.rm = TRUE),
    sd_y = sd(value, na.rm = TRUE),
    n = n(),
    sd_m = sd_y / sqrt(n),
    conf.low = m - sd_m * qnorm(0.975),
    conf.high = m + sd_m * qnorm(0.975)
  ) %>%
  left_join(data.frame(outcome = outcome_list, lab = outcome_labs)) %>%
  filter(outcome %in% o_nimby)

# Generate plots for Figures A10, A11, A12
pf(o = o_nimby[1])
pf(o = o_nimby[2])
pf(o = o_nimby[3])
