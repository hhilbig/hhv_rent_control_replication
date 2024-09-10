# Clear environment and load packages
rm(list = ls())

pacman::p_load(
  haven, fastDummies, pbapply, readxl, writexl,
  tidyverse, stringr, lubridate, tidyverse, rdrobust
)

# Source helper functions
source("source_results/functions.R")

# Load and prepare data ------------------------------------------------

# Read main dataset
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_rd_relative))

# Define outcomes and labels
outcome_list <- c(
  "y_financial_diff", "y_move_5years", "y_immig_berlin", "y_immig_kiez",
  "y_constr_kiez", "y_diff_constr", "party_left_hte", "y_constr_kiez_bin",
  "y_immig_kiez_bin", "index_wellbeing", "index_redistribution",
  "index_nimbyism"
)
outcome_labs <- c(
  "Financial situation difficult", "Propensity to move", "Support more immigration to Berlin",
  "Support local-level immigration", "Support local-level construction",
  "Difference: local-level - Berlin construction", "Support for left-wing parties",
  "Support local-level construction (binary)", "Support local-level immigration (binary)",
  "Wellbeing index", "Redistribution index", "NIMBYism index"
)

# Define covariates
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size", "days_since_ad")
cov_list <- c(cov_cat, cov_cont)
cov_list_notimesincead <- setdiff(cov_list, "days_since_ad")
cov_list_no_posttreat <- setdiff(cov_list, c("days_since_ad", "party", "apartment_size"))

# Identify continuous outcomes
outcome_list_cont <- setdiff(outcome_list, c("party_left_hte", "y_constr_kiez_bin", "y_immig_kiez_bin"))

# Standardize continuous outcomes
df <- df %>%
  mutate(across(all_of(outcome_list_cont), ~ . / sd(., na.rm = TRUE)))

# Generate subsets
ss_list <- list(
  df,
  df %>% filter(!rent_imputed == 1),
  df %>% filter(gentri2 == 1 & rent_hte_tenants_terc == "hi"),
  df %>% filter(gentri2 == 0 & rent_hte_tenants_terc == "hi")
)
ss_labels <- c(
  "1 Full",
  "2 No units where rent is imputed",
  "7 Gentrification high - High rent",
  "8 Gentrification low - High rent"
)

# Regression Discontinuity Analysis ------------------------------------

res <- pblapply(outcome_list, function(o) {
  lapply(seq_along(ss_list), function(i) {
    ss_df <- ss_list[[i]]
    ss_lab <- ss_labels[i]

    y <- ss_df %>% pull(!!o)
    x <- ss_df %>% pull(years_rel_2013)

    # Prepare covariate matrices
    cmat_no_time_since_ad <- model.matrix(~ . - 1, data = ss_df[, cov_list_notimesincead])
    cmat_no_posttreat <- model.matrix(~ . - 1, data = ss_df[, cov_list_no_posttreat])

    # RD without covariates
    rd <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5)
    rd_clean1 <- tidy_rd(rd, se_nr = 3) %>%
      mutate(outcome = o, ss = ss_lab, covars = "No covars", rent_cutoff = 0)

    # RD with covariates (excluding time since ad)
    rd_cov <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5, covs = cmat_no_time_since_ad)
    rd_clean3 <- tidy_rd(rd_cov, se_nr = 3) %>%
      mutate(outcome = o, ss = ss_lab, covars = "Covars", rent_cutoff = 0)

    # RD with covariates (excluding post-treatment)
    rd_cov_no_post <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5, covs = cmat_no_posttreat)
    rd_clean4 <- tidy_rd(rd_cov_no_post, se_nr = 3) %>%
      mutate(outcome = o, ss = ss_lab, covars = "Covars (no post-treatment)", rent_cutoff = 0)

    bind_rows(rd_clean1, rd_clean3, rd_clean4)
  }) %>% bind_rows()
}) %>%
  bind_rows() %>%
  left_join(tibble(outcome = outcome_list, label = outcome_labs))

# Add line breaks to labels for better readability
res <- res %>%
  mutate(label = add_linebreak_vector(label, min_length = 20))

# Save results
write_rds(res, "saved_results/rd_res.rds")

# Regression Kink Analysis ---------------------------------------------

# Clear environment and load fresh data
rm(list = ls())
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_kink_relative))

source("source_results/functions.R")

# Define outcomes for RK analysis
outcome_list <- c(
  "y_immig_kiez", "y_constr_kiez",
  "index_wellbeing", "index_redistribution", "index_nimbyism"
)
outcome_labs <- c(
  "Support local-level immigration", "Support local-level construction",
  "Wellbeing index", "Redistribution index", "NIMBYism index"
)

# Define covariates
cov_list <- c(
  "educ", "sex", "party", "apartment_size",
  "days_since_ad", "post_code", "interior_quality"
)
cov_list_notimesincead <- setdiff(cov_list, "days_since_ad")
cov_list_no_posttreat <- setdiff(cov_list, c("days_since_ad", "party", "apartment_size"))

# Standardize outcomes
df <- df %>%
  mutate(across(all_of(outcome_list), ~ . / sd(., na.rm = TRUE)))

# Generate subsets
ss_list <- list(df, df %>% filter(!rent_imputed == 1))
ss_labels <- c("Full", "No units where rent is imputed")

# Regression Kink Analysis
res_rk <- pblapply(outcome_list, function(o) {
  lapply(seq_along(ss_list), function(i) {
    ss_df <- ss_list[[i]]
    ss_lab <- ss_labels[i]

    y <- ss_df %>% pull(!!o)
    x <- ss_df %>% pull(rent_sqm_final_relative)

    # Prepare covariate matrices
    cmat_no_time_since_ad <- model.matrix(~ . - 1, data = ss_df[, cov_list_notimesincead])
    cmat_no_posttreat <- model.matrix(~ . - 1, data = ss_df[, cov_list_no_posttreat])

    # RK without covariates
    rk <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5, deriv = 1)
    rk_clean1 <- tidy_rd(rk, se_nr = 3) %>%
      mutate(outcome = o, ss = ss_lab, covars = "No covars", rent_cutoff = 0)

    # RK with covariates (excluding time since ad)
    rk_cov <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5, covs = cmat_no_time_since_ad, deriv = 1)
    rk_clean3 <- tidy_rd(rk_cov, se_nr = 3) %>%
      mutate(outcome = o, ss = ss_lab, covars = "Covars", rent_cutoff = 0)

    # RK with covariates (excluding post-treatment)
    rk_cov_no_post <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5, covs = cmat_no_posttreat, deriv = 1)
    rk_clean4 <- tidy_rd(rk_cov_no_post, se_nr = 3) %>%
      mutate(outcome = o, ss = ss_lab, covars = "Covars (no post-treatment)", rent_cutoff = 0)

    bind_rows(rk_clean1, rk_clean3, rk_clean4)
  }) %>% bind_rows()
}) %>%
  bind_rows() %>%
  left_join(tibble(outcome = outcome_list, label = outcome_labs))

# Add line breaks to labels for better readability
res_rk <- res_rk %>%
  mutate(label = add_linebreak_vector(label, min_length = 20))

# Save RK results
write_rds(res_rk, "saved_results/rk_res.rds")
