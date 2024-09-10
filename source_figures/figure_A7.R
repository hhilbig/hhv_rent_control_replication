# Clear environment and load packages
rm(list = ls())
pacman::p_load(
  readr, tidyverse, stringr, purrr,
  ggplot2, readxl, rdrobust, fastDummies, pbapply
)

# Get data for RD
df_rd <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_rd_relative))

# Define covariates
cov_cat <- c("educ", "sex", "party", "interior_quality")
cov_cont <- c("apartment_size")

# Process RD data
df_rd <- df_rd %>%
  fastDummies::dummy_cols(
    select_columns = cov_cat,
    remove_first_dummy = F,
    remove_selected_columns = T
  ) %>%
  mutate_at(vars(one_of(cov_cont)), list(~ (. / 100)))

# Get list of covariates
outcome_list <- colnames(df_rd) %>%
  str_filter(paste0(cov_cat, collapse = "|")) %>%
  str_filter("other1|other2|other3", neg = T) %>%
  c(., cov_cont)

# Generate data sets
ss_list_rd <- list(df_rd)
ss_labels <- c("Full")

# RD analysis
res_rd <- pblapply(outcome_list, function(o) {
  lapply(1:length(ss_list_rd), function(i) {
    ss_df <- ss_list_rd[[i]]
    ss_lab <- ss_labels[i]

    x <- ss_df %>% pull(years_rel_2013)
    y <- ss_df %>% pull(!!o)

    rd <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5)

    rd %>%
      tidy_rd(se_nr = 3) %>%
      mutate(
        outcome = o, ss = ss_lab,
        covars = "No covars",
        rent_cutoff = 0
      )
  }) %>% reduce(rbind)
}) %>%
  reduce(rbind) %>%
  mutate(label = outcome, model = "RDD")

# Kink balance ------------------------------------------------------------

df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_kink_relative))

##

cov_cat <- c("educ", "sex", "party", "interior_quality")
cov_cont <- c("apartment_size")

df <- df %>%
  fastDummies::dummy_cols(
    select_columns = cov_cat,
    remove_first_dummy = F,
    remove_selected_columns = T
  )

df <- df %>%
  mutate_at(vars(one_of(cov_cont)), list(~ (. / 100)))

## Get list of covars

outcome_list <- colnames(df) %>%
  str_filter(paste0(cov_cat, collapse = "|")) %>%
  str_filter("other1|other2|other3", neg = T) %>%
  c(., cov_cont)

## Gen data sets

ss_list <- list(df)
ss_labels <- c("Full")

## Get some results

df$years_rel_2013 %>% unique()

o <- outcome_list[1]
i <- 1

res_kink <- pblapply(outcome_list, function(o) {
  lapply(1:length(ss_list), function(i) {
    ss_df <- ss_list[[i]]
    ss_lab <- ss_labels[i]

    y <- ss_df %>% pull(!!o)
    x <- ss_df %>% pull(rent_sqm_final_relative)

    ## Estimate

    rd <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5, deriv = 1)

    rd_clean1 <- rd %>%
      tidy_rd(se_nr = 3) %>%
      mutate(
        outcome = o, ss = ss_lab,
        covars = "No covars",
        rent_cutoff = 0
      )

    rd_clean1
  }) %>% reduce(rbind)
}) %>%
  reduce(rbind) %>%
  mutate(label = outcome) %>%
  mutate(model = "RDK")

# Combine RD and Kink results
all_results <- bind_rows(res_rd, res_kink)

# Process combined results
df <- all_results %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    model = ifelse(model == "RDD", "Regression discontinuity sample", "Regression kink sample")
  ) %>%
  filter(!str_detect(outcome, "hte"), !abs(estimate) > 2, !is.na(p.value))

# Load covariate labels and join with data
cb <- read_excel("data/covar_labels.xlsx")
df <- df %>%
  left_join(cb, by = c("outcome" = "var")) %>%
  mutate(
    g_order = paste0(group_order, group, within_order),
    lab = fct_reorder(lab, g_order, .desc = TRUE)
  )

# Create Figure A7
p1 <- ggplot(df, aes(lab, estimate, group = p.value < 0.05)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(shape = 21, position = position_dodge(0.4), fill = "white", size = 2) +
  theme_bw() +
  coord_flip() +
  facet_wrap(~model, scales = "free_x") +
  labs(x = "", y = "Estimate") +
  theme(panel.spacing = unit(1.5, "lines"))

print(p1)
