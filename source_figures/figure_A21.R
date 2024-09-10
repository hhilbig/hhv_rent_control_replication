# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, purrr, pbapply, rdrobust)

# Clear environment
rm(list = ls())

# Load and preprocess data
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_rd_relative))

source("source_results/functions.R")

# Define outcomes and indices
outcome_list <- c("y_immig_kiez", "y_constr_kiez")
outcome_labs <- c("Support local-level immigration", "Support local-level construction")

index_list <- c("index_wellbeing", "index_redistribution", "index_nimbyism")
index_labs <- c("Wellbeing index", "Redistribution index", "NIMBYism index")

outcome_list <- c(outcome_list, index_list)
outcome_labs <- c(outcome_labs, index_labs)

# Define covariates
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size", "months_since_ad")
cov_list <- c(cov_cat, cov_cont)
cov_list_notimesincead <- setdiff(cov_list, "days_since_ad")
cov_list_no_posttreat <- setdiff(cov_list, c("days_since_ad", "party", "apartment_size"))

# Identify continuous outcome variables
outcome_list_cont <- outcome_list[sapply(outcome_list, function(x) length(unique(na.omit(df[[x]]))) > 2)]

# Standardize continuous outcomes
df <- df %>%
  mutate(across(all_of(outcome_list_cont), ~ . / sd(., na.rm = TRUE)))

# Get list of districts (Bezirke)
b_list <- unique(df$bezname)

# Generate datasets excluding one district at a time
ss_list <- lapply(b_list, function(x) df %>% filter(bezname != x))
ss_labels <- b_list

## Get results

res <- pblapply(outcome_list, function(o) {
  lapply(1:length(ss_list), function(i) {
    ## 1. Use all obs or only no rent imputations

    ss_df <- ss_list[[i]]
    ss_lab <- ss_labels[i]

    ## 2. Get y and x

    y <- ss_df %>% pull(!!o)
    x <- ss_df %>% pull(years_rel_2013)

    cat(ss_lab, o)

    ## Do not condition on party for the party HTE stuff
    ## Do not condition on party if party is the outcome

    if (str_detect(ss_lab, "party") | o == "hte_party_left") {
      cov_list_notimesincead <- setdiff(cov_list_notimesincead, "party")
      cov_list_no_posttreat <- setdiff(cov_list_no_posttreat, "party")
    }

    ## 3. Get matrix of covars

    ## Matrix of cvars omitting time since ad

    cmat_no_time_since_ad <- ss_df %>%
      dplyr::select(one_of(cov_list_notimesincead)) %>%
      fastDummies::dummy_cols(
        remove_first_dummy = T,
        remove_selected_columns = T
      ) %>%
      as.matrix()

    ## Matrix of cvars omitting post treatment cvars

    cmat_no_posttreat <- ss_df %>%
      dplyr::select(one_of(cov_list_no_posttreat)) %>%
      fastDummies::dummy_cols(
        remove_first_dummy = T,
        remove_selected_columns = T
      ) %>%
      as.matrix()

    ## Estimate
    ## Note that year cutoffs are independent of pre poliy rents

    rd <- rdrobust(y = y, x = x, c = 0, h = 5, b = 5)

    rd_clean1 <- rd %>%
      tidy_rd(se_nr = 3) %>%
      mutate(
        outcome = o, ss = ss_lab,
        covars = "No covars",
        rent_cutoff = 0
      )


    ## Estimate w/ covars, but not time since ad

    rd <- rdrobust(
      y = y, x = x, c = 0, h = 5, b = 5,
      covs = cmat_no_time_since_ad
    )

    rd_clean3 <- rd %>%
      tidy_rd(se_nr = 3) %>%
      mutate(
        outcome = o, ss = ss_lab,
        covars = "Covars",
        rent_cutoff = 0
      )

    ## Estimate w/ covars, but no post treatment covars

    rd <- rdrobust(
      y = y, x = x, c = 0, h = 5, b = 5,
      covs = cmat_no_posttreat
    )

    rd_clean4 <- rd %>%
      tidy_rd(se_nr = 3) %>%
      mutate(
        outcome = o, ss = ss_lab,
        covars = "Covars (no post-treatment)",
        rent_cutoff = 0
      )

    bind_rows(
      rd_clean1,
      rd_clean3, rd_clean4
    )
  }) %>% reduce(rbind)
}) %>%
  reduce(rbind) %>%
  left_join(data.frame(
    outcome = outcome_list,
    label = outcome_labs
  ))

## Format

res <- res %>%
  mutate(label = add_linebreak_vector(label, min_length = 20))

## Get CIs

res <- res %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    covars = case_when(
      covars == "Covars" ~ "Covariates incl.",
      covars == "No covars" ~ "No covariates",
      str_detect(covars, "post") ~ "Covariates incl.\n(no post-treatment covariates)"
    ),
    covars = factor(covars, levels = unique(covars)[c(3, 2, 1)])
  ) %>%
  filter(covars != "No covariates")

## Get NIMBYism outcomes

o_nimby <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")

# Filter results for NIMBYism outcomes
res_full <- res %>%
  filter(outcome %in% o_nimby)

# Create Figure A21
p1 <- res_full %>%
  ggplot(aes(ss, estimate, covars)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = covars),
    width = 0, linewidth = 0.9, position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = covars),
    width = 0, linewidth = 0.6, position = position_dodge(0.4)
  ) +
  geom_point(
    aes(color = covars, shape = covars, fill = covars),
    position = position_dodge(0.4), size = 2
  ) +
  facet_wrap(~label) +
  scale_fill_grey(name = "", end = 0.6, start = 0) +
  scale_color_grey(name = "", end = 0.6, start = 0) +
  scale_shape_manual(values = c(21:24), name = "") +
  labs(
    x = "Omitted district",
    y = "RD estimate\n(standard deviations)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2)) +
  coord_flip()

# Display the plot
print(p1)
