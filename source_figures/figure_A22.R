# Load required packages
pacman::p_load(tidyverse, readxl, rdd, rdrobust, pbapply)

# Clear environment
rm(list = ls())

source("source_results/functions.R")

# Load data
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_rd_relative))


# Load measures
measures <- read_xlsx("data/measures.xlsx")

# Define outcomes
outcome_list <- c("y_immig_kiez", "y_constr_kiez")
outcome_labs <- c("Support local-level immigration", "Support local-level construction")

# Add indices to outcomes
index_list <- c("index_wellbeing", "index_redistribution", "index_nimbyism")
index_labs <- c("Wellbeing index", "Redistribution index", "NIMBYism index")

outcome_list <- c(outcome_list, index_list)
outcome_labs <- c(outcome_labs, index_labs)

# Define covariates
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size", "days_since_ad")
cov_list <- c(cov_cat, cov_cont)
cov_list_notimesincead <- setdiff(cov_list, "days_since_ad")
cov_list_no_posttreat <- setdiff(cov_list, c("days_since_ad", "party", "apartment_size"))

# Convert outcomes to standard deviations
df <- df %>%
  mutate(across(all_of(outcome_list), ~ . / sd(., na.rm = TRUE)))

# Generate dataset list
ss_list <- list(df)
ss_labels <- c("1 Full")

# Transform bandwidth
df <- df %>%
  mutate(years_rel_2013 = ifelse(years_rel_2013 > -1, years_rel_2013 + 1, years_rel_2013))

# Define bandwidth list
bw_list <- 2:4

# Run RDD analysis
res <- pblapply(outcome_list, function(o) {
  lapply(seq_along(ss_list), function(i) {
    lapply(bw_list, function(bw) {
      cat(o, i, bw, "\n")

      ss_df <- ss_list[[i]]
      ss_lab <- ss_labels[i]

      y <- ss_df %>% pull(!!o)
      x <- ss_df %>% pull(years_rel_2013)

      # Prepare covariate matrices
      cmat_no_time_since_ad <- ss_df %>%
        select(all_of(cov_list_notimesincead)) %>%
        fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
        as.matrix()

      cmat_no_posttreat <- ss_df %>%
        select(all_of(cov_list_no_posttreat)) %>%
        fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
        as.matrix()

      # RDD estimation
      out <- rdd::RDestimate(
        as.formula(paste0(o, "~ years_rel_2013 |", paste0(cov_list, collapse = "+"))),
        data = ss_df %>% mutate(years_rel_2013 = ifelse(years_rel_2013 > -1, years_rel_2013 + 1, years_rel_2013)),
        bw = bw + 1
      )

      rd_clean1 <- data.frame(
        estimate = out$est[1:2], std.error = out$se[1:2],
        statistic = out$z[1:2], p.value = out$p[1:2],
        conf.low = out$ci[1:2, 1], conf.high = out$ci[1:2, 2], n = out$obs[1:2],
        outcome = o, ss = ss_lab, covars = "Covars", method = "rdd",
        bw = c(bw, bw / 2)
      )

      # Robust RDD estimation
      tryCatch(
        {
          rd4 <- rdrobust(
            y = y, x = x, c = 0, h = c(bw + 1, bw + 1), b = c(bw + 1, bw + 1),
            kernel = "triangular", p = 1
          )
          rd5 <- rdrobust(
            y = y, x = x, c = 0, h = c(bw + 1, bw + 1), b = c(bw + 1, bw + 1),
            kernel = "triangular", p = 1, covs = cmat_no_time_since_ad
          )
          rd6 <- rdrobust(
            y = y, x = x, c = 0, h = c(bw + 1, bw + 1), b = c(bw + 1, bw + 1),
            kernel = "triangular", p = 1, covs = cmat_no_posttreat
          )

          rd_clean4 <- rd4 %>%
            tidy_rd(se_nr = 3) %>%
            mutate(
              outcome = o, ss = ss_lab, covars = "No covars",
              rent_cutoff = 0, method = "rdrobust", bw = bw
            )

          rd_clean5 <- rd5 %>%
            tidy_rd(se_nr = 3) %>%
            mutate(
              outcome = o, ss = ss_lab, covars = "Covars",
              rent_cutoff = 0, method = "rdrobust", bw = bw
            )

          rd_clean6 <- rd6 %>%
            tidy_rd(se_nr = 3) %>%
            mutate(
              outcome = o, ss = ss_lab, covars = "Covars (no post-treatment)",
              rent_cutoff = 0, method = "rdrobust", bw = bw
            )

          rd_clean1 <- bind_rows(rd_clean4, rd_clean5, rd_clean6)
        },
        error = function(e) {
          cat("ERROR:", conditionMessage(e), "\n")
        }
      )

      rd_clean1
    }) %>% reduce(bind_rows)
  }) %>% reduce(bind_rows)
}) %>%
  reduce(bind_rows) %>%
  left_join(data.frame(outcome = outcome_list, label = outcome_labs))

# Process results
res <- res %>%
  mutate(label = add_linebreak_vector(label, min_length = 20))

o_nimby <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")

res <- res %>%
  filter(str_detect(ss, "Full"), outcome %in% o_nimby) %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    tenant = "Treated tenants\nvs. non-treated"
  )

res_main <- read_rds("saved_results/rd_res.rds") %>%
  filter(str_detect(ss, "Full"), outcome %in% o_nimby) %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    tenant = "Treated tenants\nvs. non-treated"
  )

# Prepare data for plotting
res_full <- res %>%
  mutate(
    covars = case_when(
      covars == "Covars" ~ "Covariates incl.",
      covars == "No covars" ~ "No covariates",
      str_detect(covars, "post") ~ "Covariates incl.\n(no post-treatment covariates)"
    ),
    covars = factor(covars, levels = unique(covars)[c(3, 2, 1)])
  ) %>%
  filter(covars == "Covariates incl.")

# Create Figure A.22
p1 <- res_full %>%
  ggplot(aes(factor(bw), estimate, method)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = method),
    width = 0, size = 0.9, position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = method),
    width = 0, size = 0.6, position = position_dodge(0.4)
  ) +
  geom_point(aes(color = method), shape = 19, position = position_dodge(0.4), size = 2) +
  facet_wrap(~label) +
  scale_color_manual(
    values = c("grey50", "black"),
    labels = c("Estimated using rdd", "Estimated using rdrobust"),
    name = ""
  ) +
  labs(
    x = "Bandwidth in years",
    y = "RD estimate\n(standard deviations)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2))

# Display the plot
print(p1)
