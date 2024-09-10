# Load required packages
pacman::p_load(
    tidyverse, readr, readxl,
    fastDummies, rdrobust, pbapply, fixest
)


# Calculate balance for RDD

rm(list = ls())
source("source_results/functions.R")

## Get data

df <- read_rds("data/data_main.rds") %>%
    filter(!is.na(treated_rd_relative))

##

cov_cat <- c("educ", "sex", "party", "interior_quality")
cov_cont <- c("apartment_size")

df <- df %>%
    fastDummies::dummy_cols(
        select_columns = cov_cat,
        remove_first_dummy = F,
        remove_selected_columns = T
    )

## Standardize

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

res <- pblapply(outcome_list, function(o) {
    lapply(1:length(ss_list), function(i) {
        cat(o)

        ## 1. Use all obs or only no rent imputations

        ss_df <- ss_list[[i]]
        ss_lab <- ss_labels[i]

        ## 2. Get y and x

        x <- ss_df %>% pull(years_rel_2013)
        y <- ss_df %>% pull(!!o)

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

        rd_clean1
    }) %>% reduce(rbind)
}) %>%
    reduce(rbind) %>%
    mutate(label = outcome)





# Load and filter data
df <- read_rds("data/data_main.rds") %>%
    filter(!is.na(treated_rd_relative))

# Define covariate lists
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size", "days_since_ad")
cov_list <- c(cov_cat, cov_cont)
cov_list_notimesincead <- setdiff(cov_list, "days_since_ad")
cov_list_no_posttreat <- setdiff(cov_list, c("days_since_ad", "party", "apartment_size"))

# Convert outcomes to standard deviations
olist <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")
df <- df %>%
    mutate(across(all_of(olist), ~ . / sd(., na.rm = TRUE)))

# Create covariate matrices
cmat_no_time_since_ad <- df %>%
    select(all_of(cov_list_notimesincead)) %>%
    dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
    as.matrix()

cmat_no_posttreat <- df %>%
    select(all_of(cov_list_no_posttreat)) %>%
    dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
    as.matrix()

# Estimate RDD
rd <- rdrobust(
    y = df$index_nimbyism,
    x = df$years_rel_2013,
    covs = cmat_no_time_since_ad,
    c = 0, h = 5, b = 5
)

# Display main RDD result
rd %>% tidy_rd(se_nr = 3)

# Prepare data for low/high rent analysis
df <- df %>%
    mutate(rent_2019_plz_above_median2 = make_binary(med_rent_plz_2019)) %>%
    filter(!is.na(rent_hte_tenants))

# Split data into low and high rent groups
lo <- df$rent_2019_plz_above_median == 0
hi <- df$rent_2019_plz_above_median == 1
df_lo <- df %>% filter(lo)
df_hi <- df %>% filter(hi)

# Check correlation between rent and gentrification
print(table(df$rent_2019_plz_above_median, df$gentri2))
print(cor(df$rent_2019_plz_above_median, df$gentri2, use = "complete.obs"))

# Estimate RDD for low and high rent areas
rd_lo <- rdrobust(y = df_lo$index_nimbyism, x = df_lo$years_rel_2013, covs = cmat_no_time_since_ad[lo, ], c = 0, h = 5, b = 5)
rd_hi <- rdrobust(y = df_hi$index_nimbyism, x = df_hi$years_rel_2013, covs = cmat_no_time_since_ad[hi, ], c = 0, h = 5, b = 5)

# Process RDD results
m1 <- rd_lo %>%
    tidy_rd(3) %>%
    mutate(covars = "Covariates incl.", samp = "Low-rent areas")
m2 <- rd_hi %>%
    tidy_rd(3) %>%
    mutate(covars = "Covariates incl.", samp = "High-rent areas")

# Estimate RDD without post-treatment covariates
rd_lo_2 <- rdrobust(
    y = df_lo$index_nimbyism, x = df_lo$years_rel_2013, covs = cmat_no_posttreat[lo, ], c = 0,
    h = 5, b = 5
)
rd_hi_2 <- rdrobust(
    y = df_hi$index_nimbyism, x = df_hi$years_rel_2013, covs = cmat_no_posttreat[hi, ], c = 0,
    h = 5, b = 5
)

# Process RDD results without post-treatment covariates
m3 <- rd_lo_2 %>%
    tidy_rd(3) %>%
    mutate(covars = "Covariates incl.\n(no post-treatment covariates", samp = "Low-rent areas")
m4 <- rd_hi_2 %>%
    tidy_rd(3) %>%
    mutate(covars = "Covariates incl.\n(no post-treatment covariates", samp = "High-rent areas")

# Combine all results
res_full <- bind_rows(m1, m2, m3, m4) %>%
    mutate(
        label = "Outcome: NIMBYism index",
        conf.low90 = estimate - qnorm(0.95) * std.error,
        conf.high90 = estimate + qnorm(0.95) * std.error,
        covars = fct_rev(as.factor(covars))
    )

pd <- position_dodge(0.4)

# Figure A17 ---------------------------------

p1 <- res_full %>%
    ggplot(aes(samp, estimate, covars)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_errorbar(
        aes(
            ymin = conf.low90, ymax = conf.high90,
            color = covars
        ),
        width = 0, size = 0.9,
        position = pd
    ) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = covars),
        width = 0, size = 0.6,
        position = pd
    ) +
    geom_point(aes(color = covars, shape = covars, fill = covars),
        position = pd, size = 2
    ) +
    theme_bw() +
    scale_fill_grey(name = "", end = 0.6, start = 0) +
    scale_color_grey(name = "", end = 0.6, start = 0) +
    scale_shape_manual(values = c(21:24), name = "") +
    coord_flip() +
    xlab("") +
    ylab("RD estimate\n(standard deviations)") +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2)) +
    facet_wrap(~label) +
    scale_y_continuous(breaks = -1:1)
p1

rm(list = ls())
# Load required packages
pacman::p_load(tidyverse, readr, readxl, fixest, ggplot2, fixest)
source("source_results/functions.R")

# Load data
df <- read_rds("data/data_owners.rds")

# Define covariate lists
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size", "days_since_ad")
cov_list <- c(cov_cat, cov_cont)
cov_list_notimesincead <- setdiff(cov_list, "days_since_ad")
cov_list_no_posttreat <- setdiff(cov_list, c("days_since_ad", "party", "apartment_size"))

# Prepare outcome variables
olist <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")
mean_y_immig_kiez <- mean(df$y_immig_kiez, na.rm = TRUE)
mean_y_constr_kiez <- mean(df$y_constr_kiez, na.rm = TRUE)

# Standardize outcomes
df <- df %>%
    mutate(across(all_of(olist), ~ . / sd(., na.rm = TRUE)))

# Create covariate matrices
cmat_no_time_since_ad <- df %>%
    select(all_of(cov_list_notimesincead)) %>%
    fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
    as.matrix()

cmat_no_posttreat <- df %>%
    select(all_of(cov_list_no_posttreat)) %>%
    fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
    as.matrix()

# Create binary rent variable
df <- df %>%
    mutate(rent_2019_plz_above_median2 = make_binary(med_rent_plz_2019))

# Check rent variable
print(mean(df$rent_2019_plz_above_median2, na.rm = TRUE))
print(sum(is.na(df$rent_2019_plz_above_median2)))

# Regression models
olist <- c("y_immig_kiez", "y_constr_kiez")

m1 <- feols(.[olist] ~ med_rent_plz_2019, df, cluster = ~post_code)
m2 <- feols(.[olist] ~ med_rent_plz_2019 + educ + sex + party + interior_quality + post_code + apartment_size,
    df,
    cluster = ~post_code
)

# Process regression results
m1_df <- lapply(m1, tidy_feols) %>% bind_rows()
m2_df <- lapply(m2, tidy_feols) %>% bind_rows()

# Combine results
m_full <- bind_rows(m1_df, m2_df) %>%
    filter(term == "med_rent_plz_2019") %>%
    mutate(
        covars = rep(c("No covariates", "Covariates incl."), each = 2),
        outcome = rep(c("Support local-level immigration", "Support local-level construction"), 2)
    )

# Add means to results
m_full <- m_full %>%
    left_join(tibble(
        outcome = c("Support local-level immigration", "Support local-level construction"),
        mean_dv = c(mean_y_immig_kiez, mean_y_constr_kiez)
    )) %>%
    mutate(outcome = paste0(outcome, "\n(N=", n, ")\n(Mean: ", round(mean_dv, 1), ")"))

# Create Figure A18
p1 <- ggplot(m_full, aes(outcome, estimate, fill = covars)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high, color = covars),
        width = 0,
        position = position_dodge(0.4)
    ) +
    geom_point(
        aes(color = covars, shape = covars, fill = covars),
        position = position_dodge(0.4),
        size = 2
    ) +
    theme_bw() +
    scale_fill_grey(name = "", end = 0.6, start = 0) +
    scale_color_grey(name = "", end = 0.6, start = 0) +
    scale_shape_manual(values = c(21:24), name = "") +
    coord_flip() +
    labs(x = "", y = "Difference between high-rent and\nlow-rent areas (standard deviations)") +
    theme(legend.position = "bottom")

# Display plot
print(p1)
