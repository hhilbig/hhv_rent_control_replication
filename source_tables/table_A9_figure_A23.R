# Clean environment and load packages
rm(list = ls())

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(conflicted, tidyverse, fixest, lubridate, readr, ggplot2, rdrobust)

source("source_results/functions.R")

# Set conflict preferences
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::lag)
conflicts_prefer(lubridate::week)
conflicts_prefer(lubridate::wday)

# Load and filter data
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_rd_relative))

# Define covariates
cov_cat <- c("educ", "sex", "party", "interior_quality", "post_code")
cov_cont <- c("apartment_size", "days_since_ad")
cov_list <- c(cov_cat, cov_cont)
cov_list_no_posttreat <- setdiff(cov_list, c("days_since_ad", "party", "apartment_size"))
cov_list_notimesincead <- setdiff(cov_list, "days_since_ad")

# Convert outcomes to standard deviations
olist <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")
df <- df %>%
  mutate(across(all_of(olist), ~ . / sd(., na.rm = TRUE)))

# Create covariate matrices
create_cmat <- function(df, vars) {
  df %>%
    select(all_of(vars)) %>%
    fastDummies::dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
    as.matrix()
}

cmat_no_time_since_ad <- create_cmat(df, cov_list_notimesincead)
cmat_no_posttreat <- create_cmat(df, cov_list_no_posttreat)
cmat <- create_cmat(df, cov_list)


# Main RDD analysis
rd <- rdrobust(
  y = df$index_nimbyism,
  x = df$years_rel_2013,
  covs = cmat,
  c = 0, h = 5, b = 5
)

# Display RDD main result
rd %>% tidy_rd(se_nr = 3)

# Time trend analysis
df_time <- df %>%
  select(date, index_nimbyism, treated_rd_relative) %>%
  mutate(
    date = as.Date(date),
    week = week(date),
    day = yday(date)
  ) %>%
  filter(!is.na(week))

# Load and process Nexis data
process_nexis_data <- function(path) {
  read_rds(path) %>%
    arrange(day) %>%
    select(day, n) %>%
    mutate(n_lagged = lag(n)) %>%
    rename(n_day = n, n_day_lagged = n_lagged)
}

df_nexis_day <- process_nexis_data("data/nexis_date_df.rds")

# Create full day dataframe
df_nexis_day <- data.frame(day = 1:180) %>%
  left_join(df_nexis_day, by = "day") %>%
  mutate(across(c(n_day, n_day_lagged), ~ ifelse(is.na(.), 0, .)))

# Process weekly Nexis data
df_nexis_week <- read_rds("data/nexis_date_df.rds") %>%
  mutate(date_week = floor_date(Date, "week")) %>%
  group_by(date_week) %>%
  summarise(n_week = sum(n)) %>%
  ungroup() %>%
  arrange(date_week) %>%
  mutate(
    n_week_lagged = lag(n_week),
    day_week = wday(date_week, label = TRUE),
    week = week(date_week)
  )

# Create week dataframe
week_df <- data.frame(week = 1:26) %>%
  mutate(date_week = as.Date("2020-12-27") + 7 * (week - 1)) %>%
  select(-week) %>%
  left_join(df_nexis_week, by = "date_week")

# Process time data
df_time <- df_time %>%
  mutate(
    date_week = floor_date(date, "week"),
    week = week(date_week)
  )

# Get survey responses per week
df_time_res_per_week <- df_time %>%
  group_by(date_week) %>%
  summarise(n_resp_week = n()) %>%
  ungroup()

# Merge weekly data
df_nexis_week <- df_nexis_week %>%
  left_join(df_time_res_per_week, by = "date_week")

# Create Figure A23
df_nexis_week %>%
  filter(week %in% 12:17) %>%
  select(date_week, n_week, n_resp_week) %>%
  pivot_longer(-date_week) %>%
  ggplot(aes(x = date_week, y = value, linetype = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Week", y = "Number") +
  scale_linetype_discrete(
    name = "",
    labels = rev(c(
      "Articles about rent control in Berlin",
      "Survey responses (online)"
    ))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Merge main data with daily and weekly Nexis data
df_time_use <- df_time %>%
  left_join(df_nexis_day, relationship = "many-to-one") %>%
  left_join(week_df, relationship = "many-to-one")

# Estimate regressions of index_nimbyism
# Models 1-2: Weekly regressions
m1 <- feols(index_nimbyism ~ week, data = df_time_use, cluster = ~week)
m2 <- feols(index_nimbyism ~ week + n_week, data = df_time_use, cluster = ~week)

# Models 3-4: Daily regressions
m3 <- feols(index_nimbyism ~ day, data = df_time_use, cluster = ~day)
m4 <- feols(index_nimbyism ~ day + n_day, data = df_time_use, cluster = ~day)

# Models 5-6: Daily regressions for treated and control groups
m5 <- feols(index_nimbyism ~ day + n_day,
  data = df_time_use %>% filter(treated_rd_relative == 1), cluster = ~day
)
m6 <- feols(index_nimbyism ~ day + n_day,
  data = df_time_use %>% filter(treated_rd_relative == 0), cluster = ~day
)

# Models 7-8: Weekly regressions for treated and control groups
m7 <- feols(index_nimbyism ~ week + n_week,
  data = df_time_use %>% filter(treated_rd_relative == 1), cluster = ~week
)
m8 <- feols(index_nimbyism ~ week + n_week,
  data = df_time_use %>% filter(treated_rd_relative == 0), cluster = ~week
)

# Set variable labels for fixest output
fixest::setFixest_dict(
  week = "Week of response ($t$)",
  day = "Day of response ($t$)",
  n_day = "Articles / day ($N_t$)",
  n_week = "Articles / week ($N_t$)"
)

# Table A9 ---------------------------------------------

mlist <- list(m1, m2, m7, m8, m3, m4, m5, m6)
etable(mlist,
  tex = T, title = "regressions",
  drop = "Constant", digits = 3, digits.stats = 3
)

# Filter and plot Nexis data
df_nexis_week <- df_nexis_week %>%
  filter(week %in% 12:17)
plot(df_nexis_week$week, df_nexis_week$n_week,
  type = "l",
  main = "Weekly Nexis Articles", xlab = "Week", ylab = "Number of Articles"
)
cat("Total articles:", sum(df_nexis_week$n_week), "\n")
