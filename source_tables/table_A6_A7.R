# Clear workspace
rm(list = ls())

pacman::p_load(tidyverse, readxl)

# Load and process main results
res_v1 <- read_rds("saved_results/rd_res.rds") %>%
  filter(str_detect(ss, "Full")) %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    rent_exclusion = "Excluding low-rent\napartments",
    tenant = "Treated tenants\nvs. non-treated"
  )

# Recode covariate levels
res_v1 <- res_v1 %>%
  mutate(covars = case_when(
    covars == "Covars" ~ "2 All covariates",
    covars == "No covars" ~ "1 No covariates",
    str_detect(covars, "time") ~ "3 Time since ad omitted",
    str_detect(covars, "post") ~ "4 No post-treatment covariates"
  )) %>%
  mutate(covars = factor(covars, levels = unique(covars)[c(4, 3, 2, 1)]))

# Set up for plotting
pd <- position_dodge(0.4)
o_nimby <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")

# Create table dataframe
res_tab <- res_v1 %>%
  filter(outcome %in% o_nimby) %>%
  dplyr::select(tenant, label, covars, estimate, std.error, statistic, p.value, n) %>%
  mutate(
    label = str_replace_all(label, "\n", " "),
    covars = str_replace_all(covars, "\n", " "),
    label = as.factor(label)
  )

# Relabel outcomes
levels(res_tab$label) <- c(
  "3NIMBYism index", "1Support local-level construction",
  "2Support local-level immigration"
)

# Final formatting of table dataframe
res_tab <- res_tab %>%
  mutate(label = as.character(label)) %>%
  arrange(tenant, label, covars) %>%
  mutate(
    covars = substr(covars, 3, 100),
    label = substr(label, 2, 100)
  ) %>%
  dplyr::select(-tenant)

# Load libraries for table creation
library(kableExtra)
library(knitr)

## Table A6

kable(res_tab,
  "latex",
  longtable = F,
  booktabs = T, col.names = c(
    "Outcome",
    "Estimate",
    "Specification",
    "SE",
    "T-stat",
    "P",
    "N"
  ),
  linesep = "",
  caption = "Main results",
  label = "tab:main_appendix",
  escape = F, digits = 3
) %>%
  kable_styling(
    latex_options = c("repeat_header"),
    font_size = 8
  ) %>%
  # collapse_rows() %>%
  row_spec(0, bold = T) %>%
  column_spec(1, width = "5cm") %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "1cm") %>%
  column_spec(4, width = "1cm") %>%
  column_spec(5, width = "1cm") %>%
  column_spec(6, width = "1cm") %>%
  column_spec(7, width = "1cm") %>%
  kable_styling(latex_options = "HOLD_position")

#### RK results ####

## Get results
## Start just with the main covars and no covars

res_v1 <- read_rds("saved_results/rk_res.rds") %>%
  filter(str_detect(ss, "Full")) %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error
  ) %>%
  mutate(rent_exclusion = "Excluding low-rent\napartments") %>%
  mutate(tenant = "Treated tenants\nvs. non-treated")

## Combine

res_v1 <- res_v1 %>%
  mutate(covars = case_when(
    covars == "Covars" ~ "2 All covariates",
    covars == "No covars" ~ "1 No covariates",
    str_detect(covars, "time") ~ "3 Time since ad omitted",
    str_detect(covars, "post") ~ "4 No post-treatment covariates"
  )) %>%
  mutate(covars = factor(covars, levels = unique(covars)[c(4, 3, 2, 1)]))

## Create table DF

res_tab <- res_v1 %>%
  filter(outcome %in% o_nimby) %>%
  dplyr::select(tenant, label, covars, estimate, std.error, statistic, p.value, n) %>%
  mutate(
    label = str_replace_all(label, "\n", " "),
    covars = str_replace_all(covars, "\n", " ")
  ) %>%
  mutate(label = as.factor(label))

levels(res_tab$label) <- c(
  "3NIMBYism index", "1Support local-level construction",
  "2Support local-level immigration"
)
res_tab <- res_tab %>%
  mutate(label = as.character(label)) %>%
  arrange(tenant, label, covars) %>%
  mutate(
    covars = substr(covars, 3, 100),
    label = substr(label, 2, 100)
  ) %>%
  dplyr::select(-tenant)

## Make table

library(kableExtra)
library(knitr)

## Table A7

kable(res_tab,
  "latex",
  longtable = F,
  booktabs = T, col.names = c(
    "Outcome",
    "Estimate",
    "Specification",
    "SE",
    "T-stat",
    "P",
    "N"
  ),
  linesep = "",
  caption = "Main results",
  label = "tab:main_appendix",
  escape = F, digits = 3
) %>%
  kable_styling(
    latex_options = c("repeat_header"),
    font_size = 8
  ) %>%
  # collapse_rows() %>%
  row_spec(0, bold = T) %>%
  column_spec(1, width = "5cm") %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "1cm") %>%
  column_spec(4, width = "1cm") %>%
  column_spec(5, width = "1cm") %>%
  column_spec(6, width = "1cm") %>%
  column_spec(7, width = "1cm") %>%
  kable_styling(latex_options = "HOLD_position")
