# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, kableExtra, fastDummies)

# Clear environment
rm(list = ls())

source("source_results/functions.R")

# Load and prepare data
df <- read_rds("data/data_main.rds") %>%
  filter(!is.na(treated_rd_relative)) %>%
  mutate(index_nimbyism = ifelse(!index_nimbyism %% 0.5 == 0, NA, index_nimbyism))

# Define outcomes
outcome_list <- c(
  "y_financial_diff", "y_move_5years", "y_immig_berlin", "y_immig_kiez",
  "y_constr_berlin", "y_constr_kiez"
)

outcome_labs <- c(
  "Financial situation difficult", "Propensity to move", "Support more immigration to Berlin",
  "Support local-level immigration", "Support more construction in Berlin",
  "Support local-level construction"
)

# Add indices to outcomes
index_list <- "index_nimbyism"
index_labs <- "NIMBYism index"

# Combine outcome lists
outcome_list <- c(outcome_list, index_list)
outcome_labs <- c(outcome_labs, index_labs)

# Function to summarize data
summarize_data <- function(data) {
  data %>%
    dplyr::select(apartment_type, one_of(outcome_list)) %>%
    group_by(apartment_type) %>%
    group_map(~ lapply(., function(x) summarize_vec(x)) %>%
      reduce(rbind)) %>%
    reduce(rbind) %>%
    cbind(outcome_labs, .) %>%
    slice(6, 4, 5, 3, 7, 1, 2)
}

# Get means for all tenants
out <- summarize_data(df)

# Get means for left-leaning party supporters
out_left <- summarize_data(df %>% filter(party_left_hte == 1))

# Get means for right-leaning party supporters
out_right <- summarize_data(df %>% filter(party_left_hte == 0))

# Create Table A3
table_a3 <- kable(rbind(out, out_left, out_right),
  "latex",
  longtable = FALSE,
  booktabs = TRUE,
  col.names = c("Variable", "Mean", "Median", "SD", "N", "Min", "Max"),
  linesep = "",
  caption = "Summary statistics",
  label = "tab:sumstatso",
  escape = FALSE,
  digits = 2
) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "3.5cm") %>%
  kable_styling(latex_options = "HOLD_position") %>%
  pack_rows("All tenants", 1, 7, hline_after = FALSE) %>%
  pack_rows("Left-leaning tenants", 8, 14, hline_after = FALSE) %>%
  pack_rows("Right-leaning tenants", 15, 21, hline_after = FALSE)

print(table_a3)


# Clear environment except for df
rm(list = setdiff(ls(), "df"))
source("source_results/functions.R")

# Prepare data for controls
cov_cat <- c("educ", "sex", "party", "interior_quality")
cov_cont <- c("apartment_size")

df <- df %>%
  fastDummies::dummy_cols(
    select_columns = cov_cat,
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE
  )


# Get list of covariates
outcome_list <- colnames(df) %>%
  str_filter(paste0(cov_cat, collapse = "|")) %>%
  str_filter("other1|other2|other3", neg = TRUE) %>%
  c(., cov_cont) %>%
  str_filter("hte", neg = TRUE)

# Load covariate labels
cb <- read_excel("data/covar_labels.xlsx")

# Prepare label list
label_list <- data.frame(outcome_list) %>%
  left_join(cb, by = c("outcome_list" = "var")) %>%
  mutate(g_order = paste0(group_order, group, within_order)) %>%
  mutate(lab = fct_reorder(lab, g_order, .desc = TRUE))

# Summarize data for controls
out <- df %>%
  dplyr::select(one_of(outcome_list)) %>%
  lapply(function(x) summarize_vec(x)) %>%
  reduce(rbind) %>%
  cbind(label_list, .) %>%
  arrange(g_order) %>%
  dplyr::select(-1, -3:-7)

# Create Table A4
table_a4 <- kable(out,
  "latex",
  longtable = FALSE,
  booktabs = TRUE,
  col.names = c("Variable", "Mean", "Median", "SD", "N", "Min", "Max"),
  linesep = "",
  caption = "Summary statistics",
  label = "tab:sumstatso",
  escape = FALSE,
  digits = 2
) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "3.5cm") %>%
  kable_styling(latex_options = "HOLD_position")

# Output tables (uncomment to print)
print(table_a4)
