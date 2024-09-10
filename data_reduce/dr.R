# Get
# Clear environment
rm(list = ls())

# Load required libraries
pacman::p_load(readr, tidyverse, ggplot2, readxl)

# Read data and filter
df <- read_rds("data/data_main_orig.rds") %>%
    dplyr::select(
        -id, -language, -educ_other, -matches("covid"),
        -apartment_residents, -apartment_rent,
        -party_other, -email, -na_fields, -party_other2, -party_other3,
        -coder_comment, -street, -street_number, -add_full, -merge, -wohnlage,
        -laerm, -amount, -rent_cutoff, -gentri1, -y_state_intervention_bin, -y_state_redistr_bin, -microm_avg_income, -y_state_fin_support, -y_state_intervention, -y_leisure, -y_poor_people,
        -y_social_constr_kiez, -y_social_constr_berlin
    )

df %>% glimpse()

write_rds(df, "data/data_main.rds")

# Same w/ owners

df <- read_rds("data/data_owners_orig.rds") %>%
    filter(!is.na(treated_rd_relative)) %>%
    dplyr::select(
        -id, -language, -code, -educ_other, -matches("covid"),
        -apartment_residents, -apartment_rent,
        -party_other, -email, -na_fields, -party_other2, -party_other3,
        -coder_comment, -street, -street_number, -add_full, -merge, -wohnlage,
        -laerm, -amount, -rent_cutoff, -gentri1, -y_state_intervention_bin, -y_state_redistr_bin, -microm_avg_income, -y_state_fin_support, -y_state_intervention, -y_leisure, -y_poor_people,
        -y_social_constr_kiez, -y_social_constr_berlin
    )

glimpse(df)

write_rds(df, "data/data_owners.rds")

# LL DF

library(readxl)

df <- read_csv("../01_Data/16_PLRZuordnung_447und542/AdressenMitLatLon_PLR447und542.csv") %>%
    select(matches("plr|code|lon|lat")) %>%
    select(-matches("name|_num")) %>%
    mutate(code = as.numeric(code_string)) %>%
    select(-code_string) %>%
    distinct(code, .keep_all = TRUE)

write_rds(df, "data/ll_df.rds")
