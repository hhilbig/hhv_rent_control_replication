# Clear environment
rm(list = ls())

# Load required libraries
pacman::p_load(readr, tidyverse, ggplot2, readxl)

# Read data and filter
df <- read_rds("data/data_main.rds") %>%
       filter(!is.na(treated_rd_relative))

# Create outcome lists
outcome_list <- c(
       "y_immig_kiez",
       "y_constr_kiez",
       "index_nimbyism"
)
outcome_labs <- c(
       "Support local-level immigration", "Support local-level construction",
       "NIMBYism index"
)

# Clean data: round to nearest 0.5 for all outcomes
df <- df %>%
       mutate_at(
              vars(one_of(outcome_list)),
              list(~ ifelse(. %% 0.5 != 0, NA, .))
       )

# Clean data: round to nearest integer for non-index outcomes
df <- df %>%
       mutate_at(
              vars(one_of(c(
                     outcome_list,
                     "y_constr_berlin",
                     "y_immig_berlin"
              ) %>% str_subset("index", negate = T))),
              list(~ ifelse(. %% 1 != 0, NA, .))
       )

# Function to create distribution plot
p_dist <- function(o, o_lab, b = seq(1, 4, 1), w = 0.8,
                   l = c(
                          "1 Strongly\ndisagree",
                          "2 Disagree",
                          "3 Agree",
                          "4 Strongly\nagree"
                   )) {
       # Calculate mean
       m <- mean(df[[o]], na.rm = TRUE)

       # Create plot
       p1 <- df %>%
              group_by_at(vars(one_of(o))) %>%
              summarise(n = n()) %>%
              ungroup() %>%
              mutate(n = n / sum(n)) %>%
              ggplot(aes_string(x = o, y = "n")) +
              geom_bar(stat = "identity", fill = "grey95", color = "black", width = w) +
              geom_vline(xintercept = m, linetype = "solid", linewidth = 1.5) +
              xlab(o_lab) +
              ylab("Share of respondents") +
              theme_bw() +
              scale_x_continuous(breaks = b, labels = l)
       return(p1)
}

# Figure A5: Distribution plots for first three outcomes
# Also figure 2
p_dist(o = outcome_list[1], o_lab = outcome_labs[1])
p_dist(o = outcome_list[2], o_lab = outcome_labs[2])
p_dist(
       o = outcome_list[3], o_lab = outcome_labs[3], b = seq(1, 4, 0.5),
       l = seq(1, 4, 0.5),
       w = 0.4
)

# Calculate differences between city-wide and local-level preferences
df <- df %>%
       mutate(
              diff_constr = y_constr_berlin - y_constr_kiez,
              diff_immig = y_immig_berlin - y_immig_kiez
       )

# Figure A6: Distribution plots for differences in preferences
p_dist(
       o = "diff_constr",
       o_lab = "Diff. between city-wide and\nlocal-level construction preferences",
       b = -3:3,
       l = -3:3
)

p_dist(
       o = "diff_immig",
       o_lab = "Diff. between city-wide and\nlocal-level immigration preferences",
       b = -3:3,
       l = -3:3
)
