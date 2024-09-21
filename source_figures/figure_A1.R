# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, Hmisc, ggplot2, haschaR)

# Read and process data
civ <- read_excel("data/civey_survey.xlsx", skip = 9) %>%
    rename(
        id = 1, support_rent_control = 2, date = 3, zip = 4,
        age = 5, gender = 6, pop_density = 7, party_id = 9, weight = Gewicht
    ) %>%
    select(id, support_rent_control, zip, weight, party_id) %>%
    mutate(
        party_id = recode(party_id,
            `Grüne` = "Greens",
            `Linke` = "Left party",
            `Sonstige` = "Other",
            `Nichtwähler` = "Do not vote"
        ),
        berlin = as.integer(substr(zip, 1, 2) %in% c("10", "12", "13", "14")),
        support_rent_control_bin = as.integer(support_rent_control %in% c(
            "Eher ja",
            "Ja, auf jeden Fall"
        ))
    )

# Calculate weighted support for rent control
calculate_support <- function(data, filter_condition = TRUE) {
    data %>%
        filter({{ filter_condition }}) %>%
        summarise(
            support_rent_control = weighted.mean(support_rent_control_bin, weight),
            sd = sqrt(wtd.var(support_rent_control_bin, weight)),
            n = n(),
            se_mean = sd / sqrt(n)
        )
}

b_support <- calculate_support(civ, berlin == 1) %>% mutate(party_id = "Berlin only")
overall_support <- calculate_support(civ) %>% mutate(party_id = "Full sample")

# Prepare data for plotting
plot_df <- bind_rows(b_support, overall_support) %>%
    mutate(
        conf.low = support_rent_control - 1.96 * se_mean,
        conf.high = support_rent_control + 1.96 * se_mean
    )

# Calculate support by party in Berlin
b_support_party <- calculate_support(civ %>% filter(berlin == 1), TRUE) %>%
    group_by(party_id) %>%
    mutate(
        conf.low = support_rent_control - 1.96 * se_mean,
        conf.high = support_rent_control + 1.96 * se_mean,
        party_id = fct_reorder(party_id, support_rent_control)
    )

# Combine data for final plot
final_plot_data <- bind_rows(b_support_party, plot_df) %>%
    mutate(
        group = if_else(str_detect(party_id, "Full|Berlin"), "Overall", "By party (Berlin only)"),
        party_id = fct_reorder(party_id, support_rent_control),
        group = fct_rev(as.factor(group))
    )

# Create final plot
p2 <- ggplot(final_plot_data, aes(party_id, support_rent_control * 100)) +
    geom_errorbar(aes(ymin = conf.low * 100, ymax = conf.high * 100), width = 0) +
    geom_point() +
    labs(x = "", y = "% support for\nnationwide rent control") +
    theme_bw() +
    facet_wrap(~group, ncol = 2, scales = "free_x", drop = TRUE) +
    x_axis_45deg() +
    theme(panel.grid.major.y = element_line(color = "grey90", linetype = "solid"))
p2
