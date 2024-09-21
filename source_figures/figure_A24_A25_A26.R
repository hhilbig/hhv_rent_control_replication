### Plot Generation for Rent Analysis in Berlin (2007-2021)
# Figures A.24, A.25, A.26: Asking rents of newly constructed buildings in Berlin over time

# Load required packages
rm(list = ls())
pacman::p_load(tidyverse, ggplot2, ggrepel)

# Figure A.24: Median Rent Over Time for Newly Constructed vs. Not Newly Constructed Buildings
filtered_data <- read_rds("data/data_fig24.rds")

A24 <- ggplot(filtered_data, aes(
    x = jahr, y = median_qmmiete,
    color = group, linetype = group, shape = group
)) +
    geom_line(show.legend = TRUE) +
    geom_point(size = 2, show.legend = TRUE) +
    labs(
        x = "Year",
        y = "Median: Net Cold Asking Rent per Square Meter",
        title = "Median (Asking) Rent in Berlin Over Time",
        subtitle = "Rental offers in newly constructed buildings vs. the rest (net cold rents per square meter)"
    ) +
    scale_color_manual(values = c(
        "newly constructed" = "black",
        "not newly constructed" = "black"
    )) +
    scale_linetype_manual(values = c("newly constructed" = "solid", "not newly constructed" = "dotted")) +
    scale_shape_manual(values = c("newly constructed" = 16, "not newly constructed" = 1)) +
    scale_x_continuous(breaks = unique(filtered_data$jahr)) +
    scale_y_continuous(limits = c(0, 25)) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_text(
        data = subset(filtered_data, jahr %in% c(2007, 2021)),
        aes(label = sprintf("%.2f", median_qmmiete), color = group),
        vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE
    ) +
    guides(linetype = guide_legend(title = NULL), shape = guide_legend(title = NULL), color = guide_legend(title = NULL))


print(A24)

# Figure A.25: Rent development in Gentrification Areas
gentrification_data <- read_rds("data/data_fig25.rds")

A25 <- ggplot(gentrification_data, aes(
    x = jahr, y = median_qmmiete,
    linetype = group, shape = group
)) +
    geom_line(show.legend = TRUE) +
    geom_point(show.legend = TRUE) +
    labs(
        x = "Year",
        y = "Median: Net Cold Asking Rent per Square Meter",
        title = "Median (Asking) Rent in Gentrification Areas in Berlin Over Time"
    ) +
    scale_x_continuous(breaks = unique(gentrification_data$jahr)) +
    scale_y_continuous(limits = c(0, 25)) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_text(
        data = subset(gentrification_data, jahr %in% c(2007, 2021)),
        aes(label = sprintf("%.2f", median_qmmiete)),
        color = "black", vjust = -1.5, size = 2, fontface = "bold", show.legend = FALSE
    ) +
    scale_linetype_manual(values = c(
        "newly constructed" = "solid",
        "not newly constructed" = "dotted"
    )) +
    scale_shape_manual(values = c(
        "newly constructed" = 16,
        "not newly constructed" = 1
    )) +
    guides(linetype = guide_legend(title = NULL), shape = guide_legend(title = NULL))


print(A25)

# Figure A.26: Rent development in Non-Gentrification Areas
non_gentrification_data <- read_rds("data/data_fig26.rds")

A26 <- ggplot(non_gentrification_data, aes(
    x = jahr,
    y = median_qmmiete, linetype = group, shape = group
)) +
    geom_line(show.legend = TRUE) +
    geom_point(show.legend = TRUE) +
    labs(
        x = "Year",
        y = "Median: Net Cold Asking Rent per Square Meter",
        title = "Median (Asking) Rent in Non-Gentrification Areas in Berlin Over Time"
    ) +
    scale_x_continuous(breaks = unique(non_gentrification_data$jahr)) +
    scale_y_continuous(limits = c(0, 25)) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_text(
        data = subset(non_gentrification_data, jahr %in% c(2007, 2021)),
        aes(label = sprintf("%.2f", median_qmmiete)),
        color = "black", vjust = -1.5, size = 2, fontface = "bold", show.legend = FALSE
    ) +
    scale_linetype_manual(values = c(
        "newly constructed" = "solid",
        "not newly constructed" = "dotted"
    )) +
    scale_shape_manual(values = c("newly constructed" = 16, "not newly constructed" = 1)) +
    guides(linetype = guide_legend(title = NULL), shape = guide_legend(title = NULL))



print(A26)
