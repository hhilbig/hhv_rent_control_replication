# Clear environment
rm(list = ls())

# Load required packages
pacman::p_load(readr, tidyverse, stringr, ggplot2, readxl)

# Load and process RDD results
res_full <- read_rds("saved_results/rd_res.rds") %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    tenant = case_when(
      str_detect(ss, "7") ~ "Treated tenants vs. non-treated\n(high-rent apartments\nin gentrifying areas)",
      str_detect(ss, "8") ~ "Treated tenants vs. non-treated\n(high-rent apartments\nin non-gentrifying areas)"
    ),
    covars = case_when(
      covars == "Covars" ~ "Covariates incl.",
      covars == "No covars" ~ "No covariates",
      str_detect(covars, "time") ~ "Covariates incl.\n(time since ad omitted)",
      str_detect(covars, "post") ~ "Covariates incl.\n(no post-treatment covariates)"
    )
  ) %>%
  filter(
    str_detect(ss, "7|8"),
    covars != "No covariates"
  ) %>%
  mutate(
    ss_number = substr(ss, 1, 3),
    covars = factor(covars, levels = unique(covars)[c(4, 3, 2, 1)])
  )

o_nimby <- "index_nimbyism"

# Create Figure 5
p1 <- res_full %>%
  filter(outcome %in% o_nimby) %>%
  ggplot(aes(tenant, estimate, covars)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = covars),
    width = 0, size = 0.9, position = position_dodge(0.4)
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = covars),
    width = 0, size = 0.6, position = position_dodge(0.4)
  ) +
  geom_point(
    aes(color = covars, shape = covars, fill = covars),
    position = position_dodge(0.4), size = 2
  ) +
  scale_fill_grey(name = "", end = 0.6, start = 0) +
  scale_color_grey(name = "", end = 0.6, start = 0) +
  scale_shape_manual(values = c(21:24), name = "") +
  scale_y_continuous(breaks = -1:1) +
  coord_flip() +
  facet_wrap(~label) +
  labs(x = "", y = "RD estimate\n(standard deviations)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2))

# Display the plot
print(p1)
