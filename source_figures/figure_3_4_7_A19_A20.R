# Clear environment
rm(list = ls())

# Load required packages
pacman::p_load(tidyverse, readxl)

# Load RDD results
res <- read_rds("saved_results/rd_res.rds")

# Process full sample results
res_full <- res %>%
  filter(str_detect(ss, "Full")) %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    tenant = "Treated tenants\nvs. non-treated",
    covars = case_when(
      covars == "Covars" ~ "Covariates incl.",
      covars == "No covars" ~ "No covariates",
      str_detect(covars, "post") ~ "Covariates incl.\n(no post-treatment covariates)"
    )
  ) %>%
  mutate(covars = factor(covars, levels = unique(covars)[c(3, 2, 1)])) %>%
  filter(covars != "No covariates")

# Process results for non-imputed rent units
res_rent_not_imputed <- res %>%
  filter(str_detect(ss, "2 No units where rent is imputed")) %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error,
    tenant = "Treated tenants\nvs. non-treated",
    covars = case_when(
      covars == "Covars" ~ "Covariates incl.",
      covars == "No covars" ~ "No covariates",
      str_detect(covars, "post") ~ "Covariates incl.\n(no post-treatment covariates)"
    )
  ) %>%
  mutate(covars = factor(covars, levels = unique(covars)[c(3, 2, 1)])) %>%
  filter(covars != "No covariates")

# Set up plot parameters
pd <- position_dodge(0.4)

# Load measures and define NIMBY index
measures <- read_excel("data/Measures.xlsx")
o_nimby <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")

# Figure 3: NIMBY index
p1 <- res_full %>%
  filter(outcome %in% o_nimby) %>%
  ggplot(aes(tenant, estimate, covars)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = covars),
    width = 0, size = 0.9, position = pd
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = covars),
    width = 0, size = 0.6, position = pd
  ) +
  geom_point(
    aes(color = covars, shape = covars, fill = covars),
    position = pd, size = 2
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

print(p1)

# Figure 4: Financial difficulty and moving intentions
p2 <- res_full %>%
  filter(outcome %in% c("y_financial_diff", "y_move_5years"), !std.error < 0.001) %>%
  ggplot(aes(tenant, estimate, covars)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = covars),
    width = 0, size = 0.9, position = pd
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = covars),
    width = 0, size = 0.6, position = pd
  ) +
  geom_point(
    aes(color = covars, shape = covars, fill = covars),
    position = pd, size = 2
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

print(p2)

# Figure 7: Support for local-level construction
p3 <- res_full %>%
  filter(outcome %in% c("y_diff_constr"), !std.error < 0.001) %>%
  mutate(label = "Support for local-level\nrel. to city-wide construction") %>%
  ggplot(aes(tenant, estimate, covars)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = covars),
    width = 0, size = 0.9, position = pd
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = covars),
    width = 0, size = 0.6, position = pd
  ) +
  geom_point(
    aes(color = covars, shape = covars, fill = covars),
    position = pd, size = 2
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
  guides(fill = guide_legend(ncol = 1))

print(p3)

# Figure A.19: Binary outcomes
p4 <- res_full %>%
  filter(str_detect(outcome, "_bin")) %>%
  ggplot(aes(tenant, estimate, covars)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = covars),
    width = 0, size = 0.9, position = pd
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = covars),
    width = 0, size = 0.6, position = pd
  ) +
  geom_point(
    aes(color = covars, shape = covars, fill = covars),
    position = pd, size = 2
  ) +
  scale_fill_grey(name = "", end = 0.6, start = 0) +
  scale_color_grey(name = "", end = 0.6, start = 0) +
  scale_shape_manual(values = c(21:24), name = "") +
  coord_flip() +
  facet_wrap(~label) +
  labs(x = "", y = "RD estimate") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2))

print(p4)

# Figure A.20: NIMBY index for non-imputed rent units
p5 <- res_rent_not_imputed %>%
  filter(outcome %in% o_nimby) %>%
  ggplot(aes(tenant, estimate, covars)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(
    aes(ymin = conf.low90, ymax = conf.high90, color = covars),
    width = 0, size = 0.9, position = pd
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = covars),
    width = 0, size = 0.6, position = pd
  ) +
  geom_point(
    aes(color = covars, shape = covars, fill = covars),
    position = pd, size = 2
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

print(p5)
