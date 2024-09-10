rm(list = ls())

pacman::p_load(tidyverse, readxl)

## Get results
## Start just with the main covars and no covars

res_full <- read_rds("saved_results/rk_res.rds") %>%
  filter(str_detect(ss, "Full")) %>%
  mutate(
    conf.low90 = estimate - qnorm(.95) * std.error,
    conf.high90 = estimate + qnorm(.95) * std.error
  ) %>%
  mutate(tenant = "Treated tenants\nvs. non-treated")

## Combine

res_full <- res_full %>%
  mutate(covars = case_when(
    covars == "Covars" ~ "Covariates incl.",
    covars == "No covars" ~ "No covariates",
    str_detect(covars, "time") ~ "Covariates incl.\n(time since ad omitted)",
    str_detect(covars, "post") ~ "Covariates incl.\n(no post-treatment covariates)"
  )) %>%
  mutate(covars = factor(covars, levels = unique(covars)[c(4, 3, 2, 1)])) %>%
  filter(!covars == "No covariates")

## Plot

pd <- position_dodge(0.4)

## Individual outcomes - NIMBY index

o_nimby <- c("y_immig_kiez", "y_constr_kiez", "index_nimbyism")
o_nimby %>% dput()


# Figure 6 ----------------------------------------------

p1 <- res_full %>%
  filter(outcome %in% o_nimby) %>%
  ggplot(aes(tenant, estimate, covars)) +
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
  ylab("RK estimate\n(standard deviations)") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2)) +
  facet_wrap(~label) +
  scale_y_continuous(breaks = -1:1)
p1
