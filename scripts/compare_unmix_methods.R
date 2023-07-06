library(tidyverse)
library(lubridate)

forest_igbps <- c("ENF", "EBF", "DBF", "MF")
meta  <- read_csv("data_working/ameriflux_site_metadata.csv")
unmix <- read_csv("data_in/gee/mod_unmix_8day_k2_log_linear.csv") %>%
  select(-.geo, -`system:index`) %>%
  mutate(date = str_sub(date, 1, 10),
         date = parse_date(date, "%Y_%m_%d")) %>%
  left_join(
    meta %>% select(SITE_ID, IGBP),
    by="SITE_ID"
  ) %>%
  filter(IGBP %in% forest_igbps)

# LAI Range vs. Tb - Tf
unmix %>%
  select(T_for_linear, T_bare_linear, T_bare_log, T_for_log, Lai_min,
         Lai_max, SITE_ID, date, Lai, LST) %>%
  mutate(
    T_diff_linear = T_bare_linear - T_for_linear,
    T_diff_log = T_bare_log - T_for_log,
    Lai_diff = Lai_max - Lai_min
  ) %>%
  select(T_diff_linear, T_diff_log, Lai_diff) %>%
  pivot_longer(contains("T_diff")) %>%
  mutate(name = recode(name,
                       "T_diff_linear"="Linear Regression",
                       "T_diff_log"="Log-transform Regression")) %>%
  # Filter unstable regressions
  filter(abs(value) < 50) %>%
  ggplot(aes(x=Lai_diff, y=value)) + 
  geom_bin2d() +
  facet_wrap(~ name) + 
  labs(x="Kernel LAI range",
       y="Tbare - T_for")

# LAI range vs. Tfor - LST
unmix %>%
  select(T_for_linear, T_for_log, Lai_min, Lai_max, LST) %>%
  mutate(Lai_diff = Lai_max - Lai_min,
         LST_diff_linear = T_for_linear - LST,
         LST_diff_log = T_for_log - LST) %>%
  pivot_longer(contains("LST_diff")) %>%
  filter(abs(value) < 50) %>%
  ggplot(aes(x=Lai_diff, y=value)) +
  geom_point() +
  facet_wrap(~ name)

# Tfor difference between two methods
unmix %>%
  filter(abs(T_for_linear-T_for_log) < 50) %>%
  ggplot(aes(x=T_for_linear, y=T_for_log)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")

unmix %>%
  filter(abs(T_for_linear - T_for_log) < 50) %>%
  pivot_longer(contains("T_for")) %>%
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~ name)
