#' @description Descriptive analysis among countries using WPP and World Bank data
#' @date 2024-09-15
#' @update_description Including 1980-2020 and creating summary graphs
#' ----------------------------------------------------------------------------

# General settings
rm(list = ls())
options(timeout = 600, scipen = 9999)

# Packages
library(pacman)
p_load(wpp2024, tidyverse, wbstats)

# Fertility rates ---------------------------------------------------------

# Importing data

data(percentASFR1dt)
data(tfr1dt)

# Filtering data for our countries of analysis

fertility_data <- percentASFR1dt %>%
  as_tibble() %>%
  filter(country_code %in% c(231, 356, 604, 704)) %>%
  filter(year %in% c(1980,1985, 1995, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
  left_join(
    tfr1dt %>% as_tibble() %>% select(country_code, year, tfr),
    by = c("country_code", "year"),
    keep = FALSE
  ) %>%
  mutate(
    asfr = ((pasfr * tfr)/100)*1000
  ) %>%
  mutate(
    age = case_when(
      age < 15 ~ 10,
      age %in% 15:19 ~ 15,
      age %in% 20:24 ~ 20,
      age %in% 25:29 ~ 25,
      age %in% 30:34 ~ 30,
      age %in% 35:39 ~ 35,
      age %in% 40:44 ~ 40,
      age %in% 45:49 ~ 45,
      TRUE ~ 50
    )
  ) %>%
  group_by(country_code, name, year, age) %>%
  reframe(
    pasfr = sum(pasfr),
    asfr = sum(asfr),
    tfr = tfr
  )

# Visualizing PASFR for each country

percent <- fertility_data %>%
  ggplot() +
  aes(
    x = age,
    y = pasfr,
    group = as.factor(year),
    color = as.factor(year)
  ) +
  geom_line(linewidth = .9) +
  geom_point() +
  lemon::facet_rep_wrap(name ~ ., repeat.tick.labels = TRUE) +
  scale_color_brewer(palette = "OrRd") +
  theme_bw()

asfr <- fertility_data %>%
  ggplot() +
  aes(
    x = age,
    y = asfr,
    group = as.factor(year),
    color = as.factor(year)
  ) +
  geom_line(linewidth = .9) +
  geom_point() +
  lemon::facet_rep_wrap(name ~ ., repeat.tick.labels = TRUE) +
  scale_color_brewer(palette = "OrRd") +
  theme_bw()

# Visualizing TFR for each country

tfr <- fertility_data %>%
  select(name, year, tfr) %>%
  distinct() %>%
  ggplot() +
  aes(
    x = year,
    y = tfr,
    group = as.factor(name),
    color = as.factor(name)
  ) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  geom_hline(yintercept = 2.1, color = "grey30", linetype = "dashed", size = .9) +
  scale_color_brewer(palette = "Set3") +
  theme_bw()

# Visualizing ASFR15 for each country

asfr15 <- fertility_data %>%
  filter(age == 15) %>%
  distinct() %>%
  ggplot() +
  aes(
    x = year,
    y = asfr,
    group = as.factor(name),
    color = as.factor(name)
  ) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  scale_color_brewer(palette = "Set3") +
  theme_bw()

# Visualizing ASFR10 for each country

asfr10 <- fertility_data %>%
  filter(age == 10) %>%
  distinct() %>%
  ggplot() +
  aes(
    x = year,
    y = asfr,
    group = as.factor(name),
    color = as.factor(name)
  ) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  scale_color_brewer(palette = "Set3") +
  theme_bw()

# Visualizing relative variation between periods

variation <- fertility_data %>%
  filter(age < 50) %>%
  distinct() %>%
  arrange(country_code, age, year) %>%
  mutate(
    lead_pasfr = lead(pasfr),
    .by = c(country_code, age)
  ) %>%
  mutate(
    pasfr_var = (lead_pasfr - pasfr)/pasfr
  ) %>%
  mutate(
    lag_pasfr = lag(pasfr_var),
    .by = c(country_code, age)
  ) %>%
  mutate(
    lag_pasfr = case_when(is.na(lag_pasfr) ~ 1, TRUE ~ 1+lag_pasfr)
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = lag_pasfr,
    group = as.factor(age),
    color = as.factor(age)
  ) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  scale_color_viridis_d() +
  lemon::facet_rep_wrap(name ~ ., repeat.tick.labels = TRUE) +
  theme_bw()
