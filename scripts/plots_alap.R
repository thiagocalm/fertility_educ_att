#' @description Descriptive analysis - plots ALAP Conference 2024
#' @date 2024-12-01
#' @update_description Creating plots
#' ----------------------------------------------------------------------------

# General settings
rm(list = ls())
options(timeout = 600, scipen = 9999)

# Packages
library(pacman)
p_load(wpp2024, tidyverse, WDI, patchwork)

# Map ---------------------------------------------------------

# Importing UN data

data(percentASFR1dt)
data(tfr1dt)

# Importing auxiliar table

lac_table <- read_csv("./docs/lac_countries.csv")

# Filtering data for our countries of analysis

fertility_data <- percentASFR1dt %>%
  as_tibble() %>%
  filter(year %in% c(1980,1985, 1995, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
  filter(country_code %in% lac_table$m49_code) %>%
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
  ) %>%
  distinct()

# Visualizing PASFR among countries

percent <- fertility_data %>%
  filter(age == 15) %>%
  filter(year %in% c(1990,2000,2010)) %>%
  select(-c(asfr,tfr, age)) %>%
  pivot_wider(names_from = year, values_from = pasfr) %>%
  mutate(
    dif_2000_1990 = `2000`-`1990`,
    dif_2010_2000 = `2010`-`2000`,
    dif_2010_1990 = `2010`-`1990`
  ) %>%
  select(country_code, name, dif_2000_1990, dif_2010_2000, dif_2010_1990) %>%
  ggplot() +
  geom_segment(
    # aes(x=name, xend=name, y=dif_2000_1990, yend=dif_2010_2000),
    aes(x=name, xend=name, y=0, yend=dif_2010_1990),
    color="grey"
  ) +
  geom_point(
    aes(x=name, y=dif_2010_1990),
    color=rgb(0.2,0.7,0.1,0.5),
    size=3
  ) +
  # geom_point(
  #   aes(x=name, y=dif_2000_1990),
  #   color=rgb(0.2,0.7,0.1,0.5),
  #   size=3
  # ) +
  # geom_point(
  #   aes(x=name, y=dif_2010_2000),
  #   color=rgb(0.7,0.2,0.1,0.5),
  #   size=3
  # ) +
  coord_flip() +
  theme_bw()

lac_estimates <- fertility_data %>%
  filter(age == 15) %>%
  filter(year %in% c(1990,2000,2010)) %>%
  select(-c(pasfr,tfr, age)) %>%
  pivot_wider(names_from = year, values_from = asfr) %>%
  mutate(
    dif_2000_1990 = (`2000`-`1990`)/`1990`*100,
    dif_2010_2000 = (`2010`-`2000`)/`2000`*100,
    dif_2010_1990 = (`2010`-`1990`)/`1990`*100
  ) %>%
  summarise(dif_2000_1990 = mean(dif_2000_1990),
            dif_2010_2000 = mean(dif_2010_2000),
            dif_2010_1990 = mean(dif_2010_1990))

asfr <- fertility_data %>%
  filter(age == 15) %>%
  filter(year %in% c(1990,2000,2010)) %>%
  select(-c(pasfr,tfr, age)) %>%
  pivot_wider(names_from = year, values_from = asfr) %>%
  mutate(
    dif_2000_1990 = (`2000`-`1990`)/`1990`*100,
    dif_2010_2000 = (`2010`-`2000`)/`2000`*100,
    dif_2010_1990 = (`2010`-`1990`)/`1990`*100
  ) %>%
  select(country_code, name, dif_2000_1990, dif_2010_2000, dif_2010_1990) %>%
  ggplot() +
  geom_segment(
    aes(x=name, xend=name, y=dif_2000_1990, yend=dif_2010_2000),
    # aes(x=name, xend=name, y=0, yend=dif_2010_1990),
    color="grey"
  ) +
  # geom_point(
  #   aes(x=name, y=dif_2010_1990),
  #   color=rgb(0.2,0.7,0.1,0.5),
  #   size=3
  # ) +
  geom_point(
    aes(x=name, y=dif_2000_1990),
    color=rgb(0.2,0.7,0.1,0.5),
    size=3
  ) +
  geom_point(
    aes(x=name, y=dif_2010_2000),
    color=rgb(0.7,0.2,0.1,0.5),
    size=3
  ) +
  geom_hline(
    yintercept = lac_estimates$dif_2000_1990,
    color=rgb(0.2,0.7,0.1,0.5),
    linetype = "dashed",
    linewidth = 1.05,
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = lac_estimates$dif_2010_2000,
    color=rgb(0.7,0.2,0.1,0.5),
    linetype = "dashed",
    linewidth = 1.05,
    alpha = 0.8
  ) +
  coord_flip() +
  theme_bw()

# Improve LAC estimates in a right way
# Ordering estimates: (i) by bigger negative difference? (ii) by bigger positive difference?
# include texts into the graphic explaining labels and changes
# including arrows showing differences
# changing values


# Setting graph -----------------------------------------------------------

fig <- (
  percent +
    scale_x_continuous(breaks = seq(10,50,5)) +
    labs(
      y = "Relative age-specific fertility rates (%)",
      x = "Age (in 5-years group)",
      color = "Years",
      title = "(i) Relative Age-specific Fertility Rate"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 16, vjust = .5, hjust = .5),
      axis.title = element_text(face = "bold", size = 12, vjust = .5, hjust = .5),
      axis.text = element_text(size = 12, vjust = .5, hjust = .5),
      legend.title = element_text(face = "bold", size = 12, vjust = .5, hjust = .5),
      legend.position = c("bottom")
    )
) + (

  educ_graph +
    coord_cartesian(ylim = c(0,100)) +
    scale_y_continuous(breaks = seq(0,100,10)) +
    scale_x_continuous(breaks = seq(2010,2018,1)) +
    labs(
      y = "Share of female population aged 15+ with education secondary or more (%)",
      x = "Years",
      color = "Countries",
      title = "(ii) Share of female population aged 15+ with education secondary or more"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 16, vjust = .5, hjust = .5),
      axis.title = element_text(face = "bold", size = 12, vjust = .5, hjust = .5),
      axis.text = element_text(size = 12, vjust = .5, hjust = .5),
      legend.title = element_text(face = "bold", size = 12, vjust = .5, hjust = .5),
      legend.position = c("bottom")
    )

)
