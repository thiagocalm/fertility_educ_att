#' @description Descriptive analysis - plots ALAP Conference 2024
#' @date 2024-12-04
#' @update_description Adjusts in plots
#' ----------------------------------------------------------------------------

# General settings
rm(list = ls())
options(timeout = 600, scipen = 9999)

# Packages
library(pacman)
p_load(wpp2024, tidyverse, WDI, patchwork, forcats, sf, spData, ggrepel, ggspatial, wesanderson)

# Fertility ---------------------------------------------------------------


# Importing UN data

data(percentASFR1dt)
data(tfr1dt)

# Importing auxiliar table

lac_table <- read_csv("./docs/lac_countries.csv")

countries_lac <- c(
  "Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
  "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica",
  "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
  "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
  "Dominica", "Saba"
)

# Filtering data for our countries of analysis

fertility_data <- percentASFR1dt %>%
  as_tibble() %>%
  filter(year %in% c(1980,1985, 1995, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
  filter(country_code %in% c(1830,lac_table$m49_code)) %>%
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
    ),
    name = case_when(
      name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      name == "Bolivia (Plurinational State of)" ~ "Bolivia",
      TRUE ~ name
    )
  ) %>%
  group_by(country_code, name, year, age) %>%
  reframe(
    pasfr = sum(pasfr)/5,
    asfr = sum(asfr)/5,
    tfr = tfr
  ) %>%
  distinct()

lac_estimates <- fertility_data %>%
  filter(country_code == 1830) %>%
  filter(age == 15) %>%
  filter(year %in% c(1995,2005,2015)) %>%
  select(-c(pasfr,tfr, age)) %>%
  pivot_wider(names_from = year, values_from = asfr) %>%
  mutate(
    dif_2005_1995 = (`2005`-`1995`)/`1995`*100,
    dif_2015_2005 = (`2015`-`2005`)/`2005`*100,
    dif_2015_1995 = (`2015`-`1995`)/`1995`*100
  )

# ploting graph
asfr_df <- fertility_data %>%
  filter(country_code != 1830) %>%
  filter(name %in% countries_lac) %>%
  filter(age == 15) %>%
  filter(year %in% c(1995,2005,2015)) %>%
  select(-c(pasfr,tfr, age)) %>%
  pivot_wider(names_from = year, values_from = asfr) %>%
  mutate(
    dif_2005_1995 = (`2005`-`1995`)/`1995`*100,
    dif_2015_2005 = (`2015`-`2005`)/`2005`*100,
    dif_2015_1995 = (`2015`-`1995`)/`1995`*100
  ) %>%
  select(country_code, name, dif_2005_1995, dif_2015_2005, dif_2015_1995) %>%
  mutate(name = fct_reorder(name, desc(dif_2015_1995)))

asfr <- asfr_df %>%
  ggplot() +
  geom_segment(
    aes(x=name, xend=name, y=0, yend=dif_2015_1995),
    color="grey76"
  ) +
  geom_point(
    aes(x=name, y=dif_2015_1995),
    color=rgb(0.2,0.7,0.1,0.5),
    size=4
  ) +
  geom_hline(
    yintercept = lac_estimates$dif_2015_1995,
    color="#238443",
    linetype = "dashed",
    linewidth = 1.06,
    alpha = 0.8
  ) +
  geom_text(
    aes(
      x = "Cuba",
      y = -43,
      label = "America Latina y Caribe redujo un",
    ),
    color = "#238443",
    hjust = 0
  ) +
  geom_text(
    aes(
      x = "Mexico",
      y = -43,
      label = paste0(round(lac_estimates$dif_2015_1995,1), "% la TEF (15-19) entre 1995 y 2015.")
    ),
    color = "#238443",
    hjust = 0
  ) +
  geom_text(
    aes(
      x = "Belize",
      y = -44,
      label = "Peru redujo un",
    ),
    color = "#238443",
    hjust = 0
  ) +
  geom_text(
    aes(
      x = "Peru",
      y = -44,
      label = paste0(round(dif_2015_1995[name == "Peru"],1), "% la TEF (15-19)")
    ),
    color = "#238443",
    hjust = 0
  ) +
  geom_text(
    aes(
      x = "Suriname",
      y = -44,
      label = " entre 1995 y 2015"
    ),
    color = "#238443",
    hjust = 0
  ) +
  scale_y_continuous(breaks = seq(0,-100,-10), labels = \(x) paste(x, "%")) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(face = "bold",color = "#636363",size = 12, vjust = .5, hjust = .5)
  )

asfr

# Map ---------------------------------------------------------

# Map ASFR 15-19 in 2015

data("world") # importing data

lac_shp <- world %>%
  filter(name_long %in% countries_lac) %>%
  left_join(
    fertility_data %>%
      filter(age == 15) %>%
      filter(year %in% c(2015)) %>%
      select(name, country_code, asfr),
    by = c("name_long" = "name"),
    keep = FALSE
  )

peru_shp <- lac_shp %>%
  filter(name_long == "Peru")

map <- lac_shp %>%
  ggplot() +
  geom_sf(aes(fill = asfr),
          colour = "black", size = 0.1) +
  geom_sf(data = lac_shp,
          fill = "transparent",
          colour = "black", size = 0.6) +
  geom_sf(data = peru_shp,
          fill = "transparent",
          colour = "black", size = 0.8) +
  scale_fill_viridis_c(option = 2, direction = -1) +
  guides(fill = guide_colourbar(title = "TEF (15-19) en 2015\n(x1000)")) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.0, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.justification = c("right", "top"),
    legend.position.inside = c(0.8, 0.2),
    legend.box.just = "center",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(
      fill = "#f7f7f7",
      color = "#636363"
    ),
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 10, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#bdbdbd",linewidth = .3),
    panel.background = element_blank()
  )

map
# Parity ------------------------------------------------------------------

# creating dataframe to analysis

parfr_df <- fertility_data %>%
  filter(age %in% 10:45) %>%
  filter(year %in% c(1990,1995,2000,2005,2010,2015)) %>%
  filter(name == "Peru") %>%
  select(age,year,pasfr) %>%
  arrange(age,year) %>%
  mutate(
    pasfr_l1 = lag(pasfr,1),
    .by = c(age)
  ) %>%
  mutate(
    var = case_when(is.na(pasfr_l1) ~ 1, TRUE ~ (pasfr - pasfr_l1)/pasfr_l1)
  ) %>%
  mutate(
    pasfr_cum = cumsum(var),
    .by = c(age)
  )

pasfr <- parfr_df %>%
  mutate(
    age = factor(
      age,
      levels = seq(10,45,5),
      labels = c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49")
    )
  ) %>%
  ggplot() +
  aes(
    x = year, y = pasfr_cum, color = as.factor(age)
  ) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 4) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed",linewidth = 1.05) +
  scale_color_viridis_d(option = 1, direction = 1,end = .8) +
  labs(
    color = "Grupos de edad\n(5 años)"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(0.3,1.7)) +
  scale_y_continuous(breaks = seq(0.2,1.8,.2)) +
  theme(
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    # legend.justification = c("right", "top"),
    legend.position.inside = c(0.8, 0.2),
    legend.box.just = "center",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(
      fill = "#f7f7f7",
      color = "#636363"
    ),
    axis.title = element_blank(),
    axis.text = element_text(face = "bold", size = 10, color = "#636363", hjust = .5, vjust = .5),
    panel.grid = element_line(color = "#bdbdbd",linewidth = .3),
    panel.background = element_blank()
  )

pasfr


# Second porpose of graphic - summarizing it ------------------------------

# The idea is: let's summarise the behaviour of the countries in just one map

# ASFR and PASFR
tfr_asfr_df <- fertility_data %>%
  filter(age == 15) %>%
  filter(name %in% countries_lac | country_code == 1830) %>%
  mutate(
    diff_cases = case_when(
      name == "Peru" ~ 1,
      country_code == 1830 ~ 2,
      TRUE ~ 0
    )
  )

# creating the graphic
ggplot() +
  geom_line(
    data = tfr_asfr_df[tfr_asfr_df$diff_cases == 0,],
    aes(
      x = tfr,
      y = asfr,
      group = interaction(name,name)
    ),
    linewidth = 1.5, alpha = 0.8, color = "grey90"
  ) +
  geom_line(
    data = tfr_asfr_df[tfr_asfr_df$diff_cases == 1,],
    aes(
      x = tfr,
      y = asfr,
      group = interaction(name,name)
    ),
    linewidth = 1.5, color = "#f46d43"
  ) +
  geom_point(
    data = tfr_asfr_df[tfr_asfr_df$diff_cases == 1,],
    aes(
      x = tfr,
      y = asfr,
      group = interaction(name,name)
    ),
    size = 3, color = "#f46d43"
  ) +
  geom_line(
    data = tfr_asfr_df[tfr_asfr_df$diff_cases == 2,],
    aes(
      x = tfr,
      y = asfr,
      group = interaction(name,name)
    ),
    linewidth = 1.5, color = "#3288bd"
  ) +
  geom_point(
    data = tfr_asfr_df[tfr_asfr_df$diff_cases == 2,],
    aes(
      x = tfr,
      y = asfr,
      group = interaction(name,name)
    ),
    size = 3, color = "#3288bd"
  ) +
  geom_segment(
    aes(x = 1.5, y = 125, xend = 2.5, yend = 87.5),
    arrow = arrow(),
    size = 1.1,
    color = "#3288bd"
  ) +
  geom_segment(
    aes(x = 5, y = 50, xend = 3.95, yend = 80),
    arrow = arrow(),
    size = 1.1,
    color = "#f46d43"
  ) +
  geom_text(
    aes(
      x = 0,
      y = 140,
      label = "America Latina y Caribe redujo:\nLa TFG en 53% entre 1980 (4.1) y 2020 (1.9)\nLaTEF (15-19 años) en 42.6% entre 1980 (95.1) y 2020 (54.6)",
    ),
    color = "#3288bd",
    hjust = 0
  ) +
  geom_text(
    aes(
      x = 4,
      y = 37.5,
      label = "Peru redujo:\nLa TFG en 58% entre 1980 (5.0) y 2020 (2.1)\nLaTEF (15-19 años) en 50.5% entre 1980 (92.4) y 2020 (45.7)",
    ),
    color = "#f46d43",
    hjust = 0
  ) +
  coord_cartesian(xlim = c(0,6.5), ylim = c(0,200)) +
  scale_y_continuous(breaks = seq(0,200,25)) +
  scale_x_continuous(breaks = seq(0,7,0.5)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(face = "bold", size = 10, color = "#636363", hjust = .5, vjust = .5),
    panel.grid = element_line(color = "#d9d9d9",linewidth = .3)
  )
