#' @description Descriptive analysis - plots ALAP Conference 2024
#' @date 2024-12-01
#' @update_description Creating plots
#' ----------------------------------------------------------------------------

# General settings
rm(list = ls())
options(timeout = 600, scipen = 9999)

# Packages
library(pacman)
p_load(wpp2024, tidyverse, WDI, patchwork, forcats, sf, spData, ggrepel)

# Map ---------------------------------------------------------

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

# Visualizing PASFR among countries

# percent <- fertility_data %>%
#   filter(age == 15) %>%
#   filter(year %in% c(1995,2005,2015)) %>%
#   select(-c(asfr,tfr, age)) %>%
#   pivot_wider(names_from = year, values_from = pasfr) %>%
#   mutate(
#     dif_2000_1990 = `2005`-`1995`,
#     dif_2010_2000 = `2015`-`2005`,
#     dif_2010_1990 = `2015`-`1995`
#   ) %>%
#   select(country_code, name, dif_2005_1995, dif_2015_2005, dif_2015_1995) %>%
#   ggplot() +
#   geom_segment(
#     # aes(x=name, xend=name, y=dif_2005_1995, yend=dif_2015_2005),
#     aes(x=name, xend=name, y=0, yend=dif_2015_1995),
#     color="grey"
#   ) +
#   geom_point(
#     aes(x=name, y=dif_2015_1995),
#     color=rgb(0.2,0.7,0.1,0.5),
#     size=3
#   ) +
#   # geom_point(
#   #   aes(x=name, y=dif_2005_1995),
#   #   color=rgb(0.2,0.7,0.1,0.5),
#   #   size=3
#   # ) +
#   # geom_point(
#   #   aes(x=name, y=dif_2015_2005),
#   #   color=rgb(0.7,0.2,0.1,0.5),
#   #   size=3
#   # ) +
#   coord_flip() +
#   theme_bw()

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
  ) #%>%
  # summarise(dif_2005_1995 = mean(dif_2005_1995),
  #           dif_2015_2005 = mean(dif_2015_2005),
  #           dif_2015_1995 = mean(dif_2015_1995))

asfr <- fertility_data %>%
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
  mutate(name = fct_reorder(name, desc(dif_2015_1995))) %>%
  ggplot() +
  geom_segment(
    # aes(x=name, xend=name, y=dif_2005_1995, yend=dif_2015_2005),
    aes(x=name, xend=name, y=0, yend=dif_2015_1995),
    color="grey"
  ) +
  geom_point(
    aes(x=name, y=dif_2015_1995),
    color=rgb(0.2,0.7,0.1,0.5),
    size=3
  ) +
  # geom_point(
  #   aes(x=name, y=dif_2005_1995),
  #   color=rgb(0.2,0.7,0.1,0.5),
  #   size=3
  # ) +
  # geom_point(
  #   aes(x=name, y=dif_2015_2005),
  #   color=rgb(0.7,0.2,0.1,0.5),
  #   size=3
  # ) +
  geom_hline(
    yintercept = lac_estimates$dif_2015_1995,
    color=rgb(0.2,0.7,0.1,0.5),
    linetype = "dashed",
    linewidth = 1.05,
    alpha = 0.8
  ) +
  # geom_hline(
  #   yintercept = lac_estimates$dif_2005_1995,
  #   color=rgb(0.2,0.7,0.1,0.5),
  #   linetype = "dashed",
  #   linewidth = 1.05,
  #   alpha = 0.8
  # ) +
  # geom_hline(
  #   yintercept = lac_estimates$dif_2015_2005,
  #   color=rgb(0.7,0.2,0.1,0.5),
  #   linetype = "dashed",
  #   linewidth = 1.05,
  #   alpha = 0.8
  # ) +
  coord_flip() +
  theme_minimal()

# Improve LAC estimates in a right way
# Ordering estimates: (i) by bigger negative difference? (ii) by bigger positive difference?
# include texts into the graphic explaining labels and changes
# including arrows showing differences
# changing values

# Map ASFR 15-19 in 2010

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

map <- lac_shp %>%
  ggplot() +
  geom_sf(aes(fill = asfr),
          colour = "black", size = 0.1) +
  geom_sf(data = lac_shp,
          fill = "transparent",
          colour = "black", size = 0.6) +
  scale_fill_viridis_c(option = 2,direction = -1) +
  guides(fill = guide_colourbar(title = "ASFR 15-19 (2010)")) +
  labs(
    title = "ASFR 15-19 (2010) - Latin America and Caribbean",
    caption = "Source: UNDP, World Population Prospects, Revision 2024."
  ) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())

# Setting graph -----------------------------------------------------------

# map + data

asfr + map

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
