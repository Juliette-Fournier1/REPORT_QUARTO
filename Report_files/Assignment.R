install.packages("tidyverse")
library(tidyverse)

install.packages("gapminder")
library(gapminder)


unicef_indicator_1.clean <- read.csv("~/Documents/NEOMA/4A/S2 - DCU/Data/unicef_indicator_1 clean.csv", sep=";")
unicef_indicator_2.clean <- read.csv("~/Documents/NEOMA/4A/S2 - DCU/Data/unicef_indicator_2 clean.csv", sep=";")
unicef_metadata.life.exp <- read.csv("~/Documents/NEOMA/4A/S2 - DCU/Data/unicef_metadata life exp.csv", sep=";")

utile???
data_join <- full_join(unicef_indicator_1.clean, unicef_metadata.life.exp, join_by(country == country, time_period == year))
data_join <- full_join(unicef_indicator_1.clean, unicef_indicator_2.clean, join_by(country == country, time_period == time_period))

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)


world_map <- map_data("world")

data_join <- unicef_indicator_1.clean %>%
  full_join(unicef_indicator_2.clean, by = c("country", "time_period")) %>%
  full_join(unicef_metadata.life.exp, by = c("country", "time_period" = "year"))

map_data_join <- full_join(data_join, world_map, by = c("country" = "region"))

pastel_red <- "#FF6969"
pastel_blue <- "#85C1E9"

library(dplyr)

install.packages(mapproj)
library(mapproj)

install.packages("countrycode")
library(countrycode)

country_list <- data.frame(country = as.character(countrycode::codelist$country.name.en),
                           iso2 = countrycode::codelist$iso2c)

country_list$continent <- countrycode(country_list$iso2, origin = "iso2c", destination = "continent")

map_data_join_2021_with_continent <- full_join(map_data_join_2021, country_list, by = c("country" = "country"))


map_data_join_2021 <- map_data_join %>%
  filter(time_period == 2021)

CARTE

ggplot(map_data_join_2021) +
  aes(x = long, y = lat, group = group, fill = obs_value.x) +
  geom_polygon(data = map_data_join) +
  scale_fill_gradient(low = pastel_red, high = pastel_blue, name = "Proportion of population using piped drinking water sources (%)") +
  theme_bw() +
  labs(title = "The injustice of water: World map of access to piped drinking water in 2021",
       x = "Longitude",
       y = "Latitude") +
  theme(legend.position = "bottom")

SCATTER PLOT

map_data_filtered <- map_data_join_2021_with_continent %>%
  filter(!is.na(continent))

ggplot(map_data_filtered, aes(x = Life.expectancy.at.birth..total..years., y = obs_value.x, color = continent)) +
  geom_point() +
  labs(title = "Access to piped drinking water and life expectancy: a vital relationship in 2021",
       x = "Life Expectancy at birth (years)",
       y = "Proportion of population using piped drinking water sources (%)")

BART CHART

map_data_join_with_continent <- full_join(map_data_join, country_list, by = c("country" = "country"))

continent_means_water_ok <- map_data_join_with_continent %>%
  filter(!is.na(continent), time_period >= 2010, time_period <= 2021) %>%
  group_by(continent, time_period) %>%
  summarize(mean_water_access = mean(obs_value.x, na.rm = TRUE)) %>%
  filter(continent != "Antarctica")

ggplot(continent_means_water_ok, aes(x = factor(continent), y = mean_water_access, fill = factor(continent))) +
  geom_bar(stat = "identity") +
  labs(title = "Mean access to piped drinking water by continent",
       x = "Continent",
       y = "Proportion of population using piped drinking water sources (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ time_period, scales = "free_x") +
  scale_fill_discrete(name = "Continent")


TIME SERIES

mean_water_access_time_series <- data_join %>%
  group_by(time_period) %>%
  summarize(mean_water_access = mean(obs_value.x, na.rm = TRUE))

mean_life_expectancy_time_series <- data_join %>%
  group_by(time_period) %>%
  summarize(mean_life_expectancy = mean(Life.expectancy.at.birth..total..years., na.rm = TRUE))

mean_data <- inner_join(mean_water_access_time_series, mean_life_expectancy_time_series, by = "time_period")

mean_data <- mean_data[complete.cases(mean_data), ]

ggplot(mean_data, aes(x = time_period)) +
  geom_line(aes(y = mean_water_access, color = "Proportion of population using piped drinking water sources (%)")) +
  geom_line(aes(y = mean_life_expectancy, color = "Life Expectancy at birth (years)")) +
  labs(title = "Evolution of Global Piped Drinking Water Access and Life Expectancy (2000-2021)",
       x = "Year",
       y = "Variables",
       color = "") +
  scale_color_manual(values = c("Proportion of population using piped drinking water sources (%)" = "#85C1E9", "Life Expectancy at birth (years)" = "#577E57")) +
  theme_minimal() +
  xlim(2000, 2021) +
  theme(legend.position = "bottom")

