## Bryan Ahaneku Assignment 3
library(mapproj)
library(maps)
library(tidyverse)
library(lintr)
library(styler)
library(ggplot2)
library(reshape2)
style_file("analysis.R")
lint("analysis.R")

data <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv")
# What percent of total population are black/white people admitted to prison?
percent_total_pop_black_prison_adm <- ((data$total_pop / data$black_prison_adm)
* 100)
percent_total_pop_white_prison_adm <- ((data$total_pop / data$white_prison_adm)
* 100)
# What's the minimum percentage for black/white people?
min_black_percentage <- round(min(percent_total_pop_black_prison_adm, na.rm = T), 2)
min_white_percentage <- round(min(percent_total_pop_white_prison_adm, na.rm = T), 2)
diff_min <- min_black_percentage - min_white_percentage

# What's the average pupulation of white vs black people in prison
mean_black_prison_pop <- round(mean(data$black_prison_pop, na.rm = T), 2)
mean_white_prison_pop <- round(mean(data$white_prison_pop, na.rm = T), 2)
# Each year, how many black females were in prison vs white females in prison
df_interest <- data %>%
  arrange(year) %>%
  na.omit(df_interest) %>%
  select(year, black_female_prison_pop, white_female_prison_pop) %>%
  group_by(year) %>%
  summarize(
    black = sum(black_female_prison_pop),
    white = sum(white_female_prison_pop)
  )

# fixing data so it can be graphed
df_interest <- df_interest %>%
  pivot_longer(black:white, names_to = "race", values_to = "Population")

# line graph
line_chart <- ggplot(df_interest, aes(x = year, y = Population, colour = race)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Population") +
  labs(color = "Race") +
  ggtitle("Black vs White Female Population in Prison") +
  theme(plot.title = element_text(hjust = 0.5))

# plot_2
point_chart <- ggplot(data = data, aes(x = total_jail_pop, y = total_jail_adm)) +
  geom_point(aes(alpha = total_jail_pop)) +
  labs(title = "Total jail vs prison population") +
  geom_smooth(method = "loess", se = F) +
  theme(legend.position = "none") +
  xlab("Total Jail Population") +
  ylab("Total Jail Admissions")

# making map
data_mod <- data %>%
  filter(year == max(year)) %>%
  select(yfips, year, fips, state, county_name, black_jail_pop)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(data_mod, by = "fips") %>%
  filter(state == "CA")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

blk_jail_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +
  labs(fill = "Black Jail Population", title = "Population Black In Jail") +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_jail_pop)),
    na.value = "white", low = "yellow", high = "red"
  ) +
  blank_theme
