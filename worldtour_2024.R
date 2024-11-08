# worldtour_2024.R
# Timothy Skolds
# Created 10/17/24
# Analysis of 2024 WorldTour Results

library(tidyverse)
library(gridExtra)

# Import Data
worldtour_2024 <- read_csv(file = "../data/worldtour_2024.csv") %>%
  filter(!is.na(Winner),
         !is.na(Team)) %>%
  rename("TeamStatus" = "Team Status")

# Test analysis, number of wins per team
wins_per_team <- ggplot(worldtour_2024,
                        aes(x = Team,
                            fill = Team)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Team",
       y = "Number of Wins",
       title = "WT Wins by Team") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) # Removes space below zero

# PT WT wins
PT_wins <- worldtour_2024 %>%
  filter(TeamStatus == "PT")

wins_per_PT <- ggplot(PT_wins,
                      aes(x = Team,
                          fill = Team)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Team",
       y = "Number of Wins",
       title = "WT Wins per ProTeam") +
  theme(legend.position = "none")
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Boxplot
wins_team <- worldtour_2024 %>%
  group_by(Team) %>%
  summarise(Wins = n())

wins_boxplot <- ggplot(wins_team,
                       aes(y = Wins)) +
  geom_boxplot(fill = "limegreen") +
  theme_bw() +

wins_PT <- PT_wins %>%
  group_by(Team) %>%
  summarise(Wins = n())

wins_boxplot_PT <- ggplot(wins_PT,
                          aes(y = Wins)) +
  geom_boxplot(fill = "pink") +
  theme_bw()

grid.arrange(wins_per_team,
             wins_per_PT,
             wins_boxplot,
             wins_boxplot_PT,
             ncol = 2)


# Find top riders
top_10_riders <- worldtour_2024 %>%
  group_by(Winner, Team) %>%
  summarize(Wins = n(), .groups = 'drop') %>%
  slice_max(n = 11, order_by = Wins)
print(top_10_riders)

# Map of Pogi's wins
pogi_wins <- worldtour_2024 %>%
  filter(Winner == "T. Pogacar") %>%
  group_by(Country) %>%
  summarise(Wins = n())

library(sf)

europe_geo_data <- st_read("../data/europe.geojson") %>%
  rename("Country" = "NAME")

pogi_data_merged <- europe_geo_data %>%
  left_join(pogi_wins,
            by = "Country")

ggplot(data = pogi_data_merged) +
  geom_sf(aes(fill = Wins),
          linewidth = 0,
          alpha = 0.9) +
  theme_void() +
  scale_fill_viridis_c(
    trans = "log", breaks = seq(from = 1,
                                to = 10,
                                by = 1),
    name = "Number of Wins",
    guide = guide_legend(
      keyheight = unit(1, units = "mm"),
      keywidth = unit(4, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1)) +
  theme(legend.position = c(0.3,0.1)) +
  labs(title = "T. Pogacar 2024 WorldTour Wins",
       caption = "NOTE: One win in Canada
       Data from FirstCycling")











