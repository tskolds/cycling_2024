# worldtour_2024.R
# Timothy Skolds
# Created 10/17/24
# Analysis of 2024 WorldTour Results

library(tidyverse)
library(gridExtra)

# Import Data
worldtour_2024 <- read_csv(file = "./worldtour_2024.csv") %>%
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
       title = "WT Wins by Team",
       caption = "Data sourced from FirstCycling") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) # Removes space below zero
print(wins_per_team)

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
       title = "WT Wins per ProTeam",
       caption = "Data sourced from FirstCycling") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
print(wins_per_PT)

# Find top riders
top_10_riders <- worldtour_2024 %>%
  group_by(Winner, Team) %>%
  summarize(Wins = n(), .groups = 'drop') %>%
  slice_max(n = 11, order_by = Wins)
print(top_10_riders)

# Timeseries of "Big 4"
big_four_TS <- ggplot(worldtour_2024,
                      aes(x = Date,
                          y = )

