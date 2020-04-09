
#Load Libraries
library(tidyverse)
library(devtools)
library(lubridate)
library(janitor)
devtools::install_github("thebioengineer/tidytuesdayR")
library(rcartocolor)
library(patchwork)
library(extrafont)
loadfonts(device = 'win')

#Load Data
tuesdata <- tidytuesdayR::tt_load('2020-04-07')
tuesdata <- tidytuesdayR::tt_load(2020, week = 15)

tdf_winners <- tuesdata$tdf_winners
stage_data <- tuesdata$stage_data
tdf_stages <- tuesdata$tdf_stages


#Create Dataset for charts
n_stage_data <- tdf_stages %>%
  #thanks to @delaBJL for text formatting and getting the same 'Type' across Tour editions
  mutate(Type = stringr::str_to_lower(Type)) %>%
  mutate(
    Type = case_when(
      grepl("flat", Type) ~ "Flat",
      grepl("plain", Type) ~ "Flat",
      grepl("time trial", Type) ~ "Time Trial",
      grepl("mountain", Type) ~ "Mountain",
      grepl("transition", Type) ~ "Transition",
      grepl("intermediate", Type) ~ "Transition",
      grepl("half", Type) ~ "Transition",
      grepl("hilly", Type) ~ "Mountain",
      TRUE ~ Type
    ),
    Year = year(Date)
  ) %>%
  select(Year, Type) %>%
  #remove 'Transition' Stage for simplicity
  filter(Type != 'Transition') %>%
  group_by(Year) %>%
  count(Type) %>%
  #for cumulative graphs, need to turn NAs into 0
  pivot_wider(
    id_cols = Year,
    names_from = Type,
    values_from = n,
    values_fill = list(n = 0)
  ) %>%
  pivot_longer(-Year,
               names_to = 'Type',
               values_to = 'n') %>%
  #calculate percentage of each 'Type' per tour
  mutate(percentage = n / sum(n))



#Create Graphs
cm_plot <-
  ggplot(n_stage_data, aes(Year, percentage*100, fill = Type)) +
  geom_area(aes(colour = Type, fill = after_scale(alpha(colour, 0.4))), size = 1) +
  scale_colour_carto_d(palette = 'TealGrn') +
  labs(
    title = 'More Stages are Planned for the Mountains with Progressive Tour Stages',
    subtitle = 'How was each tour shaped in terms of Type of stage?',
    y = 'Percent of Tour (%)',
    colour = 'Type of Stage') +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    text = element_text(family = 'Roboto'))


pt_plot <-
  ggplot(n_stage_data, aes(Year, n)) +
  geom_line(aes(colour = Type), alpha = 0.5) +
  geom_point(aes(colour = Type, fill = after_scale(alpha(colour, 0.8))),
             pch = 21,
             size = 2) +
  scale_colour_carto_d(palette = 'TealGrn') +
  labs(
    subtitle = 'A look at the number of each type of stage per tour',
    y = 'Number of Stages',
    colour = 'Type of Stage',
    caption = 'TidyTuesday Project
               Data from github.com/alastairrushworth/tdf') +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    legend.position = 'none',
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    text = element_text(family = 'Roboto')
  )

#Final Plot
cm_plot / pt_plot

ggsave('TDF_stages.png', 
       width = 25,
       height = 15,
       units = 'cm',
       device='png',
       type='cairo')
