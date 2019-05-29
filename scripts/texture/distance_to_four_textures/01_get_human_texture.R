# get most frequently described texture
library(tidyverse)
library(here)


HUMAN_TEXTURE <- here("data/raw/animals_texture.csv")
TIDY_HUMAN_TEXTURE <- here("data/processed/tidy_human_texture_response.csv")

texture_data <- read_csv(HUMAN_TEXTURE) %>%
  select(-texture)

animal_texture_combo <- texture_data %>%
  gather("texture", "value", -1:-4) %>%
  distinct(group, animal, texture)

correct_values <- texture_data %>%
  distinct(animal, expected, .keep_all = T) %>%
  filter(expected == 1)%>%
  select(-subject, -group) %>%
  select(-expected) %>%
  gather("texture", "correct", -1) %>%
  filter(correct == 1)

texture_count_data <- texture_data %>%
  select(-expected)%>%
  gather("texture", "value", -1:-3) %>%
  filter(value == 1) %>%
  group_by(group, animal, texture) %>%
  summarize(prop = n()/20) %>%
  full_join(animal_texture_combo) %>%
  ungroup()%>%
  mutate(prop = replace_na(prop, 0),
         texture = fct_relevel(texture, "scales", "skin", "fur", "feathers"),
         animal = fct_relevel(animal,  "goldfish", "snake", "crocodile", "lizard", "eel", "shark", "turtle", "toad",
                              "dolphin", "frog", "worm", "elephant", "hippo", "rhino", "pig", "platypus", "seal",
                              "ferret", "gorilla", "sheep", "fox", "bear", "horse", "donkey", "cat", "penguin", "ostrich",
                              "flamingo", "peacock", "pigeon")) %>%
  left_join(correct_values) %>%
  mutate(correct  = replace_na(correct,0))

write_csv(texture_count_data, TIDY_HUMAN_TEXTURE)
