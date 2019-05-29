# get most frequently described texture
library(tidyverse)
library(here)


HUMAN_TEXTURE <- here("data/raw/animals_texture.csv")
TIDY_HUMAN_TEXTURE <- here("data/processed/tidy_human_texture_response.csv")

texture_data <- read_csv(HUMAN_TEXTURE)

texture_data %>%
  distinct(animal, texture)

texture_data %>%
  distinct(subject, group) %>%
  count(group)

texture_count_data <- texture_data %>%
  group_by(group, animal, texture, expected) %>%
  summarize(prop = n()/20) %>%
  data.frame() %>%
  filter(!(animal == "turtle" & texture == "skin")) %>% # this is just an error, I think
  filter(expected == 1) %>%
  mutate(texture = fct_relevel(texture, "scales", "skin", "fur", "feathers"),
         animal = fct_relevel(animal,  "goldfish", "snake", "crocodile", "lizard", "eel", "shark", "turtle", "toad",
                              "dolphin", "frog", "worm", "elephant", "hippo", "rhino", "pig", "platypus", "seal",
                              "ferret", "gorilla", "sheep", "fox", "bear", "horse", "donkey", "cat", "penguin", "ostrich",
                              "flamingo", "peacock", "pigeon")) %>%
  select(-expected)

write_csv(texture_count_data, TIDY_HUMAN_TEXTURE)
# reproduce their figure
texture_count_data %>%
  ggplot(aes(x = texture, y = fct_rev(animal), fill = prop))  +
  geom_tile() +
  facet_wrap(~fct_rev(group)) +
  ylab("animal") +
  scale_fill_gradient(low = "white", high = "black", limits = c(0,1))  +
  theme_classic()
  
