# get most frequently described texture
library(tidyverse)
library(here)


HUMAN_TEXTURE <- here("data/raw/animals_texture.csv")

texture_data <- read_csv(HUMAN_TEXTURE)

texture_data %>%
  distinct(subject, group) %>%
  count(group)

count_data <- texture_data %>%
 # select(subject, group, animal, texture) %>%
  group_by(group, animal, texture, expected) %>%
  summarize(prop = n()/20) %>%
  data.frame() %>%
  filter(!(animal == "turtle" & texture == "skin")) %>% # this is just an error, I think
  filter(expected == 1) %>%
  mutate(texture = fct_relevel(texture, "scales", "skin", "fur", "feathers"))

# reproduce their figure
count_data %>%
  ggplot(aes(x = texture, y = animal, fill = prop))  +
  geom_tile() +
  facet_wrap(~fct_rev(group)) +
  scale_fill_gradient(low = "white", high = "black")  +
  theme_classic()
  
