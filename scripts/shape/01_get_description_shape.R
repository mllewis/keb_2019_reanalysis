# get most frequently described shape
library(tidyverse)
library(here)


CB_SHAPE <- here("data/raw/animals_CB_descriptions_shape.csv")
S_SHAPE <- here("data/raw/animals_S_descriptions_shape.csv")
DESCRIPTIONS_OUTFILE <- here('data/processed/animal_shape_descriptions.csv')

cb_data <- read_csv(CB_SHAPE, 
         col_names = c("animal", "sub_id", "w1", "w2", "w3", "w4")) %>%
  fill(animal) %>%
  unite(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  gather("word_id", "word", 2:5) %>%
  separate(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  mutate(sub_type = "CB")
  

sighted_data <- read_csv(S_SHAPE, 
                    col_names = c("animal", "sub_id", "w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9", "w10", "w11")) %>%
  fill(animal) %>%
  unite(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  gather("word_id", "word", 2:12) %>%
  separate(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  mutate(sub_type = "SIGHTED")


desc_data <- bind_rows(cb_data, sighted_data) %>%
  mutate(word = str_replace_all(word, "'", "")) %>%
  filter(word != "") %>%
  filter(!is.na(word))

write_csv(desc_data, DESCRIPTIONS_OUTFILE)
