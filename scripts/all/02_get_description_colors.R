# get most frequently described colors
library(tidyverse)
library(here)


CB_COLORS <- here("data/raw/animals_CB_descriptions_color.csv")
S_COLORS <- here("data/raw/animals_S_descriptions_color.csv")
COLOR_DESCRIPTIONS_OUTFILE <- here('data/processed/animal_color_descriptions.csv')


cb_data <- read_csv(CB_COLORS, 
         col_names = c("animal", "sub_id", "w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8")) %>%
  fill(animal) %>%
  unite(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  gather("word_id", "word", 2:9) %>%
  separate(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  mutate(sub_type = "CB")
  

sighted_data <- read_csv(S_COLORS, 
                    col_names = c("animal", "sub_id", "w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8")) %>%
  fill(animal) %>%
  unite(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  gather("word_id", "word", 2:9) %>%

  separate(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  mutate(sub_type = "SIGHTED")


desc_data <- bind_rows(cb_data, sighted_data) %>%
  mutate(word = str_replace_all(word, "'", "")) %>%
  filter(word != "") %>%
  filter(!is.na(word))

bad_words <- c("fly", "exotic", "IDK", "wild", "ambiguous", "water", "environment",
                "farm", "zoo", "bears", "unusual", "skin", "birds", "NA")

color_words <- data.frame(word = c(setdiff(unique(desc_data$word), bad_words)),
                          type = "color")

write_csv(color_words, COLOR_DESCRIPTIONS_OUTFILE)

