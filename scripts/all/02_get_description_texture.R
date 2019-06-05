# get most frequently described texture
library(tidyverse)
library(here)


CB_TEXTURE <- here("data/raw/animals_CB_descriptions_texture.csv")
S_TEXTURE <- here("data/raw/animals_S_descriptions_texture.csv")
DESCRIPTIONS_OUTFILE <- here('data/processed/animal_texture_descriptions.csv')

cb_data <- read_csv(CB_TEXTURE, 
         col_names = c("animal", "sub_id", "w1", "w2", "w3", "w4")) %>%
  fill(animal) %>%
  unite(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  gather("word_id", "word", 2:5) %>%
  separate(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  mutate(sub_type = "CB")
  
sighted_data <- read_csv(S_TEXTURE, 
                    col_names = c("animal", "sub_id", "w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9", "w10")) %>%
  fill(animal) %>%
  unite(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  gather("word_id", "word", 2:11) %>%
  separate(animal_id, c("animal", "sub_id"),sep =  "-") %>%
  mutate(sub_type = "SIGHTED")


desc_data <- bind_rows(cb_data, sighted_data) %>%
  mutate(word = str_replace_all(word, "'", ""),
         word = tolower(word)) %>%
  filter(word != "") %>%
  filter(!is.na(word)) 

bad_words <- c("hi", "unknown", "thick fur", "fine fur", "no", "some",
               "slightly", "na", "idk", "humanlike", "shoft", "nofur", "shoft", "softskin")

texture_words <- data.frame(word = c(setdiff(unique(desc_data$word), bad_words)),
                          type = "texture")

write_csv(texture_words, DESCRIPTIONS_OUTFILE)
