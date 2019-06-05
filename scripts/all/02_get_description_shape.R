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
  mutate(word = str_replace_all(word, "'", ""),
         word = str_replace_all(word, " ", "")) %>%
  filter(word != "") %>%
  filter(!is.na(word)) 

bad_words <- c("cats",  "idk", "farm", "fish", "bears", "mouse", "zoo", "mammal",
               "mammals",  "creatures", "bird", "aquatic", "cansitup", "twol", "fee",  "likehuman", "nolegs",
               "humanlike", "no", "horselike",  "rodentlike",  "fishlike", "IDK",
               "catlike", "birdlike", "two", "birds", "squirrellike", "primates", "aesthpleasing", "fourlegs",
               "equine", "wild", "four", "ocean", "water", "bearlike", "fourlegs", "flying", 
               "cape", "relative", "fast", "swimmers", "longerthantall", "shortlegs", "lowtoground",
               "lowfront", "builtforspeed","footballlike", "grasping", "sweet", "standon", "walkon", "situp", "onground")
#body par5 words
#"hair", "fins", "neck", "ears","paws", "tails", "limbs", "hooves","nose","hand","bodies", 
  #              "feet", "body", "arms", "wings","legs",  "hands",  "fur", "feathers", "scales", "horns")



shape_words <- data.frame(word = c(setdiff(unique(desc_data$word), bad_words)),
                          type = "shape")

write_csv(shape_words, DESCRIPTIONS_OUTFILE)
