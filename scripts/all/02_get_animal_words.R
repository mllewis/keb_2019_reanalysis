# get animal words (+ words for texture task)
library(tidyverse)
library(here)

DESCRIPTIONS_OUTFILE <- here('data/processed/animal_name_words.csv')

TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv")
human_data <- read_csv(TIDY_HUMAN_PATH)

animals <- unique(c(human_data$animal1, human_data$animal2))
TEXTURE_ANIMALS <- c( "goldfish", "snake", "crocodile", "lizard", "eel", "shark", "turtle", "toad",
                      "dolphin", "frog", "worm", "elephant", "hippo", "rhino", "pig", "platypus", "seal",
                      "ferret", "gorilla", "sheep", "fox", "bear", "horse", "donkey", "cat", "penguin", "ostrich",
                      "flamingo", "peacock", "pigeon")
TEXURE_WORDS1 <- c("fur", "scales", "skin", "feathers", "animals", "animal", "feather", "scale")

animal_words <- bind_rows(data.frame(word = animals,
                                     type = "target_animal"),
                          data.frame(word = TEXTURE_ANIMALS,
                                     type = "texture_animal"),
                          data.frame(word = TEXURE_WORDS1,
                                     type = "texture_word"))

write_csv(animal_words, DESCRIPTIONS_OUTFILE)
