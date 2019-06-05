library(tidyverse)
library(data.table)
library(here)

SHAPE_DESCRIPTIONS <- here('data/processed/animal_shape_descriptions.csv')
NAME_DESCRIPTIONS <- here('data/processed/animal_name_words.csv')
COLOR_DESCRIPTIONS <- here('data/processed/animal_color_descriptions.csv')
TEXTURE_DESCRIPTIONS <- here('data/processed/animal_texture_descriptions.csv')
OUTFILE <- here("data/processed/words_to_cache.csv")

all_words <- c(SHAPE_DESCRIPTIONS,
               NAME_DESCRIPTIONS, 
               COLOR_DESCRIPTIONS, 
               TEXTURE_DESCRIPTIONS) %>%
  map_df(read_csv)

write_csv(all_words, OUTFILE)
