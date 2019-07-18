library(tidyverse)
library(data.table)
library(here)

ALL_WORDS <-  here("data/processed/words_to_cache.csv")
LANG_VECTORS <-  here("data/processed/cached_vectors_coca.csv")
OUTPATH <- here("data/processed/animal_texture_vectors_coca.csv")

all_targ_words <- read_csv(ALL_WORDS) 
textures <- filter(all_targ_words, type == "texture") %>%
  pull(word)

animals <- filter(all_targ_words, type == "target_animal") %>%
  pull(word)

target_vecs <- read_csv(LANG_VECTORS) %>%
  filter(target_word %in% c(textures, animals))

#######  get color vectors (distances) for each animal ####### 
word_word_dists <- philentropy::distance(as.matrix(target_vecs[,-1]), 
                                         method = "cosine") %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 

long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1) %>%
  filter(!(word1 %in% textures),
           word2 %in% textures)  

get_language_similarity <- function(sim_df){
    sim_df %>%
      group_by(word1) %>%
      spread(word2, language_similarity)
}

texture_sim <-  get_language_similarity(long_word_word_dists) %>%
  ungroup() %>%
  rename(animal = word1)

write_csv(texture_sim, OUTPATH)
