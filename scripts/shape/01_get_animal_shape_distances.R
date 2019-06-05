library(tidyverse)
library(data.table)
library(here)

ALL_WORDS <-  here("data/processed/words_to_cache.csv")
LANG_VECTORS <-  here("data/processed/cached_vectors_wiki.csv")
OUTPATH <- here("data/processed/animal_shape_vectors_wiki.csv")

all_targ_words <- read_csv(ALL_WORDS) 
shapes <- filter(all_targ_words, type == "shape") %>%
  pull(word)

animals <- filter(all_targ_words, type == "target_animal") %>%
  pull(word)

target_vecs <- read_csv(LANG_VECTORS) %>%
  filter(target_word %in% c(shapes, animals))

#######  get color vectors (distances) for each animal ####### 
word_word_dists <- philentropy::distance(as.matrix(target_vecs[,-1]), 
                                         method = "cosine") %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 

long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1) %>%
  filter(!(word1 %in% shapes),
           word2 %in% shapes)  

get_language_similarity <- function(sim_df){
  sim_df %>%
    group_by(word1) %>%
    spread(word2, language_similarity)
}

shape_sim <-  get_language_similarity(long_word_word_dists) %>%
  ungroup() %>%
  rename(animal = word1)

write_csv(shape_sim, OUTPATH)
