library(tidyverse)
library(data.table)
library(here)

DESC_COLORS <-  here("data/processed/words_to_cache.csv")
LANG_VECTORS <-  here("data/processed/cached_vectors_wiki_gary.csv")
OUTPATH <- here("data/processed/animal_color_vectors_wiki_gary.csv")

all_targ_words <- read_csv(DESC_COLORS) 
colors <- filter(all_targ_words, type == "color") %>%
  pull(word)

animals <- filter(all_targ_words, type == "target_animal") %>%
  pull(word)

####### get vectors positions for target words (animals and colors) ####### 
target_vecs <- read_csv(LANG_VECTORS) %>%
  filter(target_word %in% c(colors, animals))

#######  get color vectors (distances) for each animal ####### 
word_word_dists <- philentropy::distance(as.matrix(target_vecs[,-1]), 
                                         method = "cosine") %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 

long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1) %>%
  filter(!(word1 %in% colors),
           word2 %in% colors)  

get_language_similarity <- function(sim_df){
    sim_df %>%
      group_by(word1) %>%
      spread(word2, language_similarity)
}

color_sim <-  get_language_similarity(long_word_word_dists) %>%
  ungroup() %>%
  rename(animal = word1)

write_csv(color_sim, OUTPATH)
