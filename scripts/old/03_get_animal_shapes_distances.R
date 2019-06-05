library(tidyverse)
library(data.table)
library(here)

LANG_VECTORS <- here("data/processed/animal_shape_raw_vectors_wiki.csv")
TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv")
DESC_COLORS <-  here('data/processed/animal_shape_descriptions.csv')
OUTPATH <- here("data/processed/animal_shape_vectors_wiki.csv")
NORM_TYPE <- "raw"

human_data <- read_csv(TIDY_HUMAN_PATH)
unique_animals <- unique(c(human_data$animal1,human_data$animal2))

colors <- read_csv(DESC_COLORS) %>%
  #filter(sub_type == "CB") %>%
  #filter(word_id == "w1") %>%
  #filter(!is.na(word)) %>%
  count(word) %>%
  arrange(-n) %>%
  pull(word)

####### get vectors positions for target words (animals and colors) ####### 
target_vecs <- read_csv(LANG_VECTORS) %>%
  filter(target_word %in% c(colors, unique_animals))

#######  get color vectors (distances) for each animal ####### 
word_word_dists <- philentropy::distance(as.matrix(target_vecs[,-1]), 
                                         method = "cosine") %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 

long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1) %>%
  filter(!(word1 %in% colors),
           word2 %in% colors)  

get_color_similarity <- function(sim_df, type){
  if (type == "norm"){
    sim_df %>%
      group_by(word1) %>%
      mutate(language_similarity_norm = language_similarity/sum(language_similarity)) %>%
      select(-language_similarity) %>%
      spread(word2, language_similarity_norm)
  } else if (type == "raw"){
    sim_df %>%
      group_by(word1) %>%
      spread(word2, language_similarity)
  }
}

color_sim <-  get_color_similarity(long_word_word_dists, NORM_TYPE) %>%
  ungroup() %>%
  rename(animal = word1)

write_csv(color_sim, OUTPATH)
