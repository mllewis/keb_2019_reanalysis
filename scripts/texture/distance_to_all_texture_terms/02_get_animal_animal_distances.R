library(tidyverse)
library(data.table)
library(here)

LANG_DISTANCE_OUTPATH <- here("data/processed/animal_texture_distances_language_wiki.csv")
TEXTURE_SIM_PATH <- here("data/processed/animal_texture_vectors_wiki.csv")
texture_sim <- read_csv(TEXTURE_SIM_PATH)

unique_pairs <- cross_df(list(animal1 = texture_sim$animal, animal2  = texture_sim$animal))

####### get animal-animal distances based on color ####### 
###  SIMPLE DISTANCE
word_word_dists <- philentropy::distance(as.matrix(texture_sim[,-1]), 
                                         method = "euclidean") %>%
  as.data.frame()  %>%
  mutate(word1 = texture_sim$animal)

colnames(word_word_dists) = c(texture_sim$animal, "animal1") 

long_texture_texture_dists_sd <- gather(word_word_dists, 
                                    "animal2", 
                                    "language_similarity_simple_dist_texture",
                                    -animal1) %>%
  arrange(-language_similarity_simple_dist_texture)  %>%
  right_join(unique_pairs)

write_csv(long_texture_texture_dists_sd, LANG_DISTANCE_OUTPATH)
