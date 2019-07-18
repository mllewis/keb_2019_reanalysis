library(tidyverse)
library(data.table)
library(here)

LANG_DISTANCE_OUTPATH <- here("data/processed/animal_shape_distances_language_wiki_gary.csv")
SHAPE_SIM_PATH <- here("data/processed/animal_shape_vectors_wiki_gary.csv")
shape_sim <- read_csv(SHAPE_SIM_PATH)

unique_pairs <- cross_df(list(animal1 = shape_sim$animal, animal2  = shape_sim$animal))

####### get animal-animal distances based on color ####### 
###  SIMPLE DISTANCE
word_word_dists <- philentropy::distance(as.matrix(shape_sim[,-1]), 
                                         method = "euclidean") %>%
  as.data.frame()  %>%
  mutate(word1 = shape_sim$animal)

colnames(word_word_dists) = c(shape_sim$animal, "animal1") 

long_shape_shape_dists_sd <- gather(word_word_dists, 
                                    "animal2", 
                                    "language_similarity_simple_dist_shape",
                                    -animal1) %>%
  arrange(-language_similarity_simple_dist_shape)  %>%
  right_join(unique_pairs)


write_csv(long_shape_shape_dists_sd, LANG_DISTANCE_OUTPATH)
