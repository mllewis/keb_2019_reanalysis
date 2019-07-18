library(tidyverse)
library(data.table)
library(here)

LANG_DISTANCE_OUTPATH <- here("data/processed/animal_color_distances_language_wiki_gary.csv")
COLOR_SIM_PATH <- here("data/processed/animal_color_vectors_wiki_gary.csv")
color_sim <- read_csv(COLOR_SIM_PATH)

unique_pairs <- cross_df(list(animal1 = color_sim$animal, animal2  = color_sim$animal))

####### get animal-animal distances based on color ####### 
###  SIMPLE DISTANCE
word_word_dists <- philentropy::distance(as.matrix(color_sim[,-1]), 
                                         method = "euclidean") %>%
  as.data.frame()  %>%
  mutate(word1 = color_sim$animal)

colnames(word_word_dists) = c(color_sim$animal, "animal1") 

long_color_color_dists_sd <- gather(word_word_dists, 
                                 "animal2", 
                                 "language_similarity_simple_dist_color",
                                 -animal1) %>%
  arrange(-language_similarity_simple_dist_color)  %>%
  right_join(unique_pairs)


write_csv(long_color_color_dists_sd, LANG_DISTANCE_OUTPATH)
