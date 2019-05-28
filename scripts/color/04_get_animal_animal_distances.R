library(tidyverse)
library(data.table)
library(here)

LANG_DISTANCE_OUTPATH <- here("data/processed/animal_color_distances_language_wiki.csv")
COLOR_SIM_PATH <- here("data/processed/animal_color_vectors_wiki.csv")
color_sim <- read_csv(COLOR_SIM_PATH)
NPC <- 10

unique_pairs <- combn(color_sim$animal, 2) %>%
  t() %>%
  as.data.frame() %>%
  set_names(c("animal1", "animal2"))  %>%
  mutate_all(as.character) %>%
  filter(animal1 != animal2) 

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

### UNWEIGHTED PCA
df_for_pca <- color_sim %>% 
  select(-1) %>%
  as.matrix() %>%
  t()

pca_df <- prcomp(df_for_pca, center = T, scale = T) # summary(pca_df)

word_word_dists <- philentropy::distance(as.matrix(pca_df$rotation[,1:NPC]), 
                                         method = "euclidean") %>%
  as.data.frame()  %>%
  mutate(word1 = color_sim$animal)

colnames(word_word_dists) = c(color_sim$animal, "animal1") 

long_color_color_dists_pca <- gather(word_word_dists, 
                                 "animal2", 
                                 "language_similarity_PCA",
                                 -animal1) %>%
  arrange(-language_similarity_PCA)  %>%
  right_join(unique_pairs)

### WEIGHTED PCA
weights <- summary(pca_df)$importance %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname == "Proportion of Variance") %>%
  select(-rowname) %>%
  select(1:NPC) %>%
  unlist(use.names = F)

word_word_dists <- distances::distances(pca_df$rotation[,1:NPC],
                                          weights = sqrt(weights)) %>%
  as.matrix()  %>%
  as.data.frame() %>%
  mutate(word1 = color_sim$animal)


colnames(word_word_dists) = c(color_sim$animal, "animal1") 

long_color_color_dists_wpca <- gather(word_word_dists, 
                                 "animal2", 
                                 "language_similarity_weighted_PCA",
                                 -animal1) %>%
  arrange(-language_similarity_weighted_PCA)  %>%
  right_join(unique_pairs)

# bin distances together
all_lang_distances <- long_color_color_dists_sd %>%
  full_join(long_color_color_dists_pca) %>%
  full_join(long_color_color_dists_wpca) 

write_csv(all_lang_distances, LANG_DISTANCE_OUTPATH)
