# get raw animal distances for comparision to taxonomic differences
library(tidyverse)
library(data.table)
library(here)

VEC_PATH <- here('data/processed/cached_vectors_coca_all_cleaned_gary.csv')
OUTPATH <- here("data/processed/animal_distances_coca_all_cleaned_gary.csv")

TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv")

human_data <- read_csv(TIDY_HUMAN_PATH)
unique_animals <- unique(c(human_data$animal1, human_data$animal2))

all_vecs <- read_csv(VEC_PATH)
target_vecs <- all_vecs %>%
  filter(target_word %in% unique_animals)

word_word_dists <- coop::cosine(t(as.matrix(target_vecs[,-1]))) %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 
long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1)

write_csv(long_word_word_dists, OUTPATH)
