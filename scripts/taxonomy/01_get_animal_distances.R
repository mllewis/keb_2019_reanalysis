# get raw animal distances for comparision to taxonomic differences
library(tidyverse)
library(data.table)
library(here)

WIKIPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
OUTPATH <- here("data/processed/animal_distances_wiki.csv")
TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv")

human_data <- read_csv(TIDY_HUMAN_PATH)
unique_animals <- unique(c(human_data$animal1, human_data$animal2))

wmodel <- fread(
  WIKIPATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

target_vecs <- wmodel %>%
  filter(target_word %in% unique_animals)

word_word_dists <- coop::cosine(t(as.matrix(target_vecs[,-1]))) %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 
long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1)

write_csv(long_word_word_dists, OUTPATH)
