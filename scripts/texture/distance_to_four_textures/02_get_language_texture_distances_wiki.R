# get pairwise distances for four target words
library(tidyverse)
library(here)
library(data.table)

TIDY_HUMAN_TEXTURE <- here("data/processed/tidy_human_texture_response.csv")
OUTPATH <- here("data/processed/animal_texture_langauge_distances_wiki.csv")
WIKIPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"

human_texture_data <- read_csv(TIDY_HUMAN_TEXTURE)

TEXURE_WORDS <- c("fur", "scales", "skin", "feathers")
target_words <- c(human_texture_data$animal, TEXURE_WORDS)


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
  filter(target_word %in% target_words) %>%
  select(target_word, everything())


word_word_dists <- philentropy::distance(as.matrix(target_vecs[,-1]), 
                                         method = "cosine") %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 

long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1) %>%
  filter(!(word1 %in% TEXURE_WORDS),
         word2 %in% TEXURE_WORDS)  

write_csv(long_word_word_dists, OUTPATH)

