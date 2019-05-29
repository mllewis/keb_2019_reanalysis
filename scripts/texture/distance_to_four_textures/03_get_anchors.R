# get most frequently described texture
library(tidyverse)
library(here)
library(data.table)
library(text2vec)

OUTPATH <- here("data/processed/animal_texture_langauge_distances.csv")
WIKIPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
FREQ <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/1_mtld_measure/data/control_variables/SUBTLEXus_corpus.txt"

TEXURE_WORDS <- c("fur", "scales", "skin", "feathers", "animals", "animal")

wmodel <- fread(
  WIKIPATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))


freq <- read_tsv(FREQ) %>%
  janitor::clean_names() %>%
  select(word, lg10wf)

target_vecs <- wmodel %>%
  filter(target_word %in% freq$word) %>%
  filter(!(target_word %in% TEXURE_WORDS)) %>%
  select(target_word, everything())

texture_vecs <- wmodel %>%
  filter(target_word %in% TEXURE_WORDS) %>%
  select(target_word, everything())

word_word_dists <- sim2(as.matrix(target_vecs[,-1]),
                               as.matrix(texture_vecs[,-1]), method = "cosine") %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word) %>%
  select(word1, everything())

colnames(word_word_dists) = c("word1", texture_vecs$target_word) 

long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1) 

animal_nn <- word_word_dists %>%
  arrange(-animal) %>%
  slice(1:1000) %>%
  pull(word1)

animals_nn <- word_word_dists %>%
  arrange(-animals) %>%
  slice(1:1000) %>%
  pull(word1)

unique(c(animal_nn, )

long_word_word_dists %>%
  group_by(word2) %>%
  filter(word2 != "animal") %>%
  arrange(-language_similarity) %>%
  slice(1:500) %>%
  filter(word1 %in% animal_nn) %>%
  data.frame()

