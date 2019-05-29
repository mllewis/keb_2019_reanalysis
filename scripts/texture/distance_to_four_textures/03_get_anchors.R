# get most frequently described texture
library(tidyverse)
library(here)
library(data.table)
library(text2vec)

OUTPATH <- here("data/processed/animal_texture_anchor_words.csv")
WIKIPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
FREQ <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/1_mtld_measure/data/control_variables/SUBTLEXus_corpus.txt"

TEXURE_WORDS <- c("fur", "scales", "skin", "feathers", "animals", "animal", "feather", "scale")

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

animal_words <- unique(c(animal_nn, animals_nn))

target_anchor_words <- long_word_word_dists %>%
  filter(!(word2 %in% c("animal", "animals"))) %>%
  mutate(word2 = case_when(word2 %in% c("feathers", "feather") ~ "feathers",
                           word2 %in% c("scales", "scale") ~ "scales", 
                           TRUE~ word2)) %>%
  group_by(word1, word2) %>%
  summarize_if(is.numeric, mean) %>% # deal with cases where there's two for, e.g., feather/feathers
  group_by(word2) %>%
  arrange(-language_similarity) %>%
  slice(1:10000) %>%
  filter(word1 %in% animal_words) %>%
  left_join(freq, by = c("word1" = "word")) %>%
  filter(lg10wf >= 1.5) %>%
  slice(1:50) %>%
  mutate(n = 1:n()) %>%
  data.frame() %>%
  rename(target_word = word1,
         anchor = word2)

write_csv(target_anchor_words, OUTPATH)

