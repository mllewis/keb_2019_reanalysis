library(tidyverse)
library(data.table)
library(here)

OUTPATH <- here("data/processed/cached_vectors_wiki.csv")
TARGET_WORDS <- here("data/processed/words_to_cache.csv")
MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec" 

target_words <- read_csv(TARGET_WORDS) %>%
  distinct(word) %>%
  pull(word)

wmodel <- fread(
  MODEL_PATH,
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
# missing:  "unmuscular" "semibright"
write_csv(target_vecs, OUTPATH)
