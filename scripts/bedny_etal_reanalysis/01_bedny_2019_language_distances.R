library(tidyverse)
library(here)
library(data.table)

OUTFILE <- here("data/processed/bedny_2019_lang_distances.csv")
MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec" 
BEDNY_DATA <- here("data/raw/datalong_CBSAMT.csv")
bedny_data <- read_csv(BEDNY_DATA)

human_words <-bedny_data %>%
  filter(C1 == "Light", C2 == "Light")  %>%
  distinct(V1, V2) 
targ_words <- unique(c(human_words$V1,human_words$V2))


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
  filter(target_word %in% targ_words) %>%
  select(target_word, everything())

# get distances
word_word_dists <- coop::cosine(t(as.matrix(target_vecs[,-1]))) %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word)

colnames(word_word_dists) = c( target_vecs$target_word, "word1") 

long_word_word_dists <- gather(word_word_dists, "word2", "language_similarity", -word1)  

write_csv(long_word_word_dists, OUTFILE)
