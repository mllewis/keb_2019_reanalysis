library(tidyverse)
library(data.table)
library(here)

OUTPATH <- here("data/processed/animal_texture_raw_vectors_wiki.csv")
TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv")
DESC_COLORS <-  here('data/processed/animal_texture_descriptions.csv')
WIKIPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"


human_data <- read_csv(TIDY_HUMAN_PATH)
unique_animals <- unique(c(human_data$animal1,human_data$animal2))
 
bad_words <- c("hi", "unknown", "thick fur", "fine fur", "no", "some", "slightly")
desc_colors <- read_csv(DESC_COLORS) %>%
  mutate(word = tolower(word)) %>%
  distinct(word) %>%
  filter(!(word %in% bad_words), 
         !is.na(word)) %>%
  pull(word) 

####### get vectors positions for target words (animals and textures) ####### 
wmodel <- fread(
  WIKIPATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

target_words <- c(unique_animals, desc_colors)

target_vecs <- wmodel %>%
  filter(target_word %in% target_words) %>%
  select(target_word, everything())

write_csv(target_vecs, OUTPATH)
