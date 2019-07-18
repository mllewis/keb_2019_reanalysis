# get animal-texture similarity with range of different num of anchor words
library(tidyverse)
library(here)
library(data.table)

ANCHOR_WORDS <- here("data/processed/animal_texture_anchor_words.csv")
WIKIPATH <- #"/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
OUTPATH <- #here("data/processed/animal_texture_langauge_distances_with_anchors.csv")

TIDY_HUMAN_TEXTURE <- here("data/processed/tidy_human_texture_response.csv")
human_texture_data <- read_csv(TIDY_HUMAN_TEXTURE)
animals <- unique(human_texture_data$animal)

# make anchor sets
TEXURE_WORDS <- c("fur", "scales", "skin", "feathers")
anchor_words <- read_csv(ANCHOR_WORDS) %>%
  select(anchor, target_word, n)

make_anchor_word_sets <- function(anchor_id, anchor_df){
  exact_anchor <- anchor_df %>%
    distinct(anchor) %>%
    mutate(target_word = anchor,
           n = 0)
  
  anchor_df %>%
    filter(n <= anchor_id) %>%
    bind_rows(exact_anchor) %>%
    mutate(set_id = anchor_id) %>%
    arrange(anchor,n) %>%
    select(set_id, everything())
}

anchor_word_sets <- map_df(list(0,10,20,30,40,50), 
                           make_anchor_word_sets, 
                           anchor_words) 

# get language vectors
wmodel <- fread(
  WIKIPATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

filtered_wmodel <- wmodel %>% # just to make iteration faster
  filter(target_word %in% c(anchor_word_sets$target_word, animals)) 

# get language distances for each anchor set x animal
get_anchor_distance <- function(anchor_set, embeddings, targ_animals){
  target_vecs <- embeddings %>%
    filter(target_word %in% anchor_set$target_word) %>%
    select(target_word, everything())
  
  animals_vecs <- wmodel %>%
    filter(target_word %in% targ_animals) %>%
    select(target_word, everything())

  word_word_dists <- sim2(as.matrix(target_vecs[,-1]),
                          as.matrix(animals_vecs[,-1]), 
                          method = "cosine") %>%
  as.data.frame()  %>%
  mutate(word1 = target_vecs$target_word) %>%
  select(word1, everything())

  colnames(word_word_dists) = c("word1", animals_vecs$target_word) 

  long_word_word_dists <- gather(word_word_dists, "word2",
                                 "language_similarity", -word1) 

  sims_df <- anchor_set %>% # <- debug this.
            left_join(long_word_word_dists, 
                      by = c("target_word" = "word1")) %>%
    rename(animal = word2)
  
  sims_df
}

animals_texure_distances <- anchor_word_sets %>%
                              group_by(set_id) %>%
                              nest() %>%
                              mutate(temp = map(data, get_anchor_distance, 
                                          filtered_wmodel, 
                                          animals)) %>%
  select(-data) %>%
  unnest()

mean_language_similarity <- animals_texure_distances %>%
  group_by(set_id, animal, anchor) %>%
  summarize(language_similarity = mean(language_similarity))

write_csv(mean_language_similarity, OUTPATH)

