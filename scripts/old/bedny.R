library(R.matlab)
library(tidyverse)
library(data.table)

CB_SORTING_PATH <- here("data/animals_sorting_CB_allData.mat")
S_SORTING_PATH <- here("data/animals_sorting_S_allData.mat")
KEY_PATH <- here("data/animal_keys.mat")

CB <- readMat(CB_SORTING_PATH)
S <- readMat(S_SORTING_PATH)
key <- readMat(KEY_PATH)
WIKIPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"


wmodel <- fread(
  WIKIPATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

target_words <-  flatten(key) %>%
  unlist() %>%
  as.character() 
target_words <- target_words[-1:-29]

unique_pairs <- combn(target_words, 2) %>%
  t() %>%
  as.data.frame() %>%
  set_names(c("animal1", "animal2"))

TARGET_DOMAIN <- "color"
TASKS <- c( 'objects', 'habitat', 'food', 'shape', 'skin', 'color')
domain_id <- which(TASKS == TARGET_DOMAIN)

CB_color <- flatten(CB[[1]][domain_id]) %>%
  as.data.frame() %>%
  set_names(target_words) %>%
  mutate(animal1 = target_words)  %>%
  gather("animal2", "blind", -animal1)

S_color <- flatten(S[[1]][domain_id]) %>%
  as.data.frame() %>%
  set_names(target_words) %>%
  mutate(animal1 = target_words)  %>%
  gather("animal2", "sighted", -animal1)


animal_vecs <- wmodel %>%
  filter(target_word %in% target_words)

word_word_dists <- philentropy::distance(as.matrix(animal_vecs[,-1]), 
                                         method = "cosine") %>%
  as.data.frame()  %>%
  mutate(animal1 = target_words)

colnames(word_word_dists) = c(target_words, "animal1") 

long_word_word_dists <- gather(word_word_dists, "animal2", "language_similarity", -animal1)

all_sims <- full_join(S_color, CB_color) %>%
  full_join(long_word_word_dists)  %>%
  filter(animal1 != animal2) %>%
  right_join(unique_pairs)

all_sims_wide <- all_sims %>%
  gather("participant_type", "similarity", c(-1,-2,-5))

ggplot(all_sims_wide, aes(x = language_similarity, y = similarity, 
                          group = participant_type, color = participant_type)) +
  geom_point(alpha = .2)  +
  ylab("human rated similarity (Kim, Elli, & Bedny data)") +
  xlab("language similarity (word embeddings)") +
  ggtitle("Pairwise animal similarity based on shape similarity") +
  geom_smooth(method = "lm") +
  theme_classic()

cor.test(all_sims$language_similarity, all_sims$sighted)
cor.test(all_sims$language_similarity, all_sims$blind)


# skin = .16
# shape = .19
# food = .05
# habitat = .02
# http://rpubs.com/mll/483486
       