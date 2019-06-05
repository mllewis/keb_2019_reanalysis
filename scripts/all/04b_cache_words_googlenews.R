library(tidyverse)
library(data.table)
library(here)
library(reticulate)

use_python("/usr/local/bin/python")
gensim <- import("gensim")

OUTPATH <- here("data/processed/cached_vectors_google.csv")
TARGET_WORDS <- here("data/processed/words_to_cache.csv")
MODEL_PATH <- "/Users/mollylewis/Desktop/GoogleNews-vectors-negative300.bin" #"/Volumes/wilbur_the_great/fasttext_models/wiki.en.vec" 

target_words <- read_csv(TARGET_WORDS) %>%
  distinct(word) %>%
  pull(word)

target_words_clean <- c(setdiff(target_words, c("killerwhale", "polarbear")), 
                        "killer_whale", "polar_bear")



# Load pretrained model (since intermediate data is not included, the model cannot be refined with additional data)
model <- gensim$models$KeyedVectors$load_word2vec_format(MODEL_PATH, binary=TRUE, limit = 3000000L)#, limit = 5000000L) #, limit = 1000000L)) #, limit = 1000000L)

get_google_vectors <- function(this_word, this_model){
  print(this_word)
  vecs <- try(t(this_model$wv$get_vector(this_word)))
  if (class(vecs) != "try-error"){
    targ_vecs <- vecs
  } else {
    targ_vecs <- t(rep(NA, 300))
  }
  data.frame(target_word = this_word,
             targ_vecs)
}

target_vecs_google <- map_df(target_words_clean, get_google_vectors, model) 

target_vecs_clean <- target_vecs_google %>%
  mutate(target_word = str_replace_all(target_word, "_", "")) %>% # get rid of _ in polar bear, killer whale
  filter(!is.na(X1))
# missing: "unmuscular" "grey"    "semibright", "ochre"   


write_csv(target_vecs_clean, OUTPATH)
