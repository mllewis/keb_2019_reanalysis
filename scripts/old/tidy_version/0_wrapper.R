library(tidyverse)
library(data.table)
library(here)
load("script")

MODEL_PATH <- "/Volumes/My\ Passport/fil9.vec"
TARGET_WORDS <- here("data/processed/words_to_cache.csv")
CACHED_WORDS <- here("data/processed/cached_vectors_wiki_gary.csv")
ANIMAL_COLOR <- here("data/processed/animal_color_vectors_wiki_gary.csv")
ANIMAL_ANIMAL_COLOR  <- here("data/processed/animal_color_distances_language_wiki_gary.csv")

ANIMAL_TEXTURE <- here("data/processed/animal_color_vectors_wiki_gary.csv")
ANIMAL_ANIMAL_TEXTURE  <- here("data/processed/animal_color_distances_language_wiki_gary.csv")

cache_words(TARGET_WORDS, MODEL_PATH, CACHED_WORDS)
get_animal_color_distances(TARGET_WORDS, CACHED_WORDS, ANIMAL_COLOR)
get_animal_animal_distances(ANIMAL_ANIMAL_COLOR, ANIMAL_COLOR)
