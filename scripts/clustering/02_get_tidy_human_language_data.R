library(tidyverse)
library(here)

LANG_ANIMAL_DISTANCE_COLOR <- here("data/processed/animal_color_distances_language_wiki.csv")
LANG_ANIMAL_DISTANCE_SHAPE<- here("data/processed/animal_shape_distances_language_wiki.csv")
LANG_ANIMAL_DISTANCE_TEXTURE <- here("data/processed/animal_texture_distances_language_wiki.csv")
TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv")
OUTFILE <- here("data/processed/tidy_human_wiki_language_data.csv")

language_data <- read_csv(LANG_ANIMAL_DISTANCE_COLOR) %>%
  left_join(read_csv(LANG_ANIMAL_DISTANCE_SHAPE), by  = c("animal1", "animal2")) %>%
  left_join(read_csv(LANG_ANIMAL_DISTANCE_TEXTURE),by  = c("animal1", "animal2")) %>%
  select(-contains("PCA")) %>%
  gather("knowledge_type", "similarity_value", -animal1, -animal2) %>%
  rowwise() %>%
  mutate(knowledge_source = "language",
         knowledge_type = str_split(knowledge_type, "dist_")[[1]][2])

human_data <- read_csv(TIDY_HUMAN_PATH) %>%
  rowwise()%>%
  mutate(knowledge_type = str_split(similarity_type, "similarity_")[[1]][2]) %>%
  rename(knowledge_source = participant_type,
         similarity_value = human_similarity) %>%
  select(-similarity_type) %>%
  mutate(knowledge_type = case_when(knowledge_type == "skin" ~ "texture", 
                                    TRUE ~ knowledge_type))

all_data <- bind_rows(language_data, human_data) %>%
  select(knowledge_source, knowledge_type, animal1, animal2, similarity_value) %>%
  mutate(similarity_value = case_when(knowledge_source == "human"~ 1- similarity_value, 
                                      TRUE ~ similarity_value)) %>%
  filter(animal2 > animal1)

write_csv(all_data, OUTFILE)