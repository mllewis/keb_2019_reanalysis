# get tidy human data

library(R.matlab)
library(tidyverse)
library(data.table)
library(here)

KEY_PATH <- here("data/raw/animal_keys.mat")
CB_SORTING_PATH <- here("data/raw/animals_sorting_CB_allData.mat")
S_SORTING_PATH <- here("data/raw/animals_sorting_S_allData.mat")
TIDY_HUMAN_PATH <- here("data/procesed/tidy_human_data.csv")

animal_labels <- readMat(KEY_PATH) %>%
  flatten() %>%
  unlist() %>%
  as.character()  %>%
  .[30:59] # 1-29 are objects (practice round)

cb_data <- readMat(CB_SORTING_PATH)
s_data <- readMat(S_SORTING_PATH)
TASKS <- c("objects", 'habitat', 'food', 'shape', 'skin', 'color')

get_tidy_data_for_domain <- function(target_domain, this_cb, this_s, labels, all_tasks){
  
    if (target_domain != "objects"){
        print(target_domain)
        domain_id <- which(all_tasks == target_domain)

        unique_pairs <- combn(labels, 2) %>%
          t() %>%
          as.data.frame() %>%
          set_names(c("animal1", "animal2"))  %>%
          mutate_all(as.character) %>%
          filter(animal1 != animal2) 
        
         CB_data <- flatten(this_cb[[1]][domain_id]) %>%
          as.data.frame() %>%
          set_names(labels) %>%
          mutate(animal1 = labels)  %>%
          gather("animal2", "human_similarity", -animal1) %>%
          mutate(participant_type = "blind") %>%
          right_join(unique_pairs)
        
        S_data <- flatten(this_s[[1]][domain_id]) %>%
          as.data.frame() %>%
          set_names(labels) %>%
          mutate(animal1 = labels)  %>%
          gather("animal2", "human_similarity", -animal1) %>%
          mutate(participant_type = "sighted") %>%
          right_join(unique_pairs)
          
        bind_rows(CB_data, S_data) %>%
          mutate(similarity_type = paste0("human_similarity_", target_domain))
    }
}

all_df <- map_df(TASKS, get_tidy_data_for_domain, cb_data, s_data, animal_labels, TASKS) %>%
  select(participant_type, similarity_type, everything())

write_csv(all_df, TIDY_HUMAN_PATH)
