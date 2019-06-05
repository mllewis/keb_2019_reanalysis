# get taxonomic dsitances
library(tidyverse)
library(data.table)
library(here)

OUTPATH <- here("data/processed/animal_distances_taxonomic.csv")
TAXONOMIC_PATH <- here("data/raw/taxonomy_matrix.mat")
taxonomic_data <- readMat(TAXONOMIC_PATH)[[2]]  

LABELS <- c("shark", "swan", "flamingo", "pigeon", "crow", "elephant", 
            "mammoth", "sloth", "beaver", "gorilla", "bat", "rhino", 
            "zebra", "llama", "hippo", "killerwhale", "dolphin", "giraffe",
            "sheep", "goat", "deer", "pig", "boar", "lion", "panther", "cheetah",
            "skunk", "panda", "polarbear", "grizzly") %>% rev() # from SI fig s2

colnames(taxonomic_data) <- LABELS
rownames(taxonomic_data) <- LABELS


taxonomic_long <- taxonomic_data %>%
  as.data.frame() %>%
  rownames_to_column("animal1") %>%
  gather("animal2", "similarity", -animal1) %>%
  mutate(sim_type = "taxonomy")

write_csv(taxonomic_long, OUTPATH)
