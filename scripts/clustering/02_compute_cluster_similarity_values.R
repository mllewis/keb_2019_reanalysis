library(tidyverse)
library(here)
library(dendextend)
library(ggdendro)
library(clues)
library(gtools)

OUTFILE <- here("data/processed/cluster_similarity_values.csv")

####function for computing hierarchical clusters
get_hclust <- function(current_sims){
  wide_sims <- current_sims %>%
    select(animal1, animal2, similarity_value) %>%
    spread(animal1, similarity_value) %>%
    select(-animal2)
  
  wide_sims_mat <- as.matrix(wide_sims)
  rownames(wide_sims_mat) <- colnames(wide_sims)
  
  #replace NA's with 0's along the diagonal
  wide_sims_mat[is.na(wide_sims_mat)] <- 0
  
  wide_sims_mat %>%
    dist() %>%
    hclust()
}

##load data
TIDY_HUMAN_WIKI_DATA <- here("data/processed/tidy_human_wiki_language_data.csv")

all_data <- read_csv(TIDY_HUMAN_WIKI_DATA)

#compute all clusters
all_hclusts <- all_data %>%
  filter(!(knowledge_type %in% c("habitat", "food"))) %>%
  group_by(knowledge_source, knowledge_type) %>%
  nest() %>%
  mutate(hclusts = map(data, get_hclust)) %>%
  select(-data)

#set seed for random untangle() procedure
set.seed(100)

#data and knowledge sources to iterate over
knowledge_sources=c("sighted","blind","language")
knowledge_types=c("color","shape","texture")

#create data frame containing entanglement, FM values, and other indices of cluster similarity
cluster_similarity_values <- data.frame()
for (kt in knowledge_types) {
  for (i in 1:length(permutations(3,2,knowledge_sources)[,1])) {
    temp_1=list()
    temp_2=list()
    print(i)
    #cluster
    knowledge_source_1 <- permutations(3,2,knowledge_sources)[i,1]
    knowledge_source_2 <- permutations(3,2,knowledge_sources)[i,2]
    temp_1 <- all_hclusts %>%
      filter(knowledge_type == kt,
             knowledge_source == knowledge_source_1) %>%
      pull(hclusts)
    hc_1 <- temp_1[[1]]
    temp_2 <- all_hclusts %>%
      filter(knowledge_type == kt,
             knowledge_source == knowledge_source_2) %>%
      pull(hclusts)
    hc_2 <- temp_2[[1]]
    #entanglement
    dends <- dendlist(as.dendrogram(hc_1),as.dendrogram(hc_2))
    x_step2side <- dends %>%
      untangle(method = "step2side") 
    x_random <- dends %>%
      untangle(method = "random", R = 1000) 
    
    #FM values for different cut levels
    FM_5 <- FM_index(cutree(hc_1, k=5), cutree(hc_2, k=5))[1]
    FM_10 <- FM_index(cutree(hc_1, k=10), cutree(hc_2, k=10))[1]
    FM_15 <- FM_index(cutree(hc_1, k=15), cutree(hc_2, k=15))[1]
    FM_20 <- FM_index(cutree(hc_1, k=20), cutree(hc_2, k=20)) [1]
    E_FM_5 <- attr(FM_index(cutree(hc_1, k=5), cutree(hc_2, k=5)),"E_FM")
    E_FM_10 <- attr(FM_index(cutree(hc_1, k=10), cutree(hc_2, k=10)),"E_FM")
    E_FM_15 <- attr(FM_index(cutree(hc_1, k=15), cutree(hc_2, k=15)),"E_FM")
    E_FM_20 <- attr(FM_index(cutree(hc_1, k=20), cutree(hc_2, k=20)),"E_FM")
    V_FM_5 <- attr(FM_index(cutree(hc_1, k=5), cutree(hc_2, k=5)),"V_FM")
    V_FM_10 <- attr(FM_index(cutree(hc_1, k=10), cutree(hc_2, k=10)),"V_FM")
    V_FM_15 <- attr(FM_index(cutree(hc_1, k=15), cutree(hc_2, k=15)),"V_FM")
    V_FM_20 <- attr(FM_index(cutree(hc_1, k=20), cutree(hc_2, k=20)),"V_FM")
    Z_FM_5 <- (FM_5 - E_FM_5)/sqrt(V_FM_5)
    Z_FM_10 <- (FM_10 - E_FM_10)/sqrt(V_FM_10)
    Z_FM_15 <- (FM_15 - E_FM_15)/sqrt(V_FM_15)
    Z_FM_20 <- (FM_20 - E_FM_20)/sqrt(V_FM_20)
    
    #other similarity indices
    rand_5 <- adjustedRand(cutree(hc_1, k=5), cutree(hc_2, k=5))["Rand"]
    adjustedRand_5 <- adjustedRand(cutree(hc_1, k=5), cutree(hc_2, k=5))["HA"]
    jaccard_5 <- adjustedRand(cutree(hc_1, k=5), cutree(hc_2, k=5))["Jaccard"]
    rand_10 <- adjustedRand(cutree(hc_1, k=10), cutree(hc_2, k=10))["Rand"]
    adjustedRand_10 <- adjustedRand(cutree(hc_1, k=10), cutree(hc_2, k=10))["HA"]
    jaccard_10 <- adjustedRand(cutree(hc_1, k=10), cutree(hc_2, k=10))["Jaccard"]
    rand_15 <- adjustedRand(cutree(hc_1, k=15), cutree(hc_2, k=15))["Rand"]
    adjustedRand_15 <- adjustedRand(cutree(hc_1, k=15), cutree(hc_2, k=15))["HA"]
    jaccard_15 <- adjustedRand(cutree(hc_1, k=15), cutree(hc_2, k=15))["Jaccard"]
    rand_20 <- adjustedRand(cutree(hc_1, k=20), cutree(hc_2, k=20))["Rand"]
    adjustedRand_20 <- adjustedRand(cutree(hc_1, k=20), cutree(hc_2, k=20))["HA"]
    jaccard_20 <- adjustedRand(cutree(hc_1, k=20), cutree(hc_2, k=20))["Jaccard"]
    
    
    temp <- data.frame(
      knowledge_source_1=knowledge_source_1,
      knowledge_source_2=knowledge_source_2,
      knowledge_type=kt,
      entangle_original=round(entanglement(dends),3),
      entangle_step2side=round(entanglement(x_step2side),3),
      entangle_random=round(entanglement(x_random),3),
      FM_5 = FM_5, FM_10 = FM_10, FM_15=FM_15, FM_20=FM_20,
      E_FM_5 = E_FM_5, E_FM_10 = E_FM_10, E_FM_15=E_FM_15, E_FM_20=E_FM_20,
      V_FM_5 = V_FM_5, V_FM_10 = V_FM_10, V_FM_15=V_FM_15, V_FM_20=V_FM_20,
      Z_FM_5 = Z_FM_5, Z_FM_10 = Z_FM_10, Z_FM_15=Z_FM_15, Z_FM_20=Z_FM_20,
      rand_5 = rand_5, rand_10 = rand_10, rand_15=rand_15, rand_20=rand_20,
      adjustedRand_5 = adjustedRand_5, adjustedRand_10 = adjustedRand_10, adjustedRand_15 = adjustedRand_15,adjustedRand_20=adjustedRand_20,
      jaccard_5 = jaccard_5, jaccard_10 = jaccard_10, jaccard_15=jaccard_15, jaccard_20=jaccard_20,
      row.names=NULL)
    cluster_similarity_values <- rbind(temp, cluster_similarity_values)
  }
  
}

#write data frame to file 
write_csv(cluster_similarity_values, OUTFILE)