library(tidyverse)
library(here)
library(Matrix)
library(dendextend)
library(ggdendro)
library(clues)
library(gtools)

#### useful functions ####
convert_similarity_to_matrix <- function(wide_data) {
  #work into symmetric matrix (messy)
  #convert to matrix
  m <- as.matrix(wide_data)
  #add extra row
  m <- rbind(m,c(rep(NA,ncol(m)-1),1.0))
  row.names(m)[nrow(m)] <- colnames(m)[ncol(m)]
  #add extra column
  m <- cbind(c(1.0,rep(NA,nrow(m)-1)),m)
  colnames(m)[1] <- row.names(m)[1]
  #convert into symmetric matrix (using forceSymmetric in Matrix package)
  diag(m) <- 1
  m <- forceSymmetric(m)
  #return
  m
}

convert_similarity_to_distance <- function(wide_data, col_name,reverse_dist=T, human_data=T) {
  #convert human similarity values based on a similarity column name
  #extract subset of wide human data
  temp <- as.data.frame(wide_data) %>%
    select(animal1,animal2, !!col_name) %>%
    spread(animal2,!!col_name,fill="", convert=T)
  #change animal1 column to row name
  row.names(temp) <- temp$animal1
  temp <- temp %>%
    select(-animal1)
  if (human_data) {
    #convert to symmetric matrix
    temp <- convert_similarity_to_matrix(temp)
  } else {
    #convert to matrix
    temp <- as.matrix(temp)
  }
  
  if (reverse_dist) {
    #convert from similarity to "distance"
    temp <- 1- temp
  }
  #return
  temp
}

##load data
LANG_ANIMAL_DISTANCE_COLOR <- here("data/processed/animal_color_distances_language_wiki.csv")
LANG_ANIMAL_DISTANCE_COLOR
LANG_ANIMAL_DISTANCE_SHAPE<- here("data/processed/animal_shape_distances_language_wiki.csv")
LANG_ANIMAL_DISTANCE_TEXTURE <- here("data/processed/animal_texture_distances_language_wiki.csv")
TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv")

language_data <- read_csv(LANG_ANIMAL_DISTANCE_COLOR) %>%
  left_join(read_csv(LANG_ANIMAL_DISTANCE_SHAPE), by  = c("animal1", "animal2")) %>%
  left_join(read_csv(LANG_ANIMAL_DISTANCE_TEXTURE),by  = c("animal1", "animal2")) %>%
  select(-contains("PCA"))
human_data <- read_csv(TIDY_HUMAN_PATH)
human_data_wide <- human_data %>%
  unite("measure", c("participant_type", "similarity_type")) %>%
  spread(measure, human_similarity)

#rename human "skin" columns to texture
colnames(human_data_wide)[colnames(human_data_wide)=="blind_human_similarity_skin"] <- "blind_human_similarity_texture"
colnames(human_data_wide)[colnames(human_data_wide)=="sighted_human_similarity_skin"] <- "sighted_human_similarity_texture"

#set seed for random untangle() procedure
set.seed(100)

##compute list of clusters
cluster_list=list()
data_sources=c("sighted","blind","language")
knowledge_types=c("color","shape","texture")
for (knowledge_type in knowledge_types) {
  for (data_source in data_sources) {
    if (data_source == "language") {
      cluster_list[[knowledge_type]][[data_source]] <- language_data %>%
        convert_similarity_to_distance(paste(data_source,"_similarity_simple_dist_",knowledge_type,sep=""), reverse_dist=F, human_data=F) %>%
        as.dist() %>%
        hclust()
    } else {
      cluster_list[[knowledge_type]][[data_source]] <- human_data_wide %>%
        convert_similarity_to_distance(paste(data_source,"_human_similarity_",knowledge_type,sep="")) %>%
        as.dist() %>%
        hclust()
    }
  }
}

#create data frame containing entanglement, FM values, and other indices of cluster similarity
cluster_similarity_values <- data.frame()
for (knowledge_type in knowledge_types) {
  for (i in 1:length(permutations(3,2,data_sources)[,1])) {
    print(i)
    #cluster
    data_source_1 <- permutations(3,2,data_sources)[i,1]
    data_source_2 <- permutations(3,2,data_sources)[i,2]
    hc_1 <- cluster_list[[knowledge_type]][[data_source_1]]
    hc_2 <- cluster_list[[knowledge_type]][[data_source_2]]
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
      data_source_1=data_source_1,
      data_source_2=data_source_2,
      knowledge_type=knowledge_type,
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
write.csv(cluster_similarity_values, "cluster_similarity_values.csv",row.names=F)