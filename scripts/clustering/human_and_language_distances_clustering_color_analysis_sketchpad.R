# compare human and language data
library(tidyverse)
library(data.table)
library(here)
library(Matrix)
library(dendextend)
library(clusteval)
library(fossil)
library(clues)

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

convert_similarity_to_distance <- function(wide_data, col_name,reverse_dist=T) {
  #convert human similarity values based on a similarity column name
  #extract subset of wide human data
  temp <- as.data.frame(wide_data) %>%
    select(animal1,animal2, !!col_name) %>%
    spread(animal2,!!col_name,fill="", convert=T)
  #change animal1 column to row name
  row.names(temp) <- temp$animal1
  temp <- temp %>%
    select(-animal1)
  #convert to symmetric matrix
  temp <- convert_similarity_to_matrix(temp)
  if (reverse_dist) {
    #convert from similarity to "distance"
    temp <- 1- temp
  }
  #return
  temp
}


LANG_ANIMAL_DISTANCE_COLOR <- here("data/processed/animal_color_distances_language_wiki.csv")
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

#### COLOR ####
#sighted human color
m <- convert_similarity_to_distance(human_data_wide, "sighted_human_similarity_color")
temp <- as.dist(m)
plot(hclust(temp))
hc1 <- hclust(as.dist(m))

#blind human color
m <- convert_similarity_to_distance(human_data_wide, "blind_human_similarity_color")
temp <- as.dist(m)
plot(hclust(temp))
hc2 <- hclust(as.dist(m))

#language color
m <- convert_similarity_to_distance(language_data, "language_similarity_simple_dist_color", reverse_dist=F)
temp <- as.dist(m)
plot(hclust(temp))
hc3 <- hclust(as.dist(m))


#compare sighted and blind
FM_index(cutree(hc1, k=5), cutree(hc2, k=5))

#compare sighted to language
FM_index(cutree(hc1, k=5), cutree(hc3, k=5))

#compare blind to language
FM_index(cutree(hc2, k=5), cutree(hc3, k=5))


#Jaccard and Rand indices
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="jaccard")
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="rand")

#sighted to language
cluster_similarity(cutree(hc1, k=5), cutree(hc3, k=5),similarity="jaccard")
cluster_similarity(cutree(hc1, k=5), cutree(hc3, k=5),similarity="rand")
adj.rand.index(cutree(hc1, k=5), cutree(hc3, k=5))
adjustedRand(cutree(hc1, k=5), cutree(hc3, k=5))

#blind to language
cluster_similarity(cutree(hc2, k=5), cutree(hc3, k=5),similarity="jaccard")
cluster_similarity(cutree(hc2, k=5), cutree(hc3, k=5),similarity="rand")
adj.rand.index(cutree(hc2, k=2), cutree(hc3, k=2))
adj.rand.index(cutree(hc2, k=3), cutree(hc3, k=3))
adj.rand.index(cutree(hc2, k=4), cutree(hc3, k=4))
adj.rand.index(cutree(hc2, k=5), cutree(hc3, k=5))
adj.rand.index(cutree(hc2, k=6), cutree(hc3, k=6))
adj.rand.index(cutree(hc2, k=7), cutree(hc3, k=7))
adjustedRand(cutree(hc2, k=5), cutree(hc3, k=5))




#Bk plot
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend3 <- as.dendrogram(hc3)
Bk_plot(dend1, dend2, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")

Bk_plot(dend1, dend3, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")
Bk_plot(dend2, dend3, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")

dend_diff(dend1, dend2)
dend_diff(dend2, dend3)
dend_diff(dend1, dend3)

dendlist(dend1, dend2) %>% 
  untangle(method = "step1side") %>% 
  tanglegram(common_subtrees_color_branches = TRUE)

x <- dendlist(dend1, dend2) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("SIGHTED    entanglement =", round(entanglement(x), 2),"   BLIND"))

x <- dendlist(dend2, dend3) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("BLIND   entanglement =", round(entanglement(x), 2),"   LANGUAGE"))

x <- dendlist(dend1, dend3) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("BLIND   entanglement =", round(entanglement(x), 2),"   LANGUAGE"))

#### SHAPE ####
#sighted human shape
m <- convert_similarity_to_distance(human_data_wide, "sighted_human_similarity_shape")
temp <- as.dist(m)
plot(hclust(temp))
hc1 <- hclust(as.dist(m))

#blind human shape
m <- convert_similarity_to_distance(human_data_wide, "blind_human_similarity_shape")
temp <- as.dist(m)
plot(hclust(temp))
hc2 <- hclust(as.dist(m))

#language
m <- convert_similarity_to_distance(language_data, "language_similarity_simple_dist_shape", reverse_dist=F)
temp <- as.dist(m)
plot(hclust(temp))
hc3 <- hclust(as.dist(m))

#compare sighted and blind
FM_index(cutree(hc1, k=5), cutree(hc2, k=5))

#Jaccard and Rand indices
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="jaccard")
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="rand")

#Bk plot
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
Bk_plot(dend1, dend2, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")

dend_diff(dend1, dend2)

dendlist(dend1, dend2) %>% 
  untangle(method = "step1side") %>% 
  tanglegram(common_subtrees_color_branches = TRUE)

x <- dendlist(dend1, dend2) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("SIGHTED    entanglement =", round(entanglement(x), 2),"   BLIND"))

#compare sighted and blind
FM_index(cutree(hc1, k=5), cutree(hc2, k=5))

#compare sighted to language
FM_index(cutree(hc1, k=5), cutree(hc3, k=5))

#compare blind to language
FM_index(cutree(hc2, k=5), cutree(hc3, k=5))


#Jaccard and Rand indices
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="jaccard")
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="rand")

#sighted to language
cluster_similarity(cutree(hc1, k=5), cutree(hc3, k=5),similarity="jaccard")
cluster_similarity(cutree(hc1, k=5), cutree(hc3, k=5),similarity="rand")
adj.rand.index(cutree(hc1, k=5), cutree(hc3, k=5))
adjustedRand(cutree(hc1, k=5), cutree(hc3, k=5))

#blind to language
cluster_similarity(cutree(hc2, k=5), cutree(hc3, k=5),similarity="jaccard")
cluster_similarity(cutree(hc2, k=5), cutree(hc3, k=5),similarity="rand")
adj.rand.index(cutree(hc2, k=2), cutree(hc3, k=2))
adj.rand.index(cutree(hc2, k=3), cutree(hc3, k=3))
adj.rand.index(cutree(hc2, k=4), cutree(hc3, k=4))
adj.rand.index(cutree(hc2, k=5), cutree(hc3, k=5))
adj.rand.index(cutree(hc2, k=6), cutree(hc3, k=6))
adj.rand.index(cutree(hc2, k=7), cutree(hc3, k=7))
adjustedRand(cutree(hc2, k=5), cutree(hc3, k=5))




#Bk plot
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend3 <- as.dendrogram(hc3)
Bk_plot(dend1, dend2, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")

Bk_plot(dend1, dend3, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")
Bk_plot(dend2, dend3, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")

dend_diff(dend1, dend2)
dend_diff(dend2, dend3)
dend_diff(dend1, dend3)

dendlist(dend1, dend2) %>% 
  untangle(method = "step1side") %>% 
  tanglegram(common_subtrees_color_branches = TRUE)

x <- dendlist(dend1, dend2) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("SIGHTED    entanglement =", round(entanglement(x), 2),"   BLIND"))

dendlist(dend2, dend3) %>% 
  untangle(method = "step1side") %>% 
  tanglegram(common_subtrees_color_branches = TRUE)


x <- dendlist(dend2, dend3) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("BLIND   entanglement =", round(entanglement(x), 2),"   LANGUAGE"))

x <- dendlist(dend1, dend3) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("SIGHTED  entanglement =", round(entanglement(x), 2),"   LANGUAGE"))



#### FOOD ####

#sighted human food
m <- convert_similarity_to_distance(human_data_wide, "sighted_human_similarity_food")
temp <- as.dist(m)
plot(hclust(temp))
hc1 <- hclust(as.dist(m))

#blind human food
m <- convert_similarity_to_distance(human_data_wide, "blind_human_similarity_food")
temp <- as.dist(m)
plot(hclust(temp))
hc2 <- hclust(as.dist(m))

#compare sighted and blind
FM_index(cutree(hc1, k=5), cutree(hc2, k=5))

#Jaccard and Rand indices
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="jaccard")
cluster_similarity(cutree(hc1, k=5), cutree(hc2, k=5),similarity="rand")

#Bk plot
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
Bk_plot(dend1, dend2, main="FM-Index for different numbers of clusters.\nDots closer to 1 indicate more similar clusterings.\nDots above red line are significant.")

dend_diff(dend1, dend2)

dendlist(dend1, dend2) %>% 
  untangle(method = "step1side") %>% 
  tanglegram(common_subtrees_color_branches = TRUE)

x <- dendlist(dend1, dend2) %>% 
  untangle(method = "random", R=1000) %>% 
  tanglegram(common_subtrees_color_branches = TRUE)
x %>%
  plot(main = paste("SIGHTED    entanglement =", round(entanglement(x), 2),"   BLIND"))


#### LANGUAGE TO HUMAN ####
