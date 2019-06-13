library(tidyverse)
library(here)
library(knitr)
library(broom)
library(Matrix)
library(dendextend)


### Shape-texture-color plot ###
LANG_ANIMAL_DISTANCE_COLOR <- here("data/processed/animal_color_distances_language_wiki.csv")
LANG_ANIMAL_DISTANCE_SHAPE<- here("data/processed/animal_shape_distances_language_wiki.csv")
LANG_ANIMAL_DISTANCE_TEXTURE <- here("data/processed/animal_texture_distances_language_wiki.csv")

TIDY_HUMAN_PATH <- here("data/processed/tidy_human_data.csv") 
language_data <- read_csv(LANG_ANIMAL_DISTANCE_COLOR) %>%
  left_join(read_csv(LANG_ANIMAL_DISTANCE_SHAPE), by  = c("animal1", "animal2")) %>%
  left_join(read_csv(LANG_ANIMAL_DISTANCE_TEXTURE),by  = c("animal1", "animal2"))  %>%
  mutate_if(is.numeric, ~-.x )

human_data <- read_csv(TIDY_HUMAN_PATH) 
full_sim_data <- full_join(language_data, human_data, by = c("animal1", "animal2"))   

human_data_wide <- human_data %>%
  unite("measure", c("participant_type", "similarity_type")) %>%
  spread(measure, human_similarity)

full_sim_data_wide2 <-  full_join(language_data, human_data,
                                  by = c("animal1", "animal2"))    %>%
  spread(similarity_type, human_similarity) %>%
  filter(animal1 < animal2)


color_cors <- full_sim_data_wide2 %>%
  group_by(participant_type) %>%
  nest() %>%
  mutate(temp = map(data, ~ tidy(cor.test(.$language_similarity_simple_dist_color,
                                          .$human_similarity_color, method = "spearman"))),
         n = map(data, nrow),
         dimension = "Color") %>%
  select(-data) %>%
  unnest() 

texture_cors <- full_sim_data_wide2 %>%
  group_by(participant_type) %>%
  nest() %>%
  mutate(temp = map(data, ~ tidy(cor.test(.$language_similarity_simple_dist_texture,
                                          .$human_similarity_skin, method = "spearman"))),
         n = map(data, nrow),
         dimension = "Skin Texture") %>%
  select(-data) %>%
  unnest() 


shape_cors <- full_sim_data_wide2 %>%
  group_by(participant_type) %>%
  nest() %>%
  mutate(temp = map(data, ~ tidy(cor.test(.$language_similarity_simple_dist_shape,
                                          .$human_similarity_shape, method = "spearman"))),
         n = map(data, nrow),
         dimension = "Shape") %>%
  select(-data) %>%
  unnest()

TAXONOMIC_PATH <- here("data/raw/taxonomy_matrix.mat")
taxonomic_data <- R.matlab::readMat(TAXONOMIC_PATH)[[2]]  
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
  mutate(sim_type = "taxonomic_similarity")
LANGUAGE_PATH_WIKI <- here("data/processed/animal_distances_wiki.csv")
language_data_wiki <- read_csv(LANGUAGE_PATH_WIKI) %>%
  spread(word2, language_similarity) %>%
  select(-word1)
all_corrs_mat_langs_wiki <- as.matrix(language_data_wiki)
rownames(all_corrs_mat_langs_wiki) <- colnames(language_data_wiki)
language_long_wiki <- all_corrs_mat_langs_wiki %>%
  as.data.frame() %>%
  rownames_to_column("animal1") %>%
  gather("animal2", "similarity", -animal1)  %>%
  mutate(sim_type = "lang_wiki_similarity")

taxo_corr <- bind_rows(taxonomic_long, language_long_wiki) %>%
  filter(animal1 < animal2) %>%
  spread(sim_type, similarity)  %>%
  mutate(participant_type = "Ground Truth") %>%
  group_by(participant_type) %>%
  nest() %>%
  mutate(temp = map(data, ~ tidy(cor.test(.$lang_wiki_similarity,
                                          -.$taxonomic_similarity, method = "spearman"))),
         n = map(data, nrow),
         dimension = "Taxonomy")  %>%
  select(-data) %>%
  unnest() 




cor_df <- color_cors %>%
  bind_rows(texture_cors) %>%
  bind_rows(shape_cors) %>%
  bind_rows(taxo_corr) %>%
  select(-method, -alternative, -statistic) %>%
  mutate(se = 1/sqrt(n-3),
         estimate_se_l = estimate - se,
         estimate_se_h = estimate + se,
         dimension = fct_relevel(dimension, "Taxonomy", "Shape", "Skin Texture"),
         participant_type = str_to_title(participant_type)) %>%
  rowwise() %>%
  mutate(sig = case_when(p.value < .01 ~ "**",
                         p.value < .05 ~ "*",
                         TRUE ~ ""))

# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0121945
library(cocor)
bscorr<-cor(human_data_wide$blind_human_similarity_color,human_data_wide$sighted_human_similarity_color)
r.jk <-  0.149 # Correlation (language, blind)
r.jh <- 0.0855  # Correlation (language, sighted)
r.kh <- bscorr # Correlation (intelligence, shoe size)
n <- 435 # Size of the group
cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n, 
                         var.labels=c("language", "blind", "sighted"))


fig_a <- ggplot(cor_df, aes(x = fct_rev(participant_type), y = estimate, fill = participant_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~dimension, drop = T, scales="free_x",space = "free_x") +
  xlab("Language as predictor of...") +
  geom_text(aes(y = estimate + .07, label = sig), size = 6) +
  geom_linerange(aes(ymin = estimate_se_l, ymax = estimate_se_h)) +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = c( "#0345E1", "yellow","#DB3A26")) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0, 0.05)),
    name = "Fisher's Z-transformed rho",
    limits = c(0, .42)) +
  theme(legend.position = "none")

kable(cor_df)


### Texture plot  ###

TIDY_HUMAN_TEXTURE <- here("data/processed/tidy_human_texture_response.csv")
LANGUAGE_SIMILARITY <- here("data/processed/animal_texture_langauge_distances_with_anchors.csv")

human_data <- read_csv(TIDY_HUMAN_TEXTURE) %>%
  rename(similarity = prop) %>%
  select(-correct)

ground_truth <- read_csv(TIDY_HUMAN_TEXTURE)  %>%
  distinct(animal, texture, correct) %>%
  filter(correct == 1) %>%
  rename(ground_truth_texture = texture) %>%
  select(-correct)

language_data  <- read_csv(LANGUAGE_SIMILARITY) %>%
  filter(set_id == 0) %>%
  select(-set_id) %>%
  rename(texture = anchor)  

language_and_human_df<- left_join(human_data, language_data)

language_correct <- language_and_human_df %>%
  distinct(animal, texture, language_similarity) %>%
  group_by(animal) %>%
  filter(language_similarity == max(language_similarity)) %>%
  select(-contains("similarity")) %>%
  rename(language_texture = texture)

human_correct <- language_and_human_df %>%
  group_by(group, animal) %>%
  filter(similarity == max(similarity)) %>%
  select(-contains("similarity")) %>%
  slice(1)  %>% # get rid of one case where there's a tie (go with Kim et al. judgement) 
  spread(group, texture) %>%
  rename(CB_texture = CB, S_texture = S)

accuracy_table <- full_join(language_correct, human_correct) %>%
  left_join(ground_truth) %>%
  mutate(CB_correct = CB_texture == language_texture,
         S_correct = S_texture == language_texture,
         gt_correct = ground_truth_texture == language_texture) %>%
  select(animal, contains("correct")) %>%
  gather("group", "value", -animal) %>%
  group_by(group) %>%
  nest() %>%
  mutate(temp = map(data, ~ binom.test(sum(.x$value), nrow(.x), p = .25) %>% tidy())) %>%
  select(-data) %>%
  unnest() %>%
  mutate(se = sqrt((estimate*(1-estimate))/parameter),
         group = fct_recode(group, "Blind" = "CB_correct", "Sighted" = "S_correct", "Ground Truth" = "gt_correct"),
         sig = case_when(p.value < .01 ~ "**", p.value < .05 ~ '*', TRUE ~ ""),
         group = fct_relevel(group, "Ground Truth", "Sighted", "Blind"))

fig_b <- ggplot(accuracy_table, aes(x = group, y = estimate, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Skin Texture Type\nProportion Correct") +
  xlab("Language as predictor of...") +
  geom_hline(aes(yintercept = .25), linetype = 2) +
  geom_text(aes(y = estimate + .15, label = sig), size = 6) +
  #ggtitle("Language Predictions in Texture Feature Task") +
  geom_linerange(aes(ymin = estimate - se , ymax = estimate + se )) +
  theme_classic(base_size = 13) +
  scale_fill_manual(values = rev(c("#0345E1", "#DB3A26", "yellow"))) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0, 0.05))) +
  theme(legend.position = "none")


blank <- ggplot() +
  theme_void()
pdf("xfull_plot.pdf", width= 10, height = 10)
top_row <- plot_grid(fig_a, fig_b, 
                     labels = c('A', 'B'), 
                     ncol = 2,
                     rel_widths = c(2, 1),
                     label_size = 18)
plot_grid(top_row, blank, labels = c('', 'C'), ncol = 1,
          rel_heights = c(1, 2.1), label_size = 18)
dev.off()


### DENDROGRAM ### 

TIDY_HUMAN_WIKI_DATA <- here("data/processed/tidy_human_wiki_language_data.csv")

all_data <- read_csv(TIDY_HUMAN_WIKI_DATA)

all_hclusts <- all_data %>%
  filter(!(knowledge_type %in% c("habitat", "food"))) %>%
  group_by(knowledge_source, knowledge_type) %>%
  nest() %>%
  mutate(hclusts = map(data, get_hclust)) %>%
  select(-data)

current_dends <- all_hclusts %>%
  filter(knowledge_type == "shape",
         knowledge_source %in% c("language", "blind")) %>%
  mutate(dendros = map(hclusts, as.dendrogram)) %>%
  pull(dendros)

pdf("tanglegram_shape_blind.pdf", width= 5, height = 5)
#layout(matrix(1:6, nrow=1, byrow=TRUE), widths= c(5, 3, 5, 5, 3, 5))
fig1c_1 <- dendlist(current_dends[[1]], current_dends[[2]]) %>%
  untangle(method = "step2side") %>%
  tanglegram(axes=F, color_lines="grey",
             common_subtrees_color_lines = FALSE,
             highlight_branches_lwd = FALSE,
             highlight_distinct_edges=FALSE,
             margin_outer = 1.5,
             lwd = 2,
             edge.lwd = 3,
             margin_inner= 5,
             main_left ="Language",
             main_right ="Blind", just_one = TRUE) 
dev.off()

pdf("tanglegram_shape_sighted.pdf", width= 5, height = 5)

current_dends <- all_hclusts %>%
  filter(knowledge_type == "shape",
         knowledge_source %in% c("language", "sighted")) %>%
  mutate(dendros = map(hclusts, as.dendrogram)) %>%
  pull(dendros)

fig1c_2 <- dendlist(current_dends[[1]], current_dends[[2]]) %>%
  untangle(method = "step2side") %>%
  tanglegram(axes = F, color_lines="grey",
             common_subtrees_color_lines = FALSE,
             highlight_branches_lwd = FALSE,
             highlight_distinct_edges = FALSE,
             margin_outer = 1.5,
             lwd = 2,
             edge.lwd = 3,
             margin_inner= 5,
             main_left = "Language",
             main_right =" Sighted", just_one = TRUE) 
dev.off()
