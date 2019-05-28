# compare human and language data
library(tidyverse)
library(data.table)
library(here)

LANG_ANIMAL_DISTANCE <- here("data/animal_color_distances_language.csv")
TIDY_HUMAN_PATH <- here("data/tidy_human_data.csv")

language_data <- read_csv(LANG_ANIMAL_DISTANCE)
human_data <- read_csv(TIDY_HUMAN_PATH) %>%
  spread(similarity_type, human_similarity)

full_sim_data <- full_join(language_data, human_data, by = c("animal1", "animal2"))   
  #mutate_at(vars(contains("human")),~ log(.x + 1))
  #mutate_if(is.numeric, ~ log(.x + 1))

full_sim_data %>%
  ggplot(aes(y = human_similarity_color , x = language_similarity_simple_dist, color = participant_type)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_classic()
  
plot_data <- full_sim_data %>%
  spread(participant_type, human_similarity_color) %>%
  select_if(is.numeric) 

cor.test(plot_data$blind, plot_data$language_similarity_simple_dist)
full_sim_data %>%
  select(-6:-7)%>%
  gather(measure, value, -1:-2) %>%
  ggplot(aes(x = value)) +
  geom_density() + 
  facet_wrap(~measure)
long_corr <- cor(plot_data, 
                 use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("v2") %>%
  gather("v1", "estimate", -v2)

long_p <- corrplot::cor.mtest(plot_data, 
                              use = "pairwise.complete.obs")$p %>%
  as.data.frame(row.names = names(plot_data)) %>%
  do(setNames(.,names(plot_data))) %>%
  rownames_to_column("v2") %>%
  gather("v1", "p", -v2)

corr_df <- full_join(long_corr, long_p) %>%
  mutate(estimate_char = case_when(v1 == v2 ~ "", 
                                   TRUE ~ as.character(round(estimate,2))),
         estimate = case_when(v1 == v2 ~ as.numeric(NA), 
                              TRUE ~ estimate),
         estimate_color = case_when(p < .05 ~ estimate, TRUE ~ 0 ))

ggplot(corr_df, aes(v1, fct_rev(v2), fill = estimate_color)) + 
  geom_tile() + #rectangles for each correlation
  #add actual correlation value in the rectangle
  geom_text(aes(label = estimate_char), size=3) + 
  scale_fill_gradient2(low ="blue", mid = "white", high = "red", 
                       midpoint = 0, space = "Lab", guide = "colourbar",
                       name = "Pearson's r") +
  ggtitle("Pairwise Correlation Coefficients") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), #, hjust = .95, vjust = .2), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


lm(human_similarity_color~ language_similarity_simple_dist*participant_type + 
     human_similarity_habitat +
     human_similarity_food + 
     human_similarity_shape*participant_type +
     human_similarity_skin ,  data = full_sim_data %>% mutate_if(is.numeric, scale)) %>%
  summary()


lm(human_similarity_color~ language_similarity_simple_dist*participant_type ,
     #human_similarity_habitat +
     #human_similarity_food + 
     #human_similarity_shape +
     #human_similarity_skin ,  
     data = full_sim_data %>% mutate_if(is.numeric, scale)) %>%
  summary()


