
cache_words <- function(TARGET_WORDS, MODEL_PATH, OUTPATH){
  
  target_words <- read_csv(TARGET_WORDS) %>%
    distinct(word) %>%
    pull(word)
  
  target_words_clean <- c(setdiff(target_words, c("killerwhale", "polarbear")), 
                          "killer_whale", "polar_bear")
  
  
  # read in model
  model <- fread(
    MODEL_PATH,
    header = FALSE,
    skip = 1,
    quote = "",
    encoding = "UTF-8",
    data.table = TRUE,
    col.names = c("word",
                  unlist(lapply(2:101, function(x) paste0("V", x)))))
  
  
  target_vecs_google <- filter(model, word %in% target_words_clean) %>%
    rename(target_word = word)
  
  
  target_vecs_clean <- target_vecs_google %>%
    mutate(target_word = str_replace_all(target_word, "_", "")) # get rid of _ in polar bear, killer whale
  
  write_csv(target_vecs_clean, OUTPATH)
}
