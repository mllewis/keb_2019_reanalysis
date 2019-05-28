# get frequent names from xkcd naming data
library(tidyverse)
library(RSQLite)
library(data.table)
library(here)

XKCD_RAWDATA <- "/Users/mollylewis/Downloads/colorsurvey/mainsurvey.db"
FREQUENT_COLORS <- here('data/frequent_colors.csv')

NCOLORS <- 50

sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = XKCD_RAWDATA)
answers <- dbReadTable(db, "answers") %>%
  as.data.table()

freq_color_names <- answers %>%
  count(colorname) %>%
  arrange(-n) %>%
  filter(!str_detect(colorname, " ")) %>%
  filter(!str_detect(colorname, "-"))


top_colors <- freq_color_names %>%
  slice(1:NCOLORS) %>%
  data.frame()

write_csv(top_colors, FREQUENT_COLORS)