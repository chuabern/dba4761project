rm(list=ls())
#----------------------------------------------------------------
library(dplyr)
library(tidyxl)
library(tidyr)
library(purrr)
library(janitor)
#----------------------------------------------------------------
name_file <- "biotech"
name_sheet <- "data"
path_data <- file.path(paste(name_file, "xlsx", sep="."))
data <- xlsx_cells(path_data, name_sheet)

nm <- data %>% 
  filter(row == 1) %>% 
  pull(character)

data <- data %>%
  filter(row >=2) %>%
  select(character, numeric, col, row) %>% 
  mutate(numeric = replace_na(numeric, 0))

text_vec=c(1:5, 37:39, 113, 120, 150, 171:172, 174)
data <- data %>% 
  select(-numeric) %>%
  filter(col %in% text_vec) %>%
  spread(col, character) %>%
  left_join(
    data %>% 
      select(-character) %>%
      filter(!col %in% text_vec) %>%
      spread(col, numeric),
    by="row"
  )

nm = c(nm[text_vec], nm[-text_vec])
data <- data %>% 
  select(-row) %>%
  set_names(nm) %>% 
  clean_names()  
#----------------------------------------------------------------

save(data, file=paste(name_file, "rda", sep="."))
#----------------------------------------------------------------

rm(name_file, name_sheet)
rm(text_vec, nm, data)
rm(path_data)  
