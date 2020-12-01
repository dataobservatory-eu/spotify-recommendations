## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(spotifyrecommendations)

sk_genre_table <-  readxl::read_excel('data-raw/SK_Genre_Table.xlsx') %>%
  setNames (c ("target_genre", "genre", "distance" )) %>%
  mutate ( national_identity = "sk")

local_genre_table <- sk_genre_table

usethis::use_data(local_genre_table, overwrite = TRUE)
