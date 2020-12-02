## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(spotifyrecommendations)

sk_artist_distance <- read.csv(file.path(
  'data-raw', 'SK_Artist_Distances.csv'
))  %>%
  setNames ( ., c("recommendation", "spotify_artist_id", "distance")) %>%
  mutate ( national_identity = "sk")

artist_distances <- sk_artist_distance

usethis::use_data(artist_distances, overwrite = TRUE)
