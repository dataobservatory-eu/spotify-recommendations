## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(spotifyrecommendations)

sk_top_tracks <- read.csv(file.path(
  'data-raw', 'SK_Artist_Top_Tracks_Table.csv'
))  %>%
  setNames(., c("spotify_artist_id", names(.)[2:ncol(.)]))

names ( sk_top_tracks )
listen_local_artists <- sk_top_tracks %>%
  select ( all_of (c(
    "spotify_artist_id", "name", "all_english_title",
    "all_slovak_title", "any_slovak_title",
    "is_considered_slovak", "considered_czech", "known_slovak_city",
    "genre_1", "genre_2", "genre_3"
  ))) %>%
  mutate (
    language = case_when (
      all_english_title == 1 ~ "en",
      all_slovak_title == 1 ~ "sk",
      TRUE ~ NA_character_),
    national_identity = case_when (
      considered_czech == 1 ~ "cz",
      is_considered_slovak == 1 ~ "sk",
      )
    ) %>%
  rename ( city = known_slovak_city,
           artist_name = name ) %>%
  distinct ( spotify_artist_id, .keep_all = TRUE ) %>%
  select ( all_of (c("spotify_artist_id", "artist_name",
                     "national_identity","language",
                     "genre_1", "genre_2", "genre_3")))

usethis::use_data(listen_local_artists, overwrite = TRUE)
