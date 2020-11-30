#' @title Get Slovak Artists IDs
#
#  Get unique tracks and unique artists from a playlist in a list.
#
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct
#' @importFrom tidyselect all_of

get_slovak_artist_ids <- function() {


  data(listen_local_artists, envir =  environment())
  listen_local_artists %>%
    dplyr::filter ( national_identity == "sk") %>%
    dplyr::select ( all_of("spotify_artist_id") ) %>%
    dplyr::distinct () %>% unlist()
}




slovak_artist_ids
