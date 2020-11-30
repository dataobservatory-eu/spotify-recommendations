#' @title Get Slovak Artists IDs
#
#  Get unique tracks and unique artists from a playlist in a list.
#
#' @param national_identity One of \code{'cz'} or \code{'sk'}.
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct
#' @importFrom tidyselect all_of
#' @example get_slovak_artist_ids()
#' @return A character vector or artist IDs.
#' @export

get_national_artist_ids <- function( national_identity = 'sk') {
  data(listen_local_artists, envir =  environment())
  listen_local_artists %>%
    dplyr::filter ( national_identity == "sk") %>%
    dplyr::select ( all_of("spotify_artist_id") ) %>%
    dplyr::distinct () %>% unlist() %>% as.character()
}
