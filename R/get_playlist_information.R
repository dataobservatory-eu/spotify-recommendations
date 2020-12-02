#' @title Get Playlist Information
#'
#'  Get unique tracks and unique artists from a playlist in a list.
#'
#' @param playlist_id A single Spotify playlist id
#' @param token Authorization token
#' @importFrom purrr map_df possibly
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr count group_by arrange
#' @return A list of unique tracks and unique artists from a playlist.
#' @export

get_playlist_information <- function( playlist_id,
                                      token = NULL) {

  if (is.null(token)) token <- get_spotify_access_token()

  . <- id <- n <- NULL

  possibly_get_playlist <- purrr::possibly(
    .f = spotifyr::get_playlist, NULL)

  user_playlist <- possibly_get_playlist(playlist_id,
                                         authorization = token )

  if ( is.null(user_playlist) ) {
    warning ("Could not get playlist")
    return(NULL)}

  user_playlist_features <- spotifyr::get_playlist_audio_features(
    playlist_uris = playlist_id,
    authorization = token )

  user_playlist_features$release_country_code <- get_release_country(
    user_playlist_features$track.external_ids.isrc)

  user_playlist_artists <- do.call ( rbind,
                                     user_playlist_features$track.album.artists)

  unique_playlist_artists <-  user_playlist_artists  %>%
    group_by ( id ) %>%
    count() %>%
    arrange ( n )

  unique_playlist_tracks <- user_playlist_features

   list (
    user_playlist_tracks =  unique_playlist_tracks,
    user_playlist_artists = unique_playlist_artists
    )
}
