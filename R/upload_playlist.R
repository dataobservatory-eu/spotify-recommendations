#' @title Upload Playlist
#'
#' Get unique tracks and unique artists from a playlist in a list.
#'
#' @param user_id A single Spotify playlist id
#' @param playlist_name Name for a new playlist
#' @param playlist_description New description for the playlist
#' @importFrom spotifyr get_user_playlists add_tracks_to_playlist
#' @importFrom dplyr filter
#' @return Does not return anything.
#' @export

upload_playlist <- function (user_id = 'rx4xjay1368opqg2i7nabuo5c',
                             playlist_name = 'test',
                             playlist_description = "listen local test") {

  spotifyr::create_playlist(user_id,
                            name = playlist_name,
                            description = "Listen Local Test")

  user_playlists <- spotifyr::get_user_playlists(user_id = 'rx4xjay1368opqg2i7nabuo5c')
  target_playlist <- user_playlists %>%
    filter ( name == new_name,
             description == new_description)

  spotifyr::add_tracks_to_playlist(playlist_id = target_playlist$id,
                                   uris = track_uris )
}
