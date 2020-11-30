#' @title Get Initial Recommendations
#
#' @param playlist_information A list received from get_playlist_information.
#' @importFrom dplyr bind_rows
#' @importFrom spotifyr get_recommendations_all
#' @return A tibble of recommendations.
#' @export

initial_recommendations <- function( playlist_information,
                                     release_country_code = "SK",
                                     target_ids = slovak_ids ) {

  all_artists <- playlist_information$user_playlist_artists

  recommendations_by_artists <- get_recommendations_artists_all(
    artist_ids = all_artists$id
  )

  recommendations_by_tracks <- spotifyr::get_recommendations_all(
    track_ids = unique ( playlist_information[[1]]$track.id )
  )

  recommendations_by_tracks <- get_relevant_recommendations(
    seed_tracks = playlist_information$user_playlist_tracks$track.id,
    release_country_code = release_country_code,
    target_ids = target_ids)

  if ( !is.null(recommendations_by_tracks)) {
    if ( nrow(recommendations_by_tracks)>0){
      recommendations_by_tracks$release_country_code <- get_release_country(
        recommendations_by_tracks$external_ids.isrc )
      dplyr::bind_rows ( recommendations_by_artists,
                         recommendations_by_tracks  )
    }
  } else {
    recommendations_by_artists
  }
}
