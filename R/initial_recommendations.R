#' @title Get Initial Recommendations
#
#' @param playlist_information A list received from get_playlist_information.
#' @importFrom dplyr bind_rows
#' @importFrom spotifyr get_recommendations_all
#' @importFrom purrr possibly
#' @return A tibble of recommendations.
#' @export

initial_recommendations <- function( playlist_information,
                                     target_ids = slovak_artist_ids ) {

  all_artists <- playlist_information$user_playlist_artists

  recommendations_by_artists <- purrr::possibly(
    .f = get_recommendations_artists_all,
    otherwise = NULL)(artist_ids = all_artists$id)

  if ( is.null(recommendations_by_artists)) {
    warning ("No recommendation was made on the basis of original artists")
  }

  recommendations_by_tracks <- purrr::possibly(
    .f = spotifyr::get_recommendations_all,
    otherwise = NULL
  )(
    track_ids = unique (
      playlist_information$user_playlist_tracks$track.id )
  )

  if ( !is.null(recommendations_by_artists)) {
    recommendations_by_artists %>%
      mutate ( release_country_code = get_release_country(
        isrc = external_ids.isrc
      ))
  } else {

  }

  if ( ! is.null(recommendations_by_tracks)) {
    recommendations_by_tracks %>%
      mutate ( release_country_code = get_release_country(
        isrc = external_ids.isrc
      ))
      } else {
    warning ("No recommendation was made on the basis of original tracks.")

  }


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
