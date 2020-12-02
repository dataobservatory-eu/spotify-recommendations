#' @title Get Local Recommendations
#'
#' @param playlist_information A list received from get_playlist_information.
#' @param target_nationality Defaults to \code{"sk"}
#' @param target_release If recommendation should be limited to target,
#' defaults to \code{NULL}
#' @param limit Number of playlist items used for recommendation seed.
#' @param n number of required target country recommendations
#' @importFrom dplyr bind_rows ungroup filter select sample_n
#' @importFrom tidyselect all_of
#' @return A tibble of recommendations.
#' @export

get_local_recommendations <- function(
  user_playlist_id = "6KHw5aZWWsmRqpT7o290Mo",
  target_nationality = "sk",
  target_release  = NULL,
  limit = 20,
  n = 4
) {

  user_playlist_info <- get_playlist_information(
    playlist_id  = user_playlist_id
  )

  vars_to_select <- c( "id", "name", "popularity",
                       "uri", "external_ids.isrc",
                       "release_country_code",
                       "target_artists")

  target_artist_ids <- get_national_artist_ids(target_nationality)

  initial_user_recommendations <- initial_recommendations(
    playlist_information = user_playlist_info ,
    target_ids = target_artist_ids,
    limit = limit )

  local_recommendations <- initial_user_recommendations

  if ( !is.null(target_nationality)) {
    local_recommendations <- local_recommendations %>%
      filter ( target_nationality == TRUE)
  }

  if ( !is.null(target_release)) {
    local_recommendations <- local_recommendations %>%
      filter ( target_release == target_release )
  }

  local_recommendations <- local_recommendations %>%
    select ( all_of(vars_to_select))

  if ( nrow(local_recommendations)>=n) {
    return(local_recommendations)
  }

  ### ----- artist based recommendations ---------------------

  recommended_by_artist_similarity <- get_local_artist_recommendations(
    user_playlist_artists = user_playlist_info$user_playlist_artists,
    n = n
  )

  local_recommendations_by_artist <- get_track_recommendations_artist(
    spotify_artist_id = recommended_by_artist_similarity,
    target_nationality = target_nationality,
    n = n)

  local_recommendations_2 <- local_recommendations_by_artist %>%
    select ( all_of (vars_to_select)) %>%
    bind_rows (
    local_recommendations) %>%
    ungroup()

  if ( nrow(local_recommendations_2)>=n) {
    return(sample_n(local_recommendations_2,size = n))
  }

  ### ----- genre based recommendations ---------------------

  user_artists_by_genre <- get_artist_genre(
    user_playlist_artists = user_playlist_info$user_playlist_artists
  )

  recommended_by_genre <- get_artist_recommendations_genre(
    artists_by_genre = user_artists_by_genre,
    target_nationality = target_nationality
  )

  local_recommendations_by_genre <- get_track_recommendations_artist(
    spotify_artist_id = recommended_by_genre,
    target_nationality = target_nationality,
    n = n)

  local_recommendations_3 <- local_recommendations_by_genre %>%
    select ( all_of (vars_to_select)) %>%
    bind_rows (
      local_recommendations_2) %>%
    ungroup()

  if ( nrow(local_recommendations_3)>=n) {
   sample_n(local_recommendations_3,size = n)
  } else {
    local_recommendations_3
  }
}
