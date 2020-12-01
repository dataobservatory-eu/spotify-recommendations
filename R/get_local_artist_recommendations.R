#' @title Get Slovak Artists IDs
#'
#' Recommend local artists from user_playlist_artist data frame.
#'
#' @param user_playlist_artists A user_playlist_artist data frame.
#' @param n Number of required recommendations (maximum value)
#' @importFrom spotifyr get_playlist_audio_features
#' @importFrom dplyr select filter distinct anti_join sample_n arrange
#' @importFrom dplyr arrange rename left_join group_by slice_head
#' @importFrom dplyr full_join
#' @importFrom tidyselect all_of
#' @examples
#' get_national_artist_ids ( national_identity = 'sk' )
#' @return Returns n or less artist ids.
#' @export

get_local_artist_recommendations <- function( user_playlist_artists,
                                   n ) {
  . <- id <- spotify_artist_id <- distance <- NULL

  data ( "artist_distances", envir=environment() )

  rec <- user_playlist_artists %>%
    rename ( spotify_artist_id = id ) %>%
    left_join ( artist_distances, by = "spotify_artist_id") %>%
    filter ( !is.na(recommendation)) %>%
    arrange ( distance )

  return_seed_size <- ifelse  (nrow(rec)>n,n,nrow(rec))

  ## Try a diverse seed:

  diverse_rec <- rec %>%
    group_by (spotify_artist_id) %>%
    sample_n (size = 1, replace = FALSE ) %>%
    arrange ( distance )

  diverse_rec <- diverse_rec[1:3,]

  if ( nrow (diverse_rec) < n ) {
    further_rec <-  rec %>%
      anti_join ( diverse_rec,c("spotify_artist_id", "n",
                                "recommendation", "distance",
                                "national_identity")  ) %>%
      ungroup() %>%
      arrange ( distance ) %>%
      slice_head ( n = n-nrow(diverse_rec)  )

    diverse_rec %>%
      full_join ( further_rec,
                  by = c("spotify_artist_id", "n",
                         "recommendation", "distance",
                         "national_identity")) %>%
      select ( spotify_artist_id ) %>%
      unlist () %>% as.character()
  } else {
    diverse_rec$spotify_artist_id
  }
}
