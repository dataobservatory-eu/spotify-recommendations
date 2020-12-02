get_track_recommendations_artist <- function ( spotify_artist_id,
                                               target_nationality = "sk",
                                               target_release = NULL) {

  data("listen_local_artists", envir=environment())

  target_artists <- listen_local_artists %>%
    filter ( target_nationality == target_nationality ) %>%
    distinct ( spotify_artist_id ) %>% unlist() %>% as.character()

  get_top_tracks <- function (id) {
    top_tracks <- purrr::possibly(.f = get_artist_top_tracks, NULL)(id)

    fn_detect_artists <- function(x) {
      ifelse ( any (x %in% target_artists), TRUE, FALSE)
    }

    if (is.null(top_tracks)) return(top_tracks)

    if (!is.null(target_release)) {
      top_tracks <- top_tracks %>% mutate (
        release_country = get_release_country(external_ids.isrc)) %>%
        filter ( tolower(release_country) == tolower(target_release)) %>%
        select (-all_of("release_country"))
    }

    if (!is.null(target_nationality)) {
      top_tracks <- top_tracks[sapply ( top_tracks$artists, function(x) fn_detect_artists(x) ),]
    }

    if ( !is.null(top_tracks)) {

      top_tracks$artists

      top_tracks %>%
        dplyr::filter ( is_playable ) %>%
        dplyr::select ( all_of(c("id", "name", "popularity",
                                 "uri", "album.id", "album.name", "album.album_type",
                                 "external_ids.isrc")))
    }

  }
  tmp <- lapply ( sample ( spotify_artist_id, 5 ), get_top_tracks)

  do.call ( rbind, tmp ) %>%
    distinct ( external_ids.isrc, .keep_all = TRUE ) %>%
    filter ( )

}
