#' @title Get Recommendations For All Artists
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' @param artist_ids Spotify Artist IDs
#' @importFrom purrr map_df
#' @importFrom spotifyr get_recommendations
#' @export


get_recommendations_artists_all <- function (artist_ids) {

  get_recs <- function(i, ids, vec_length ) {
    start <- i
    end <- ifelse(i + 4 > vec_length, vec_length, i + 4)
    seeds <- ids[c(start:end)]
    recs <- get_recommendations(limit = end + 1 - start,
                                seed_artists = seeds)
    recs
  }

  artist_length <- length(artist_ids)
  artist_seq <- seq(from = 1, to = artist_length, by = 5)
  all_recs <- map_df(artist_seq, ~get_recs(.x, artist_ids,
                                           artist_length))
  all_recs
}
