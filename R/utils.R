#' @title Artist Nationality
#'
#' @param artists A list of artists with artist IDs
#' @return A logical vector.
#' @keywords internal

get_artist_nationality <- function( artists,
                                    target_ids ) {

  if ( ! "character" %in% class (target_ids) ) {
    stop ( "target_ids must be a character vector" )
  }

  is_target <- function(x) any ( x %in% target_ids )

  as.logical(unlist (
    sapply ( artists$id, is_target
    )
  ))
}
