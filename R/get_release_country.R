#' @title Get Release Country
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
#' @param isrc A vector of iscr codes
#' @importFrom dplyr case_when
#' @export

get_release_country <- function ( isrc ) {
  dplyr::case_when (
    isrc %in% c("QM", "QZ", "US") ~  "US",
    isrc %in% c("BR", "BX") ~  "BR",
    TRUE ~ isrc )
}

