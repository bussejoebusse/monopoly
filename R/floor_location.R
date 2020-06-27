#' Floors board location
#'
#' This function resets players location counter when they pass Go
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' floor_location(a_turn)
#' }
#' @export
#' floor_location

floor_location <- function(a_turn){

  a_turn %>%
    dplyr::mutate(location = location - (40* floor(location / 41)))

}
