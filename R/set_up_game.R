#' Set-up a new game
#'
#' This function creates a tibble detailing game start information. This tibble can be assigned a name and used as an input into take_turn()
#' @importFrom magrittr %>%
#' @param players number of players, an integer, defaults to 4
#' @examples
#' \dontrun{
#' set_up_game(players = 2)
#' }
#' @export
#' set_up_game

set_up_game <- function(players = 4){

  tibble::tibble(player_id = 1:players) %>%
    dplyr::mutate(turn_id = 0,
                  sub_turn_id = NA,
                  roll = NA,
                  location = 1,
                  doubles = NA,
                  jail = NA,
                  chance = NA)

}
