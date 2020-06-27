#' Simulate a game of Monopoly
#'
#' Simulate a game of Monopoly, iteratively running take_turn()
#' @param players number of players, an integer, defaults to 4
#' @param turn_limit maxiumum number of simulated turns, an integer, defaults to 1,000
#' @examples
#' \dontrun{
#' simulate_game(5, 100)
#' }
#' @export
#' simulate_game

simulate_game <- function(players = 4, turn_limit = 1000){

  current <- set_up_game(players = players)

  repeat{

    current <- dplyr::bind_rows(current, take_turn(current))

    if(max(current$turn_id) >= turn_limit){

      break

    }

  }

  current

}
