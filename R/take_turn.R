#' Have a player take their turn
#'
#' This function simulates a players' turns, based on their previous location on the board.
#' @importFrom magrittr %>%
#' @param last_turn information about the last turn, a tibble generated either by set_up_game() or take_turn()
#' @examples
#' \dontrun{
#' take_turn(player = 1, last_turn = set_up_game())
#' }
#' @export
#' take_turn

take_turn <- function(last_turn){

  player_turn <- function(player){

    roll_1 <- roll_dice()

    turn_1 <- last_turn %>%
      dplyr::filter(player_id == player) %>%
      utils::tail(1) %>%
      dplyr::mutate(turn_id = turn_id + 1,
                    jail = jail + 1,
                    sub_turn_id = 1,
                    roll = paste(roll_1, collapse = ","),
                    doubles = sum(duplicated(roll_1)),
                    chance = NA)

    if(turn_1$jail %in% 1:3){

      turn_1 <- turn_1 %>%
        dplyr::mutate(jail = ifelse(sum(duplicated(roll_1)) == 1, NA, jail))

    }else{

      turn_1 <- turn_1 %>%
        dplyr::mutate(location = location + sum(roll_1),
                      jail = NA) %>%
        floor_location() %>%
        take_a_chance()

    }

    if(sum(duplicated(roll_1)) == 1 & !turn_1$jail %in% 1:3){

      roll_2 <- roll_dice()

      turn_2 <- turn_1 %>%
        dplyr::mutate(sub_turn_id = 2,
                      chance = NA,
                      roll = paste(roll_2, collapse = ","),
                      location = location + sum(roll_2),
                      doubles = sum(duplicated(roll_2))) %>%
        floor_location() %>%
        take_a_chance()

      if(sum(duplicated(roll_2)) == 1 & !turn_1$jail %in% 1:3){

        roll_3 <- roll_dice()

        turn_3 <- turn_2 %>%
          dplyr::mutate(sub_turn_id = 3,
                        chance = NA,
                        roll = paste(roll_3, collapse = ","),
                        doubles = sum(duplicated(roll_3)),
                        location = ifelse(doubles == 1, 11, location + sum(roll_3)),
                        jail = ifelse(doubles == 1, 0, jail)) %>%
          floor_location() %>%
          take_a_chance()

        dplyr::bind_rows(turn_1, turn_2, turn_3)

      }else{

        dplyr::bind_rows(turn_1, turn_2)

      }}else{

        turn_1

      }

  }

  purrr::map_dfr(unique(last_turn$player_id), player_turn)


}
