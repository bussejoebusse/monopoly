#' Take a chance
#'
#' Simulate picking a Chance card from the Chance or Community Chest pile
#' @param a_sub_turn a tibble detailing sub turn informtion, generated as part of take_turn()
#' @examples
#' \dontrun{
#' take_a_chance(turn_1)
#' }
#' @export
#' take_a_chance

take_a_chance <- function(a_sub_turn){

  if(a_sub_turn$location %in% chance_locations$location){

    card_type <- dplyr::filter(chance_locations, location == a_sub_turn$location) %>%
      dplyr::pull(type)

    chance_outcomes <- chances %>%
      dplyr::filter(id == sample(16, 1) & type == card_type) %>%
      dplyr::mutate(card = paste(type, card, sep =  " - "),
                    location = ifelse(stringr::str_detect(location, "x"),
                                      as.numeric(stringr::str_remove(location, "x")) + a_sub_turn$location,
                                      as.numeric(location)),
                    jail = ifelse(location == 11, 1, NA)) %>%
      dplyr::select(card, location, jail)

    a_sub_turn %>%
      dplyr::mutate(location = ifelse(!is.na(chance_outcomes$location), chance_outcomes$location, location),
                    chance = chance_outcomes$card,
                    jail = ifelse(!is.na(chance_outcomes$jail), chance_outcomes$jail, jail)) %>%
      floor_location()

  }else{

    a_sub_turn

  }

}
