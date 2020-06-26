take_a_chance_2 <- function(a_turn){

  if(a_turn$location %in% chance_locations){

    card_type <- filter(chances, location == a_turn$location) %>%
      pull(name)

    chance_outcomes <- chance_cards %>%
      filter(id == sample(16, 1) & type == card_type) %>%
      mutate(card = paste(type, card, sep =  " - "),
             location = ifelse(str_detect(location, "x"),
                               as.numeric(str_remove(location, "x")) + a_turn$location,
                               as.numeric(location)),
             jail = ifelse(location == 11, 1, NA)) %>%
      select(card, location, jail)

    a_turn %>%
      mutate(location = ifelse(!is.na(chance_outcomes$location), chance_outcomes$location, location),
             chance = chance_outcomes$card,
             jail = ifelse(!is.na(chance_outcomes$jail), chance_outcomes$jail, jail))

  }else{

    a_turn

  }

}
