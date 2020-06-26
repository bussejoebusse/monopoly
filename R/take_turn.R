player_turn <- function(player){

  roll_1 <- dice_roll()

  turn_1 <- current %>%
    filter(player_id == player,
           turn_id == turn) %>%
    tail(1) %>%
    mutate(turn_id = turn_id + 1,
           jail = jail + 1,
           sub_turn_id = 1,
           roll = paste(roll_1, collapse = ","),
           doubles = sum(duplicated(roll_1)),
           chance = NA)

  if(turn_1$jail %in% 1:3){

    turn_1 <- turn_1 %>%
      mutate(location = ifelse(sum(duplicated(roll_1)) == 1, 11, location),
             jail = ifelse(sum(duplicated(roll_1)) == 1, NA, jail))

  }else{

    turn_1 <- turn_1 %>%
      mutate(location = location + sum(roll_1),
             jail = NA) %>%
      take_a_chance_2()

  }

  if(sum(duplicated(roll_1)) == 1 & !turn_1$jail %in% 1:3){

    roll_2 <- dice_roll()

    turn_2 <- turn_1 %>%
      mutate(sub_turn_id = 2,
             chance = NA,
             roll = paste(roll_2, collapse = ","),
             location = location + sum(roll_2),
             doubles = sum(duplicated(roll_2))) %>%
      take_a_chance_2()

    if(sum(duplicated(roll_2)) == 1 & !turn_1$jail %in% 1:3){

      roll_3 <- dice_roll()

      turn_3 <- turn_2 %>%
        mutate(sub_turn_id = 3,
               chance = NA,
               roll = paste(roll_3, collapse = ","),
               doubles = sum(duplicated(roll_3)),
               location = ifelse(doubles == 1, 11, location + sum(roll_3)),
               jail = ifelse(doubles == 1, 0, jail)) %>%
        take_a_chance_2

      bind_rows(turn_1, turn_2, turn_3)

    }else{

      bind_rows(turn_1, turn_2)

    }}else{

      turn_1

    }

}
