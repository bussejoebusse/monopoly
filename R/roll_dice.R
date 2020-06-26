##load packages
packages <- c("tidyverse", "readxl")

sapply(packages, function(x){
  if(!x %in% installed.packages()[, "Package"])
    install.packages(x)
  require(x, character.only = TRUE)
})

##load data
file_path <- "//Poise.Homeoffice.Local/Home/LEO/Users/BusseJ/Desktop/Monopoly/Data.xlsx"

board <- read_excel(file_path, sheet = "Board")

chances <- board %>%
  filter(name %in% c("Chance", "Community Chest")) %>%
  select(-type)

chance_locations <- chances %>%
  pull(location)

chance_cards <- read_excel(file_path, sheet = "Action Cards")



##choose game variables
players <- 4

turn_limit <- 20000

##set up the board
begin_game <- tibble(player_id = 1:players) %>%
  mutate(turn_id = 0,
         sub_turn_id = NA,
         roll = NA,
         location = 1,
         doubles = NA,
         jail = NA,
         chance = NA)

current <- begin_game

##dice roll function
dice_roll <- function(){

  sample(6, 2, TRUE)

}

##simulate player turn
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

##simulate turn for all players
run_turn <- function(x){

  turn <- x

  map_dfr(1:players, player_turn) %>%
    mutate(location = location - (40* floor(location / 41)))

}

##iterate player turns to turn limit
repeat{

  turn <- max(current$turn_id)

  current <- bind_rows(current, run_turn(turn))

  if(max(current$turn_id == turn_limit)){

    break

  }

}

check <- current %>%
  arrange(player_id) %>%
  left_join(board)

ggplot(check, aes(location))+
  geom_bar()

check_summary <- check %>%
  count(name) %>%
  mutate(total = sum(n),
         probability = n / total) %>%
  arrange(probability)

##needs jail
##probs needs to take turn df as input
##need to change location, jail, and should have chance card output as well
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



take_a_chance(3)

take_a_chance <- function(a_location){

  if(a_location %in% chance_locations){

    card_type <- filter(chances, location == a_location) %>%
      pull(name)

    chance_cards %>%
      filter(id == sample(16, 1) & type == card_type) %>%
      mutate(card = paste(type, card, sep =  " - "),
             location = ifelse(str_detect(location, "x"),
                               as.numeric(str_remove(location, "x")) + a_location,
                               as.numeric(location)),
             jail = ifelse(location == 11, 1, NA)) %>%
      select(card, location, jail)

  }else{

    tibble(card = NA,
           location = a_location,
           jail = NA)

  }

}



take_a_chance(3)

sapply(1:3,function(x){
  c("cat", "dog")[x]
})
