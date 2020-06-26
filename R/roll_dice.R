

##dice roll function
dice_roll <- function(){

  sample(6, 2, TRUE)

}

##simulate player turn


##simulate turn for all players
run_turn <- function(x){

  turn <- x

  map_dfr(1:players, player_turn) %>%
    mutate(location = location - (40* floor(location / 41)))

}
