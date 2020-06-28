set.seed(1)

the <- simulate_game(turn_limit = 10000)

summary <- the %>%
  filter(turn_id !=0)

cum_count <- function(x){

  summary %>%
    dplyr::filter(turn_id == x) %>%
    dplyr::count(turn_id, location)

}

out <- purrr::map_dfr(unique(summary$turn_id), cum_count)

lsa <- out %>%
  group_by(location) %>%
  mutate(cum_count = cumsum(n))

nurg <- tibble(location = rep(board$location, max(lsa$turn_id)),
               turn_id = sort(rep(unique(lsa$turn_id), length(board$location))))

blurg <- left_join(nurg, lsa) %>%
  left_join(board) %>%
  mutate(cum_count = ifelse(turn_id == 1 & is.na(cum_count), 0, cum_count)) %>%
  group_by(location) %>%
  mutate(cum_count = zoo::na.locf(cum_count),
         name = paste(location, name, sep = " - ")) %>%
  select(turn_id, name, location, cum_count, type)

levels_for_plot <- blurg %>%
  select(location, name) %>%
  unique() %>%
  arrange(desc(location))

for_plot <- blurg %>%
  filter(turn_id %in% c(1, seq(1000, 10000, 100))) %>%
  mutate(name = factor(name, levels = levels_for_plot$name))

for_plot %>%
  ggplot(aes(name, cum_count, fill = type)) +
  geom_bar(stat = "identity", alpha = 0.66) +
  scale_fill_manual(values = c(board_colours$colours))+
  labs(title='{closest_state}') +
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

for_plot %>%
  ggplot(aes(name, cum_count, fill = type)) +
  geom_bar(stat = "identity", show.legend = FALSE, alpha = 0.66) +
  scale_fill_manual(values = c(board_colours$colours))+
  labs(title='Turns = {prettyNum(closest_state, big.mark = ",")}',
       x = "", y = "") +
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")+
  transition_states(turn_id) -> p

animate(p, detail = 8, end_pause = 50,
        renderer = gifski_renderer("monopoly.gif"))

board_colours <- board %>%
  select(type) %>%
  unique() %>%
  mutate(colours = hex)

hex <- c(rgb(205, 230, 208, maxColorValue = 256),
         rgb(149, 84, 54, maxColorValue = 256),
         rgb(9, 10, 14, maxColorValue = 256),
         rgb(170, 224, 250, maxColorValue = 256),
         rgb(217, 58, 150, maxColorValue = 256),
         rgb(205, 230, 208, maxColorValue = 256),
         rgb(247, 148, 29, maxColorValue = 256),
         rgb(237, 27, 36, maxColorValue = 256),
         rgb(254, 242, 0, maxColorValue = 256),
         rgb(31, 178, 90, maxColorValue = 256),
         rgb(0, 114, 187, maxColorValue = 256))
hex
