set.seed(1)

the <- simulate_game

summary <- the %>%
  filter(turn_id !=0) %>%
  right_join(board)

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
  mutate(cum_count = ifelse(turn_id == 1 & is.na(cum_count), 0, cum_count)) %>%
  group_by(location) %>%
  mutate(cum_count = zoo::na.locf(cum_count)) %>%
  left_join(select(board, location, name)) %>%
  select(turn_id, location, name, cum_count)

for_plot <- blurg %>%
  group_by(turn_id) %>%
  arrange(turn_id, -cum_count) %>%
  mutate(rank = 1:n()) %>%
  filter(!name %in% c("Chance", "Community Chest", "Income Tax")) %>%
  filter(turn_id %in% seq(1, 1000, 10))

for_plot %>%
  ggplot() +
  aes(xmin = 0 ,
      xmax = cum_count) +
  aes(ymin = rank - .45,
      ymax = rank + .45,
      y = rank) +
  facet_wrap(~ turn_id) +
  geom_rect(alpha = .7) +
  geom_text(aes(label = name), hjust = "right", colour = "black", fontface = "bold", nudge_y = -100000)
  scale_y_reverse() -> plot


plot <- ggplot(for_plot, aes(reorder(name, rank), cum_count))+
  geom_bar(stat = "identity")+
  coord_flip() +
  facet_wrap(~ turn_id)

plot+
  facet_null()+
  transition_time(turn_id)

animate(anim, nframes = 100, fps = 100, end_pause = 30,
        renderer = gifski_renderer("vis.gif"))

ggplot(for_plot, aes(x = -rank,y = cum_count, group = name)) +
  geom_tile(aes(y = cum_count / 2, height = cum_count), width = 0.9) +
  geom_text(aes(label = name), hjust = "right", colour = "black", fontface = "bold", nudge_y = -100000) +
  geom_text(aes(label = scales::comma(cum_count)), hjust = "left", nudge_y = 100000, colour = "grey30") +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = c(0.4, 0.2),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y=element_blank()) +
  # gganimate code to transition by year:
  transition_time(turn_id) +
  ease_aes('cubic-in-out')
