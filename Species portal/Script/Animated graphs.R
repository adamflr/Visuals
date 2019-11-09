# Animated graphs
library(gganimate)

## Points over time
g <- dat %>% 
  ggplot(aes(Ostkoordinat + month(Startdatum) * 700000, Nordkoordinat, color = month(Startdatum, label = T))) + 
  geom_point(aes(group = seq_along(Id))) + 
  coord_equal() +
  #facet_wrap(dat %>% pull(Startdatum) %>% ymd() %>% month(label = T)) +
  transition_time(as.numeric(date(Startdatum))) +
  enter_fade() +
  exit_fade() +
  shadow_mark(past = T, future = F) +
  labs(title = 'Dag: {as_date(frame_time)}', x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank())

animate(g, duration = 30, end_pause = 30, nframes = 1000, height = 400, width = 1000)
anim_save("Species portal/Output/Sorgmantel2019.gif")
