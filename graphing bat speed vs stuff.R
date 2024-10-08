library(tidyverse)
library(ggpmisc)

# this pairs with explore force plate file which must be run first
plot <- bat_speed %>%
      mutate(
            playing_level = factor(playing_level,
                                   levels = c("High School", "College", "Pro"))
      ) %>%
      ggplot(aes(`peak_power_[w]_mean_cmj`, bat_speed_mph)) +
      geom_point() +
      facet_wrap(~playing_level) +
      geom_smooth(method = "lm", se = F, color = "lightblue") +
      stat_poly_eq(aes(label = ..rr.label..), 
                   formula = y ~ x, parse = TRUE, 
                   label.x = "right", label.y = "bottom",
                   size = 10) +
      theme_light(base_size = 20)
plot

ggsave("forceplate.png", plot, width = 12, height = 8)