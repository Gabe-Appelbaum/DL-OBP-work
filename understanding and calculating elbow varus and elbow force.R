library(tidyverse)
# poi_metrics <- read_csv("openbiomechanics/baseball_pitching/data/poi/poi_metrics.csv")
# 
# # poi_metrics has "elbow_varus_moment": peak elbow varus moment (Nm)
# # according to kyle wasserberger that is the force going through the elbow accounting for elbow velo, height and weight
# # let me try to piece that together from the full signal so I can see how it changes through the throw
# 
# max_elbow_varus_moment <- poi_metrics %>%
#       select(session_pitch, elbow_varus_moment)

forces_moments <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/forces_moments.csv")

metadata <- read_csv("openbiomechanics/baseball_pitching/data/metadata.csv") %>%
      select(-filename_new, -modelname_new) %>%
      mutate(
            playing_level = factor(
                  playing_level,
                  c("milb", "independent", "college", "high_school")
            )
      )

forces_moments <- left_join(
      forces_moments,
      metadata
      )
# 
# # i'll start by trying max of elbow moment y because that's referred to as varus in the documentation
# join <- left_join(
#       max_elbow_varus_moment,
#       elbow_forces_moments %>%
#             group_by(session_pitch) %>%
#             summarise(
#                   max(elbow_moment_y, na.rm = T)
#             ),
#       join_by(session_pitch)
# )

# oh yah that's it, elbow_moment_y == elbow_varus_moment. I didn't think that would work like that

# lets visualize that through the throw
forces_moments %>%
      mutate(
            event = case_when(
                  time == fp_10_time ~ "fp_10_time",
                  time == fp_100_time ~  "fp_100_time",
                  time == MER_time ~ "MER_time",
                  time == BR_time ~ "BR_time",
                  time == MIR_time ~ "MIR_time"
            ),
            event = factor(
                  event,
                  c("fp_10_time", "fp_100_time", "MER_time", "BR_time", "MIR_time")
            )
      ) %>%
      select(!c(fp_10_time, fp_100_time, MER_time, BR_time, MIR_time)) %>%
      filter(is.na(event) == F) %>%
      group_by(event, playing_level) %>%
      summarise(
            elbow = mean(elbow_moment_y)
      ) %>%
      ggplot(aes(event, elbow, group = playing_level, color = playing_level)) +
      geom_smooth() +
      scale_color_manual(
            values = c(
                  "high_school" = "#0571b0",
                  "college" = "#92c5de",
                  "independent" = "#f4a582",
                  "milb" = "#ca0020"
            )
      ) +
      scale_x_discrete(
            labels = c(
                  "fp_10_time" = "Foot Contact",
                  "fp_100_time" = "Foot Plant",
                  "MER_time" = "Full Layback",
                  "BR_time" = "Ball Release",
                  "MIR_time" = "Max I.R."
            )
      ) +
      theme_light(base_size = 20) +
      labs(
            title = "Elbow Torque Through the Delivery",
            subtitle = "Seperated by playing level",
            x = element_blank(),
            y = "Elbow varus moment"
      ) +
      theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.background = element_blank()
      )
