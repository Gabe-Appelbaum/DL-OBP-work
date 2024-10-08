library(tidyverse)

joint_velos <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/joint_velos.csv")

# turns out moments of interest were already joined in the tables you dumbass

joint_velos_moments <- joint_velos %>%
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
      filter(is.na(event) == F)

# joint_velos_moments %>%
#       ggplot(aes(event, elbow_velo, group = session_pitch)) +
#       geom_smooth()
# 
# joint_velos %>%
#       ggplot(aes(time, elbow_velo_x, group = session_pitch)) +
#       geom_line()

# magnitude of elbow velocity vector given its components
joint_velos_moments$wrist_velo <- sqrt(
            joint_velos_moments$wrist_velo_x^2 + 
            joint_velos_moments$wrist_velo_y^2 + 
            joint_velos_moments$wrist_velo_z^2
)

# now lets grab metadata and find how elbow velo changes by level
metadata <- read_csv("openbiomechanics/baseball_pitching/data/metadata.csv") %>%
      select(-filename_new, -modelname_new) %>%
      mutate(
            playing_level = factor(
                  playing_level,
                  c("milb", "independent", "college", "high_school")
            )
      )

wrist_velo <- left_join(
      joint_velos_moments %>%
            select(session_pitch, wrist_velo, event),
      metadata %>%
            select(session_pitch, playing_level, session_mass_kg, session_height_m)
)

wrist_velo_avgs <- wrist_velo %>%
      group_by(event, playing_level) %>%
      summarise(
            wrist_velo = mean(wrist_velo),
            wrist_velo_mass_adj = mean(wrist_velo*session_mass_kg),
            wrist_velo_height_adj = mean(wrist_velo*session_height_m),
            wrist_velo_fakebmi_adj = mean(wrist_velo*session_mass_kg*session_height_m)
      ) 

wrist_velo_avgs %>%
      ggplot(aes(event, wrist_velo, group = playing_level, color = playing_level)) +
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
      # labs(
      #       title = "Elbow Velocity adj for mass Through the Delivery",
      #       subtitle = "Seperated by playing level",
      #       x = element_blank()#, y = "Elbow velocity * Athlete Mass"
      # ) +
      theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.background = element_blank()
      )
