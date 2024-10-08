library(tidyverse)

joint_velos <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/joint_velos.csv")

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
joint_velos_moments$elbow_velo <- sqrt(
            joint_velos_moments$elbow_velo_x^2 + 
            joint_velos_moments$elbow_velo_y^2 + 
            joint_velos_moments$elbow_velo_z^2
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

elbow_velo <- left_join(
      joint_velos_moments %>%
            select(session_pitch, elbow_velo, event),
      metadata %>%
            select(session_pitch, playing_level, session_mass_kg, session_height_m)
)

# elbow_velo$forearm_inertia <- 
#       (1/3) * 
#       (.016*elbow_velo$session_mass_kg) * 
#       (.16*elbow_velo$session_height_m)^2
# 
elbow_velo_avgs <- elbow_velo %>%
      group_by(event, playing_level) %>%
      summarise(
            elbow_velo = mean(elbow_velo)#,
            # elbow_velo_mass_adj = mean(elbow_velo*session_mass_kg),
            # elbow_velo_height_adj = mean(elbow_velo*session_height_m),
            # elbow_velo_fakebmi_adj = mean(elbow_velo*session_mass_kg*session_height_m),
            # # this one \/ seems to be nonsense
            # #elbow_ke = mean(0.5 * session_mass_kg * (elbow_velo^2)) this one
            # elbow_ke_from_forearm = mean(0.5 * forearm_inertia * elbow_velo^2)
      )

elbow_velo_avgs %>%
      ggplot(aes(event, elbow_velo, group = playing_level, color = playing_level)) +
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
            title = "Elbow Velo Through the Delivery",
            subtitle = "Seperated by playing level",
            x = element_blank(),
            y = "Elbow velocity"
      ) +
      theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.background = element_blank()
      )

# height and weight averages by level
# metadata %>%
#       group_by(playing_level) %>%
#       summarise(
#             mean(session_mass_kg),
#             mean(session_height_m)
#       )

# plot weights of players by level

metadata %>%
      ggplot(aes(session_mass_kg, color = playing_level, fill = playing_level)) +
      facet_wrap(~playing_level) +
      geom_bar() +
      scale_color_manual(
            values = c(
                  "high_school" = "#0571b0",
                  "college" = "#92c5de",
                  "independent" = "#f4a582",
                  "milb" = "#ca0020"
            )
      ) +
      scale_fill_manual(
            values = c(
                  "high_school" = "#0571b0",
                  "college" = "#92c5de",
                  "independent" = "#f4a582",
                  "milb" = "#ca0020"
            )
      )

elbow_velo %>%
      mutate(
            playing_level = case_when(
                  playing_level %in% c("milb", "independent") ~ "pro",
                  T ~ playing_level
            )
      ) %>%
      group_by(event, playing_level) %>%
      summarise(
            elbow_velo = mean(elbow_velo),
            elbow_velo_mass_adj = mean(elbow_velo*session_mass_kg),
            elbow_velo_height_adj = mean(elbow_velo*session_height_m),
            elbow_velo_fakebmi_adj = mean(elbow_velo*session_mass_kg*session_height_m)
      )  %>%
      ggplot(aes(event, elbow_velo_mass_adj, group = playing_level, color = playing_level)) +
      geom_smooth() +
      scale_color_manual(
            values = c(
                  "high_school" = "#0571b0",
                  "college" = "#92c5de",
                  "pro" = "#ca0020"
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
            title = "Elbow Velocity adj for mass Through the Delivery",
            subtitle = "Seperated by playing level",
            x = element_blank(),
            y = "Elbow velocity * Athlete Mass"
      ) +
      theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.background = element_blank()
      )
