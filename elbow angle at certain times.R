library(tidyverse)

metadata <- read_csv("openbiomechanics/baseball_pitching/data/metadata.csv") %>%
      select(-filename_new, -modelname_new) %>%
      mutate(
            playing_level = factor(
                  playing_level,
                  c("high_school", "college", "independent", "milb")
            )
      )

forces_moments <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/forces_moments.csv")%>%
      select(session_pitch, fp_10_time, fp_100_time, MER_time, BR_time, MIR_time)

joint_angles <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/joint_angles.csv")

# need to change the structure of the moments file
# pitch, time, event
# 1031_2, 1.1806, fp_10_time
# etc...

# first step to that is get it down to only unique rows
forces_moments <- unique(forces_moments)
# now we pivot narrower
forces_moments <- forces_moments %>%
      pivot_longer(
            cols = fp_10_time:MIR_time,
            names_to = "event",
            values_to = "time"
      ) %>%
      select(session_pitch, time, event)

# now we get a dataframe of elbow and shoulder flexion including the times it happens
# then we remove the times that don't coincide with one of those moments
joint_angles_df <- joint_angles %>%
# i want to add shoulder angles but I dont understand them as well as elbow so just elbow for now
      select(session_pitch, time, elbow_angle_x, elbow_angle_z, shoulder_angle_y) %>%
      inner_join(., forces_moments, join_by(session_pitch, time))

# now lets factor the events and then graph elbow flexion along the events
joint_angles_df <- joint_angles_df %>%
      mutate(
            event = factor(
                  event,
                  c("fp_10_time", "fp_100_time", "MER_time", "BR_time", "MIR_time")
            )
      )

# now plot along the event timelines
joint_angles_df %>%
      ggplot(aes(event, elbow_angle_x, group = session_pitch)) +
      geom_smooth()

# plot pronation/suppination and shoulder adduction/abduction
joint_angles_df %>%
      ggplot(aes(event, elbow_angle_z, group = session_pitch)) +
      geom_smooth()

joint_angles_df %>%
      ggplot(aes(event, shoulder_angle_y, group = session_pitch)) +
      geom_smooth()

# super cool, now lets get avg's for each pitcher and do only certain levels
joint_angles_df <- joint_angles_df %>%
      left_join(., metadata, join_by(session_pitch))

avg_angles <- joint_angles_df %>%
      group_by(user, event, playing_level) %>%
      summarise(
            mean_elbow_angle = mean(elbow_angle_x),
            mean_pronation_suppination = mean(elbow_angle_z),
            mean_shoulder_add_abd = mean(shoulder_angle_y)
      ) %>%
      ungroup()

avg_angles %>%
      mutate(user = as.character(user)) %>%
      ggplot(aes(event, mean_elbow_angle, group = user, color = user)) +
      facet_wrap(~playing_level, ncol = 4) +
      #geom_line()
      geom_smooth() +
      #geom_smooth(span = 0.5, se = FALSE) +
      #scale_color_brewer(palette = "Set3") +
      theme(legend.position = "none") #+
      #xlim(min_value, max_value) +  # Set appropriate limits
      #ylim(min_value, max_value)

avg_angles %>%
      mutate(user = as.character(user)) %>%
      ggplot(aes(event, mean_shoulder_add_abd, group = user, color = user)) +
      facet_wrap(~playing_level, ncol = 4) +
      geom_smooth() +
      theme(legend.position = "none")

# now lets do k means using the path of the elbow angle

# first step is to get just elbow angle and then do a pivot_wider
wide_avg_elbow <- avg_angles %>%
      select(-mean_pronation_suppination, -mean_shoulder_add_abd) %>%
      pivot_wider(
            names_from = event,
            values_from = mean_elbow_angle
      )
