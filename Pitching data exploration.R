library(tidyverse)

energy_flow <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/energy_flow.csv")
force_plate <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/force_plate.csv")
forces_moments <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/forces_moments.csv")
joint_angles <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/joint_angles.csv")
joint_velos <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/joint_velos.csv")
landmarks <- read_csv("openbiomechanics/baseball_pitching/data/full_sig/landmarks.csv")
poi_metrics <- read_csv("openbiomechanics/baseball_pitching/data/poi/poi_metrics.csv")


# correlation matrix
cor <- cor(poi_metrics[,5:ncol(poi_metrics)]) %>%
      as.data.frame() %>%
      select(pitch_speed_mph)

velo_model <- lm(pitch_speed_mph ~ 
                       elbow_transfer_fp_br +
                       # shoulder_transfer_fp_br +
                       # thorax_distal_transfer_fp_br +
                       shoulder_internal_rotation_moment +
                       elbow_varus_moment,
                 data = poi_metrics)

summary(velo_model) # adj r squared of 0.51
confint(velo_model)

# try the same but with velo above 90

velo_model <- lm(pitch_speed_mph ~ 
                       elbow_transfer_fp_br +
                       shoulder_transfer_fp_br +
                       thorax_distal_transfer_fp_br +
                       shoulder_internal_rotation_moment +
                       elbow_varus_moment,
                 data = poi_metrics %>%
                       filter(pitch_speed_mph > 90))
# holy cow its bad now, 0.17 adj r sq
summary(velo_model)
confint(velo_model)


# redo correlation matrix for velo>90
cor90 <- cor(
      poi_metrics[,5:ncol(poi_metrics)] %>%
            filter(pitch_speed_mph > 90)
      ) %>%
      as.data.frame() %>%
      select(pitch_speed_mph)

# now the new model
velo_model <- lm(pitch_speed_mph ~ 
                       max_shoulder_internal_rotational_velo +
                       max_torso_rotational_velo +
                       torso_anterior_tilt_fp +
                       lead_knee_extension_angular_velo_br +
                       rear_grf_angle_at_max +
                       lead_knee_generation_fp_br +
                       max_pelvis_rotational_velo,
                 data = poi_metrics %>%
                       filter(pitch_speed_mph > 90))
# THIS IS 0.3 adj r sq
summary(velo_model)
confint(velo_model)


# redo correlation matrix for velo>85
cor85 <- cor(
      poi_metrics[,5:ncol(poi_metrics)] %>%
            filter(pitch_speed_mph > 85)
) %>%
      as.data.frame() %>%
      select(pitch_speed_mph)

# now the new model
velo_model <- lm(pitch_speed_mph ~ 
                       elbow_transfer_fp_br +
                       shoulder_transfer_fp_br +
                       thorax_distal_transfer_fp_br,
                 data = poi_metrics %>%
                       filter(pitch_speed_mph > 85))
# THIS IS 0.1 adj r sq
summary(velo_model)
confint(velo_model)
