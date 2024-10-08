library(tidyverse)

df <- read_csv("openbiomechanics/high_performance/data/hp_obp.csv") %>%
      select(! c(`peak_takeoff_force_[n]_mean_pp`, 
                 `peak_eccentric_force_[n]_mean_pp`,
                 `peak_takeoff_force_asymmetry_[%_l,r]_mean_pp`,
                 `peak_eccentric_force_asymmetry_[%_l,r]_mean_pp`)
             )

bat_speed <- df %>%
      filter(is.na(bat_speed_mph) == F) %>%
      select(!pitch_speed_mph_group)

bat_speed_col_pro <- bat_speed %>%
      filter(playing_level %in% c("College", "Pro"))

# bat speed vs peak power

bat_speed %>%
      ggplot(aes(`peak_power_[w]_mean_cmj`, bat_speed_mph)) +
      geom_point()


model <- lm(bat_speed_mph ~ `peak_power_[w]_mean_cmj`, bat_speed)
summary(model)
# peak power to bat speed is so real, shout out to kyle connell

# power to bat speed is less accurate when filtered to non high schoolers
model2 <- lm(bat_speed_mph ~ `peak_power_[w]_mean_cmj`, bat_speed_col_pro)
summary(model2)


# lets try concentric peak force
model <- lm(bat_speed_mph ~ `concentric_peak_force_[n]_mean_cmj`, bat_speed)
summary(model)

# power to bat speed is less accurate when filtered to non high schoolers
model2 <- lm(bat_speed_mph ~ `concentric_peak_force_[n]_mean_cmj`, bat_speed_col_pro)
summary(model2)

# pretty similar but less accurate

# must correlation plot
cor_matrix_accessible <- bat_speed %>%
      select(
            !c(
                  test_date,
                  playing_level,
                  bat_speed_mph_group,
                  pitching_session_date,
                  pitch_speed_mph,
                  pitching_max_hss,
                  hitting_session_date,
            )
      )

cor <- cor(na.omit(cor_matrix_accessible)) %>%
      as.data.frame() %>%
      select(bat_speed_mph)


# new regression based on correlation matrix
model <- lm(bat_speed_mph ~ 
                  `peak_power_[w]_mean_cmj` + 
                  `concentric_peak_force_[n]_mean_cmj` +
                  `eccentric_peak_force_[n]_mean_cmj` +
                  `peak_vertical_force_[n]_max_imtp`,
            bat_speed)

summary(model)

##
model <- lm(bat_speed_mph ~ 
                  `peak_power_[w]_mean_cmj` + 
                  `concentric_peak_force_[n]_mean_cmj` +
                  `eccentric_peak_force_[n]_mean_cmj` +
                  `peak_vertical_force_[n]_max_imtp`,
            bat_speed_col_pro)

summary(model)

# honestly pretty similar to just using peak power