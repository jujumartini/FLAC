df_nld <- 
  arrow::read_feather(
    file = path("FLAC_AIM1_DATA",
                "4_AIM1_MERGED_DATA",
                "AIM1_MERGED_NOLDUS_ACTIVITY_POSTURE",
                "CO_ALL_NOLDUS_ACTIVITY_POSTURE.feather")
  )

# Behavior
test <-
  df_nld %>% 
  group_by(study, subject, visit) %>% 
  mutate(run = vec_identify_runs(behavior_noldus),
         .after = behavior_noldus) %>% 
  group_by(study, subject, visit, run) %>% 
  summarise(
    variable = "behavior",
    value    = behavior_noldus[1],
    duration = duration_behavior_noldus[1],
    minutes  = as.double(duration_behavior_noldus[1]) / 60,
    .groups = "drop"
  ) %>% 
  group_by(variable, value) %>% 
  summarise(
    minutes = sum(minutes),
    .groups = "drop"
  ) %>% 
  mutate(
    total = sum(minutes),
    perc = minutes / total
  ) %>% 
  arrange(minutes) %>% 
  mutate(value = as_factor(value)) %>% 
  as.data.table()


ggplot(data = test) +
  geom_col(mapping = aes(y = value,
                         x = minutes)) +
  geom_text(mapping = aes(x = minutes,
                          y = value,
                          label = scales::percent(perc,
                                                  accuracy = 0.1)),
            hjust = -0.1) +
  scale_x_continuous(breaks = scales::extended_breaks(7)) +
  theme_minimal()

ggsave(
  filename = stri_c("flac_aim1_activity_distribution_", Sys.Date(), ".png"),
  plot = last_plot(),
  device = "png",
  width  = 1024 * 4,
  height = 600 * 4,
  units  = "px",
  dpi    = 100 * 4
)

# Posture
test <-
  df_nld %>% 
  group_by(study, subject, visit) %>% 
  mutate(run = vec_identify_runs(posture_noldus),
         .after = posture_noldus) %>% 
  group_by(study, subject, visit, run) %>% 
  summarise(
    variable = "behavior",
    value    = posture_noldus[1],
    duration = duration_posture_noldus[1],
    minutes  = as.double(duration_posture_noldus[1]) / 60,
    .groups = "drop"
  ) %>% 
  group_by(variable, value) %>% 
  summarise(
    minutes = sum(minutes),
    .groups = "drop"
  ) %>% 
  mutate(
    total = sum(minutes),
    perc = minutes / total
  ) %>% 
  arrange(minutes) %>% 
  mutate(value = as_factor(value)) %>% 
  as.data.table()


ggplot(data = test) +
  geom_col(mapping = aes(y = value,
                         x = minutes)) +
  geom_text(mapping = aes(x = minutes,
                          y = value,
                          label = scales::percent(perc,
                                                  accuracy = 0.1)),
            hjust = -0.1) +
  scale_x_continuous(breaks = scales::extended_breaks(7)) +
  theme_minimal()

ggsave(
  filename = stri_c("flac_aim1_posture_distribution_", Sys.Date(), ".png"),
  plot = last_plot(),
  device = "png",
  width  = 1024 * 4,
  height = 600 * 4,
  units  = "px",
  dpi    = 100 * 4
)
