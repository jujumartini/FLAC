####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  FLAC AIM 1                               ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Setup -----------------------------------------------------------------------
source("./R/Scripts/02_functions.R")
fdr_merge <- 
  path("FLAC_AIM1_DATA",
       "4_AIM1_MERGED_DATA")
fdr_process <- 
  path("S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
       "MARTINEZ", "2_Conferences", "2022_ICAMPAM",
       "3_data", "4_processed")
fdr_result <- 
  path("S:", "_R_CHS_Research", "PAHRL", "Student Access",
       "0_Students", "MARTINEZ", "2_Conferences", "2022_ICAMPAM",
       "4_results")
fdr_figure <- 
  path("S:", "_R_CHS_Research", "PAHRL", "Student Access",
       "0_Students", "MARTINEZ", "2_Conferences", "2022_ICAMPAM",
       "4_results", "3_graphics")

initiate_analysis(fdr_result = fdr_result)
## Summary ---------------------------------------------------------------------

## Bias ------------------------------------------------------------------------
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_60.feather",
  vct_criterion     = c("standard",
                        "rmr"),
  vct_estimate      = c("noldus"),
  output            =   "percent" # time, percent
)

## Confusion Matrix ------------------------------------------------------------
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = "NOLDUS_CHAMBER_RMR",
  fnm_mer       = "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_60.feather",
  variable      = "intensity",
  vct_criterion = c("standard",
                    "rmr"),
  vct_estimate  = c("noldus"),
  output        = "percent" # minute, percent, both
)


## Plot ------------------------------------------------------------------------
dir_ls(path = fdr_result,
       type = "file",
       regexp = "BIAS.*\\.feather"
         # vct_variable |> 
         # stri_trans_toupper() %>% 
         # stri_c("BIAS", ., sep = "_") |> 
         # stri_c(collapse = "|")
       ) |> 
  purrr::map_dfr(.x = _,
                 .f = ~arrow::read_feather(.x))

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  2022_ICAMPAM                             ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Setup -----------------------------------------------------------------------
source("./R/Scripts/02_functions.R")
fdr_raw <- 
  fs::path("FLAC_AIM1_DATA",
           "1_AIM1_RAW_DATA")
fdr_merge <- 
  path("FLAC_AIM1_DATA",
       "4_AIM1_MERGED_DATA")
fdr_process <- 
  path("S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
       "MARTINEZ", "2_Conferences", "2022_ICAMPAM",
       "3_data", "4_processed")
fdr_result <- 
  path("S:", "_R_CHS_Research", "PAHRL", "Student Access",
       "0_Students", "MARTINEZ", "2_Conferences", "2022_ICAMPAM",
       "4_results")
initiate_analysis(fdr_result = fdr_result)

## Summary ---------------------------------------------------------------------
compute_summary_subject_characteristics(
  fdr_read     = fdr_raw,
  fdr_write    = fdr_result,
  fnm_teleform = "COV1MERGED2022-03-09v2.csv",
  filter_sub   = 
    c(
      1001, 1002, 1003, 1004, 1005,
      1007, 1008, 1009, 1010, #1011,
      1025, 1031, #1036,
      1063, 1067,
      1070 #1072, 1073, 1074, 1075,
      #1076
    ) |> 
    as.integer()
)
compute_summary_visit(
  fdr_read   = fdr_process,
  fdr_write  = fdr_result,
  time_units = "hours" # secs, mins, hours
)
## Bias ------------------------------------------------------------------------
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_60.feather",
  vct_criterion     = c("standard",
                        "rmr"),
  vct_estimate      = c("noldus"),
  output            =   "percent" # time, percent
)

## Confusion Matrix ------------------------------------------------------------
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = "NOLDUS_CHAMBER_RMR",
  fnm_mer       = "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_60.feather",
  variable      = "intensity",
  vct_criterion = c("standard",
                    "rmr"),
  vct_estimate  = c("noldus"),
  output        = "percent" # minute, percent, both
)


## Plot ------------------------------------------------------------------------
fig_bias_minute <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard", "rmr"),
    vct_estimate     = c("noldus"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Rmr" = "RMR"),
                            estimate  = c("Noldus" = "Video"),
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = TRUE,
    separate_values  = FALSE,
    flip_axes        = FALSE,
    type             = "minute"
  )
fig_bias_percent <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard", "rmr"),
    vct_estimate     = c("noldus"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Rmr" = "RMR"),
                            estimate  = c("Noldus" = "Video"),
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = TRUE,
    separate_values  = FALSE,
    flip_axes        = FALSE,
    type             = "percent"
  )

ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_MINUTES_VIDEO_TO_STANDARD_RMR",
                          ext = "png"),
  plot   = last_plot(),
  device = "png",
  path   = fdr_result,
  # width  = 1024 * 9, # 1024, 2048, 3072, 4096, 5120, 6144, 7168, 8192, 9216
  width  = 930 * 15,
  # height = 500 * 9,  # 600, 1200, 1800, 2400, 3000, 3600, 4200, 4800, 5400
  height = 400 * 15,
  units  = "px",
  dpi    = 100 * 15   #  100,  200,  300,  400,  500,  600,  700,  800,  900 dpi
)

# FIGURE 2
df <- 
  readxl::read_xlsx(path = path(fdr_result,
                                "3_figure_table",
                                "Table_3.xlsx"),
                    sheet = 2) |> 
  mutate(`MET Range` = 
           `MET Range` |> 
           as_factor()) |> 
  rename("Standard " = Standard) |> 
  pivot_longer(cols = !`MET Range`,
               names_to = "METs",
               values_to = "Percentage")

ggplot(data = df) +
  geom_col(mapping = aes(x    = Percentage,
                         y    = `MET Range`,
                         fill = METs),
           position = position_dodge()) +
  coord_capped_cart(
    left = brackets_vertical(direction = 'right',
                             length = unit(0.035, "npc"))
  ) +
  scale_y_discrete(limits = rev)  +
  scale_x_continuous(labels = label_percent(),
                     expand = expansion(mult = 0.01),
                     breaks = scales::breaks_extended(n = 5)) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    text                       = element_text(
      family        = "",                                # DEFAULT
      face          = "plain",                           # DEFAULT
      colour        = "black",                           # DEFAULT
      size          = 12,
      hjust         = 0.5,                               # DEFAULT
      vjust         = 0.5,                               # DEFAULT
      angle         = 0,                                 # DEFAULT
      lineheight    = 0.9,                               # DEFAULT
      margin        = margin(t = 0,                      # DEFAULT
                             r = 0,                      # DEFAULT
                             b = 0,                      # DEFAULT
                             l = 0,                      # DEFAULT
                             unit = "pt"),               # DEFAULT
      debug         = FALSE,                             # DEFAULT
      inherit.blank = FALSE                              # DEFAULT
    ),
    title                      = element_text(
      family        = "",                                # DEFAULT
      face          = "bold",                            # DEFAULT
      colour        = "black",                           # DEFAULT
      size          = rel(1.2),
      hjust         = 0.5,                               # DEFAULT
      vjust         = 0.5,                               # DEFAULT
      angle         = 0,                                 # DEFAULT
      lineheight    = 0.9,                               # DEFAULT
      margin        = margin(t = 0,                      # DEFAULT
                             r = 0,                      # DEFAULT
                             b = 0,                      # DEFAULT
                             l = 0,                      # DEFAULT
                             unit = "pt"),               # DEFAULT
      debug         = FALSE,                             # DEFAULT
      inherit.blank = FALSE                              # DEFAULT
    ),
    axis.text                  = element_text(
      family        = NULL,                              # DEFAULT
      face          = NULL,                              # DEFAULT
      colour        = "black",
      size          = rel(0.8),                          # DEFAULT
      hjust         = NULL,                              # DEFAULT
      vjust         = NULL,                              # DEFAULT
      angle         = NULL,                              # DEFAULT
      lineheight    = NULL,                              # DEFAULT
      margin        = NULL,                              # DEFAULT
      debug         = NULL,                              # DEFAULT
      inherit.blank = TRUE                               # DEFAULT
    ),
    plot.title                 = element_text(hjust = 0.5),
    panel.border               = element_blank(),
    # axis.ticks.y.left = element_line(),
    # TODO: CHANGE THIS LATER AS it is not the same for collapse == TRUE.
    axis.line.y                = element_line(),
    axis.line.x                = element_line(),
    # axis.line    = element_line(),
    panel.background           = element_rect(
      fill          = NA,
      colour        = NA,                                # DEFAULT
      size          = NULL,                              # DEFAULT
      linetype      = NULL,                              # DEFAULT
      inherit.blank = TRUE                               # DEFAULT
    ),
    legend.key                 = element_rect(fill = NA),
    panel.grid.major.x         = element_line(
      color = "#BFBFBF"
    ),
    panel.grid.minor.x         = element_line(
      color = "#BFBFBF"
    ),
    panel.grid.major.y = element_blank(), # Different
    legend.box.spacing = unit(0.0, "npc"), # Different
    legend.spacing.y = unit(0.035, "npc"), # Different
    # legend.spacing.x =  unit(1.0, 'cm'),
    # legend.key.width = unit(0.035, "npc"),
    plot.background = element_blank(), # Different makes background transparent.
    legend.background = element_blank(), # Different
    legend.direction = "vertical",
    legend.text = element_text(angle = -90),
    strip.background           = element_rect(fill = "white",
                                              color = NULL),
    strip.text                 = element_text(color = "black")
  )
ggsave(
  filename = path_ext_set("Figure_2",
                          ext = "png"),
  plot   = last_plot(),
  device = "png",
  path   = dir_ls(path = fdr_result,
                  type = "directory",
                  regexp = "figure_table"),
  width  = 400 * 20,
  height = 420 * 20,
  # width  = 500 * 15,
  # height = 524 * 15,
  # width  = 790 * 15,
  # height = 830 * 15,
  units  = "px",
  dpi    = 100 * 20
)


####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  PROJECT: CHAAC                           ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Setup -----------------------------------------------------------------------
source("./R/Scripts/02_functions.R")
fdr_raw <- 
  fs::path("FLAC_AIM1_DATA",
           "1_AIM1_RAW_DATA")
fdr_merge <- 
  path("FLAC_AIM1_DATA",
       "4_AIM1_MERGED_DATA")
fdr_process <- 
  fs::path("FLAC_AIM1_DATA",
           "5_AIM1_PROJECTS",
           "AIM1_WRIST_ACC_CHAMBER_COMPARISON_HLTHY",
           "1_data", "4_processed")
fdr_result <- 
  fs::path("FLAC_AIM1_DATA",
           "5_AIM1_PROJECTS",
           "AIM1_WRIST_ACC_CHAMBER_COMPARISON_HLTHY",
           "2_results")
initiate_analysis(fdr_result = fdr_result)

## Summary ---------------------------------------------------------------------
compute_summary_subject_characteristics(
  fdr_read     = fdr_raw,
  fdr_write    = fdr_result,
  fnm_teleform = "COV1MERGED2022-03-09v2.csv",
  filter_sub   = 
    arrow::read_feather(path(fdr_process,
                             "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather")) |> 
    pull(subject) |> 
    unique()
)
compute_summary_visit(
  fdr_read   = fdr_process,
  fdr_write  = fdr_result,
  time_units = "hours" # secs, mins, hours
)
## Bias ------------------------------------------------------------------------
# NORMAL in TIME
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "time" # time, percent
)
# NORMAL in PERCENTAGE
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "percent" # time, percent
)
# GE 5 min in TIME
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_300.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "time" # time, percent
)
# GE 5 min in PERCENTAGE
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_300.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "percent" # time, percent
)
# GE 10 min in TIME
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_600.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "time" # time, percent
)
# GE 10 min in PERCENTAGE
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_600.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "percent" # time, percent
)
# GE 15 min in TIME
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_900.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "time" # time, percent
)
# GE 15 min in PERCENTAGE
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_900.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "percent" # time, percent
)
# GE 20 min in TIME
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1200.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "time" # time, percent
)
# GE 20 min in PERCENTAGE
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1200.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "percent" # time, percent
)
# GE 25 min in TIME
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1500.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "time" # time, percent
)
# GE 25 min in PERCENTAGE
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1500.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                        "montoye", "rowlands", "staudenmayer"),
  output            =   "percent" # time, percent
)

# Putting all the bias results together in one table.
vct_fpa_bias <- 
  dir_ls(path = path(fdr_result,
                     "2_csv"),
         regexp = "BIAS_INTENSITY_MINUTES")
ord_duration <- 
  vct_fpa_bias |> 
  path_file() |> 
  stri_extract_all_regex(pattern = "\\d*") |> 
  vec_unchop() |> 
  stri_remove_empty() |> 
  as.integer() |> 
  order()
vct_duration <- 
  c(1,
    vct_fpa_bias |> 
      path_file() |> 
      stri_extract_all_regex(pattern = "\\d*") |> 
      vec_unchop() |> 
      stri_remove_empty() |> 
      as.integer() /
      60) |> 
  as.character()
  # vct_fpa_bias
  # path_file() |> 
  # path_ext_remove() |> 
  # stri_extract_all_regex(pattern = "DUR.*") |> 
  # vec_unchop()
vct_duration[-1] <- 
  vct_duration[-1][ord_duration]
vct_fpa_bias[-1] <- 
  vct_fpa_bias[-1][ord_duration]


for (i in seq_along(vct_fpa_bias)) {
  
  fpa_bais <- 
    vct_fpa_bias[i]
  duration <- 
    vct_duration[i]
  
  df_bias <- 
    fread(file = fpa_bais,
          sep = ",") |> 
    mutate(Estimate = 
             Estimate |> 
             stri_trans_totitle(),
           Value    = 
             Value |> 
             stri_trans_totitle()) |> 
    as_tibble()
  df_criterion <- 
    df_bias |> 
    filter(Estimate == "Freedson") |> 
    mutate(Estimate = "Chamber Minutes ± SD") |> 
    rename(duration = `Mean ± SD`) |> 
    select(Value, Estimate, duration)
  colnames(df_criterion)[colnames(df_criterion) == "duration"] <- duration
  df_bias <- 
    df_bias |> 
    unite(col = !!duration,
          # Bias:CI,
          `Bias  (minutes)`:`CI  (minutes)`,
          sep = " ") |> 
    select(Value, Estimate, last_col()) %>% 
    bind_rows(df_criterion,
              .) |> 
    mutate(across(.cols = Estimate:Value,
                  .fns = ~forcats::as_factor(.x))) |> 
    arrange(Value) |>
    rename(Method    = Estimate,
           Intensity = Value)
  
  
  if (i == 1) {
    
    df_combined <- 
      df_bias
    
  } else {
    
    df_combined <- 
      left_join(df_combined,
                df_bias,
                by = c("Intensity", "Method"))
    
  }
  
}

df_combined |> 
  fwrite(
    file = path(
      fdr_result,
      "2_csv",
      "CO_BIAS_INTENSITY_MINUTES_COMBINED_LONG.csv"
      # "CO_BIAS_INTENSITY_PERCENT_COMBINED_LONG.csv"
    ),
    sep = ",",
    bom = TRUE
  )
df_combined |> 
  pivot_longer(cols = !Intensity:Method,
               names_to = "Duration",
               values_to = "Value") |> 
  pivot_wider(names_from = "Method",
              values_from = "Value") |> 
  fwrite(
    file = path(
      fdr_result,
      "2_csv",
      "CO_BIAS_INTENSITY_MINUTES_COMBINED_WIDE.csv"
      # "CO_BIAS_INTENSITY_PERCENT_COMBINED_WIDE.csv"
    ),
    sep = ",",
    bom = TRUE
  )


## Confusion Matrix ------------------------------------------------------------
# NORMAL
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  output        = "percent" # minute, percent, both
)
# GE 5 min
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_300.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  output        = "percent" # minute, percent, both
)
# GE 10 min
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_600.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  output        = "percent" # minute, percent, both
)
# GE 15 min
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_900.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  output        = "percent" # minute, percent, both
)
# GE 20 min
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1200.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  output        = "percent" # minute, percent, both
)
# GE 25 min
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1500.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  output        = "percent" # minute, percent, both
)


## IRR ------------------------------------------------------------------------
# NORMAL
compute_irr(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  kappa_weight  = "squared" # unweighted, equal, squared
)
# GE 5 min
compute_irr(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_300.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  kappa_weight  = "squared" # unweighted, equal, squared
)
# GE 10 min
compute_irr(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_600.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  kappa_weight  = "squared" # unweighted, equal, squared
)
# GE 15 min
compute_irr(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_900.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  kappa_weight  = "squared" # unweighted, equal, squared
)
# GE 20 min
compute_irr(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1200.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  kappa_weight  = "squared" # unweighted, equal, squared
)
# GE 25 min
compute_irr(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1500.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                    "montoye", "rowlands", "staudenmayer"),
  kappa_weight  = "squared" # unweighted, equal, squared
)

## Plot ------------------------------------------------------------------------
###  Separate Values - Flip Axes - Normal  :::::::::::::::::::::::::::::::::::::
fig_bias_minute <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = TRUE,
    duration_type    = "normal",
    type             = "minute"
  )
fig_bias_minute + 
  cowplot::draw_label(label = "POOp",
                      x = -100, y = 1)
  annotate(geom = "text", label = c("POOP", vec_rep("", 2)), x = 1, y = -0.05) + 
  coord_flip(ylim = c(1,7),
             clip = "off") 
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_NORMAL_MINUTES_BY_ESTIMATE_VALUES_FLIPPED",
                          ext = "png"),
  plot     = fig_bias_minute,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 700 * 15,
  height   = 1200 * 15,
  units    = "px",
  dpi      = 100 * 15
)
fig_bias_percent <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = TRUE,
    duration_type    = "normal",
    type             = "percent"
  )
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_NORMAL_PERCENT_BY_ESTIMATE_VALUES_FLIPPED",
                          ext = "png"),
  plot     = fig_bias_percent,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 700 * 15,
  height   = 1200 * 15,
  units    = "px",
  dpi      = 100 * 15
)
###  Separate Values - Flip Axes - 5 min  ::::::::::::::::::::::::::::::::::::::
fig_bias_minute <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = TRUE,
    duration_type    = "ge_than",
    type             = "minute"
  )
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_BOUTS_MINUTES_BY_ESTIMATE_VALUES_FLIPPED",
                          ext = "png"),
  plot     = fig_bias_minute,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 700 * 15,
  height   = 1200 * 15,
  units    = "px",
  dpi      = 100 * 15
)
fig_bias_percent <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = TRUE,
    duration_type    = "ge_than",
    type             = "percent"
  )
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_BOUTS_PERCENT_BY_ESTIMATE_VALUES_FLIPPED",
                          ext = "png"),
  plot     = fig_bias_percent,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 700 * 15,
  height   = 1200 * 15,
  units    = "px",
  dpi      = 100 * 15
)
###  Separate Values - Normal  :::::::::::::::::::::::::::::::::::::::::::::::::
fig_bias_minute <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = FALSE,
    duration_type    = "normal",
    type             = "minute"
  )
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_NORMAL_MINUTES_BY_ESTIMATE_VALUES",
                          ext = "png"),
  plot     = fig_bias_minute,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 1200 * 15,
  height   = 700 * 15,
  units    = "px",
  dpi      = 100 * 15
)
fig_bias_percent <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = FALSE,
    duration_type    = "normal",
    type             = "percent"
  )
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_NORMAL_PERCENT_BY_ESTIMATE_VALUES",
                          ext = "png"),
  plot     = fig_bias_percent,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 1200 * 15,
  height   = 700 * 15,
  units    = "px",
  dpi      = 100 * 15
)
###  Separate Values - Bouts  :::::::::::::::::::::::::::::::::::::::::::::::::
fig_bias_minute <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = FALSE,
    duration_type    = "ge_than",
    type             = "minute"
  )
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_BOUTS_MINUTES_BY_ESTIMATE_VALUES",
                          ext = "png"),
  plot     = fig_bias_minute,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 1200 * 15,
  height   = 700 * 15,
  units    = "px",
  dpi      = 100 * 15
)
fig_bias_percent <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = c("freedson", "sojourn3x", "hildebrand", "marcotte",
                         "montoye", "rowlands", "staudenmayer"),
    vct_variable     = c("intensity"),
    lst_recode       = list(criterion = c("Standard" = "Chamber"),
                            estimate  = NULL,
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = FALSE,
    separate_values  = TRUE,
    flip_axes        = FALSE,
    duration_type    = "ge_than",
    type             = "percent"
  )
ggsave(
  filename = path_ext_set("CO_BIAS_INTENSITY_BOUTS_PERCENT_BY_ESTIMATE_VALUES",
                          ext = "png"),
  plot     = fig_bias_percent,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 1200 * 15,
  height   = 700 * 15,
  units    = "px",
  dpi      = 100 * 15
)

# Mean graphs -------------------------------------------------------------
library(ggpubr)

df <- 
  fread(file = path(fdr_process,
                    "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.csv"),
        sep = ",")
df_mean <- 
  df |> 
  as_tibble() |> 
  select(-contains("dark/obscured/oof")) |> 
  pivot_longer(cols = sedentary_freedson:last_col(),
               names_to = c("intensity", "source"),
               names_sep = "_",
               values_to = "value") |> 
  mutate(intensity = 
           intensity |> 
           stri_trans_totitle() |> 
           forcats::as_factor() |> 
           forcats::fct_recode(MVPA = "Mvpa"),
         source = 
           source |> 
           stri_trans_totitle() |> 
           forcats::as_factor() |> 
           forcats::fct_relevel("Standard",
                                "Freedson",
                                "Sojourn3x",
                                "Hildebrand",
                                "Marcotte",
                                "Montoye",
                                "Rowlands",
                                "Staudenmayer") |> 
           forcats::fct_recode(Chamber = "Standard")) |> 
  select(Intensity = intensity,
         Source    = source,
         value) |> 
  group_by(Intensity, Source) |>
  summarize(`Mean (minutes)` =
              value |>
              mean() |>
              round(digits = 1) %>%
              if_else(. == 0,
                      true = 10,
                      false = .),
            sd   =
              value |>
              sd() |>
              round(digits = 1) %>%
              if_else(. == 0,
                      true = 10,
                      false = .),
            .groups = "drop") |> 
  mutate(
    upper_error = `Mean (minutes)` + sd,
    lower_error = `Mean (minutes)` - 50,
    lower_error = 
      case_when(
        Intensity == "Sedentary" & Source == "Hildebrand" ~ `Mean (minutes)` + 50,
        Intensity == "Sedentary" ~ `Mean (minutes)` / 2.1,
        Intensity == "Light"  & Source == "Montoye" ~ `Mean (minutes)` + 50,
        Intensity == "Light" ~ `Mean (minutes)` / 2.1,
        Intensity == "MVPA" & Source == "Rowlands" ~ `Mean (minutes)` + 25,
        Intensity == "MVPA" ~ `Mean (minutes)` / 2.1
        # Intensity == "Sedentary" & Source == "Hildebrand" ~ `Mean (minutes)` + 50,
        # Intensity == "Sedentary" ~ `Mean (minutes)` - 50,
        # Intensity == "Light" ~ `Mean (minutes)` - 40,
        # Intensity == "MVPA" & Source == "Rowlands" ~ `Mean (minutes)` + 25,
        # Intensity == "MVPA" ~ `Mean (minutes)` - 15
      ),
    p.signif_point = upper_error * 1.10
  ) |> 
  left_join(
    compare_means(formula = Time ~ Source,
                  data    = df_plot,
                  method = "t.test",
                  paired = TRUE,
                  group.by = "Intensity",
                  ref.group = "Chamber",
                  p.adjust.method = "holm") |> 
      select(Intensity,
             Source = group2,
             p.adj) |> 
      mutate(
        p.signif = 
          case_when(
            p.adj <= 0.0001 ~ "****",
            p.adj <= 0.001 ~ "***",
            p.adj <= 0.01 ~ "**",
            p.adj <= 0.05 ~ "*",
            p.adj > 0.05 ~ ""
          )
      ) |> 
      add_row(Intensity = factor("Sedentary", levels = c("Sedentary", "Light", "MVPA")),
              Source    = "Chamber",
              p.signif  = "",
              .before = 1) |> 
      add_row(Intensity = factor("Light", levels = c("Sedentary", "Light", "MVPA")),
              Source    = "Chamber",
              p.signif  = "",
              .before = 1) |> 
      add_row(Intensity = factor("MVPA", levels = c("Sedentary", "Light", "MVPA")),
              Source    = "Chamber",
              p.signif  = "",
              .before = 1) |> 
      mutate(Source = forcats::as_factor(Source)) |> 
      arrange(Intensity),
    by = c("Intensity", "Source")
  )
ggplot(data = df_mean) +
  geom_col(mapping = aes(x = Source,
                         y = `Mean (minutes)`),
           color = "black",
           fill = NA,
           position = position_dodge()) +
  geom_errorbar(mapping = aes(x = Source,
                              ymin = `Mean (minutes)`,
                              ymax = upper_error),
                width = 0.25) +
  # geom_point(mapping = aes(x = Source,
  #                          y = lower_error),
  #            shape = 8) +
  geom_text(mapping = aes(x = Source,
                          y = lower_error,
                          # y = p.signif_point,
                          label = p.signif)) +
  facet_grid(rows = vars(Intensity),
             scales = "free_y") +
  theme(
    text                       = element_text(
      family        = "",                                # DEFAULT
      face          = "plain",                           # DEFAULT
      colour        = "black",                           # DEFAULT
      size          = 12,
      hjust         = 0.5,                               # DEFAULT
      vjust         = 0.5,                               # DEFAULT
      angle         = 0,                                 # DEFAULT
      lineheight    = 0.9,                               # DEFAULT
      margin        = margin(t = 0,                      # DEFAULT
                             r = 0,                      # DEFAULT
                             b = 0,                      # DEFAULT
                             l = 0,                      # DEFAULT
                             unit = "pt"),               # DEFAULT
      debug         = FALSE,                             # DEFAULT
      inherit.blank = FALSE                              # DEFAULT
    ),
    title                      = element_text(
      family        = "",                                # DEFAULT
      face          = "bold",                            # DEFAULT
      colour        = "black",                           # DEFAULT
      size          = rel(1.2),
      hjust         = 0.5,                               # DEFAULT
      vjust         = 0.5,                               # DEFAULT
      angle         = 0,                                 # DEFAULT
      lineheight    = 0.9,                               # DEFAULT
      margin        = margin(t = 0,                      # DEFAULT
                             r = 0,                      # DEFAULT
                             b = 0,                      # DEFAULT
                             l = 0,                      # DEFAULT
                             unit = "pt"),               # DEFAULT
      debug         = FALSE,                             # DEFAULT
      inherit.blank = FALSE                              # DEFAULT
    ),
    axis.text                  = element_text(
      family        = NULL,                              # DEFAULT
      face          = NULL,                              # DEFAULT
      colour        = "black",
      size          = rel(0.8),                          # DEFAULT
      hjust         = NULL,                              # DEFAULT
      vjust         = NULL,                              # DEFAULT
      angle         = NULL,                              # DEFAULT
      lineheight    = NULL,                              # DEFAULT
      margin        = NULL,                              # DEFAULT
      debug         = NULL,                              # DEFAULT
      inherit.blank = TRUE                               # DEFAULT
    ),
    plot.title                 = element_text(hjust = 0.5),
    panel.border               = element_blank(),
    # axis.ticks.y.left = element_line(),
    # TODO: CHANGE THIS LATER AS it is not the same for collapse == TRUE.
    axis.line.y                = element_line(),
    axis.line.x                = element_line(),
    # axis.line    = element_line(),
    panel.background           = element_rect(
      fill          = NA,
      colour        = NA,                                # DEFAULT
      size          = NULL,                              # DEFAULT
      linetype      = NULL,                              # DEFAULT
      inherit.blank = TRUE                               # DEFAULT
    ),
    legend.key                 = element_rect(fill = NA),
    # panel.grid.major.x         = element_line(
    #   color = "grey85"
    # ),
    # panel.grid.minor.x         = element_line(
    #   color = "grey85"
    # ),
    # panel.grid.major.y       = element_blank(),
    strip.background           = element_rect(fill = "white",
                                              color = NULL),
    strip.text                 = element_text(color = "black")
  )
ggsave(
  filename = "mean_compared.png",
  plot = last_plot(),
  device = "png",
  path = path(fdr_result,
              "3_figure_table"),
  width  = 900 * 3,
  height = 700 * 3,
  units  = "px",
  dpi    = 100 * 3
)


df_plot <- 
  df |> 
  as_tibble() |> 
  select(-contains("dark/obscured/oof")) |> 
  pivot_longer(cols = sedentary_freedson:last_col(),
               names_to = c("intensity", "source"),
               names_sep = "_",
               values_to = "value") |> 
  mutate(intensity = 
           intensity |> 
           stri_trans_totitle() |> 
           forcats::as_factor() |> 
           forcats::fct_recode(MVPA = "Mvpa"),
         source = 
           source |> 
           stri_trans_totitle() |> 
           forcats::as_factor() |> 
           forcats::fct_relevel("Standard",
                                "Freedson",
                                "Sojourn3x",
                                "Hildebrand",
                                "Marcotte",
                                "Montoye",
                                "Rowlands",
                                "Staudenmayer") |> 
           forcats::fct_recode(Chamber = "Standard")) |> 
  select(Intensity = intensity,
         Source    = source,
         Time = value)

# p.vector <- NULL
# (lm(formula = `Time (minutes)` ~ Source,
#    data = df_plot) |> 
#   summary())$coefficients[2,4]
# for(i in 1:100){
#   mod <- lm(y ~ x[,i])
#   p.vector[i] <- summary(mod)$coef[2,4]
# }
# 
# p.bh.adjusted <- p.adjust(p.vector, method='BH')
# p.bonf.adjusted <- p.adjust(p.vector, method='bonferroni')
list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
     symbols = c("****", "***", "**", "*", "ns"))
# pull(p.format)
# compare_means(
#   formula   = Time ~ Source,
#   data      = df_plot,
#   method    = "anova",
#   paired    = TRUE,
#   group.by  = "Intensity"
#   # ref.group = "Chamber"
# )
ggbarplot(
  df_plot,
  x = "Intensity",
  y = "Time",
  fill = "Source",
  # label = TRUE,
  add = "mean_se",
  error.plot = "upper_errorbar",
  position = position_dodge()
)
ggbarplot(
  df_plot,
  x = "Source",
  y = "Time",
  facet.by = "Intensity",
  # fill = "Source",
  # label = TRUE,
  add = "mean_se",
  error.plot = "upper_errorbar",
  position = position_dodge()
)
ggbarplot(
  ToothGrowth,
  x = "dose",
  y = "len",
  color = "supp",
  add = "mean_se",
  palette = c("#00AFBB", "#E7B800"),
  position = position_dodge()
)

# summary_distribution_function -------------------------------------------
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
