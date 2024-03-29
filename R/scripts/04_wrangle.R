# RUN this line the first time you open this script
source("./R/Scripts/02_functions.R")

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  FLAC AIM 1                               ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fdr_raw <- 
  fs::path("FLAC_AIM1_DATA",
           "1_AIM1_RAW_DATA")
fdr_clean <- 
  fs::path("FLAC_AIM1_DATA",
           "2_AIM1_CLEANED_DATA")
fdr_shape <-
  fs::path("FLAC_AIM1_DATA",
           "3_AIM1_SHAPED_DATA")
fdr_merge <- 
  fs::path("FLAC_AIM1_DATA",
           "4_AIM1_MERGED_DATA")
fdr_chaac <- 
  fs::path("FLAC_AIM1_DATA",
           "5_AIM1_PROJECTS",
           "AIM1_WRIST_ACC_CHAMBER_COMPARISON_HLTHY",
           "1_data", "4_processed")

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                                     RMR                                  ----
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
shape_rmr(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "RMR",
  filter_sub   = NULL,
  project_only = FALSE
)
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                                   CHAMBER                                ----
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
read_chamber(
  fdr_raw_tsv = "FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_RAW_CHAMBER_TSV",
  fdr_raw_csv = "FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_RAW_CHAMBER_CSV"
)

# CHAMBER FILES ARE CLEANED MANUALLY.

shape_chamber(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "CHAMBER",
  filter_sub   = NULL,
  project_only = FALSE
)
merge_chamber_rmr(
  fdr_read     = fdr_shape,
  fdr_write    = fdr_merge,
  fdr_project  = NULL,
  fld_chm      = "CHAMBER",
  fld_rmr      = "RMR",
  fld_merge    = "CHAMBER_RMR",
  filter_sub   = NULL,
  project_only = FALSE
)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                                   NOLDUS                                 ----
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
clean_noldus(
  fdr_read     = fdr_raw,
  fdr_write    = fdr_clean,
  fdr_project  = NULL,
  fld_src_1    = "NOLDUS_ACTIVITY",
  fld_src_2    = "NOLDUS_POSTURE",
  filter_sub   = NULL,
  project_only = FALSE
)
shape_noldus(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  fld_src_1    = "NOLDUS_ACTIVITY",
  fld_src_2    = "NOLDUS_POSTURE",
  filter_sub   = NULL,
  project_only = FALSE
)
merge_noldus(
  fdr_read     = fdr_shape,
  fdr_write    = fdr_merge,
  fdr_project  = NULL,
  fld_src_1    = "NOLDUS_ACTIVITY",
  fld_src_2    = "NOLDUS_POSTURE",
  fld_merge    = "NOLDUS_ACTIVITY_POSTURE",
  filter_sub   = NULL,
  project_only = FALSE
)
merge_noldus_chamber_rmr(
  fdr_read     = fdr_merge,
  fdr_write    = fdr_merge,
  fdr_project  = NULL,
  fld_src_1    = "NOLDUS_ACTIVITY_POSTURE",
  fld_src_2    = "CHAMBER_RMR",
  fld_merge    = "NOLDUS_CHAMBER_RMR",
  filter_sub   = NULL,
  project_only = FALSE
)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                                ACTIGRAPH RAW                             ----
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
shape_ag_raw_flac(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "GT3X_LW_CSV_RAW",
  freq         = 100,
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
shape_ag_raw_flac(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "GT3X_RH_CSV_RAW",
  freq         = 100,
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
shape_ag_raw_flac(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "GT3X_RW_CSV_RAW",
  freq         = 100,
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                               ACTIGRAPH EPOCH                            ----
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
shape_ag_sec_flac(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "GT3X_LW_CSV_1SEC",
  epoch        = 1,
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
shape_ag_sec_flac(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "GT3X_RH_CSV_1SEC",
  epoch        = 1,
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
shape_ag_sec_flac(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "GT3X_RW_CSV_1SEC",
  epoch        = 1,
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                             AG MODEL ESTIMATES                           ----
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
load(file = "./R/acc_models/lyden_2014.RData")
load(file = "./R/acc_models/staudenmayer_2015.RData")
compute_acc_model_estimates(
  fdr_read     =  fs::path("FLAC_AIM1_DATA",
                           "3_AIM1_SHAPED_DATA"),
  fdr_write    = fs::path("FLAC_AIM1_DATA",
                          "5_AIM1_PROJECTS",
                          "AIM1_WRIST_ACC_CHAMBER_COMPARISON_HLTHY"),
  fdr_project  = NULL,
  folder       = c("GT3X_RH_CSV_1SEC",
                   "GT3X_RW_CSV_1SEC",
                   "GT3X_RW_CSV_RAW"),
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                     DOINT                                 ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source("./R/Scripts/02_functions.R")
fdr_raw <- 
  fs::path("FLAC_AIM1_DATA",
           "1_AIM1_RAW_DATA")
fdr_clean <- 
  fs::path("FLAC_AIM1_DATA",
           "2_AIM1_CLEANED_DATA")
fdr_shape <-
  fs::path("FLAC_AIM1_DATA",
           "3_AIM1_SHAPED_DATA")
fdr_merge <- 
  fs::path("FLAC_AIM1_DATA",
           "4_AIM1_MERGED_DATA")
fdr_doint_clean <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOINT", "3_data", "1_cleaned"
  )
fdr_doint_shape <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOINT", "3_data", "2_shaped"
  )
fdr_doint_merge <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOINT", "3_data", "3_merged"
  )
fdr_doint_process <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOINT", "3_data", "4_processed"
  )

####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                   RMR                                  ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
shape_rmr(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = fdr_doint_shape,
  folder       = "RMR",
  filter_sub   = c(1001, 1002, 1003, 1004, 1005, 1007, 1008, 1009, 1010, 1011,
                   1025, 1031, 1063, 1067, 1069, 1070, 1072, 1073, 1074, 1075,
                   1076, 1077, 1078, 1079),
  filter_loc   = NULL,
  project_only = FALSE
)
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 CHAMBER                                ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
read_chamber(
  fdr_raw_tsv = "FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_RAW_CHAMBER_TSV",
  fdr_raw_csv = "FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_RAW_CHAMBER_CSV"
)

# CHAMBER FILES ARE CLEANED MANUALLY.

shape_chamber(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = fdr_doint_shape,
  folder       = "CHAMBER",
  filter_sub   = c(1001, 1002, 1003, 1004, 1005, 1007, 1008, 1009, 1010, 1011,
                   1025, 1031, 1063, 1067, 1069, 1070, 1072, 1073, 1074, 1075,
                   1076, 1077, 1078, 1079),
  filter_loc   = NULL,
  project_only = FALSE
)
merge_chamber_rmr(
  fdr_read     = fdr_shape,
  fdr_write    = fdr_merge,
  fdr_project  = fdr_doint_merge,
  fld_chm      = "CHAMBER",
  fld_rmr      = "RMR",
  fld_merge    = "CHAMBER_RMR",
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 NOLDUS                                 ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
clean_noldus(
  fdr_read     = fdr_raw,
  fdr_write    = fdr_clean,
  fdr_project  = fdr_doint_clean,
  fld_src_1    = "NOLDUS_ACTIVITY",
  fld_src_2    = "NOLDUS_POSTURE",
  filter_sub   = c(1001, 1002, 1003, 1004, 1005, 1007, 1008, 1009, 1010, 1011,
                   1025, 1031, 1063, 1067, 1069, 1070, 1072, 1073, 1074, 1075,
                   1076, 1077, 1078, 1079),
  filter_loc   = NULL,
  project_only = FALSE
)
shape_noldus(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = fdr_doint_shape,
  fld_src_1    = "NOLDUS_ACTIVITY",
  fld_src_2    = "NOLDUS_POSTURE",
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
merge_noldus(
  fdr_read     = fdr_shape,
  fdr_write    = fdr_merge,
  fdr_project  = fdr_doint_merge,
  fld_src_1    = "NOLDUS_ACTIVITY",
  fld_src_2    = "NOLDUS_POSTURE",
  fld_merge    = "NOLDUS_ACTIVITY_POSTURE",
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
merge_noldus_chamber_rmr(
  fdr_read     = fdr_merge,
  fdr_write    = fdr_merge,
  fdr_project  = fdr_doint_merge,
  fld_src_1    = "NOLDUS_ACTIVITY_POSTURE",
  fld_src_2    = "CHAMBER_RMR",
  fld_merge    = "NOLDUS_CHAMBER_RMR",
  filter_sub   = NULL,
  filter_loc   = NULL,
  project_only = FALSE
)
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 PROCESS                                ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LEFT OFF HERE -----------------------------------------------------------

# Loop through and do a 1 minute, 10 minutes, and 30 minutes for both process
# duration files.Then do analysis AND FINSIH UP MANUSCRIPT.
walk(
  .x = 
    list(
      list("behavior", "noldus", 60),
      list("behavior", "noldus", 600),
      list("behavior", "noldus", 1800)
    ),
  .f = 
    ~process_duration_files(
      fdr_read     = fdr_doint_merge,
      fdr_write    = fdr_doint_process,
      fld_merge    = "NOLDUS_CHAMBER_RMR",
      fnm_merge    = "CO_ALL_NOLDUS_CHAMBER_RMR.feather",
      vct_variable = c("posture",
                       "behavior",
                       "intensity",
                       "inactive"),
      vct_source   = c("noldus",
                       "rmr",
                       "standard"),
      ge_than      = .x
    )
)
walk(
  .x = 
    c(
      "CO_ALL_DUR_BEH_NOL_GE_60_NOLDUS_CHAMBER_RMR.feather",
      "CO_ALL_DUR_BEH_NOL_GE_600_NOLDUS_CHAMBER_RMR.feather",
      "CO_ALL_DUR_BEH_NOL_GE_1800_NOLDUS_CHAMBER_RMR.feather"
    ),
  .f = 
    ~process_visit_numbers(
      fdr_read         = fdr_doint_process,
      fdr_write        = fdr_doint_process,
      fnm_dur          = .x,
      summary_function = "sum",
      time_unit        = "mins" # secs, mins, hours
    )
)

process_duration_files(
  fdr_read     = fdr_doint_merge,
  fdr_write    = fdr_doint_process,
  fld_merge    = "NOLDUS_CHAMBER_RMR",
  fnm_merge    = "CO_ALL_NOLDUS_CHAMBER_RMR.feather",
  vct_variable = c("posture",
                   "behavior",
                   "intensity",
                   "inactive"),
  vct_source   = c("noldus",
                   "rmr",
                   "standard"),
  ge_than      = NULL
)
process_visit_numbers(
  fdr_read         = fdr_doint_process,
  fdr_write        = fdr_doint_process,
  fnm_dur          = "CO_ALL_DURATION_NOLDUS_CHAMBER_RMR.feather",
  summary_function = "sum",
  time_unit        = "mins" # secs, mins, hours
)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                     CHAAC                                 ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merge_chamber_ag_model_estimates(
  fdr_read     = fdr_shape,
  fdr_write    = fdr_merge,
  fdr_project  = NULL,
  fnm_acc      = "AG_MODEL_ESTIMATES",
  fnm_chm_rmr  = "CO_ALL_CHAMBER_RMR.feather",
  fnm_merge    = "CHAMBER_RMR_AG_MODEL",
  filter_sub   = NULL,
  project_only = FALSE
)
# Normal
process_duration_files(
  vct_variable = c("intensity"),
  vct_source   = c("standard","freedson", "sojourn3x", "hildebrand",
                   "marcotte", "montoye", "rowlands", "staudenmayer"),
  fdr_read     = fdr_merge,
  fdr_write    = fdr_chaac,
  fld_mer      = NULL,
  fnm_mer      = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  ge_than      = NULL
)
process_visit_numbers(
  fdr_read         = fdr_chaac,
  fdr_write        = fdr_chaac,
  fnm_dur          = "CO_ALL_DURATION_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  summary_function = "sum",
  time_unit        = "mins" # secs, mins, hours
)
# 5 minutes
process_duration_files(
  vct_variable = c("intensity"),
  vct_source   = c("standard","freedson", "sojourn3x", "hildebrand",
                   "marcotte", "montoye", "rowlands", "staudenmayer"),
  fdr_read     = fdr_merge,
  fdr_write    = fdr_chaac,
  fld_mer      = NULL,
  fnm_mer      = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  ge_than      = list("intensity", "standard", 60 * 5)
)
process_visit_numbers(
  fdr_read         = fdr_chaac,
  fdr_write        = fdr_chaac,
  fnm_dur          = "CO_ALL_DUR_INT_STA_GE_300_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  summary_function = "sum",
  time_unit        = "mins" # secs, mins, hours
)
# 10 minutes
process_duration_files(
  vct_variable = c("intensity"),
  vct_source   = c("standard","freedson", "sojourn3x", "hildebrand",
                   "marcotte", "montoye", "rowlands", "staudenmayer"),
  fdr_read     = fdr_merge,
  fdr_write    = fdr_chaac,
  fld_mer      = NULL,
  fnm_mer      = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  ge_than      = list("intensity", "standard", 60 * 10)
)
process_visit_numbers(
  fdr_read         = fdr_chaac,
  fdr_write        = fdr_chaac,
  fnm_dur          = "CO_ALL_DUR_INT_STA_GE_600_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  summary_function = "sum",
  time_unit        = "mins" # secs, mins, hours
)
# 15 minutes
process_duration_files(
  vct_variable = c("intensity"),
  vct_source   = c("standard","freedson", "sojourn3x", "hildebrand",
                   "marcotte", "montoye", "rowlands", "staudenmayer"),
  fdr_read     = fdr_merge,
  fdr_write    = fdr_chaac,
  fld_mer      = NULL,
  fnm_mer      = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  ge_than      = list("intensity", "standard", 60 * 15)
)
process_visit_numbers(
  fdr_read         = fdr_chaac,
  fdr_write        = fdr_chaac,
  fnm_dur          = "CO_ALL_DUR_INT_STA_GE_900_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  summary_function = "sum",
  time_unit        = "mins" # secs, mins, hours
)
# 20 minutes
process_duration_files(
  vct_variable = c("intensity"),
  vct_source   = c("standard","freedson", "sojourn3x", "hildebrand",
                   "marcotte", "montoye", "rowlands", "staudenmayer"),
  fdr_read     = fdr_merge,
  fdr_write    = fdr_chaac,
  fld_mer      = NULL,
  fnm_mer      = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  ge_than      = list("intensity", "standard", 60 * 20)
)
process_visit_numbers(
  fdr_read         = fdr_chaac,
  fdr_write        = fdr_chaac,
  fnm_dur          = "CO_ALL_DUR_INT_STA_GE_1200_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  summary_function = "sum",
  time_unit        = "mins" # secs, mins, hours
)
# 25 minutes
process_duration_files(
  vct_variable = c("intensity"),
  vct_source   = c("standard","freedson", "sojourn3x", "hildebrand",
                   "marcotte", "montoye", "rowlands", "staudenmayer"),
  fdr_read     = fdr_merge,
  fdr_write    = fdr_chaac,
  fld_mer      = NULL,
  fnm_mer      = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  ge_than      = list("intensity", "standard", 60 * 25)
)
process_visit_numbers(
  fdr_read         = fdr_chaac,
  fdr_write        = fdr_chaac,
  fnm_dur          = "CO_ALL_DUR_INT_STA_GE_1500_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  summary_function = "sum",
  time_unit        = "mins" # secs, mins, hours
)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  FLAC AIM 2                               ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Wrangle Noldus data.
beh_domain_key_v2 <-
  c(
    "1" = "sport&exercise",
    "2" = "leisure",
    "3" = "transportation",
    "4" = "other",
    "5" = "caring&grooming",
    "6" = "household",
    "7" = "occupation",
    "8" = "nca",
    "9" = "uncoded"
  )
pos_bucket_key_v2 <- 
  c(
    "lying"                            = 1L,
    "sitting"                          = 1L,
    "crouching/kneeling/squatting"     = 2L,
    "standing"                         = 3L,
    "other - posture"                  = 4L,
    "intermittent posture"             = 4L,
    "walking"                          = 5L,
    "stepping"                         = 5L,
    "running"                          = 5L,
    "ascending stairs"                 = 5L,
    "descending stairs"                = 5L,
    "crouching/squatting"              = 6L,
    "cycling"                          = 6L,
    "other - movement"                 = 7L,
    "intermittent movement"            = 7L,
    "intermittent p/m"                 = 8L,
    "dark/obscured/oof"                = 9L
  )
pos_domain_key_v2 <-
  c(
    "1" = "sit",
    "2" = "crouching",
    "3" = "stand",
    "4" = "posture other",
    "5" = "movement",
    "6" = "movement stationary",
    "7" = "movement other",
    "8" = "nca",
    "9" = "uncoded"
  )

docomp_beh_buck_key <-
  c(
    "Sports/Exercise"              = 1L,
    "Eating/Drinking"              = 2L,
    "Transportation"               = 3L,
    "Electronics"                  = 2L,
    "Other - Manipulating Objects" = 4L,
    "Other - Carrying Load w/ UE"  = 4L,
    "Other - Pushing Cart"         = 4L,
    "Talking - Person"             = 2L,
    "Talking - Phone"              = 2L,
    "Caring/Grooming - Adult"      = 5L,
    "Caring/Grooming - Animal/Pet" = 5L,
    "Caring/Grooming - Child"      = 5L,
    "Caring/Grooming - Self"       = 2L,
    "Cleaning"                     = 6L,
    "C/F/R/M"                      = 6L,
    "Cooking/Meal Preperation"     = 6L,
    "Laundry"                      = 6L,
    "Lawn&Garden"                  = 6L,
    "Leisure Based"                = 2L,
    "Only [P/M] Code"              = 7L,
    "Talking - Researchers"        = 7L,
    "Intermittent Activity"        = 7L,
    "Dark/Obscured/OoF"            = 8L
  )
docomp_beh_domn_key <-
  c(
    "1" = "Sport&Exercise",
    "2" = "Leisure",
    "3" = "Transportation",
    "4" = "Other",
    "5" = "Caring&Grooming",
    "6" = "Household",
    "7" = "Transition",
    "8" = "Uncoded"
  )
docomp_pos_buck_key <- 
  c(
    "Lying"                            = 1L,
    "Sitting"                          = 1L,
    "Crouching / Kneeling / Squating"  = 2L,
    "Standing"                         = 3L,
    "Other - Posture"                  = 4L,
    "Intermittent Posture"             = 4L,
    "Walking"                          = 5L,
    "Stepping"                         = 5L,
    "Running"                          = 5L,
    "Ascending Stairs"                 = 5L,
    "Descending Stairs"                = 5L,
    "Crouching / Squating"             = 6L,
    "Cycling"                          = 6L,
    "Other - Movement"                 = 7L,
    "Intermittent Movement"            = 7L,
    "Intermittent P/M"                 = 8L,
    "Dark/Obscured/OoF"                = 8L
  )
docomp_pos_domn_key <-
  c(
    "1" = "Sit",
    "2" = "Crouching",
    "3" = "Stand",
    "4" = "Posture Other",
    "5" = "Movement",
    "6" = "Movement Stationary",
    "7" = "Movement Other",
    "8" = "Transition/Uncoded"
  )
lvls_behavior <-
  c("Sports/Exercise",
    "Eating/Drinking",
    "Transportation",
    "Electronics",
    "Other - Manipulating Objects",
    "Other - Carrying Load w/ UE",
    "Other - Pushing Cart",
    "Talking - Person",
    "Talking - Phone",
    "Caring/Grooming - Adult",
    "Caring/Grooming - Animal/Pet",
    "Caring/Grooming - Child",
    "Caring/Grooming - Self",
    "Cleaning",
    "C/F/R/M",
    "Cooking/Meal Preperation",
    "Laundry" ,
    "Lawn&Garden",
    "Leisure Based",
    "Only [P/M] Code",
    "Talking - Researchers",
    "Intermittent Activity",
    "Dark/Obscured/OoF")
lvls_posture <-
  c("Lying",
    "Sitting",
    "Crouching / Kneeling / Squating",
    "Standing",
    "Other - Posture",
    "Intermittent Posture",
    "Walking",
    "Stepping",
    "Running",
    "Ascending Stairs",
    "Descending Stairs",
    "Crouching / Squating",
    "Cycling",
    "Other - Movement",
    "Intermittent Movement",
    "Intermittent P/M",
    "Dark/Obscured/OoF")
lvls_intensity <-
  c("Sedentary",
    "Light",
    "Mod-Vig",
    "Dark/Obscured/OoF")
lvls_environment <-
  c("Domestic",
    "Non-Domestic",
    "Errands/Shopping",
    "Occupation",
    "Organizational/Civic/Religiious",
    "Dark/Obscured/OoF")
# RUN this line the first time you open this script
source("./Scripts/02_functions.R")

clean_noldus_v3(
  project = "FLAC - Aim 1",
  fdr_vid_raw = "Colorado/Noldus Observer XT 14/2_Event Logs/0_raw",
  fdr_vid_clean = "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
)

merge_noldus_v3(
  fdr_vid_clean = "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean",
  fdr_vid_merged = "Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
)

##### Image Coding IRR ---
file_directory_irr <- 
  "./OxfordImageBrowser-win32-x64/6_Training/Quality Check/annotation files"
file_directory_merged <- 
  "./OxfordImageBrowser-win32-x64/6_Training/Quality Check/merged files"
file_directoy_results <- 
  "./OxfordImageBrowser-win32-x64/6_Training/Quality Check"
folder_irr <- 
  "FLAC_QC_2021-10-08"

file_list_irr <- 
  paste(file_directory_irr,
        folder_irr,
        sep = "/") %>% 
  list.files(pattern = "split") %>% 
  str_to_upper()
file_list_act <- 
  file_list_irr %>% 
  str_subset(pattern = "ACTIVITY")
file_list_pos <- 
  file_list_irr %>% 
  str_subset(pattern = "POSTURE")

read_img_irr_v1(
  fls_pos = file_list_pos,
  fls_act = file_list_act,
  fld_irr = folder_irr
)

tib_mer_tc_pos <- 
  shape_img_irr_v3(
    fdr_irr        = file_directory_irr,
    fdr_mer        = file_directory_merged,
    fld_irr        = folder_irr,
    fls_irr_schema = file_list_pos
  )
tib_mer_tc_act <- 
  shape_img_irr_v3(
    fdr_irr        = file_directory_irr,
    fdr_mer        = file_directory_merged,
    fld_irr        = folder_irr,
    fls_irr_schema = file_list_act
  )

tib_img_irr <- 
  bind_rows(
    compute_img_irr_v2(tib_mer_schema = tib_mer_tc_act),
    compute_img_irr_v2(tib_mer_schema = tib_mer_tc_pos)
  )

# Export irr results.
fnm_results <- 
  paste0("irr_results_",
         str_sub(folder_irr,
                 start = 9,
                 end = 18),
         ".csv")
fnm_results_rds <- 
  paste0("irr_results_",
         str_sub(folder_irr,
                 start = 9,
                 end = 18),
         ".rds")
vroom_write(
  tib_img_irr,
  path = paste(file_directoy_results,
               fnm_results,
               sep = "/"),
  delim = ",",
  progress = FALSE
)

readr::write_rds(
  tib_img_irr,
  file = paste(file_directoy_results,
               fnm_results_rds,
               sep = "/"),
  compress = "none"
)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                    DOWC-F                                 ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source("./R/Scripts/02_functions.R")
fdr_raw <- 
  fs::path("FLAC_AIM2_DATA",
           "1_AIM2_RAW_DATA")
fdr_clean <- 
  fs::path("FLAC_AIM2_DATA",
           "2_AIM2_CLEANED_DATA")
fdr_shape <-
  fs::path("FLAC_AIM2_DATA",
           "3_AIM2_SHAPED_DATA")
fdr_merge <- 
  fs::path("FLAC_AIM2_DATA",
           "4_AIM2_MERGED_DATA")
fdr_dowcf_clean <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOWC-F", "3_data", "1_cleaned"
  )
fdr_dowcf_shape <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOWC-F", "3_data", "2_shaped"
  )
fdr_dowcf_merge <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOWC-F", "3_data", "3_merged"
  )
fdr_dowcf_process <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOWC-F", "3_data", "4_processed"
  )
