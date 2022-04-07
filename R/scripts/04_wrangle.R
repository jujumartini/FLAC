# RUN this line the first time you open this script
source("./Scripts/02_functions.R")


# FLAC - Aim 1 ------------------------------------------------------------

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

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                             RMR                             ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
shape_rmr_v2(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "RMR",
  filter_sub   = NULL,
  project_only = FALSE
)
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                           CHAMBER                           ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
shape_chamber_v1(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  folder       = "CHAMBER",
  filter_sub   = NULL,
  project_only = FALSE
)
merge_chamber_rmr_v6(
  fdr_read     = fdr_shape,
  fdr_write    = fdr_merge,
  fdr_project  = NULL,
  fld_chm      = "CHAMBER",
  fld_rmr      = "RMR",
  fld_merge    = "CHAMBER_RMR",
  filter_sub   = NULL,
  project_only = FALSE
  
)

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                            NOLDUS                            ::
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
clean_noldus_v3(
  fdr_read     = fdr_raw,
  fdr_write    = fdr_clean,
  fdr_project  = NULL,
  fld_act      = "NOLDUS_ACTIVITY",
  fld_pos      = "NOLDUS_POSTURE",
  filter_sub   = NULL,
  project_only = FALSE
)
shape_noldus_v12(
  fdr_read     = fdr_clean,
  fdr_write    = fdr_shape,
  fdr_project  = NULL,
  fld_act      = "NOLDUS_ACTIVITY",
  fld_pos      = "NOLDUS_POSTURE",
  filter_sub   = NULL,
  project_only = FALSE
)
merge_noldus_v8(
  fdr_read     = fdr_shape,
  fdr_write    = fdr_merge,
  fdr_project  = NULL,
  fld_act      = "NOLDUS_ACTIVITY",
  fld_pos      = "NOLDUS_POSTURE",
  fld_merge    = "NOLDUS_ACTIVITY_POSTURE",
  filter_sub   = NULL,
  project_only = FALSE
)
merge_noldus_chamber_rmr(
  fdr_read     = fdr_merge,
  fdr_write    = fdr_merge,
  fdr_project  = NULL,
  fld_nld      = "NOLDUS_ACTIVITY_POSTURE",
  fld_chm      = "CHAMBER_RMR",
  fld_merge    = "NOLDUS_CHAMBER_RMR",
  filter_sub   = NULL,
  project_only = FALSE
)

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                        ACTIGRAPH RAW                        ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                       ACTIGRAPH EPOCH                       ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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


# FLAC - Aim 2 ------------------------------------------------------------
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

