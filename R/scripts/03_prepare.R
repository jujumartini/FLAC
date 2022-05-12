# RUN this line the first time you open this script
source("./R/Scripts/02_functions.R")

# FLAC AIM 1 --------------------------------------------------------------

read_chamber_v1(
  fdr_raw = fs::path("FLAC_AIM1_DATA",
                     "1_AIM1_RAW_DATA"),
  fdr_raw_csv = fs::path("FLAC_AIM1_DATA",
                         "1_AIM1_RAW_DATA",
                         "AIM1_RAW_CHAMBER_cSV")
)

# Merge raw(?) chamber data with cleaned RMR data.
merge_chamber_rmr_v4(
  fdr_chm_clean = "./FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_Raw_Chamber",
  fdr_rmr_clean = "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA/AIM1_Cleaned_RMR",
  fdr_merge     = "./FLAC_AIM1_DATA/4_AIM1_MERGED_DATA/AIM1_Merged_Chamber_RMR"
)

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


# Oxford ------------------------------------------------------------------

# Extracting frames from Brinno AVI videos.
extract_frames_v1(
  fdr_video = "FLAC_AIM3_DATA/PILOT/video",
  fdr_frame = "FLAC_AIM3_DATA/PILOT/images",
  id_date   = "DLW_Pilot_MAR01_2022_03_04",
  ext_frame = "jpg"
)

# Extract autographer images from hour folders.
extract_autographer_images_v4(
  fdr_load = "FLAC_AIM3_DATA/PILOT/images",
  id = "DLW_Pilot_BAY01"
)

# Create medium and thumbnail images ---

# put name of folder you want to create images for in between the quotation marks
# EXAMPLE: PPAQ_49_2_2014_12_14
img_set <- "DLW_Pilot_MAR01_20220303_01"

# create images
create_oxford_images_v5(
  # fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_load        = "./FLAC_AIM3_DATA/images",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)



# Other ---

# some Oxford IMGs are flipped when created. This corrects them
# make sure IMG set is in respective PPAQ_##/Tri #/ folder
img_set <- ""

unflip_oxford_imgs(folder_day = img_set)



# Noldus ------------------------------------------------------------------

copy_and_move_odx_v1(
  project = "FLAC - Aim 1"
)
copy_and_move_odx_v1(
  project = "FLAC - Aim 2"
)

view_unconverted_videos(path_flv = "./Noldus Observer XT 14/Surface Videos/flv/",
                        path_mp4 = "./Noldus Observer XT 14/Surface Videos/mp4/")

create_videos_list(path_flv = "./Noldus Observer XT 14/Surface Videos/flv/",
                   path_mp4 = "./Noldus Observer XT 14/Surface Videos/mp4/",
                   path_part = "./Noldus Observer XT 14/7_Merging Part Videos/")




# BATCH -------------------------------------------------------------------

# EXAMPLE: PPAQ_49_2_2014_12_14
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
img_set <- ""
create_oxford_images_v4(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fnm_img_set = img_set,
  begin       = NULL,
  end         = NULL
)
