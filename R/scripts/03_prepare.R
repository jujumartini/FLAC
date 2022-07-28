# RUN this line the first time you open this script
source("./R/Scripts/02_functions.R")

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  FLAC AIM 1                               ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                CHAMBER                                 ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
read_chamber_v1(
  fdr_raw = fs::path("FLAC_AIM1_DATA",
                     "1_AIM1_RAW_DATA"),
  fdr_raw_csv = fs::path("FLAC_AIM1_DATA",
                         "1_AIM1_RAW_DATA",
                         "AIM1_RAW_CHAMBER_cSV")
)
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 NOLDUS                                 ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
copy_and_move_odx_v1(
  project = "FLAC - Aim 1"
)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  FLAC AIM 2                               ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 NOLDUS                                 ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
copy_and_move_odx_v1(
  project = "FLAC - Aim 2"
)
view_unconverted_videos(
  path_flv = "./Noldus Observer XT 14/Surface Videos/flv/",
  path_mp4 = "./Noldus Observer XT 14/Surface Videos/mp4/"
)

create_videos_list(
  path_flv  = "./Noldus Observer XT 14/Surface Videos/flv/",
  path_mp4  = "./Noldus Observer XT 14/Surface Videos/mp4/",
  path_part = "./Noldus Observer XT 14/7_Merging Part Videos/"
)

####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 OXFORD                                 ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####  Create medium and thumbnail images  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Put name of folder you want to create images for in between the quotation marks
# EXAMPLE: PPAQ_49_2_2014_12_14
img_set <- ""

create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)

#### Other ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some Oxford IMGs may be flipped due to being worn incorrectly. This unflips
# them.
img_set <- ""
unflip_oxford_imgs(folder_day = img_set)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                  FLAC AIM 3                               ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 BRINNO                                 ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Example: study_subject_date <- "DLW_1111_2022_03_04" DONT INCLUDE THE PART.
study_subject_date <- "DLW_Pilot_MAR01_2022_03_04"
# Extracting frames from Brinno AVI videos.
create_brinno_media(
  fdr_read       = "FLAC_AIM3_DATA/PILOT/brinno",
  fdr_write_img  = "FLAC_AIM3_DATA/PILOT/image",
  fdr_write_vid  = "FLAC_AIM3_DATA/PILOT/video",
  id             = study_subject_date,
  extension      = "jpg",
  timezone       = "America/Chicago"
)

####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                                 OXFORD                                 ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Extract autographer images from hour folders.
extract_autographer_images_v4(
  fdr_load = "FLAC_AIM3_DATA/PILOT/images",
  id       = "DLW_Pilot_BAY01"
)

###  Create medium and thumbnail images  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Put name of folder you want to create images for in between the quotation marks
# EXAMPLE: PPAQ_49_2_2014_12_14
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
#                                     BATCH                                 ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
####                          create_oxford_images                          ----
####::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EXAMPLE: PPAQ_49_2_2014_12_14
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)
img_set <- ""
create_oxford_images(
  fdr_load        = "./OxfordImageBrowser-win32-x64/1_Image Sets to Load",
  fdr_fake_images = "./OxfordImageBrowser-win32-x64/5_Fake Images",
  fdr_img         = img_set
)