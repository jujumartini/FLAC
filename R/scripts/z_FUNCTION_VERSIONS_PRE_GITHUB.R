####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                             SMALL FUNCTIONS                             ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_mode <- function(vec) {
  
  # Statistical mode. If more than one, gives you all values.
  ux <- unique(vec)
  tab <- tabulate(match(vec, ux))
  
  return(ux[tab == max(tab)])
  
}
get_majority <- function(vec) {
  
  # If more than one, gives you the first alpha-numeric value.
  return(names(table(vec))[which.max(table(vec))])
  
}
get_domain_behavior <- function(vec_behavior,
                                vec_environment,
                                key_domain) {
  
  # # ARGUMENTS
  
  # ARG: vec_behavior
  #      Self-explanatory
  # ARG: vec_environment
  #      Self-explanatory
  # ARG: key_domain
  #      What each bucket will be named.
  
  # # TESTING
  
  # vec_behavior <-
  #   sample(c("sports/exercise",
  #            "eating/drinking",
  #            "transportation",
  #            "electronics",
  #            "other - manipulating objects",
  #            "other - carrying load w/ ue",
  #            "other - pushing cart",
  #            "talking - person",
  #            "talking - phone",
  #            "caring/grooming - adult",
  #            "caring/grooming - animal/pet",
  #            "caring/grooming - child",
  #            "caring/grooming - self",
  #            "cleaning",
  #            "c/f/r/m",
  #            "cooking/meal preparation",
  #            "laundry" ,
  #            "lawn&garden",
  #            "leisure based",
  #            "only [p/m] code",
  #            "talking - researchers",
  #            "intermittent activity",
  #            "dark/obscured/oof"),
  #          size = 20) %>%
  #   rep(each = 3)
  # vec_environment <-
  #   sample(c("domestic",
  #            "non-domestic",
  #            "errands/shopping",
  #            "occupation",
  #            "organizational/civic/religious"),
  #          size = 5) %>%
  #   rep(each = 12)
  # vec_environment <-
  #   if_else(vec_behavior == "dark/obscured/oof",
  #           true  = "dark/obscured/oof",
  #           false = vec_environment,
  #           missing = NULL)
  # key_domain <-
  #   c(
  #     "1" = "sport&exercise",
  #     "2" = "leisure",
  #     "3" = "transportation",
  #     "4" = "other",
  #     "5" = "caring&grooming",
  #     "6" = "household",
  #     "7" = "occupation",
  #     "8" = "nca",
  #     "9" = "uncoded"
  #   )
  
  # All Sport&Exercise
  lump_1 <- 
    c("sports/exercise")
  
  # All Leisure.
  lump_2 <- 
    c("eating/drinking",
      "caring/grooming - self")
  
  # Either Leisure or Occupation.
  lump_2_or_7 <- 
    c("electronics",
      "talking - person",
      "talking - phone",
      "leisure based")
  
  # All Transportation
  lump_3 <- 
    c("transportation")
  
  # All Other.
  lump_4 <- 
    c("other - manipulating objects",
      "other - carrying load w/ ue",
      "other - pushing cart")
  
  # Either Caring/Grooming (non-occupational) or Occupation.
  lump_5_or_7 <- 
    c("caring/grooming - adult",
      "caring/grooming - animal/pet",
      "caring/grooming - child")
  
  # Either Household or Occupation.
  lump_6_or_7 <- 
    c("cleaning",
      "c/f/r/m",
      "cooking/meal preparation",
      "laundry",
      "lawn&garden")
  
  # All NCA
  lump_8 <- 
    c("only [p/m] code",
      "talking - researchers",
      "intermittent activity")
  
  # All Uncoded
  lump_9 <- 
    c("dark/obscured/oof")
  
  # Non occupational environment.
  env_1 <- 
    c("domestic",
      "non-domestic",
      "errands/shopping")
  
  # Occupational environment.
  env_2 <- 
    c("occupation",
      "organizational/civic/religious")
  
  bucket <- 
    dplyr::case_when(
      vec_behavior %in% lump_1                                   ~ 1L,
      vec_behavior %in% lump_2                                   ~ 2L,
      vec_behavior %in% lump_2_or_7 & vec_environment %in% env_1 ~ 2L,
      vec_behavior %in% lump_3                                   ~ 3L,
      vec_behavior %in% lump_4                                   ~ 4L,
      vec_behavior %in% lump_5_or_7 & vec_environment %in% env_1 ~ 5L,
      vec_behavior %in% lump_6_or_7 & vec_environment %in% env_1 ~ 6L,
      vec_behavior %in% lump_2_or_7 & vec_environment %in% env_2 ~ 7L,
      vec_behavior %in% lump_5_or_7 & vec_environment %in% env_2 ~ 7L,
      vec_behavior %in% lump_6_or_7 & vec_environment %in% env_2 ~ 7L,
      vec_behavior %in% lump_8                                   ~ 8L,
      vec_behavior %in% lump_9                                   ~ 9L
    )
  
  domain <- 
    dplyr::recode(bucket, !!!key_domain)
  
  return(domain)
  
}
get_domain_posture <- function(vec_posture,
                               key_bucket,
                               key_domain) {
  
  # # ARGUMENTS
  
  # ARG: vec_posture
  #      Self-explanatory
  # ARG: key_bucket
  #      Buckets for each posture code.
  # ARG: key_domain
  #      What each bucket will be named.
  
  # # TESTING
  # vec_posture <-
  #   sample(c("lying",
  #            "sitting",
  #            "crouching/kneeling/squatting",
  #            "standing",
  #            "other - posture",
  #            "intermittent posture",
  #            "walking",
  #            "stepping",
  #            "running",
  #            "ascending stairs",
  #            "descending stairs",
  #            "crouching/squatting",
  #            "cycling",
  #            "other - movement",
  #            "intermittent movement",
  #            "intermittent p/m",
  #            "dark/obscured/oof"),
  #          size = 17) %>%
  #   rep(each = 3)
  # key_bucket <-
  #   pos_bucket_key_v2
  # key_domain <- 
  #   pos_domain_key_v2
  
  bucket <- 
    dplyr::recode(vec_posture, !!!key_bucket)
  
  domain <- 
    dplyr::recode(bucket, !!!key_domain)
  
  return(domain)
  
}
get_duration <- function(tib,
                         .code_type,
                         .source) {
  
  require(vctrs)
  
  # # CHANGES:
  
  # -Update duration function
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: ve_identify_duration
  # ARG: tib
  #      Data frame with colnames of tib before.
  # ARG: .code_type
  #      Self-explanatory
  # ARG: .source
  #      Self-explanatory
  
  # # TESTING
  
  # tib <- 
  #   tibble(
  #     subject  = rep(rlang::int(1001,
  #                               1002,
  #                               2003,
  #                               3004,
  #                               4005),
  #                    each = 450),
  #     visit    = rep(rlang::int(1, 2, 3),
  #                    each = 150,
  #                    times = 5),
  #     datetime = rep(seq.POSIXt(from = lubridate::ymd_hms("19961011_000000"),
  #                               by = 1,
  #                               length.out = 150),
  #                    times = 15),
  #     posture = 
  #       rlang::chr("sitting",
  #                  rlang::chr("sitting",
  #                             "standing",
  #                             "walking",
  #                             NA) %>% 
  #                    rep(each = 563) %>% 
  #                    sample(size = 2249)) %>% 
  #       vctrs::vec_fill_missing(direction = "down"),
  #     behavior =
  #       rlang::chr("only [p/m] code",
  #                  rlang::chr("only [p/m] code",
  #                             "cleaning",
  #                             "electronics",
  #                             "caring/grooming - self",
  #                             "other - manipulating objects",
  #                             NA) %>% 
  #                    rep(each = 375) %>% 
  #                    sample(size = 2249)) %>% 
  #       vctrs::vec_fill_missing(direction = "down"),
  #     behavior_activity = sample(behavior),
  #     intensity =
  #       rlang::chr("light",
  #                  rlang::chr("sedentary",
  #                             "light",
  #                             "mvpa",
  #                             NA) %>% 
  #                    rep(each = 563) %>% 
  #                    sample(size = 2249)) %>% 
  #       vctrs::vec_fill_missing(direction = "down"),
  #     posture_domain = 
  #       rlang::chr("sit",
  #                  rlang::chr("sit",
  #                             "stand",
  #                             "movement",
  #                             NA) %>% 
  #                    rep(each = 563) %>% 
  #                    sample(size = 2249)) %>% 
  #       vctrs::vec_fill_missing(direction = "down"),
  #     behavior_domain = 
  #       rlang::chr("nca",
  #                  rlang::chr("nca",
  #                             "household",
  #                             "leisure",
  #                             NA) %>% 
  #                    rep(each = 563) %>% 
  #                    sample(size = 2249)) %>% 
  #       vctrs::vec_fill_missing(direction = "down"),
  #     environment =
  #       rlang::chr("domestic",
  #                  rlang::chr("domestic",
  #                             "non-domestic",
  #                             "occupation",
  #                             NA) %>% 
  #                    rep(each = 563) %>% 
  #                    sample(size = 2249)) %>% 
  #       vctrs::vec_fill_missing(direction = "down")
  #   )
  # .code_type <- 
  #   "posture"
  # .source <- 
  #   "01"
  
  
  tib_duration <- 
    tib %>% 
    group_by(study, subject, visit) %>% 
    transmute(datetime,
              .data[[.code_type]],
              events = vec_identify_runs(.data[[.code_type]]),
              duration = seq_duration(vct_datetime = datetime,
                                      vct_value = .data[[.code_type]])) %>% 
    group_by(study, subject, visit, events) %>% 
    summarise(
      source     = .source,
      code_type  = .code_type,
      code       = .data[[.code_type]][1],
      start_dttm = min(datetime),
      stop_dttm  = max(datetime) + 1,
      duration   = duration[1],
      .groups = "drop"
    ) %>% 
    relocate(events,
             .after = last_col()) %>% 
    as.data.table()
  
  return(tib_duration)
  
}
get_fpa_read <- function(fdr_read,
                         fdr_write,
                         name_source_1,
                         name_source_2 = NULL,
                         name_merged = NULL,
                         filter_sub) {
  
  # name_source_1 <- 
  #   fld_chm
  # name_source_2 <- 
  #   fld_rmr
  # name_merged <- 
  #   "CHAMBER_RMR"
  # filter_sub <- 
  #   NULL
  
  vct_fdr_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  if (!is_null(name_merged)) {
    
    chk_merged <- 
      !any(
        str_detect(vct_fdr_write,
                   pattern = regex(name_merged,
                                   ignore_case = TRUE))
      )
    if (chk_merged) {
      cli_inform(
        message = 
          c("!" = 'No sub directory with phrase "{name_merged}" found in WRITE directory.',
            "i" = 'Creating sub directory "{name_merged}" to house activity files.')
      )
      fs::dir_create(path = fs::path(fdr_write,
                                     name_merged))
    }
    if (is.null(name_source_2)) {
      cli_abort(c(
        "A merged file folder was supplied but no source_2 was supplied."
      ))
    }
    vct_fpa_read <- 
      fs::dir_ls(
        path        = fdr_read,
        recurse     = FALSE,
        all         = TRUE,
        type        = "directory",
        regexp      = paste(name_source_1, name_source_2, sep = "|"),
        invert      = FALSE,
        fail        = TRUE,
        ignore.case = TRUE
      ) %>% 
      fs::dir_ls(
        type = "file",
        regexp = "\\.feather$"
      ) %>% 
      keep(.p = ~str_detect(path_file(.x),
                            pattern = filter_sub))
  } else {
    chk_source <- 
      !any(
        str_detect(vct_fdr_write,
                   pattern = regex(name_source_1,
                                   ignore_case = TRUE))
      )
    if (chk_source) {
      cli::cli_inform(
        message = 
          c("!" = 'No sub directory with phrase "{name_source_1}" found in WRITE directory.',
            "i" = 'Creating sub directory "{name_source_1}" to house activity files.')
      )
      fs::dir_create(path = fs::path(fdr_write,
                                     name_source_1))
    } 
    vct_fpa_read <- 
      fs::dir_ls(
        path        = fdr_read,
        recurse     = FALSE,
        all         = TRUE,
        type        = "directory",
        regexp      = name_source_1,
        invert      = FALSE,
        fail        = TRUE,
        ignore.case = TRUE
      ) %>% 
      fs::dir_ls(
        type = "file",
        regexp = "\\.csv$|\\.xlsx$"
      ) %>% 
      keep(.p = ~str_detect(path_file(.x),
                            pattern = filter_sub))
  }
  
  return(vct_fpa_read)
  
}
get_fpa_read_noldus <- function(fdr_read,
                                fdr_write,
                                name_activity,
                                name_posture,
                                name_merged = NULL,
                                filter_sub) {
  
  # name_activity <-
  #   "NOLDUS_ACTIVITY"
  # name_posture <-
  #   "NOLDUS_POSTURE"
  # name_merged <- 
  #   "NOLDUS_ACTIVITY_POSTURE"
  
  vct_fdr_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  if (!is_null(name_merged)) {
    
    chk_merged <- 
      !any(
        str_detect(vct_fdr_write,
                   pattern = regex(name_merged,
                                   ignore_case = TRUE))
      )
    if (chk_merged) {
      cli::cli_inform(
        message = 
          c("!" = 'No sub directory with phrase "{name_merged}" found in WRITE directory.',
            "i" = 'Creating sub directory "{name_merged}" to house activity files.')
      )
      fs::dir_create(path = fs::path(fdr_write,
                                     name_merged))
    }
    vct_fpa_read <- 
      fs::dir_ls(
        path        = fdr_read,
        recurse     = FALSE,
        all         = TRUE,
        type        = "directory",
        regexp      = paste(name_activity, name_posture, sep = "|"),
        invert      = FALSE,
        fail        = TRUE,
        ignore.case = TRUE
      ) %>% 
      fs::dir_ls(
        type = "file",
        regexp = "\\.feather$"
      ) %>% 
      keep(.p = ~str_detect(path_file(.x),
                            pattern = filter_sub))
  } else {
    chk_act <- 
      !any(
        str_detect(vct_fdr_write,
                   pattern = regex(name_activity,
                                   ignore_case = TRUE))
      )
    chk_pos <- 
      !any(
        str_detect(vct_fdr_write,
                   pattern = regex(name_posture,
                                   ignore_case = TRUE))
      )
    if (chk_act) {
      cli::cli_inform(
        message = 
          c("!" = 'No sub directory with phrase "{name_activity}" found in WRITE directory.',
            "i" = 'Creating sub directory "{name_activity}" to house activity files.')
      )
      fs::dir_create(path = fs::path(fdr_write,
                                     name_activity))
    } 
    if (chk_pos) {
      cli::cli_inform(
        message = 
          c("!" = 'No sub directory with phrase "{name_posture}" found in WRITE directory.',
            "i" = 'Creating sub directory "{name_posture}" to house activity files.')
      )
      fs::dir_create(path = fs::path(fdr_write,
                                     name_posture))
    }
    vct_fpa_read <- 
      fs::dir_ls(
        path        = fdr_read,
        recurse     = FALSE,
        all         = TRUE,
        type        = "directory",
        regexp      = paste(name_activity, name_posture, sep = "|"),
        invert      = FALSE,
        fail        = TRUE,
        ignore.case = TRUE
      ) %>% 
      fs::dir_ls(
        type = "file",
        regexp = "\\.csv$|\\.xlsx$"
      ) %>% 
      keep(.p = ~str_detect(path_file(.x),
                            pattern = filter_sub))
  }
  
  return(vct_fpa_read)
  
}
seq_events <- function(vec) {
  
  # Repeat the index of values by the duration of each value in vec.
  events <- 
    rle(vec)$lengths %>% 
    seq_along() %>% 
    rep(times = rle(vec)$lengths)
  
  return(events)
  
}
# Repeat the duration of each value in vec by the duration of each value in vec.
seq_duration <- function(vct_datetime,
                         vct_value) {
  
  # require(vctrs)
  
  # vct_datetime <- 
  #   df_img_shp$datetime
  # vct_value <- 
  #   df_img_shp$annotation
  
  df <- 
    data.table::data.table(
      datetime = vct_datetime,
      value = vct_value,
      run = vctrs::vec_identify_runs(vct_value)
    ) %>% 
    group_by(run) %>% 
    # This code treats the time1 value as what occurred between 
    # time1 - 1 second UP TO time1!! Do this for every value EXCEPT the
    # start time.
    mutate(duration = if_else(run == 1L,
                              true = difftime(time1 = datetime[n()],
                                              time2 = datetime[1],
                                              units = "secs"),
                              false = difftime(time1 = datetime[n()],
                                               time2 = datetime[1] - 1,
                                               units = "secs"),
                              missing = NULL)) %>% 
    as.data.table()
  
  # For oxford files if an annotation was applied for only one image.
  df$duration[df$duration == 0] <- 1
  
  return(df$duration)
  
}
vec_unfill <- function(x) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}
initiate_wrangle <- function(fdr_read,
                             fdr_project,
                             filter_sub,
                             filter_loc = NULL,
                             project_only,
                             type,
                             file_source) {
  
  # type <- 
  #   "Clean"
  # file_source <- 
  #   "NOLDUS"
  
  fdr_project <-
    fs::as_fs_path(fdr_project)
  filter_sub <-
    paste0(filter_sub,
           collapse = "|")
  filter_loc <- 
    as.integer(filter_loc)
  if(!is.logical(project_only)) {
    cli_abort(c(
      "x" = "{.arg project_only} is not a logical.",
      "i" = "Class of {.arg project only} supplied is {class(project_only)}"
    ))
  }
  if(project_only & is_empty(fdr_project)) {
    cli_abort(c(
      "x" = "{.arg project_only} set to TRUE but {.arg fdr_project} not specified.",
      "i" = "Make sure {.arg fdr_project} is not NULL."
    ))
  }
  if (!is_empty(fdr_project)) {
    # Just in case.
    fs::dir_create(path = fs::path(fdr_project,
                                   fld_act))
    fs::dir_create(path = fs::path(fdr_project,
                                   fld_pos))
  }
  assign("fnm_read",
         value = "",
         envir = parent.frame())
  assign("fdr_project",
         value = fdr_project,
         envir = parent.frame())
  assign("filter_sub",
         value = filter_sub,
         envir = parent.frame())
  assign("info_flac_aim",
         value = fdr_read %>%
           fs::path_dir() %>%
           str_extract(pattern = "AIM\\d{1}"),
         envir = parent.frame())
  assign("info_function",
         value = type,
         envir = parent.frame())
  assign("info_source",
         value = file_source,
         envir = parent.frame())
  assign("cnt",
         value = 0,
         envir = parent.frame())
  assign("progress_format",
         # value =  "{info_function}ing... | {cli::pb_current}/{cli::pb_total} | [{cli::pb_elapsed}]",
         # value =  "{info_function}ing {str_to_title(info_source)} file {fnm_read} | {cli::pb_current}/{cli::pb_total} | [{cli::pb_elapsed}] | {cli::pb_eta_str}",
         value =  "{info_function}ing {str_to_title(info_source)} file {fnm_read} | {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | [{cli::pb_elapsed}] | {cli::pb_eta_str}",
         envir = parent.frame())
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                        GENERAL/UTILITY FUNCTIONS                        ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_videos_list_v1 <- function(path_flv,
                                  path_mp4,
                                  path_part) {
  
  # path_flv <- "./Noldus Observer XT 14/Surface Videos/flv/"
  # path_mp4 <- "./Noldus Observer XT 14/Surface Videos/mp4/"
  # path_part <- "./Noldus Observer XT 14/7_Merging Part Videos/"
  
  # flv
  list_flv <- list.files(path = path_flv,
                         pattern = ".flv",
                         recursive = T)
  list_flv <- sub(".*/",
                  "",
                  list_flv) %>% 
    sub("\\.[^\\.]*$",
        "",
        .)
  order_flv <- as.numeric(str_sub(list_flv, start = 7, end = 9))
  visit_flv <- as.numeric(str_sub(list_flv, start = 11, end = 11))
  
  df_flv <- tibble(
    visit     = visit_flv,
    ID        = order_flv,
    flv_file  = list_flv
  )
  
  # mp4
  list_mp4 <- list.files(path = path_mp4,
                         pattern = ".mp4",
                         recursive = T)
  list_mp4 <- sub(".*/",
                  "",
                  list_mp4) %>% 
    sub("\\.[^\\.]*$",
        "",
        .)
  order_mp4 <- as.numeric(str_sub(list_mp4, start = 7, end = 9))
  visit_mp4 <- as.numeric(str_sub(list_mp4, start = 11, end = 11))
  
  df_mp4 <- tibble(
    visit     = visit_mp4,
    ID        = order_mp4,
    mp4_file  = list_mp4
  )
  
  # 1st merge & converted column
  df_videos <- full_join(df_flv,
                         df_mp4,
                         by = c("visit", "ID"))
  
  df_videos$converted <- "YES"
  df_videos$converted[is.na(df_videos$flv_file)] <- "YES (No flv found)"
  df_videos$converted[is.na(df_videos$mp4_file)] <- "NO"
  
  # part videos
  list_part_flv <- 
    list.files(path = paste0(path_part,
                             "1_parts/"),
               pattern = ".flv",
               recursive = T) %>% 
    sub("_[^_]*$",
        "",
        .) %>% 
    unique()
  list_part_mp4 <- 
    list.files(path = paste0(path_part,
                             "2_joined/"),
               pattern = ".mp4",
               recursive = T) %>% 
    sub("_[^_]*$",
        "",
        .)
  list_part_fix <- 
    list.files(path = paste0(path_part,
                             "3_fixed/"),
               pattern = ".mp4",
               recursive = T) %>% 
    sub("\\.[^\\.]*$",
        "",
        .)
  
  list_part_fix_2 <- vector(mode = "character",
                            length = length(list_part_flv))
  list_part_fix_2[list_part_flv %in% list_part_fix] <- list_part_fix
  
  order_part <- as.numeric(str_sub(list_part_flv, start = 7, end = 9))
  visit_part <- as.numeric(str_sub(list_part_flv, start = 11, end = 11))
  
  df_part <- tibble(
    visit     = visit_part,
    ID        = order_part,
    flv_file  = list_part_flv,
    mp4_file  = list_part_fix_2,
    converted = "NO (Has parts: NO JOIN, NO FIX)",
    .rows = length(list_part_flv)
  )
  
  df_part$mp4_file[df_part$mp4_file == ""] <- NA
  
  df_part$converted[list_part_flv %in% list_part_mp4] <- 
    "NO (Has parts: YES JOIN, NO FIX)"
  df_part$converted[list_part_flv %in% list_part_fix] <- 
    "YES (Had parts)"
  
  # 2nd merge and re-order
  df_videos_2 <- rbind(df_videos,
                       df_part)
  
  df_videos_2 <- df_videos_2[order(df_videos_2$visit, df_videos_2$ID), ]
  
  # remove duplicate entries for videos that needed to be fixed
  paste0(df_videos_2$visit,
         df_videos_2$ID)
  lgl_dup <- duplicated(paste0(df_videos_2$visit,
                               df_videos_2$ID),
                        fromLast = TRUE)
  df_videos_2 <- df_videos_2[!(lgl_dup), ]
  
  vroom_write(df_videos_2,
              path = "./video_conversion.csv",
              delim = ",")
  
  
  # test <- df_videos_2[is.na(df_videos_2$flv_file) | is.na(df_videos_2$mp4_file), ]
  # df_videos <- df_videos_2[is.na(df_videos_2$flv_file) | is.na(df_videos_2$mp4_file), ]
  # 
  # View(df_videos_2)  
  
}


copy_and_move_odx_v1 <- function(project) {
  
  # # CHANGES:
  
  # -NA
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NA
  
  # ARG: project
  #      Self-explanatory
  
  # # TESTING
  
  # project <- 
  #   "FLAC - Aim 1"
  # project <- 
  #   "FLAC - Aim 2"
  
  switch(
    project,
    "FLAC - Aim 1" = {
      fdr_from <- 
        "./Colorado/Noldus Observer XT 14"
      fdr_to <- 
        "./Colorado/Noldus Observer XT 14/1_ODX"
      sub_vis_pattern <- 
        "CO_\\d{4}_\\w{2}"
    },
    "FLAC - Aim 2" = {
      fdr_from <- 
        "./Noldus Observer XT 14"
      fdr_to <- 
        "./Noldus Observer XT 14/1_ODX"
      sub_vis_pattern <- 
        "\\d{4}v\\d{1}"
    }
  )
  
  fls_odx <- 
    list.files(path = fdr_from,
               pattern = ".odx",
               full.names = TRUE,
               recursive = TRUE,
               ignore.case = TRUE) %>% 
    # Only retrieve files that have a "{subject}v{visit number}" sequence.
    str_subset(pattern = regex(sub_vis_pattern,
                               ignore_case = TRUE),
               negate = FALSE) %>% 
    # Keep odx files from irrelevant folders.
    str_subset(pattern = "1_ODX|Master|z_ARCHIVE|6_Training",
               negate = TRUE) %>% 
    str_subset(pattern = "3_Posture|4_Activity",
               negate = FALSE) %>% 
    # Remove ODX files that contain multiple observations if the coder forgot to
    # only export one observation
    str_subset(pattern = "_\\w{2} - ",
               negate = TRUE)
  
  # fs::dir_ls(path = "./Noldus Observer XT 14",
  #            # all = TRUE,
  #            recurse = TRUE, # can also be a digit to determine how far to recurse.
  #            type = "file",
  #            glob = "*.odx",
  #            # regexp = "3_Posture|4_Activity",
  #            ignore.case = TRUE,
  #            invert = FALSE,
  #            fail = TRUE) %>% 
  # # Only retrieve files that have a "{subject}v{visit number}" sequence.
  # str_subset(pattern = regex("\\d{4}v\\d{1}",
  #                            ignore_case = TRUE),
  #            negate = FALSE) %>% 
  # # Keep odx files from irrelevant folders.
  # str_subset(pattern = "1_ODX|Master|z_ARCHIVE|6_Training",
  #            negate = TRUE) %>% 
  # str_subset(pattern = "3_Posture|4_Activity",
  #            negate = FALSE)
  
  vec_copied <- 
    vector(mode = "logical",
           length = length(fls_odx))
  
  # fls_odx[1]
  # file_copy(path = fls_odx[1],
  #           new_path = fpa_to,
  #           overwrite = TRUE)
  
  for (i in seq_along(fls_odx)) {
    
    # The if statement makes sure the numbers can be seen within a screen 80
    # characters long.
    if (i == 30 |
        (i > 3 & i < 88) & (i - 30) %% 27 == 0 |
        i == 108 |
        (i > 88 & i < 990) & (i - 108) %% 20 == 0 |
        i == 1006 |
        i > 990 & (i - 1006) %% 16 == 0) {
      
      message(
        i,
        appendLF = TRUE
      )
      
    } else {
      
      message(
        i, " ",
        appendLF = FALSE
      )
      
    }
    
    # Use base package as it gives a logical for when it was copied over. Use lgl
    # later for a message.
    lgl_copied <- 
      file.copy(from = fls_odx[i],
                to   = fdr_to,
                overwrite = FALSE,
                recursive = FALSE,
                copy.mode = TRUE,
                copy.date = TRUE)
    
    vec_copied[i] <- 
      lgl_copied
    
  }
  
  message(appendLF = TRUE)
  
  fls_moved <- 
    fls_odx[vec_copied] %>% 
    fs::as_fs_path() %>% 
    fs::path_file() %>% 
    paste0("\n")
  msg_filler <- 
    ((80 - str_length(" DONE ")) / 2) %>% 
    as.integer() %>% 
    strrep("-",
           times = .)
  
  if (any(vec_copied)) {
    
    message(
      paste0(msg_filler, " DONE ", msg_filler), "\n",
      "\n",
      length(fls_moved), " ODX files copied over. The following files were copied over:\n",
      "\n",
      fls_moved,
      "\n",
      strrep("-",
             times = 80),
      appendLF = TRUE
    )
    
  } else {
    
    message(
      paste0(msg_filler, " DONE ", msg_filler), "\n",
      "\n",
      'No new files copied over. "1_ODX" folder is up to date,\n',
      "\n",
      strrep("-",
             times = 80),
      appendLF = TRUE
    )
    
  }
}



summarize_coding_times_v1 <- function() {
  
  project <- 
    "2017 Strath R01 FLAC"
  
  data_source <- 
    "Teleform/RAW"
  
  fdr_data <- 
    paste("S:/_R_CHS_Research/PAHRL/Student Access/4_Research",
          project,
          "Data",
          data_source,
          sep = "/")
  
  fnm_qry <- 
    "Aim2EstCodingTimeQry.xlsx"
  
  
  test <- 
    tib_coding_times <- 
    readxl::read_xlsx(
      path = paste(fdr_data,
                   fnm_qry,
                   sep = "/"),
      progress = FALSE
    )
  
  colnames(test) <- 
    colnames(test) %>% 
    str_to_lower()
  
  t2 <- 
    test %>% 
    filter(!is.na(v1assigned))
  
  colnames(t2)
  
  t3 <- 
    t2 %>% 
    pivot_longer(cols = !studyid,
                 names_to = c("visit",
                              ".value"),
                 names_pattern = "(v\\d)(.*)") %>% 
    pivot_longer(cols = !(studyid:assigned),
                 names_to = c("schema",
                              ".value"),
                 names_pattern = "(pos|act)(.*)") %>% 
    mutate(schema    = recode(schema,
                              "pos" = "posture",
                              "act" = "activity"),
           completed = comp,
           coder     = assigned,
           .keep = "unused") %>% 
    select(studyid,
           visit,
           schema,
           coder,
           completed,
           time)
  
  
  tib_count_total_na <- 
    t3 %>% 
    filter(completed == TRUE) %>% 
    count(time,
          name = "na_count") %>% 
    filter(is.na(time)) %>% 
    select(na_count)
  tib_count_total <- 
    t3 %>% 
    filter(completed == TRUE) %>% 
    summarise(
      stratum   = "TOTAL",
      schema    = NA_character_,
      coder     = NA_character_,
      completed = n(),
      sum       = sum(time,
                      na.rm = TRUE),
      average   = mean(time,
                       na.rm = TRUE),
      stddev    = sd(time,
                     na.rm = TRUE),
      median    = median(time,
                         na.rm = TRUE)
    ) %>% 
    bind_cols(tib_count_total_na) %>% 
    relocate(na_count,
             .after = completed) %>% 
    mutate(n = completed - na_count,
           .after = na_count)
  
  tib_count_schema_na <- 
    t3 %>% 
    filter(completed == TRUE) %>% 
    count(schema, time,
          name = "na_count") %>% 
    filter(is.na(time)) %>% 
    select(schema, na_count)
  tib_count_schema <- 
    t3 %>% 
    filter(completed == TRUE) %>% 
    group_by(schema) %>% 
    summarise(
      stratum   = "BY SCHEMA",
      coder     = NA_character_,
      completed = n(),
      sum       = sum(time,
                      na.rm = TRUE),
      average   = mean(time,
                       na.rm = TRUE),
      stddev    = sd(time,
                     na.rm = TRUE),
      median    = median(time,
                         na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    full_join(tib_count_schema_na,
              by = "schema") %>% 
    relocate(na_count,
             .after = completed) %>% 
    mutate(na_count = replace_na(na_count,
                                 replace = 0),
           n = completed - na_count,
           .after = na_count)
  
  tib_count_coder_na <- 
    t3 %>% 
    filter(completed == TRUE) %>% 
    count(coder, schema, time,
          name = "na_count") %>% 
    filter(is.na(time)) %>% 
    select(schema, coder, na_count)
  tib_count_coder <- 
    t3 %>% 
    filter(completed == TRUE) %>% 
    group_by(schema, coder) %>% 
    summarise(
      stratum = "BY CODER",
      completed = n(),
      sum       = sum(time,
                      na.rm = TRUE),
      average   = mean(time,
                       na.rm = TRUE),
      stddev    = sd(time,
                     na.rm = TRUE),
      median    = median(time,
                         na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    full_join(tib_count_coder_na,
              c("schema", "coder")) %>% 
    relocate(na_count,
             .after = completed) %>% 
    mutate(na_count = replace_na(na_count,
                                 replace = 0),
           n = completed - na_count,
           .after = na_count)
  
  tib_summary <- 
    bind_rows(
      tib_count_total,
      tib_count_schema,
      tib_count_coder
    ) %>% 
    arrange(!is.na(schema),
            schema,
            !is.na(coder),
            coder)
  
  tbl_summary <- 
    bind_rows(
      tib_count_total,
      tib_count_total,
      tib_count_schema,
      tib_count_schema,
      tib_count_coder
    ) %>% 
    arrange(!is.na(schema),
            schema,
            !is.na(coder),
            coder)
  
  ind <- 
    which(
      duplicated(tbl_summary)
    )
  tbl_summary[ind, ] <- NA
  
  tbl_summary <- 
    tbl_summary %>% 
    tidyr::fill(stratum,
                .direction = "down") %>% 
    group_by(stratum) %>% 
    tidyr::fill(schema,
                .direction = "down") %>% 
    ungroup() %>% 
    mutate(across(.cols = stratum:schema,
                  .fns = ~ unfill_vec(.x)))
  
  ind <- 
    (tbl_summary$schema == "posture") %>% 
    which()
  
  tbl_summary <- 
    tbl_summary %>% 
    add_row(.before = ind) %>% 
    mutate(across(.cols = sum:last_col(),
                  .fns = ~ round(.x,
                                 digits = 2))) %>% 
    mutate(across(.cols = everything(),
                  .fns = ~ replace_na(.x,
                                      replace = "")))
  
  # 1100 x 600
  tbl_summary %>% 
    formattable::formattable(align = c(rep("l",
                                           times = 3),
                                       rep("r",
                                           times = ncol(tbl_summary) - 3)))
  
}

view_unconverted_videos_v1 <- function() {
  
  list_flv <- list.files(path = "./Surface Videos/flv/",
                         pattern = ".flv",
                         recursive = T)
  list_flv <- sub(".*/",
                  "",
                  list_flv)
  order_flv <- as.numeric(str_sub(list_flv, start = 7, end = 9))
  visit_flv <- as.numeric(str_sub(list_flv, start = 11, end = 11))
  
  df_flv <- data.frame(flv_file  = list_flv,
                       visit     = visit_flv,
                       ID        = order_flv)
  
  df_flv <- df_flv[order(df_flv$visit, df_flv$ID), ]
  
  list_mp4 <- list.files(path = "./Surface Videos/mp4/",
                         pattern = ".mp4",
                         recursive = T)
  list_mp4 <- sub(".*/",
                  "",
                  list_mp4)
  order_mp4 <- as.numeric(str_sub(list_mp4, start = 7, end = 9))
  visit_mp4 <- as.numeric(str_sub(list_mp4, start = 11, end = 11))
  
  df_mp4 <- data.frame(mp4_file  = list_mp4,
                       visit     = visit_mp4,
                       ID        = order_mp4)
  
  df_mp4 <- df_mp4[order(df_mp4$visit, df_mp4$ID), ]
  
  
  df_videos <- left_join(df_flv,
                         df_mp4)
  
  View(df_videos)
  
}

view_unconverted_videos_v2 <- function(path_flv,
                                       path_mp4) {
  
  # see if we can only see the ones that need converting.
  # path_flv <- "./Noldus Observer XT 14/Surface Videos/flv/"
  # path_mp4 <- "./Noldus Observer XT 14/Surface Videos/mp4/"
  
  # flv
  list_flv <- 
    path_flv %>% 
    list.files(pattern = ".flv",
               recursive = TRUE) %>% 
    str_extract(pattern = "[^/]*$")
  order_flv <- 
    list_flv %>% 
    str_sub(start = 7,
            end = 9) %>% 
    as.numeric()
  visit_flv <- 
    list_flv %>% 
    str_sub(start = 11,
            end = 11) %>% 
    as.numeric()
  tib_flv <- 
    tibble(
      visit    = visit_flv,
      ID       = order_flv,
      flv_file = list_flv,
      .rows    = nrow(list_flv)
    )
  
  # mp4
  list_mp4 <- 
    path_mp4 %>% 
    list.files(pattern = ".mp4",
               recursive = TRUE) %>% 
    str_extract(pattern = "[^/]*$")
  order_mp4 <- 
    list_mp4 %>% 
    str_sub(start = 7,
            end = 9) %>% 
    as.numeric()
  visit_mp4 <- 
    list_mp4 %>% 
    str_sub(start = 11,
            end = 11) %>% 
    as.numeric()
  tib_mp4 <- 
    tibble(
      visit    = visit_mp4,
      ID       = order_mp4,
      mp4_file = list_mp4,
      .rows    = nrow(list_mp4)
    )
  
  # Join. Reorder based on ID.
  tib_videos <- 
    full_join(
      tib_flv,
      tib_mp4,
      by = c("visit", "ID")
    )
  # tib_videos <- 
  #   tib_videos[order(tib_videos$visit, tib_videos$ID), ]
  # tib_videos <- 
  #   tib_videos[is.na(tib_videos$flv_file) | is.na(tib_videos$mp4_file), ]
  
  View(tib_videos)
  
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                            PREPARE FUNCTIONS                            ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_oxford_images_v4 <- function(fdr_load,
                                    fdr_fake_images,
                                    fnm_img_set,
                                    begin = NULL,
                                    end = NULL) {
  
  # This version makes includes automatically checking for "fake" images.
  
  # fdr_load <- "./OxfordImageBrowser/1_Image Sets to Load"
  # fdr_fake_images <- "./OxfordImageBrowser/6_Fake Images"
  # fnm_img_set <- "PPAQ_19_1_2015_05_23"
  # begin <- NULL
  # end <- NULL
  
  lgl_loader_img_set <- 
    list.dirs(path = fdr_load,
              recursive = FALSE) %>% 
    paste("/",
          fnm_img_set,
          sep = "") %>% 
    fs::is_dir()
  
  if (sum(lgl_loader_img_set) > 1) {
    
    stop(
      "\n",
      fnm_img_set, " has been found in another loader's folder.\n",
      "Double check to make sure only one loader is cleaning/preparing ", fnm_img_set, ".",
      call. = FALSE
    )
    
  }
  
  fdr_loader_img_set <- 
    list.dirs(path = fdr_load,
              recursive = FALSE) %>% 
    paste("/",
          fnm_img_set,
          sep = "")
  fdr_loader_img_set <- fdr_loader_img_set[lgl_loader_img_set]
  
  # create medium and thumbnail folders
  dir.create(
    path = paste(fdr_loader_img_set,
                 "medium",
                 sep = "/")
  )
  dir.create(
    path = paste(fdr_loader_img_set,
                 "thumbnail",
                 sep = "/")
  )
  
  # list images
  fls_imgs <-
    list.files(
      path    = fdr_loader_img_set,
      pattern = ".JPG"
    )
  
  if (is.null(begin)) {
    
    begin <- 1L
    
  } else {
    
    begin <- as.integer(begin)
    
  }
  
  if (is.null(end)) {
    
    end <- length(fls_imgs)
    
  } else {
    
    end <- as.integer(end)
    
  }
  
  create_img <- function(directory,
                         img_name,
                         tib_fake_images) {
    
    img_original <-
      load.image(
        file = paste(directory,
                     img_name,
                     sep = "/")
      )
    img_medium <- 
      resize(
        img_original,
        size_x = 864,
        size_y = 645
      )
    img_thumb <- 
      resize(
        img_original,
        size_x = 100,
        size_y = 87
      )
    
    # for some reason file extension has to be lowercase
    substr(fnm_img, 35, 37) <- 
      tolower(substr(fnm_img, 35, 37))
    
    # save images (imager)
    save.image(
      img_medium,
      file = paste(fdr_loader_img_set,
                   "medium",
                   fnm_img,
                   sep = "/")
    )
    save.image(
      img_thumb,
      file = paste(fdr_loader_img_set,
                   "thumbnail",
                   fnm_img,
                   sep = "/")
    )
    
    return(tib_fake_images)
    
  }
  
  tib_fake_images <- 
    tibble(
      img_number = 1L,
      img_name = ""
    )
  tib_fake_images <- 
    tib_fake_images[-1, ]
  
  # create medium and thumbnail for each image (imager)
  for (i in seq_along(fls_imgs)) {
    
    # The if statement makes sure the numbers can be seen within a screen 80
    # characters long.
    if (i == 30 |
        (i > 3 & i < 88) & (i - 30) %% 27 == 0 |
        i == 108 |
        (i > 88 & i < 990) & (i - 108) %% 20 == 0 |
        i == 1006 |
        i > 990 & (i - 1006) %% 16 == 0) {
      
      message(
        i,
        appendLF = TRUE
      )
      
    } else {
      
      message(
        i, " ",
        appendLF = FALSE
      )
      
    }
    
    if (i < begin |
        i > end) {
      
      next()
      
    }
    
    fnm_img <- fls_imgs[i]
    
    tib_fake_images <- 
      tryCatch(
        expr = create_img(directory = fdr_loader_img_set,
                          img_name = fnm_img,
                          tib_fake_images = tib_fake_images),
        error = function(cnd) {
          tib_fake_images <<-
            tib_fake_images %>%
            dplyr::add_row(img_number = i,
                           img_name   = fnm_img)
          return(tib_fake_images)
        }
      )
    
    next()
    
  }
  
  if (length(tib_fake_images$img_number) == 0) {
    
    message(
      "\n",
      "---------------------------------CREATING DONE--------------------------------\n",
      "            Oxford Images for ",fnm_img_set," have been created.\n",
      "\n",
      "No fake images have been found."
    )
    
    return()
    
  }
  
  # Remove fake images from image_set.
  for (i in seq_along(tib_fake_images$img_name)) {
    
    fnm_img_fake <- tib_fake_images$img_name[i]
    
    fs::file_delete(
      path = paste(fdr_loader_img_set,
                   fnm_img_fake,
                   sep = "/")
    ) 
    
  }
  
  fnm_img_fake <- 
    paste0(
      "fake_images_",
      fnm_img_set,
      ".csv"
    )
  vroom_write(
    tib_fake_images,
    path = paste(fdr_fake_images,
                 fnm_img_fake,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
  # Determine if there are sequences.
  tib_fake_images$datetime <- 
    tib_fake_images$img_name %>% 
    str_sub(start = 18,
            end = 32) %>% 
    lubridate::ymd_hms()
  
  lgl_fake_before <- 
    tib_fake_images$img_number %in% (tib_fake_images$img_number - 1)
  lgl_fake_after <- 
    tib_fake_images$img_number %in% (tib_fake_images$img_number + 1)
  lgl_fake_seq <- 
    lgl_fake_before | lgl_fake_after
  
  ind_fake_non_seq <- 
    which(
      lgl_fake_seq == FALSE
    )
  
  if (length(ind_fake_non_seq) == length(lgl_fake_seq)) {
    
    # No fake images are sequential! :)
    warning(
      "None of the fake images are sequential; no significant time loss.\n",
      'No "time_loss.csv" was created.',
      call. = FALSE
    )
    
  } else if (length(ind_fake_non_seq) == 0) {
    
    # All fake images are sequential. :)
    warning(
      "Fake images have been found that are sequential; possible significant time loss:",
      call. = FALSE
    )
    
    # Check if there is a gap within a sequence. If there is, get new sequence
    # anchors that do not include the gap (this gap would not be included while
    # coding anyways as coders would put a break in the code line).
    ind_fake_gap <- suppressWarnings(
      which(c(diff.POSIXt(tib_fake_images$datetime,
                          lag = 1,
                          differences = 1), 1) %>% 
              as.numeric(units = "mins") >=
              15)
    )
    
    if (length(ind_fake_gap) > 0) {
      
      ind_fake_seq_anchors <- 
        c(1,
          ind_fake_gap,
          ind_fake_gap + 1,
          length(tib_fake_images$datetime)) %>% 
        sort()
      ind_fake_seq_beg <- 
        ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 1]
      ind_fake_seq_end <- 
        ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 0]
      
      diff_img_seq <- 
        difftime( time1 = tib_fake_images$datetime[ind_fake_seq_end],
                  time2 = tib_fake_images$datetime[ind_fake_seq_beg],
                  units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[ind_fake_seq_beg],
                                        tib_fake_images$img_number[ind_fake_seq_end],
                                        sep = " -- "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        ) 
      
      for (i in seq_along(diff_img_seq)) {
        
        warning(
          "One sequence of fake images is ", diff_img_seq[i], " minutes long.",
          call. = FALSE
        )
        
      }
    } else {
      
      diff_img_seq <- 
        difftime(time1 = tib_fake_images$datetime[length(tib_fake_images$datetime)],
                 time2 = tib_fake_images$datetime[1],
                 units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[1],
                                        tib_fake_images$img_number[nrow(tib_fake_images)],
                                        sep = " - "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        )
      
      warning(
        "Only one sequence found which is ", diff_img_seq, " minutes.",
        call. = FALSE
      )
      
    }
    
    fnm_fake_time <- 
      paste0(
        "time_loss_",
        fnm_img_set,
        ".csv"
      )
    vroom_write(
      tib_fake_time,
      path = paste(fdr_fake_images,
                   fnm_fake_time,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
  } else {
    
    # Not all fake images are sequential. :( Go through and filter out which
    # fake images are sequential.
    warning(
      "Fake images have been found that are sequential; possible significant time loss:",
      call. = FALSE
    )
    
    ind_fake_non_seq_anchors <-
      which(
        c(diff(ind_fake_non_seq), 1) > 1
      )
    
    if (ind_fake_non_seq[1] == 1) {
      
      # First two images are not sequential.
      if (length(ind_fake_non_seq_anchors) > 1) {
        
        # More than one sequential sequence. Loop through to get all anchors.
        for (i in seq_along(ind_fake_non_seq_anchors)) {
          
          ind_fake_seq_anchors <- 
            c(ind_fake_non_seq[ind_fake_non_seq_anchors[i]] + 1, 
              ind_fake_non_seq[ind_fake_non_seq_anchors[i] + 1] - 1)
          
          if (i == 1) {
            
            lst_seq_anchors <- 
              list(ind_fake_seq_anchors)
            
          } else if (i > 1) {
            
            lst_seq_anchors[[i]] <- ind_fake_seq_anchors
            
          }
        }
        
        ind_fake_seq_anchors <- 
          unlist(lst_seq_anchors)
        
      } else {
        
        ind_fake_seq_anchors <- 
          c(ind_fake_non_seq[ind_fake_non_seq_anchors] + 1, 
            ind_fake_non_seq[ind_fake_non_seq_anchors + 1] - 1)
        
      }
    } else if (ind_fake_non_seq[1] != 1) {
      
      # First two images are sequential.
      if (length(ind_fake_non_seq_anchors) > 1) {
        
        # More than one sequential sequence. Loop through to get all anchors.
        for (i in seq_along(ind_fake_non_seq_anchors)) {
          
          if (i == 1) {
            
            ind_fake_seq_anchors <- 
              c(1, # This is the extra step needed if ind_fake_non_seq[1] != 1.
                ind_fake_non_seq[ind_fake_non_seq_anchors[i]] - 1, # Extra step.
                ind_fake_non_seq[ind_fake_non_seq_anchors[i]] + 1, 
                ind_fake_non_seq[ind_fake_non_seq_anchors[i] + 1] - 1)
            lst_seq_anchors <- 
              list(ind_fake_seq_anchors)
            
          } else if (i > 1) {
            
            ind_fake_seq_anchors <- 
              c(ind_fake_non_seq[ind_fake_non_seq_anchors[i]] + 1, 
                ind_fake_non_seq[ind_fake_non_seq_anchors[i] + 1] - 1)
            lst_seq_anchors[[i]] <- ind_fake_seq_anchors
            
          }
        }
        
        ind_fake_seq_anchors <- 
          unlist(lst_seq_anchors)
        
      } else {
        
        ind_fake_seq_anchors <- 
          c(1, # This is the extra step needed if ind_fake_non_seq[1] != 1.
            ind_fake_non_seq[ind_fake_non_seq_anchors] - 1, # Extra step.
            ind_fake_non_seq[ind_fake_non_seq_anchors] + 1, 
            ind_fake_non_seq[ind_fake_non_seq_anchors + 1] - 1)
        
      }
    }
    
    ind_fake_seq_beg <- 
      ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 1]
    ind_fake_seq_end <- 
      ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 0]
    
    # Check if there is a gap within a sequence. If there is, get new sequence
    # anchors that do not include the gap (this gap would not be included while
    # coding anyways as coders would put a break in the code line).
    ind_fake_gap <- suppressWarnings(
      which(c(diff.POSIXt(tib_fake_images$datetime,
                          lag = 1,
                          differences = 1), 1) %>% 
              as.numeric(units = "mins") >=
              15) %>% 
        as_tibble() %>% 
        filter(value > ind_fake_seq_beg &
                 value < ind_fake_seq_end) %>% 
        unlist(use.names = FALSE)
    )
    
    if (length(ind_fake_gap) > 0) {
      
      ind_fake_seq_anchors_2 <- 
        c(ind_fake_seq_anchors,
          ind_fake_gap,
          ind_fake_gap + 1) %>% 
        sort()
      ind_fake_seq_beg_2 <- 
        ind_fake_seq_anchors_2[seq_along(ind_fake_seq_anchors_2) %% 2 == 1]
      ind_fake_seq_end_2 <- 
        ind_fake_seq_anchors_2[seq_along(ind_fake_seq_anchors_2) %% 2 == 0]
      
      diff_img_seq <- 
        difftime( time1 = tib_fake_images$datetime[ind_fake_seq_end_2],
                  time2 = tib_fake_images$datetime[ind_fake_seq_beg_2],
                  units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[ind_fake_seq_beg_2],
                                        tib_fake_images$img_number[ind_fake_seq_end_2],
                                        sep = " -- "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        ) 
      
    } else {
      
      diff_img_seq <- 
        difftime( time1 = tib_fake_images$datetime[ind_fake_seq_end],
                  time2 = tib_fake_images$datetime[ind_fake_seq_beg],
                  units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[ind_fake_seq_beg],
                                        tib_fake_images$img_number[ind_fake_seq_end],
                                        sep = " -- "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        ) 
      
    }
    
    fnm_fake_time <- 
      paste0(
        "time_loss_",
        fnm_img_set,
        ".csv"
      )
    vroom_write(
      tib_fake_time,
      path = paste(fdr_fake_images,
                   fnm_fake_time,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    for (i in seq_along(diff_img_seq)) {
      
      warning(
        "One sequence of fake images is ", diff_img_seq[i], " minutes long.",
        call. = FALSE
      )
      
    }
  }
  
  message(
    "\n",
    "---------------------------------CREATING DONE--------------------------------\n",
    "            Oxford Images for ",fnm_img_set," have been created.\n",
    "\n",
    "Fake images have been found! They have been deleted from the image set.\n",
    "Reference warnings, fake_images.csv and time_loss.csv in:\n", 
    fdr_fake_images, "\n"
  ) 
  
}

create_oxford_images_v5 <- function(fdr_load,
                                    fdr_fake_images,
                                    fdr_img) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -Update to most recent object naming scheme as of 2022 March.
  # -See if I can remove the begin & end arguments since they arent really used anymore.
  # -Left the fake images code alone so if it is needed then an error will pop
  #  up which will force me to update it. As of now I don't think it needs to be.
  # -Change to using magick package as it is 0.3 seconds faster and meshes with
  #  other packages pretty well.
  # -Force Brinno images (1920x1080) to match Oxford medium/thumbnail sizes.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -NULL
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_load
  #      File directory where image set to load are located.
  # ARG: fdr_fake_images
  #      Don't know if this is needed anymore.
  # ARG: fnm_img_set
  #      Name of the image set folder.
  # ARG: begin
  #      Don't know if this is needed anymore.
  # ARG: end
  #      Don't know if this is needed anymore.
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::  
  fdr_load <-
    "./FLAC_AIM3_DATA/images"
  fdr_fake_images <- 
    "./OxfordImageBrowser-win32-x64/6_Fake Images"
  fdr_img <- 
    "DLW_Pilot_BAY01_2022_03_10"
  
  fdr_load_img <- 
    fs::path(fdr_load,
             fdr_img)
  chk_img_set <- 
    rlang::is_empty(fdr_load_img)
  
  if (chk_img_set) {
    stop("IMG SET FOLDER NOT FOUND.")
  }
  
  # create medium and thumbnail folders
  fs::dir_create(
    path = fs::path(fdr_load_img,
                    "medium")
  )
  fs::dir_create(
    path = fs::path(fdr_load_img,
                    "thumbnail")
  )
  
  # list images
  vct_fpa_img <- 
    fs::dir_ls(
      path = fdr_load_img,
      recurse = TRUE,
      type = "file"
    )
  
  create_img <- function(fpt,
                         tib_fake_images) {
    
    fpt <-
      vct_fpa_img[i]
    ext <- 
      fs::path_ext(fpt)
    fnm <- 
      fpt %>% 
      fs::path_file() %>% 
      fs::path_ext_set(ext = stringr::str_to_lower(ext))
    
    # Autographer files are initially 2592x1936. Brinno is 1920x1080.
    img_original <- 
      magick::image_read(path = fpt)
    
    # Medium image. EITHER:
    img_original %>% 
      # Force resize even if original ratio is lost ("864x645!") OR
      # magick::image_resize(geometry = "864x645!") %>%
      # Fill with empty pixels (image_resize() %>% image_extent()). 
      magick::image_resize(geometry = "864x645") %>%
      magick::image_extent(geometry = "864x645",
                           gravity = "center",
                           color = "none") %>% 
      magick::image_write(
        path = fs::path(fdr_load_img,
                        "medium",
                        fnm)
      )
    # Thumbnail image. EITHER:
    img_original %>% 
      # Force resize even if original ratio is lost ("100x87!") OR
      # magick::image_resize(geometry = "100x87!") %>%
      # Fill with empty pixels (image_resize() %>% image_extent()). 
      magick::image_resize(geometry = "100x87") %>%
      magick::image_extent(geometry = "100x87",
                           gravity = "center",
                           color = "none") %>% 
      magick::image_write(
        path = fs::path(fdr_load_img,
                        "thumbnail",
                        fnm)
      )
    
    # dir_create(fdr_img,
    #            "test")
    # bench <- 
    # microbenchmark::microbenchmark(
    #   magick = 
    #     magick::image_read(fpt) %>% 
    #     magick::image_resize(geometry = "864x645") %>% 
    #     magick::image_resize(geometry = "100x87") %>% 
    #     magick::image_write(path = fs::path(fdr_img,
    #                                         "test",
    #                                         fs::path_file(fpt))),
    #   imager = 
    #     imager::load.image(fpt) %>% 
    #     imager::resize(size_x = 864,
    #                    size_y = 645) %>% 
    #     imager::resize(size_x = 100,
    #                    size_y = 87) %>% 
    #     imager::save.image(fs::path(fdr_img,
    #                                 "test",
    #                                 fs::path_file(fpt))),
    #   times = 100,
    #   unit = "secs"
    # )
    
    return(tib_fake_images)
    
  }
  
  tib_fake_images <- 
    tibble(
      img_number = integer(),
      img_name = character()
    )
  
  # cli_alert_info("Creating Oxford images ")
  progress_format <- 
    "Creating Oxford images {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | [{cli::pb_elapsed}] | {cli::pb_eta_str}"
  
  # create medium and thumbnail for each image (imager)
  for (i in cli_progress_along(vct_fpa_img,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_img <-
      vct_fpa_img[i]
    
    tib_fake_images <- 
      tryCatch(
        expr = create_img(fpt = fpa_img,
                          tib_fake_images = tib_fake_images),
        error = function(cnd) {
          tib_fake_images <<-
            tib_fake_images %>%
            dplyr::add_row(img_number = i,
                           img_name   = fs::path_file(fpa_img))
          return(tib_fake_images)
        }
      )
    
    next()
    
  }
  
  cli_progress_done()
  
  if (length(tib_fake_images$img_number) == 0) {
    
    message(
      "\n",
      "---------------------------------CREATING DONE--------------------------------\n",
      "            Oxford Images for ",fdr_img," have been created.\n",
      "\n",
      "No fake images have been found."
    )
    
    return()
    
  }
  
  # Remove fake images from image_set.
  for (i in seq_along(tib_fake_images$img_name)) {
    
    fnm_img_fake <- tib_fake_images$img_name[i]
    
    fs::file_delete(
      path = paste(fdr_loader_img_set,
                   fnm_img_fake,
                   sep = "/")
    ) 
    
  }
  
  fnm_img_fake <- 
    paste0(
      "fake_images_",
      fnm_img_set,
      ".csv"
    )
  vroom_write(
    tib_fake_images,
    path = paste(fdr_fake_images,
                 fnm_img_fake,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
  # Determine if there are sequences.
  tib_fake_images$datetime <- 
    tib_fake_images$img_name %>% 
    str_sub(start = 18,
            end = 32) %>% 
    lubridate::ymd_hms()
  
  lgl_fake_before <- 
    tib_fake_images$img_number %in% (tib_fake_images$img_number - 1)
  lgl_fake_after <- 
    tib_fake_images$img_number %in% (tib_fake_images$img_number + 1)
  lgl_fake_seq <- 
    lgl_fake_before | lgl_fake_after
  
  ind_fake_non_seq <- 
    which(
      lgl_fake_seq == FALSE
    )
  
  if (length(ind_fake_non_seq) == length(lgl_fake_seq)) {
    
    # No fake images are sequential! :)
    warning(
      "None of the fake images are sequential; no significant time loss.\n",
      'No "time_loss.csv" was created.',
      call. = FALSE
    )
    
  } else if (length(ind_fake_non_seq) == 0) {
    
    # All fake images are sequential. :)
    warning(
      "Fake images have been found that are sequential; possible significant time loss:",
      call. = FALSE
    )
    
    # Check if there is a gap within a sequence. If there is, get new sequence
    # anchors that do not include the gap (this gap would not be included while
    # coding anyways as coders would put a break in the code line).
    ind_fake_gap <- suppressWarnings(
      which(c(diff.POSIXt(tib_fake_images$datetime,
                          lag = 1,
                          differences = 1), 1) %>% 
              as.numeric(units = "mins") >=
              15)
    )
    
    if (length(ind_fake_gap) > 0) {
      
      ind_fake_seq_anchors <- 
        c(1,
          ind_fake_gap,
          ind_fake_gap + 1,
          length(tib_fake_images$datetime)) %>% 
        sort()
      ind_fake_seq_beg <- 
        ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 1]
      ind_fake_seq_end <- 
        ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 0]
      
      diff_img_seq <- 
        difftime( time1 = tib_fake_images$datetime[ind_fake_seq_end],
                  time2 = tib_fake_images$datetime[ind_fake_seq_beg],
                  units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[ind_fake_seq_beg],
                                        tib_fake_images$img_number[ind_fake_seq_end],
                                        sep = " -- "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        ) 
      
      for (i in seq_along(diff_img_seq)) {
        
        warning(
          "One sequence of fake images is ", diff_img_seq[i], " minutes long.",
          call. = FALSE
        )
        
      }
    } else {
      
      diff_img_seq <- 
        difftime(time1 = tib_fake_images$datetime[length(tib_fake_images$datetime)],
                 time2 = tib_fake_images$datetime[1],
                 units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[1],
                                        tib_fake_images$img_number[nrow(tib_fake_images)],
                                        sep = " - "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        )
      
      warning(
        "Only one sequence found which is ", diff_img_seq, " minutes.",
        call. = FALSE
      )
      
    }
    
    fnm_fake_time <- 
      paste0(
        "time_loss_",
        fnm_img_set,
        ".csv"
      )
    vroom_write(
      tib_fake_time,
      path = paste(fdr_fake_images,
                   fnm_fake_time,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
  } else {
    
    # Not all fake images are sequential. :( Go through and filter out which
    # fake images are sequential.
    warning(
      "Fake images have been found that are sequential; possible significant time loss:",
      call. = FALSE
    )
    
    ind_fake_non_seq_anchors <-
      which(
        c(diff(ind_fake_non_seq), 1) > 1
      )
    
    if (ind_fake_non_seq[1] == 1) {
      
      # First two images are not sequential.
      if (length(ind_fake_non_seq_anchors) > 1) {
        
        # More than one sequential sequence. Loop through to get all anchors.
        for (i in seq_along(ind_fake_non_seq_anchors)) {
          
          ind_fake_seq_anchors <- 
            c(ind_fake_non_seq[ind_fake_non_seq_anchors[i]] + 1, 
              ind_fake_non_seq[ind_fake_non_seq_anchors[i] + 1] - 1)
          
          if (i == 1) {
            
            lst_seq_anchors <- 
              list(ind_fake_seq_anchors)
            
          } else if (i > 1) {
            
            lst_seq_anchors[[i]] <- ind_fake_seq_anchors
            
          }
        }
        
        ind_fake_seq_anchors <- 
          unlist(lst_seq_anchors)
        
      } else {
        
        ind_fake_seq_anchors <- 
          c(ind_fake_non_seq[ind_fake_non_seq_anchors] + 1, 
            ind_fake_non_seq[ind_fake_non_seq_anchors + 1] - 1)
        
      }
    } else if (ind_fake_non_seq[1] != 1) {
      
      # First two images are sequential.
      if (length(ind_fake_non_seq_anchors) > 1) {
        
        # More than one sequential sequence. Loop through to get all anchors.
        for (i in seq_along(ind_fake_non_seq_anchors)) {
          
          if (i == 1) {
            
            ind_fake_seq_anchors <- 
              c(1, # This is the extra step needed if ind_fake_non_seq[1] != 1.
                ind_fake_non_seq[ind_fake_non_seq_anchors[i]] - 1, # Extra step.
                ind_fake_non_seq[ind_fake_non_seq_anchors[i]] + 1, 
                ind_fake_non_seq[ind_fake_non_seq_anchors[i] + 1] - 1)
            lst_seq_anchors <- 
              list(ind_fake_seq_anchors)
            
          } else if (i > 1) {
            
            ind_fake_seq_anchors <- 
              c(ind_fake_non_seq[ind_fake_non_seq_anchors[i]] + 1, 
                ind_fake_non_seq[ind_fake_non_seq_anchors[i] + 1] - 1)
            lst_seq_anchors[[i]] <- ind_fake_seq_anchors
            
          }
        }
        
        ind_fake_seq_anchors <- 
          unlist(lst_seq_anchors)
        
      } else {
        
        ind_fake_seq_anchors <- 
          c(1, # This is the extra step needed if ind_fake_non_seq[1] != 1.
            ind_fake_non_seq[ind_fake_non_seq_anchors] - 1, # Extra step.
            ind_fake_non_seq[ind_fake_non_seq_anchors] + 1, 
            ind_fake_non_seq[ind_fake_non_seq_anchors + 1] - 1)
        
      }
    }
    
    ind_fake_seq_beg <- 
      ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 1]
    ind_fake_seq_end <- 
      ind_fake_seq_anchors[seq_along(ind_fake_seq_anchors) %% 2 == 0]
    
    # Check if there is a gap within a sequence. If there is, get new sequence
    # anchors that do not include the gap (this gap would not be included while
    # coding anyways as coders would put a break in the code line).
    ind_fake_gap <- suppressWarnings(
      which(c(diff.POSIXt(tib_fake_images$datetime,
                          lag = 1,
                          differences = 1), 1) %>% 
              as.numeric(units = "mins") >=
              15) %>% 
        as_tibble() %>% 
        filter(value > ind_fake_seq_beg &
                 value < ind_fake_seq_end) %>% 
        unlist(use.names = FALSE)
    )
    
    if (length(ind_fake_gap) > 0) {
      
      ind_fake_seq_anchors_2 <- 
        c(ind_fake_seq_anchors,
          ind_fake_gap,
          ind_fake_gap + 1) %>% 
        sort()
      ind_fake_seq_beg_2 <- 
        ind_fake_seq_anchors_2[seq_along(ind_fake_seq_anchors_2) %% 2 == 1]
      ind_fake_seq_end_2 <- 
        ind_fake_seq_anchors_2[seq_along(ind_fake_seq_anchors_2) %% 2 == 0]
      
      diff_img_seq <- 
        difftime( time1 = tib_fake_images$datetime[ind_fake_seq_end_2],
                  time2 = tib_fake_images$datetime[ind_fake_seq_beg_2],
                  units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[ind_fake_seq_beg_2],
                                        tib_fake_images$img_number[ind_fake_seq_end_2],
                                        sep = " -- "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        ) 
      
    } else {
      
      diff_img_seq <- 
        difftime( time1 = tib_fake_images$datetime[ind_fake_seq_end],
                  time2 = tib_fake_images$datetime[ind_fake_seq_beg],
                  units = "mins") %>% 
        round(digits = 2)
      tib_fake_time <- 
        tibble(
          image_sequence        = paste(tib_fake_images$img_number[ind_fake_seq_beg],
                                        tib_fake_images$img_number[ind_fake_seq_end],
                                        sep = " -- "),
          time_loss_minutes     = diff_img_seq,
          .rows = length(diff_img_seq)
        ) 
      
    }
    
    fnm_fake_time <- 
      paste0(
        "time_loss_",
        fnm_img_set,
        ".csv"
      )
    vroom_write(
      tib_fake_time,
      path = paste(fdr_fake_images,
                   fnm_fake_time,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    for (i in seq_along(diff_img_seq)) {
      
      warning(
        "One sequence of fake images is ", diff_img_seq[i], " minutes long.",
        call. = FALSE
      )
      
    }
  }
  
  message(
    "\n",
    "---------------------------------CREATING DONE--------------------------------\n",
    "            Oxford Images for ",fnm_img_set," have been created.\n",
    "\n",
    "Fake images have been found! They have been deleted from the image set.\n",
    "Reference warnings, fake_images.csv and time_loss.csv in:\n", 
    fdr_fake_images, "\n"
  ) 
  
}

extract_autographer_images_v4 <- function(fdr_load,
                                          id) {
  
  # WAS PREVIOUSLY clean_PPAQ_images_v3 
  # study_id_trimester <- "PPAQ_08_1"
  # fpa_load <- "./OxfordImageBrowser/1_Image Sets to Load"
  
  # fdr_load <- 
  #   "FLAC_AIM3_DATA/PILOT/images"
  # id <- 
  #   "test"
  
  # Delete folders that have 256x192 and 640x480 pictures as they are not needed.
  cli_alert_info('Deleting "256_192" & "640_480" folders as they are uneeded.')
  vct_fdr_delete <-
    fs::dir_ls(
      path = fdr_load,
      recurse = TRUE,
      type = "directory",
      regexp = id
    ) %>% 
    path_filter(glob = "*/256_192|640_480")
  walk(.x = cli_progress_along(vct_fdr_delete),
       .f = ~fs::dir_delete(vct_fdr_delete[.x]))
  
  vct_fdr_id_day <- 
    fs::dir_ls(
      path = fdr_load,
      recurse = 1,
      type = "directory",
      regexp = paste0(id, "/"),
      ignore.case = TRUE
    )
  
  df_n_img <- 
    data.frame(
      day = c(fs::path_file(vct_fdr_id_day), "TOTAL"),
      n = NA_integer_
    )
  
  for (i in seq_along(vct_fdr_id_day)) {
    
    fdr_id_day <- 
      vct_fdr_id_day[i]
    
    cli_alert_info("Extracting images for day {fdr_id_day}.")
    
    cli_progress_step(
      msg = 'Moving images from hour folders...',
      msg_done = 'Moving images from hour folders...DONE'
    )
    
    vct_fpa_img <- 
      fs::dir_ls(
        path = fdr_id_day,
        recurse = TRUE,
        type = "file",
        # Regex makes sure its only imgs in hour folders.
        regexp = paste0(id, "/.*/.*/.*\\.JPG$")
      )
    df_n_img$n[i] <- 
      length(vct_fpa_img)
    fs::file_move(
      path = vct_fpa_img,
      new_path = fdr_id_day
    )
    chk_empty <- 
      fs::dir_ls(
        path = fdr_id_day,
        recurse = TRUE,
        type = "file",
        regexp = paste0(id, "/.*/.*/.*\\.JPG$"),
        fail = TRUE
      ) %>% 
      is_empty()
    
    if(chk_empty) {
      
      # Okay to delete hour folders now.
      fs::dir_ls(
        path = fdr_id_day,
        recurse = 1,
        type = "directory",
        fail = TRUE
      ) %>% 
        dir_delete()
      
    } else {
      cli_warn(
        message = 
          c(">" = "{fdr_id_day}",
            "!" = "Unexpected folder or file has been found within hour folders.",
            "i" = "Hour folders will not be deleted. Please check folders.")
      )
    }
    
    cli_progress_done()
    cli_progress_step(
      msg = 'Renaming & moving day folder...',
      msg_done = 'Renaming & moving day folder...DONE'
    )
    
    fs::file_move(
      path = fdr_id_day,
      new_path = str_replace(fdr_id_day,
                             # Matches last occurance of "/".
                             pattern = "/(?!.*/)",
                             replacement = "_")
    )
    
    cli_progress_done()
    
  }
  
  dir_delete(fs::path(fdr_load,
                      id))
  
  df_n_img <- 
    df_n_img %>% 
    add_row(
      day = "TOTAL",
      n   = sum(n)
    )
  
  cli_alert_success("SUCCESS. Done Extracting")
  
}


extract_frames_v1 <- function(fdr_video,
                              fdr_frame,
                              id_date,
                              ext_frame) {
  
  # ASSUMES THAT THERE IS AN FFMPEG FOLDER AT THE TOP DIRECTORY OF THE R PROJECT
  # FILE (IS IN THE SAME FOLDER WITH THE .Rproj)
  
  # COMMAND LINE ARGUMENTS
  # FFmpeg\bin\ffmpeg.exe -i "fooo\afoo.AVI" "foo\afoo-%04d.png"
  
  # sys::exec_wait(
  #   cmd = I("FFmpeg\\bin\\ffmpeg.exe"),
  #   args = I(c(' -i "FLAC_AIM3_DATA\\brinno test\\TLC00006.AVI"',
  #              '"FLAC_AIM3_DATA\\brinno test\\images-%04d.png"',
  #              "-hide_banner"))
  # )
  
  fdr_video <-
    "FLAC_AIM3_DATA/PILOT/video"
  fdr_frame <-
    "FLAC_AIM3_DATA/PILOT/images"
  id_date <- 
    "DLW_Pilot_STE01_2022_03_05"
  ext_frame <-
    "jpg"
  
  vct_fpa_video <- 
    fs::dir_ls(
      path = fdr_video,
      regexp = id_date
    )
  fdr_video_frame <- 
    fs::path(fdr_frame,
             id_date)
  
  # In case a folder with the video name in the frames folder was not created already.
  fs::dir_create(fdr_video_frame)
  
  # All part videos from the same date will have images be put in the same folder.
  for (i in seq_along(vct_fpa_video)) {
    
    fpa_video <- 
      vct_fpa_video[i]
    
    part_video <- 
      fpa_video %>% 
      fs::path_file() %>% 
      str_remove(pattern = paste0(id_date, "_")) %>% 
      fs::path_ext_remove()
    fpa_video_sys <- 
      paste0(
        '"',
        str_replace_all(fpa_video,
                        pattern = "\\/",
                        replacement = "\\\\"),
        '"'
      )
    fpa_frame_sys <- 
      paste0(
        '"',
        str_replace_all(fdr_video_frame,
                        pattern = "\\/",
                        replacement = "\\\\"),
        "\\", part_video, "-images-%05d.",
        ext_frame,
        '"'
      )
    sys::exec_wait(
      cmd = "FFmpeg/bin/ffmpeg.exe",
      args = I(c("-i", fpa_video_sys,
                 "-qscale:v 2",
                 fpa_frame_sys,
                 "-hide_banner"))
    )
    
  }
  
  cli_progress_done()
  cli_alert_success("Frames successfully extracted.")
  
  vct_fpa_frame <- 
    fdr_video_frame %>% 
    fs::dir_ls()
  df_n <- 
    tibble(
      fnm = 
        vct_fpa_frame %>% 
        fs::path_file() %>% 
        fs::path_ext_remove()
    ) %>% 
    tidyr::separate(col = fnm,
                    into = c("part", "n_frame"),
                    sep = "-images-",
                    remove = TRUE) 
  df_add <- 
    df_n %>% 
    group_by(part) %>% 
    summarise(add = 
                n_frame %>% 
                as.integer() %>% 
                max())
  df_add$add <- 
    df_add$add %>% 
    lag(n = 1,
        default = 0L) %>% 
    cumsum()
  df_n <- 
    left_join(df_n,
              df_add,
              by = "part") %>% 
    mutate(n_final = 
             (as.double(n_frame) + add) %>% 
             sprintf(fmt = "%05d"))
  
  progress_format <- 
    "Reading timestamps & naming images {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | [{cli::pb_elapsed}] | {cli::pb_eta_str}"
  
  # create medium and thumbnail for each image (imager)
  for (i in cli_progress_along(vct_fpa_frame,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_frame <- 
      vct_fpa_frame[i]
    n_img <- 
      df_n$n_final[i]
    fnm_frame <- 
      fpa_frame %>% 
      fs::path_file()
    
    img <- 
      magick::image_read(fpa_frame)
    img_timestamp <- 
      # Plus/minus 17 pixels to the left and right for maximal background.
      image_crop(img, "297x30+861+1050") %>% 
      # Turn image to black text to increase accuracy of ocr.
      image_negate()
    timestamp <- 
      magick::image_ocr(img_timestamp,
                        language = "eng",
                        HOCR = FALSE) %>% 
      str_remove(pattern = "\\n") %>% 
      str_replace(pattern = " ",
                  replacement = "_") %>% 
      str_replace_all(pattern = ":|/",
                      replacement = "")
    # So it can be recognized by Oxford Image Browser.
    fnm_frame_new <- 
      paste0(
        "00000000000", n_img, "_",
        timestamp, "A"
      ) %>% 
      fs::path_ext_set(ext = ext_frame)
    fpa_frame_new <- 
      fs::path(fdr_video_frame,
               fnm_frame_new)
    fs::file_move(
      path = fpa_frame,
      new_path = fpa_frame_new
    )
    # Change Date modified of renamed frame to timestamp.
    Sys.setFileTime(
      fpa_frame_new,
      time = lubridate::ymd_hms(timestamp,
                                tz = "America/Chicago")
    )
    
  }
  
  cli_alert_success("SUCCESS. {i} images renamed.")
  
  vct_fnm_frame_new <- 
    fdr_video_frame %>% 
    fs::dir_ls() %>% 
    fs::path_file()
  chk_fnm_frame_new <- 
    vct_fnm_frame_new %>% 
    fs::path_ext_remove() %>% 
    str_remove(pattern = "\\d{16}_") %>% 
    lubridate::ymd_hms(quiet = TRUE) %>% 
    is.na() %>% 
    any()
  
  if (chk_fnm_frame_new){
    
    # One on the new filenames were not guessed correctly by the 
    # tesseract OCR.
    vct_fnm_frame_wrong <- 
      vct_fnm_frame_new[vct_fnm_frame_new %>% 
                          fs::path_ext_remove() %>% 
                          str_remove(pattern = "\\d{16}_") %>% 
                          lubridate::ymd_hms(quiet = TRUE) %>% 
                          is.na()]
    
    for (i in seq_along(vct_fnm_frame_wrong)) {
      warning(
        vct_fnm_frame_wrong[i], ": Timestamp for frame was not guessed correctly.\n",
        "Manually change filename by looking at image.",
        call. = FALSE
      )
    }
  }
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                        %%%%
####                             READ FUNCTIONS                             ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_chamber_v1 <- function(fdr_raw,
                            fdr_raw_csv) {
  
  
  # # CHANGES:
  
  # -Literally just to change files from tsv to csv format as tsv files are inconsistent.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NONE
  # ARG: fdr_raw_tsv
  #      File directory of clean chamber files.
  # ARG: fdr_raw_csv
  #      File directory of clean rmr files.
  # ARG: fdr_merge
  #      File directory of merged files.
  
  # # TESTING
  fdr_raw <- 
    fs::path("FLAC_AIM1_DATA",
             "1_AIM1_RAW_DATA")
  fdr_raw_csv <- 
    fs::path("FLAC_AIM1_DATA",
             "1_AIM1_RAW_DATA",
             "AIM1_RAW_CHAMBER_cSV")
  
  vct_fpa_tsv <- 
    fs::dir_ls(
      path = fdr_raw,
      all = TRUE,
      recurse = TRUE,
      type = "file",
      glob = NULL,
      regexp = str_to_upper("chamber"),
      invert = FALSE,
      fail = TRUE
    ) %>% 
    fs::path_filter(glob = "*.txt")
  fnm_tsv <- 
    ""
  cli_progress_step(
    msg = "Converting file {fnm_tsv} from tsv to csv",
    msg_done = "SUCCESS"
  )
  
  for (i in seq_along(vct_fpa_tsv)) {
    
    fpa_tsv <- 
      vct_fpa_tsv[i]
    fnm_tsv <- 
      fpa_tsv %>% 
      fs::path_file()
    
    cli_progress_update()
    
    fnm_csv <- 
      fnm_tsv %>%
      fs::path_ext_set(ext = "csv")
    fpa_csv <- 
      fs::path(
        fs::dir_ls(
          path = fdr_raw,
          all = TRUE,
          recurse = TRUE,
          type = "directory",
          glob = NULL,
          regexp = str_to_upper("chamber"),
          invert = FALSE,
          fail = TRUE
        ) %>% 
          fs::path_filter(regexp = str_to_upper("csv")),
        fnm_csv
      )
    
    # Earlier chamber files have an extra \t at the end of the row AFTER the
    # header which causes problems. To solve this, we read in the chamber files
    # with header = FALSE. If it is a weird early file, it will not have the
    # header as the first row while normal files will.
    df_tsv <- 
      data.table::fread(
        file = fpa_tsv,
        sep = "\t",
        header = FALSE
      )
    
    chk_delimited <- 
      !(df_tsv[[1]][1] == "datetime")
    chk_dtm_split <- 
      is.na(df_tsv[[ncol(df_tsv)]][1])
    
    if (chk_delimited) {
      
      # It is one of the weird early files. Remove extra column and assign
      # names from readLines.
      df_csv <- 
        df_tsv %>% 
        as_tibble() %>%
        select(!where(is.logical)) %>% 
        rlang::set_names(nm = 
                           readLines(fpa_tsv, 
                                     n = 1) %>% 
                           str_split(pattern = "\t") %>% 
                           unlist()) %>% 
        as.data.table()
      
    } else if (chk_dtm_split) {
      
      # Weird later file that has datetime in two separate columns.
      df_csv <- 
        df_tsv[-1, ] %>% 
        unite(col = "datetime",
              V1, V2,
              sep = " ",
              remove = TRUE) %>% 
        rlang::set_names(nm =
                           readLines(fpa_tsv, 
                                     n = 1) %>% 
                           str_split(pattern = "\t") %>% 
                           unlist() %>% 
                           str_subset(pattern = ""))
      
    } else {
      
      # It is a normal file and we can just move the first row to header.
      df_csv <- 
        janitor::row_to_names(
          df_tsv,
          row_number = 1,
          remove_row = TRUE,
          remove_rows_above = TRUE
        )
      
    }
    
    # For later files that have SFR1 & SFR2 for consistency.
    if (any(c("SFR1", "SFR2") %in% colnames(df_csv))) {
      df_csv[, `:=`(SFR1 = NULL, SFR2 = NULL)]
    }
    
    # FINAL CHECK.
    chk_final <- 
      !all(
        colnames(df_csv) ==
          c("datetime", "O21", "O22", "CO21", "CO22", "WVP1", "WVP2",
            "FR","Deg_C", "RH", "VO2", "VCO2", "RQ", "kcal_min")
      ) | ncol(df_csv) != 14L
    if (chk_final | anyNA.data.frame(df_csv)) {
      cli_abort("{fnm_tsv} STAAAAAAAAAAAAAAAAAAAHHHHHHHHHHHPPPPPPPPPPPP")
    }
    
    data.table::fwrite(
      df_csv,
      file = fpa_csv,
      sep = ","
    )
    
  }
  
  cli_progress_done()
  
}


read_img_irr_v1 <- function(fls_pos,
                            fls_act,
                            fld_irr) {
  
  # # CHANGES:
  
  # -[NONE]
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: [NONE]
  # ARG: fls_pos
  #      File list of posture annotation files.
  # ARG: fls_act
  #      File list of activity annotation files.
  
  # # TESTING
  # fls_pos <- 
  #   fls_irr_pos
  # fls_act <- 
  #   fls_irr_act
  
  # Check to make sure all annotation files are for the correct IMG set.
  vct_file_img_set_pos <- 
    fls_pos %>% 
    str_extract(pattern = "(?:(?!_P).)*")
  vct_file_img_set_act <- 
    fls_act %>% 
    str_extract(pattern = "(?:(?!_A).)*")
  
  file_img_set <- 
    union(vct_file_img_set_pos,
          vct_file_img_set_act)
  
  if (file_img_set != fld_irr) {
    
    stop("FUUUUUUUUUUK")
    
  }
  
  # Check to make sure same number of files for both pos and act
  vct_coder_names_pos <- 
    fls_pos %>% 
    str_extract(pattern = "(?:(?!_SPLIT).)*") %>% # Captures everything before "_SPLIT".
    str_extract(pattern = "([^_]*)$") # Captures everything after last "_".
  
  vct_coder_names_act <- 
    fls_act %>% 
    str_extract(pattern = "(?:(?!_SPLIT).)*") %>% 
    str_extract(pattern = "([^_]*)$") # Captures everything after last "_"
  
  if (all(vct_coder_names_act == vct_coder_names_pos) == FALSE) {
    
    stop("FUUUUUUUUUUUUUUUUUK names")
    
  }
  
  message("File names are okay.",
          appendLF = TRUE)
}



read_timestamps_v3 <- function(fpa_timestamps,
                               fnm_timestamps) {
  
  # fpa_timestamps <- "./OxfordImageBrowser/3_Downloaded Annotation Files/MasterTimeStamp"
  # fnm_timestamps <- "TimeStamps.csv"
  
  tib_cor_times <- 
    suppressMessages(
      paste(fpa_timestamps,
            fnm_timestamps,
            sep = "/") %>% 
        vroom(delim = ",",
              col_select = 1:6,
              progress = FALSE)
    )
  
  # Consistency: column names with _ instead of . & all columns lowercase.
  colnames(tib_cor_times) <- 
    colnames(tib_cor_times) %>% 
    str_replace_all(pattern = "\\.",
                    replacement = "_") %>% 
    str_to_lower()
  
  ## CHECK: If there are any missing entries.
  miss_id <- 
    tib_cor_times$id[is.na(tib_cor_times$stopwatch_ymd_hms) |
                       is.na(tib_cor_times$picture_ymd_hms)]
  miss_tri <- 
    tib_cor_times$trimester[is.na(tib_cor_times$stopwatch_ymd_hms) |
                              is.na(tib_cor_times$picture_ymd_hms)]
  missing <- 
    bind_cols(id = miss_id,
              trimester = miss_tri)
  missing <- 
    paste(missing$id,
          missing$trimester,
          sep = "v")
  
  for (i in seq_along(missing)) { 
    
    warning(
      missing[i], ":\n",
      "    One of the timestamps were entered incorrectly!\n",
      call. = FALSE
    )
    
  }
  
  # We are assuming if timezone is NA (timezone was not stated on wearlog) the
  # timezone is America/New_York.
  tib_cor_times$timezone[is.na(tib_cor_times$timezone)] <- "America/New_York"
  
  # Before I use to think both stopwatch and picture times are America/Chicago 
  # but it goes a little deeper than that. The stopwatch times are actually
  # America/New_York UNLESS need_wearlog is TRUE (the stopwatch time will then
  # be whatever timezone is). The picture time is America/Chicago only because
  # the annotation files are outputted in America/chicago time. This will allow
  # the difference between the picture and stopwatch times to be applicable to
  # the annotation file times. Since date-time vectors in r cannot be different
  # timezones, I make it all America/chicago but the time difference is still
  # reflected.
  tib_cor_times$timestamp <- 
    NA %>% 
    as.POSIXct(tz = "America/Chicago")
  
  for (i in seq_len(nrow(tib_cor_times))) {
    
    tib_row <- i
    
    if (tib_cor_times$need_wearlog[tib_row]) {
      
      tib_cor_times$timestamp[tib_row] <-
        tib_cor_times$stopwatch_ymd_hms[tib_row] %>%
        lubridate::ymd_hms(tz = tib_cor_times$timezone[tib_row])
      
    } else {
      
      tib_cor_times$timestamp[tib_row] <- 
        tib_cor_times$stopwatch_ymd_hms[tib_row] %>% 
        lubridate::ymd_hms(tz = "America/New_York")
      
    }
  }
  
  tib_cor_times$picture <- 
    tib_cor_times$picture_ymd_hms %>% 
    lubridate::ymd_hms(tz = "America/Chicago")
  tib_cor_times$difference <- 
    difftime(
      tib_cor_times$timestamp,
      tib_cor_times$picture,
      units = "secs"
    )
  
  ## CHECK: Duplicate entries.
  id_tri <- 
    paste(tib_cor_times$id,
          tib_cor_times$trimester,
          sep = "_")
  index <- which(duplicated(id_tri))
  mistake <- id_tri[duplicated(id_tri)]
  
  # output different warning depending if entries "Difference" values are same or diff
  index_dup <- which(id_tri == mistake)
  dup <- tib_cor_times[index_dup, "difference"] %>% 
    unlist() %>% 
    unique()
  if (length(dup) != 1) {
    
    for (i in seq_along(index)) {
      
      warning(
        mistake[i], ":\n",
        "    Duplicate entry detected at line ", index[i] + 1, ".\n",
        "    Entries do not have the same.\n",
        '    Check timestamp image in "MasterTimeStamp" folder.\n',
        call. = FALSE
      )
      
    }
  } else {
    
    for (i in seq_along(index)) {
      
      warning(
        mistake[i], ":\n",
        "    Duplicate entry detected at line ", index[i] + 1, ".\n",
        "    Entries have same inputs.\n",
        "    Therefore just delete one.\n",
        call. = FALSE
      )
      
    }
  }
  
  return(tib_cor_times)
  
}

read_timestamps_v4 <- function(fdr_timestamps,
                               fnm_timestamps) {
  
  # # CHANGES:
  
  # -Make function specific to FLAC and not PPAQ anymore (v3 will be the final
  #  version for PPAQ.)
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NA
  
  # ARG: fdr_timestamps
  #      File directory of timestamp file.
  # ARG: fnm_timestamps
  #      File name of timestamp file.
  
  # # TESTING
  
  # fdr_timestamps <-
  #   "./OxfordImageBrowser-win32-x64/3_Downloaded Annotation Files/0_Time_Stamp"
  # fnm_timestamps <-
  #   "TimeStamps.csv"
  
  tib_cor_times <- 
    suppressMessages(
      paste(fdr_timestamps,
            fnm_timestamps,
            sep = "/") %>% 
        vroom(delim = ",",
              col_select = 1:4,
              progress = FALSE)
    )
  
  # Consistency: column names with _ instead of . & all columns lowercase.
  colnames(tib_cor_times) <- 
    colnames(tib_cor_times) %>% 
    str_replace_all(pattern = "\\.",
                    replacement = "_") %>% 
    str_to_lower()
  
  ## CHECK: If there are any missing entries.
  miss_id <- 
    tib_cor_times$id[is.na(tib_cor_times$stopwatch_ymd_hms) |
                       is.na(tib_cor_times$picture_ymd_hms)]
  miss_visit <- 
    tib_cor_times$visit[is.na(tib_cor_times$stopwatch_ymd_hms) |
                          is.na(tib_cor_times$picture_ymd_hms)]
  missing <- 
    bind_cols(id = miss_id,
              visit = miss_visit)
  missing <- 
    paste(missing$id,
          missing$visit,
          sep = "v")
  
  for (i in seq_along(missing)) { 
    
    warning(
      missing[i], ":\n",
      "    One of the timestamps were entered incorrectly!\n",
      call. = FALSE
    )
    
  }
  
  # All the pictures should be in America/Chicago time when working with FLAC
  # Aim 2, Aim 3, and LAFLAC data.
  tib_cor_times <- 
    tib_cor_times %>% 
    mutate(timestamp  = lubridate::ymd_hms(stopwatch_ymd_hms,
                                           tz = "America/Chicago"),
           picture    = lubridate::ymd_hms(picture_ymd_hms,
                                           tz = "America/Chicago"),
           difference = difftime(timestamp,
                                 picture,
                                 units = "secs"))
  
  ## CHECK: Duplicate entries.
  id_visit <- 
    paste(tib_cor_times$id,
          tib_cor_times$visit,
          sep = "_")
  index <- which(duplicated(id_visit))
  mistake <- id_visit[duplicated(id_visit)]
  
  # output different warning depending if entries "Difference" values are same or diff
  index_dup <- which(id_visit == mistake)
  dup <- tib_cor_times[index_dup, "difference"] %>% 
    unlist() %>% 
    unique()
  if (length(dup) != 1) {
    
    for (i in seq_along(index)) {
      
      warning(
        mistake[i], ":\n",
        "    Duplicate entry detected at line ", index[i] + 1, ".\n",
        "    Entries do not have the same.\n",
        '    Check timestamp image in "MasterTimeStamp" folder.\n',
        call. = FALSE
      )
      
    }
  } else {
    
    for (i in seq_along(index)) {
      
      warning(
        mistake[i], ":\n",
        "    Duplicate entry detected at line ", index[i] + 1, ".\n",
        "    Entries have same inputs.\n",
        "    Therefore just delete one.\n",
        call. = FALSE
      )
      
    }
  }
  
  return(tib_cor_times)
  
}

read_img_timestamps_v5 <- function(fdr_timestamps,
                                   fnm_timestamps) {
  
  # # CHANGES:
  
  # -Change name to read_img_timestamps
  # -Update missing warnings to only check partial missing entries
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NA
  
  # ARG: fdr_timestamps
  #      File directory of timestamp file.
  # ARG: fnm_timestamps
  #      File name of timestamp file.
  
  # # TESTING
  
  # fdr_timestamps <-
  #   "./OxfordImageBrowser-win32-x64/3_Downloaded Annotation Files/0_Time_Stamp"
  # fnm_timestamps <-
  #   "TimeStamps.csv"
  # fdr_timestamps <-
  #   "./3_data/0_raw/0_Time_Stamp"
  # fnm_timestamps <-
  #   "TimeStamps.csv"
  
  tib_cor_times <- 
    suppressMessages(
      paste(fdr_timestamps,
            fnm_timestamps,
            sep = "/") %>% 
        vroom(delim = ",",
              col_select = 1:4,
              progress = FALSE)
    )
  
  # Consistency: column names with _ instead of . & all columns lowercase.
  colnames(tib_cor_times) <- 
    colnames(tib_cor_times) %>% 
    str_replace_all(pattern = "\\.",
                    replacement = "_") %>% 
    str_to_lower()
  
  ## CHECK; Entries missing both stopwatch AND picture.
  missing_full <- 
    paste0(
      tib_cor_times$id[is.na(tib_cor_times$stopwatch_ymd_hms) &
                         is.na(tib_cor_times$picture_ymd_hms)],
      "v",
      visit = 
        tib_cor_times$visit[is.na(tib_cor_times$stopwatch_ymd_hms) &
                              is.na(tib_cor_times$picture_ymd_hms)]
    )
  
  for (i in seq_along(missing_full)) { 
    
    warning(
      missing_full[i], ": No stopwatch and picture timestamps entered.\n",
      call. = FALSE
    )
    
  }
  
  ## CHECK: Entries missing either stopwatch OR picture, not both. Most likely
  ##        a data entry mistake.
  missing_part <- 
    paste0(
      tib_cor_times$id[is.na(tib_cor_times$stopwatch_ymd_hms) |
                         is.na(tib_cor_times$picture_ymd_hms)],
      "v",
      visit = 
        tib_cor_times$visit[is.na(tib_cor_times$stopwatch_ymd_hms) |
                              is.na(tib_cor_times$picture_ymd_hms)]
    )
  missing_part <- 
    missing_part[!(missing_full %in% missing_part)]
  
  for (i in seq_along(missing_part)) { 
    
    warning(
      missing_part[i], ": A stopwatch or picture timestamp was not entered.\n",
      "      Double-check to make sure data is entered is correctly",
      call. = FALSE
    )
    
  }
  
  # miss_id <- 
  #   tib_cor_times$id[is.na(tib_cor_times$stopwatch_ymd_hms) |
  #                      is.na(tib_cor_times$picture_ymd_hms)]
  # miss_visit <- 
  #   tib_cor_times$visit[is.na(tib_cor_times$stopwatch_ymd_hms) |
  #                         is.na(tib_cor_times$picture_ymd_hms)]
  # missing <- 
  #   bind_cols(id = miss_id,
  #             visit = miss_visit)
  # missing <- 
  #   paste(missing$id,
  #         missing$visit,
  #         sep = "v")
  # for (i in seq_along(missing)) { 
  #   warning(
  #     missing[i], ":\n",
  #     "    One of the timestamps were entered incorrectly!\n",
  #     call. = FALSE
  #   )
  # }
  
  # All the pictures should be in America/Chicago time when working with FLAC
  # Aim 2, Aim 3, and LAFLAC data.
  tib_cor_times <- 
    tib_cor_times %>% 
    mutate(timestamp  = lubridate::ymd_hms(stopwatch_ymd_hms,
                                           tz = "America/Chicago"),
           picture    = lubridate::ymd_hms(picture_ymd_hms,
                                           tz = "America/Chicago"),
           difference = difftime(timestamp,
                                 picture,
                                 units = "secs"))
  
  ## CHECK: Duplicate entries.
  id_visit <- 
    paste(tib_cor_times$id,
          tib_cor_times$visit,
          sep = "_")
  index <- which(duplicated(id_visit))
  mistake <- id_visit[duplicated(id_visit)]
  
  # output different warning depending if entries "Difference" values are same or diff
  index_dup <- which(id_visit == mistake)
  dup <- tib_cor_times[index_dup, "difference"] %>% 
    unlist() %>% 
    unique()
  if (length(dup) != 1) {
    
    for (i in seq_along(index)) {
      
      warning(
        mistake[i], ":\n",
        "    Duplicate entry detected at line ", index[i] + 1, ".\n",
        "    Entries do not have the same.\n",
        '    Check timestamp image in "MasterTimeStamp" folder.\n',
        call. = FALSE
      )
      
    }
  } else {
    
    for (i in seq_along(index)) {
      
      warning(
        mistake[i], ":\n",
        "    Duplicate entry detected at line ", index[i] + 1, ".\n",
        "    Entries have same inputs.\n",
        "    Therefore just delete one.\n",
        call. = FALSE
      )
      
    }
  }
  
  return(tib_cor_times)
  
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                             CLEAN FUNCTIONS                             ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_actigraph_raw_v1 <- function() {
  
  # KEEP FOR OWN PROJECTS, NOT FLAC.
  
  # Piped head.data.
  # Piped start.time and made it streamlined.
  # Removed "start.time" object since it is only used in 100Hz function.
  # Go back to using data.table for reading and writing csvs. arrow error that popped up
  # with writing csv: "Error: NotImplemented: Casting a timestamp with time zone to 
  # string is not yet supported on Windows." Only use arrow for when you would've
  # wrote an rds (reference rowland function).
  # Piped data, keep it as a tibble and made it faster.
  
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  ag_model_loc <- "GT3X_LW"
  freq <- 100
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  ag_model <- 
    ag_model_loc %>% 
    str_split(pattern = "_") %>% 
    pluck(1, 1)
  ag_loc <- 
    ag_model_loc %>% 
    str_split(pattern = "_") %>% 
    pluck(1, 2)
  freq <- 
    as.integer(freq)
  
  if (!is.null(fdr_write)) {
    
    vct_fsb_write <- 
      fs::dir_ls(
        path        = fdr_write,
        recurse     = FALSE,
        all         = TRUE,
        type        = "directory",
        regexp      = NULL,
        invert      = FALSE,
        fail        = TRUE,
        ignore.case = TRUE
      ) %>% 
      fs::path_file() # Actually just gets the base of the path.
    chk_fsb_write <- 
      !any(
        str_detect(vct_fsb_write,
                   pattern = regex(paste0(ag_model_loc, "_CSV_RAW"),
                                   ignore_case = TRUE))
      )
    if (chk_fsb_write) {
      # Create a directory under fdr_write that contains "ag_model_loc"_CSV_RAW
      cli::cli_inform(
        message = 
          c("!" = 'No sub directory with phrase "{ag_model_loc}_CSV_RAW" found in WRITE directory.',
            "i" = 'Creating sub directory "{ag_model_loc}_CSV_RAW" to house activity files.')
      )
      fs::dir_create(path = fs::path(fdr_write,
                                     paste0(ag_model_loc, "_CSV_RAW")))
    } 
    
  }
  
  vct_fpa_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = paste0(ag_model_loc, "_CSV_RAW"),
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.csv")
  
  cli::cli_alert_info("Cleaning files.")
  counter <- 0
  
  for (i in seq_along(vct_fpa_read)) {
    
    fpa_read <-
      vct_fpa_read[i]
    
    fnm_read <- 
      fs::path_file(fpa_read)
    fli <- 
      list()
    fli$epoch_fnm <- 
      fnm_read %>% 
      fs::path_ext_remove() %>% 
      str_extract(pattern = "RAW$")
    fli <- 
      fnm_read %>% 
      fs::path_ext_remove() %>% 
      str_remove(pattern = "RAW$") %>% 
      str_split(pattern = "_") %>% 
      vec_unchop() %>% 
      vec_chop() %>% 
      c(., fli)
    
    # test <- 
    #   data.table()
    # test$epoch_fnm <- 
    #   fnm_read %>% 
    #   fs::path_ext_remove() %>% 
    #   str_extract(pattern = "RAW$")
    # fnm_read %>% 
    #   fs::path_ext_remove() %>% 
    #   str_remove(pattern = "RAW$") %>% 
    #   str_split(pattern = "_") %>% 
    #   vec_unchop() %>% 
    #   vec_chop() %>% 
    #   as.data.table()
    #   bind_cols(test)
    # test %>% as.list()
    
    # FLAC specific.
    chk_visit <- 
      fli %>% str_detect(pattern = "V\\d{1}") %>% any()
    
    if (chk_visit) {
      
      # Should be a FLAC that has the subject_V#....csv.
      fli <- 
        fli %>% 
        rlang::set_names(nm = 
                           c("study",
                             "model_fnm",
                             "location",
                             "subject",
                             "visit",
                             "epoch_fnm"))
      fli$visit <- 
        fli$visit %>% 
        str_remove(pattern = "V") %>% 
        as.integer()
      # str_extract(pattern = "\\d*") %>% 
      # lubridate::seconds()
      # str_remove(pattern = "ec") %>% 
      # str_to_upper()
      # str_detect(fli$epoch_fnm,
      #             pattern = "\\d*")
      # str_extract(fli$epoch_fnm,
      #             pattern = "\\d*") %>% 
      #   as.integer() %>%
      # sprintf(fmt = "00:00:%02d")
      # 
      #   fcase(
      #     fli$epoch_fnm == "RAW", "00:00:00",
      #     fli$epoch_fnm %>% 
      #       str_detect(pattern = "\\d*"), 
      #     fli$epoch_fnm %>% 
      #       str_extract(pattern = "\\d*") %>% 
      #       as.integer(),
      #     default = NA
      #   )
      
    } else {
      
      fli <- 
        fli %>% 
        rlang::set_names(nm = 
                           c("study",
                             "model_fnm",
                             "location",
                             "subject",
                             "epoch_fnm"))
      fli$visit <- 
        1L
      
    }
    
    dfi <- 
      readLines(fpa_read,
                n = 10) %>% 
      str_split(pattern = " ") %>% 
      vec_unchop() %>% 
      vec_chop(indices = list(7, 9, 11, 14, 16, 20, c(30, 27), 34)) %>% 
      rlang::set_names(nm =
                         c("model_df",
                           "actilife_version",
                           "model_firmware",
                           "date_format",
                           "sampling_rate",
                           "filter",
                           "dtm_start",
                           "epoch_df"))
    dfi$dtm_start <- 
      dfi$dtm_start %>% 
      paste0(collapse = " ") %>% 
      lubridate::mdy_hms() # make local timezone as thats what original code did.
    df_info <- 
      bind_cols(fli,
                dfi) %>% 
      mutate(subject = as.integer(subject),
             sampling_rate = as.integer(sampling_rate),
             model_fnm     = fifelse(test = model_fnm == "AG",
                                     yes = "GT3X",
                                     no = model_fnm,
                                     na = NA_character_)) %>% 
      as.data.table()
    
    # CHECKS
    
    # RAW files will always say that it was created from a GT3X+ file regardless
    # if the model was a GT3X or a LINK so it isnt used at all for checks.
    df_chk <- 
      df_info %>% 
      transmute(
        study         = !study %in% c("CO", "FLAC", "DLW"),
        subject       = is.na(subject) | !is.integer(subject),
        visit         = str_length(visit) != 1L | !is.integer(visit),
        ag_model_fnm  = model_fnm != ag_model,
        ag_model_df   = FALSE,
        ag_location   = location != ag_loc,
        model_fnm     = !model_fnm %in% c("GT3X", "LINK"),
        model_df      = FALSE,
        location      = !location %in% c("LW", "RH", "RH"),
        epoch_fnm     = epoch_fnm != "RAW",
        epoch_df      = epoch_df != "00:00:00",
        date_format   = date_format != "M/d/yyyy",
        sampling_rate = sampling_rate != freq,
        filter        = filter != "Normal"
      ) %>% 
      as.data.table()
    
    # # FOR EPOCH FILES.
    # GT3XPlus
    # wGT3XBT
    
    if (any(df_chk)) {
      
      chk <- 
        df_chk %>% 
        unlist() %>% 
        which() %>% 
        names()
      
      stop(chk)
      
    }
    
    if (is_null(fdr_write)) {
      
      next()
      
    }
    
    df_read <- 
      data.table::fread(
        fpa_read,
        skip = 10,
        sep = ",",
        header = TRUE,
        showProgress = FALSE,
      )
    colnames(df_read) <- 
      colnames(df_read) %>% 
      str_to_lower()
    
    chk_timestamp <- 
      !"timestamp" %in% colnames(df_read)
    
    if (chk_timestamp) {
      
      df_write <- 
        df_read %>% 
        transmute(
          datetime = seq.POSIXt(from = fli$dtm_start,
                                by = 1 / fli$sampling_rate,
                                length.out = dim(df_cln)[1]),
          axis_x   = `accelerometer x`,
          axis_y   = `accelerometer y`,
          axis_z   = `accelerometer z`
        ) %>% 
        as.data.table()
      
    } else {
      
      df_write <- 
        df_read %>% 
        transmute(
          datetime = timestamp,
          axis_x   = `accelerometer x`,
          axis_y   = `accelerometer y`,
          axis_z   = `accelerometer z`
        ) %>% 
        as.data.table()
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fnm_write <- 
      paste(
        df_info$study,
        df_info$subject,
        df_info$visit,
        df_info$model_fnm,
        df_info$location,
        df_info$epoch_fnm,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    fpa_write <- 
      fs::path(
        fdr_write,
        str_subset(vct_fsb_write,
                   pattern = regex(ag_model_loc,
                                   ignore_case = TRUE)),
        fnm_write
      )
    data.table::fwrite(
      df_write,
      file = fpa_write,
      sep = ","
    )
    arrow::write_feather(
      df_write,
      sink = fs::path_ext_set(path = fpa_write,
                              ext = "feather")
    )
    
  }
  
}
clean_chamber_v1 <- function() {
  
  # DONE MANUALLY.
  
  # # CHANGES:
  
  # -Change function name from clean to merge Match data flow of FLAC folders.
  # -Rename raw   TO clean in ARG and object names
  # -Rename clean TO merge in ARG and object names
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NONE
  # ARG: fdr_chm_clean
  #      File directory of clean chamber files.
  # ARG: fdr_rmr_clean
  #      File directory of clean rmr files.
  # ARG: fdr_merge
  #      File directory of merged files.
  
  # # TESTING
  
  # fdr_chm_clean <-
  #   "./FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_Raw_Chamber"
  # fdr_rmr_clean <-
  #   "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA/AIM1_Cleaned_RMR"
  # fdr_merge <-
  #   "./FLAC_AIM1_DATA/4_AIM1_MERGED_DATA/AIM1_Merged_Chamber_RMR"
  fdr_chm_raw <-
    "./3_data/0_raw/chamber"
  fdr_chm_clean <-
    "./3_data/1_cleaned/chamber"
  # fdr_chm_shape <- 
  #   "./3_data/2_shaped"
  # fdr_merge <-
  #   "./3_data/3_merged"
  
  fls_chm_raw <- 
    list.files(path = fdr_chm_raw,
               pattern = ".txt")
  
  for (i in seq_along(fls_chm_raw)) {
    
    fnm_chm_raw <- 
      fls_chm_raw[i]
    
    study <- 
      fnm_chm_raw %>% 
      str_extract(pattern = "(?:(?!_).)*") %>%
      str_extract(pattern = "[A-Z][A-Z]")
    subject <- 
      fnm_chm_raw %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]")
    # visit <- 
    #   1L
    # subject_visit <- 
    #   paste(subject,
    #         visit,
    #         sep = "v")
    
    message("Merging file #", i, ": ", fnm_chm_raw, "...", 
            appendLF = FALSE)
    
    tib_chm_raw <- suppressMessages(
      paste(fdr_chm_raw,
            fnm_chm_raw,
            sep = "/") %>% 
        vroom(delim = "\t",
              progress = FALSE)
    ) %>% 
      rename_with(.cols = everything(),
                  .fn = ~ str_to_lower(.x))
    
    lgl_missing_data <- 
      tib_chm_raw %>% is.na.data.frame() %>% any()
    
    if (lgl_missing_data) {
      
      stop("FUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUCCCCCCCCCCCCCCCCKKKKKKKKKK")
      
    }
    
    fnm_chm_clean <- 
      paste0(study, "_", subject, "_CHM.csv")
    
    vroom_write(
      tib_chm_raw,
      file = paste(fdr_chm_clean,
                   fnm_chm_clean,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  message(
    "------------------------------------COMPLETE------------------------------------\n",
    appendLF = TRUE
  )
  
}
clean_noldus_v1 <- function(project,
                            fdr_clean,
                            fdr_shape,
                            key_behavior_domain,
                            key_posture_bucket,
                            key_posture_domain) {
  # # CHANGES:
  
  # -Update to lastest naming schema
  # -Change read/write to data.table since it is faster
  # -Renamed sections.
  # -Cleaned up file path objects to now have fpt to be the full file path to 
  #  file.
  # -Don't have activity files have the "behavior_activity" column anymore.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: get_domain_behavior
  # FUNCTION: get_domain_posture
  # ARG: project
  #      Self-explanatory
  # ARG: fdr_vid_clean
  #      File directory of clean files.
  # ARG: fdr_vid_shape
  #      File directory of shaped files.
  # ARG: key_behavior_domain
  #      For "key_domain" ARG of get_domain_behavior
  # ARG: key_posture_bucket
  #      For "key_bucket" ARG of get_domain_posture
  # ARG: key_posture_domain
  #      For "key_domain" ARG of get_domain_posture
  
  # # TESTING
  
  project <-
    "FLAC - Aim 1"
  fdr_raw <- 
    fs::path("S:",
             "_V_PAHRL",
             "FLAC",
             "FLAC_AIM1_DATA",
             "1_AIM1_RAW_DATA")
  fdr_cln <-
    fs::path("3_data",
             "1_cleaned")
  
  vct_fpa_raw <- 
    fs::dir_ls(
      path        = fdr_raw,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "noldus",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.xlsx")
  
  for (i in seq_along(vct_fpa_raw)) {
    
    fpa_raw <-
      vct_fpa_raw[i]
    
    fnm_raw <- 
      fpa_raw %>% 
      fs::path_file()
    
    message("Shaping file #", i , ": ", fnm_raw, "...",
            appendLF = FALSE)
    
    fli <- 
      fnm_raw %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_| - ") %>% 
      unlist()
    fli <- 
      fli[c(4L, 5L, 1L, 6L)] %>% 
      str_to_lower()
    fli <- 
      fli %>% 
      as.list() %>% 
      rlang::set_names(nm = c("study",
                              "sbj_vst",
                              "schema",
                              "coder"))
    if (fli$study == "co") {
      
      fli$subject <- 
        fli$sbj_vst %>% 
        as.integer()
      fli$visit <- 
        1L
      
    } else {
      
      fli$subject <- 
        fli$sbj_vst %>% 
        str_extract(pattern = "(?:(?!v|V|d).)*") %>% 
        as.integer()
      fli$visit <- 
        fli$sbj_vst %>% 
        str_extract(pattern = "([^v|V|d]*)$") %>% 
        as.integer()
      
    }
    fli$file_source <- 
      "noldus"
    
    df_raw <- 
      readxl::read_xlsx(path = fpa_raw,
                        progress = FALSE) %>% 
      as.data.table()
    
    # Consistency: Make all column names lowercase
    colnames(df_raw) <- 
      colnames(df_raw) %>% 
      str_to_lower()
    
    # Shape #1: Start & Stop --------------------------------------------------
    message("Start/Stop...",
            appendLF = FALSE)
    
    df_raw <- 
      df_raw %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior == "*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      ) %>% 
      # Make Posture Uncoded codes the same as Activity.
      mutate(
        behavior = 
          dplyr::recode(
            behavior,
            "Uncoded; Start/Stop" = "[U] Start/Stop",
            "Start Time"          = "[U] Start Time",
            "Stop Time"           = "[U] Stop Time",
            # posture
            "Uncoded - Dark/Obscured/OoF"         = "[U] Dark/Obscured/OoF",
            "[P] Other"                           = "[P] Other - Posture",
            "[P} Lying"                           = "[P] Lying",
            "[P] Crouching / Kneeling / Squating" = "[P] Crouching/Kneeling/Squatting",
            "[M] Other"                           = "[M] Other - Movement",
            "[M] Crouching / Squating"            = "[M] Crouching/Squatting",
            # behavior
            "[Q] Cooking/Meal Preperation"  = "[Q] Cooking/Meal Preparation", # DOCOMP
            "[Q] Caring Grooming - Self"    = "[Q] Caring/Grooming - Self", # DOCOMP
            "[LQ] Caring Grooming - Self"   = "[LQ] Caring/Grooming - Self",
            "[LQ] Cooking/Meal Preperation" = "[LQ] Cooking/Meal Preparation",
            "[HQ] Caring Grooming - Self"   = "[HQ] Caring/Grooming - Self",
            "[HQ] Cooking/Meal Preperation" = "[HQ] Cooking/Meal Preparation"
          )
      ) %>% 
      as.data.table()
    
    if (project == "DOCOMP") {
      
      fli$start <- 
        df_raw$time_relative_hms[df_raw$behavior == "[U] Start Time"]
      fli$stop <- 
        df_raw$time_relative_hms[df_raw$behavior == "[U] Stop Time"]
      
    }
    
    # CHECK #1 Start and Stop Comment -------------------------------------------------
    
    # Change start/stop to corresponding time zone at the end.
    fli$start <- suppressWarnings(
      df_raw$comment[df_raw$behavior == "[U] Start Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    fli$stop <- suppressWarnings(
      df_raw$comment[df_raw$behavior == "[U] Stop Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    
    # See if start and stop was entered by coder.
    if (is.na(fli$start)) {
      
      message("",
              appendLF = TRUE)
      warning(
        schema, " - ", fnm_obs,
        "Start Time is not in MM-DD-YYYY hh:mm:ss",
        call. = FALSE
      )
      
    } else if (is.na(fli$stop)) {
      
      message("",
              appendLF = TRUE)
      warning(
        schema, " - ", fnm_obs,
        "Stop Time is not in MM-DD-YYYY hh:mm:ss",
        call. = FALSE
      )
      
    } else if ((df_raw$behavior == "[U] Start/Stop") %>% 
               sum() != 2) {
      
      message("",
              appendLF = TRUE)
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    # CHECK #2: Start & Stop Entry --------------------------------------------
    
    dtm_relative_hmsf_end <- 
      df_raw$time_relative_hmsf[nrow(df_raw)]
    
    dtm_relative_hms_start <- 
      df_raw$time_relative_hms[df_raw$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      df_raw$time_relative_hms[df_raw$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      df_raw$time_relative_hmsf[df_raw$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      df_raw$time_relative_hmsf[df_raw$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    df_chk <- 
      df_raw %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      ) %>% 
      as.data.table()
    
    chk_start <- 
      df_chk$time_relative_hmsf[1] != dtm_relative_hmsf_start
    chk_end <- 
      !dplyr::near(
        x = 
          (df_chk$time_relative_hmsf[nrow(df_chk)] 
           + df_chk$duration_sf[nrow(df_chk)]) %>% 
          seconds(),
        y = 
          dtm_relative_hmsf_end %>% 
          seconds(),
        tol = .01
      )
    chk_abs_vs_rel <- 
      !dplyr::near(
        x = difftime(time1 = fli$stop,
                     time2 = fli$start,
                     units = "secs"),
        y = difftime(time1 = dtm_relative_hms_stop,
                     time2 = dtm_relative_hms_start,
                     units = "secs"),
        tol = 5
      )
    
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    if (chk_start) {
      
      message("",
              appendLF = TRUE)
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    } else if (chk_end) {
      
      message("",
              appendLF = TRUE)
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    } else if (chk_abs_vs_rel) {
      
      message("",
              appendLF = TRUE)
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1
      - t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    # CHECK #3: IDK -----------------------------------------------------------
    
    nrw_chk <- 
      nrow(df_chk)
    int_duration <- 
      c(diff.POSIXt(df_chk$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_chk$time_relative_hms[nrw_chk],
                 units = "secs")) %>% 
      as.integer()
    
    if (is.na(int_duration) %>% 
        any()) {
      
      message("",
              appendLF = TRUE)
      
      stop("Duration of a code is less than a second??? IDK man")
      
    }
    
    # WRITE -------------------------------------------------------------------
    
    fnm_cln <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        fli$schema,
        fli$coder,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    fpa_cln <- 
      fs::path(
        fdr_cln,
        dplyr::case_when(fli$schema == "activity" ~ "noldus_activity",
                         fli$schema == "posture" ~ "noldus_posture"),
        fnm_cln
      )    
    data.table::fwrite(
      df_raw,
      file = fpa_cln,
      sep = ","
    )
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
}
clean_noldus_v2 <- function(fdr_read,
                            fdr_write,
                            fdr_project = NULL,
                            vct_subject_filter = NULL) {
  # # CHANGES:
  
  # -Make it focused on the main study (FLAC) and then if a fdr_prj is supplied
  #  then also write to that project.
  # -If fdr_prj is supplied then it will literally just be saving it to another
  #  location.
  # -Created vct_subject_filter argument that allows the the user to filter out 
  #  the read in files with a vector of subjects.
  # -Make read and write arguments universal.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NULL
  
  # ARG: fdr_read
  #      File directory of raw files.
  # ARG: fdr_write
  #      File directory of cleaned files.
  # ARG: fdr_project
  #      Project that is a fork of the main study. If this is supplied, it needs
  #      to be to the file directory. Sub-directories under the data directory
  #      need to at least contain one of the following words:
  #      raw, cleaned, shaped, merged, processed.
  # ARG: vct_subject_filter
  #      Vector of subjects to filter the fpa_read 
  
  # # TESTING
  
  fdr_read <- 
    fs::path("FLAC_AIM1_DATA",
             "1_AIM1_RAW_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write_nld_act <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("noldus_activity",
                                 ignore_case = TRUE))
    )
  chk_fsb_write_nld_pos <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("noldus_posture",
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write_nld_act) {
    # Create a directory under fdr_write that contains "noldus_activity"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "noldus_activity" found in WRITE directory.',
          "i" = 'Creating sub directory "NOLDUS_ACTIVITY" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "NOLDUS_ACTIVITY"))
  } 
  if (chk_fsb_write_nld_pos) {
    # Create a directory under fdr_write that contains "noldus_posture"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "noldus_posture" found in WRITE directory.',
          "i" = 'Creating sub directory "NOLDUS_POSTURE" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "NOLDUS_POSTURE"))
  }
  
  vct_fpa_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "noldus",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.xlsx")
  
  cli::cli_alert_info("Cleaning Noldus annotation files.")
  counter <- 0
  
  for (i in seq_along(vct_fpa_read)) {
    
    fpa_read <-
      vct_fpa_read[i]
    fnm_read <- 
      fs::path_file(fpa_read)
    
    cli::cli_progress_message(
      "Cleaning file #{counter + 1}: {fnm_read}"
    )
    # message("Cleaning file #", i , ": ", fnm_read, "...",
    #         appendLF = FALSE)
    
    fli <-
      fnm_read %>%
      fs::path_ext_remove() %>%
      str_split(pattern =  "_| - ") %>%
      vec_unchop() %>%
      vec_slice(1:4) %>%
      str_to_upper() %>%
      vec_chop() %>%
      rlang::set_names(nm = c("schema",
                              "study",
                              "sbj_vst",
                              "coder_initials"))
    # If "Master Project" is in filename. Shouldnt be anymore.
    # fli <- 
    #   fnm_read %>%
    #   fs::path_ext_remove() %>% 
    #   str_split(pattern = "_| - ") %>% 
    #   vctrs::vec_unchop() %>% 
    #   vctrs::vec_slice(c(1L, 4L, 5L, 6L)) %>% 
    #   str_to_upper() %>% 
    #   vec_chop() %>% 
    #   rlang::set_names(nm = c("schema",
    #                           "study",
    #                           "sbj_vst",
    #                           "coder_initials"))
    
    if (fli$study == "CO") {
      
      fli$subject <- 
        fli$sbj_vst %>% 
        as.integer()
      fli$visit <- 
        1L
      
    } else {
      
      fli$subject <- 
        fli$sbj_vst %>% 
        str_extract(pattern = "(?:(?!v|V|d).)*") %>% 
        as.integer()
      fli$visit <- 
        fli$sbj_vst %>% 
        str_extract(pattern = "([^v|V|d]*)$") %>% 
        as.integer()
      
    }
    
    fli$file_source <- 
      "NOLDUS"
    
    df_raw <- 
      readxl::read_xlsx(path = fpa_read,
                        progress = FALSE) %>% 
      as.data.table()
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                    CLEAN #1: CONSISTENCY                  ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    colnames(df_raw) <- 
      colnames(df_raw) %>% 
      str_to_lower()
    df_raw <- 
      df_raw %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior == "*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      ) %>% 
      # Make Posture Uncoded codes the same as Activity.
      mutate(
        behavior = 
          dplyr::recode(
            behavior,
            "Uncoded; Start/Stop" = "[U] Start/Stop",
            "Start Time"          = "[U] Start Time",
            "Stop Time"           = "[U] Stop Time",
            # posture
            "Uncoded - Dark/Obscured/OoF"         = "[U] Dark/Obscured/OoF",
            "[P] Other"                           = "[P] Other - Posture",
            "[P} Lying"                           = "[P] Lying",
            "[P] Crouching / Kneeling / Squating" = "[P] Crouching/Kneeling/Squatting",
            "[M] Other"                           = "[M] Other - Movement",
            "[M] Crouching / Squating"            = "[M] Crouching/Squatting",
            # behavior
            "[Q] Cooking/Meal Preperation"  = "[Q] Cooking/Meal Preparation", # DOCOMP
            "[Q] Caring Grooming - Self"    = "[Q] Caring/Grooming - Self", # DOCOMP
            "[LQ] Caring Grooming - Self"   = "[LQ] Caring/Grooming - Self",
            "[LQ] Cooking/Meal Preperation" = "[LQ] Cooking/Meal Preparation",
            "[HQ] Caring Grooming - Self"   = "[HQ] Caring/Grooming - Self",
            "[HQ] Cooking/Meal Preperation" = "[HQ] Cooking/Meal Preparation"
          )
      ) %>% 
      as.data.table()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                CHECK #1: START & STOP APPLIED              ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Change start/stop to corresponding time zone during shaping.
    fli$start <- suppressWarnings(
      df_raw$comment[df_raw$behavior == "[U] Start Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    fli$stop <- suppressWarnings(
      df_raw$comment[df_raw$behavior == "[U] Stop Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    
    # Remove since DOCOMP is done?
    # if (!rlang::is_null(fdr_prj) & 
    #     str_detect(fdr_prj,
    #                pattern = "DOCOMP")) {
    #   
    #   fli$start <- 
    #     df_raw$time_relative_hms[df_raw$behavior == "[U] Start Time"]
    #   fli$stop <- 
    #     df_raw$time_relative_hms[df_raw$behavior == "[U] Stop Time"]
    # }
    
    # See if start and stop was entered by coder.
    if ((df_raw$behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      message("",
              appendLF = TRUE)
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    } else if (is.na(fli$start)) {
      
      message("",
              appendLF = TRUE)
      warning(
        schema, " - ", fnm_obs,
        "Start Time is not in MM-DD-YYYY hh:mm:ss",
        call. = FALSE
      )
      
    } else if (is.na(fli$stop)) {
      
      message("",
              appendLF = TRUE)
      warning(
        schema, " - ", fnm_obs,
        "Stop Time is not in MM-DD-YYYY hh:mm:ss",
        call. = FALSE
      )
      
    }
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                CHECK #2: START & STOP ENTRIES              ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    dtm_relative_hmsf_end <- 
      df_raw$time_relative_hmsf[nrow(df_raw)]
    dtm_relative_hms_start <- 
      df_raw$time_relative_hms[df_raw$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      df_raw$time_relative_hms[df_raw$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      df_raw$time_relative_hmsf[df_raw$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      df_raw$time_relative_hmsf[df_raw$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    df_chk <- 
      df_raw %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      ) %>% 
      as.data.table()
    
    chk_start <- 
      df_chk$time_relative_hmsf[1] != dtm_relative_hmsf_start
    chk_end <- 
      !dplyr::near(
        x = 
          (df_chk$time_relative_hmsf[nrow(df_chk)] 
           + df_chk$duration_sf[nrow(df_chk)]) %>% 
          seconds(),
        y = 
          dtm_relative_hmsf_end %>% 
          seconds(),
        tol = .01
      )
    chk_abs_vs_rel <- 
      !dplyr::near(
        x = difftime(time1 = fli$stop,
                     time2 = fli$start,
                     units = "secs"),
        y = difftime(time1 = dtm_relative_hms_stop,
                     time2 = dtm_relative_hms_start,
                     units = "secs"),
        tol = 5
      )
    
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    if (chk_start) {
      
      message("",
              appendLF = TRUE)
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    } else if (chk_end) {
      
      message("",
              appendLF = TRUE)
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    } else if (chk_abs_vs_rel) {
      
      message("",
              appendLF = TRUE)
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1
      - t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                        CHECK #3: IDK                      ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    nrw_chk <- 
      nrow(df_chk)
    int_duration <- 
      c(diff.POSIXt(df_chk$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_chk$time_relative_hms[nrw_chk],
                 units = "secs")) %>% 
      as.integer()
    
    if (is.na(int_duration) %>% 
        any()) {
      
      message("",
              appendLF = TRUE)
      
      stop("Duration of a code is less than a second??? IDK man")
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    fnm_write <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        fli$schema,
        fli$coder_initials,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    fpa_write <- 
      fs::path(
        fdr_write,
        dplyr::case_when(
          fli$schema == "ACTIVITY" ~ 
            str_subset(vct_fsb_write,
                       pattern = regex("noldus_activity",
                                       ignore_case = TRUE)),
          fli$schema == "POSTURE" ~ 
            str_subset(vct_fsb_write,
                       pattern = regex("noldus_activity",
                                       ignore_case = TRUE))
        ),
        fnm_write
      )
    
    data.table::fwrite(
      df_raw,
      file = fpa_write,
      sep = ","
    )
    
    counter <- 
      counter + 1
    # message("DONE\n",
    #         appendLF = TRUE)
    
  }
  
  cli::cli_progress_step(
    msg = "DONE",
    msg_done = "SUCCESS. Cleaned {counter} files out of {length(vct_fpa_read)} files."
  )
  
}
clean_noldus_v3 <- function(fdr_read,
                            fdr_write,
                            fdr_project = NULL,
                            fld_act = "NOLDUS_ACTIVITY",
                            fld_pos = "NOLDUS_POSTURE",
                            filter_sub = NULL,
                            project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Include code for fdr_project
  # -   Include code for filter_sub
  # -   Change "fli" to "df_info" to make it easier to access.
  # -   Incorporate get_fpa_read_noldus as it is the same across all functions.
  # -   Incorporate initiate_wrangle as it is the same across all wrangle functions.
  # -   Change warnings/errors from base to cli.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read_noldus
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of clean noldus files.
  # ARG: fdr_write
  #      File directory of shaped noldus files.
  # ARG: fdr_project
  #      File directory to where all the shaped data resides in the project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  #      Unless project only is TRUE.
  # ARG: fld_act
  #      Folder name of shaped noldus activity files.
  # ARG: fld_pos
  #      Folder name of shaped noldus posture files.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: project_only
  #      Should shaped files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "1_AIM1_RAW_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_project <-
    NULL
  fld_act <-
    "NOLDUS_ACTIVITY"
  fld_pos <-
    "NOLDUS_POSTURE"
  filter_sub <-
    NULL
  project_only <- FALSE
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   project_only = project_only,
                   type         = "Clean",
                   file_source  = "NOLDUS")
  vct_fpa_read <- 
    get_fpa_read_noldus(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_activity = fld_act,
      name_posture  = fld_pos,
      name_merged   = NULL,
      filter_sub    = filter_sub
    )
  
  for (i in cli_progress_along(vct_fpa_read,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_read <-
      vct_fpa_read[i]
    fnm_read <- 
      fs::path_file(fpa_read)
    
    df_info <-
      fnm_read %>%
      fs::path_ext_remove() %>%
      str_split(pattern =  "_| - ") %>%
      vec_unchop() %>%
      vec_slice(1:4) %>%
      str_to_upper() %>%
      vec_chop() %>%
      as.data.table() %>% 
      rlang::set_names(nm = c("schema",
                              "study",
                              "sbj_vst",
                              "coder_initials")) %>% 
      as_tibble() %>%
      mutate(
        flac_aim = info_flac_aim,
        file_source = info_source,
        subject =
          fifelse(info_flac_aim == "AIM1",
                  yes = as.integer(sbj_vst),
                  no  = 
                    sbj_vst %>% 
                    str_extract("(?:(?!v|V|d).)*") %>% 
                    as.integer(),
                  na  = NA_integer_),
        visit = 
          fifelse(info_flac_aim == "AIM1",
                  yes = 1L,
                  no  = 
                    sbj_vst %>% 
                    str_extract("([^v|V|d]*)$") %>% 
                    as.integer(),
                  na  = NA_integer_)
      ) %>% 
      as.data.table()
    
    df_raw <- 
      readxl::read_xlsx(path = fpa_read,
                        progress = FALSE) %>% 
      as.data.table()
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                    CLEAN #1: CONSISTENCY                  ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    colnames(df_raw) <- 
      colnames(df_raw) %>% 
      str_to_lower()
    df_cln <- 
      df_raw %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior == "*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      ) %>% 
      # Make Posture Uncoded codes the same as Activity.
      mutate(
        behavior = 
          dplyr::recode(
            behavior,
            "Uncoded; Start/Stop" = "[U] Start/Stop",
            "Start Time"          = "[U] Start Time",
            "Stop Time"           = "[U] Stop Time",
            # posture
            "Uncoded - Dark/Obscured/OoF"         = "[U] Dark/Obscured/OoF",
            "[P] Other"                           = "[P] Other - Posture",
            "[P} Lying"                           = "[P] Lying",
            "[P] Crouching / Kneeling / Squating" = "[P] Crouching/Kneeling/Squatting",
            "[M] Other"                           = "[M] Other - Movement",
            "[M] Crouching / Squating"            = "[M] Crouching/Squatting",
            # behavior
            "[Q] Cooking/Meal Preperation"  = "[Q] Cooking/Meal Preparation", # DOCOMP
            "[Q] Caring Grooming - Self"    = "[Q] Caring/Grooming - Self", # DOCOMP
            "[LQ] Caring Grooming - Self"   = "[LQ] Caring/Grooming - Self",
            "[LQ] Cooking/Meal Preperation" = "[LQ] Cooking/Meal Preparation",
            "[HQ] Caring Grooming - Self"   = "[HQ] Caring/Grooming - Self",
            "[HQ] Cooking/Meal Preperation" = "[HQ] Cooking/Meal Preparation"
          )
      ) %>% 
      as.data.table()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                CHECK #1: START & STOP APPLIED              ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Change start/stop to corresponding time zone during shaping.
    df_info$start <- suppressWarnings(
      df_cln$comment[df_cln$behavior == "[U] Start Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    df_info$stop <- suppressWarnings(
      df_cln$comment[df_cln$behavior == "[U] Stop Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    
    # See if start and stop was entered by coder.
    if ((df_cln$behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      # IDK man.
      cli_abort("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    } else if (is.na(df_info$start)) {
      cli_warn(c(
        "{fnm_read}:",
        "!" = "Start Time is not in MM-DD-YYYY hh:mm:ss"
      ))
    } else if (is.na(df_info$stop)) {
      cli_warn(c(
        "{fnm_read}:",
        "!" = "Stop Time is not in MM-DD-YYYY hh:mm:ss"
      ))
    }
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                CHECK #2: START & STOP ENTRIES              ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    dtm_relative_hmsf_end <- 
      df_cln$time_relative_hmsf[nrow(df_cln)]
    dtm_relative_hms_start <- 
      df_cln$time_relative_hms[df_cln$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      df_cln$time_relative_hms[df_cln$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      df_cln$time_relative_hmsf[df_cln$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      df_cln$time_relative_hmsf[df_cln$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    df_chk <- 
      df_cln %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      ) %>% 
      as.data.table()
    
    chk_start <- 
      df_chk$time_relative_hmsf[1] != dtm_relative_hmsf_start
    chk_end <- 
      !dplyr::near(
        x = 
          (df_chk$time_relative_hmsf[nrow(df_chk)] 
           + df_chk$duration_sf[nrow(df_chk)]) %>% 
          seconds(),
        y = 
          dtm_relative_hmsf_end %>% 
          seconds(),
        tol = .01
      )
    chk_abs_vs_rel <- 
      !dplyr::near(
        x = difftime(time1 = df_info$stop,
                     time2 = df_info$start,
                     units = "secs"),
        y = difftime(time1 = dtm_relative_hms_stop,
                     time2 = dtm_relative_hms_start,
                     units = "secs"),
        tol = 5
      )
    
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    if (chk_start) {
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      cli_abort("First code does not align with start time.")
    } else if (chk_end) {
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      cli_abort("Stop time does not match last code timestamp + its duration.")
    } else if (chk_abs_vs_rel) {
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1 - t2
      cli_abort(c(
        "{fnm_read}",
        "x" = "Difference between absolute and relative times are not < 5 seconds",
        "i" = "Difference = {diff_abs_rel} seconds" 
      ))
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                        CHECK #3: IDK                      ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    nrw_chk <- 
      nrow(df_chk)
    int_duration <- 
      c(diff.POSIXt(df_chk$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_chk$time_relative_hms[nrw_chk],
                 units = "secs")) %>% 
      as.integer()
    
    if (anyNA(int_duration)) {
      cli_abort("Duration of a code is less than a second??? IDK man")
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    fnm_write <- 
      df_info %>% 
      unite(col = "file_name",
            study, subject, visit, file_source, schema, coder_initials) %>% 
      pull(file_name) %>% 
      fs::path_ext_set(ext = "csv")
    
    if (project_only) {
      fpa_project <- 
        fs::path(
          dplyr::case_when(
            df_info$schema == "ACTIVITY" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_act,
                                         ignore_case = TRUE)),
            df_info$schema == "POSTURE" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_pos,
                                         ignore_case = TRUE))
          ),
          fnm_write
        )
      data.table::fwrite(
        df_cln,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
      cnt <-
        cnt + 1
      next()
    }
    
    fpa_write <- 
      fs::path(
        dplyr::case_when(
          df_info$schema == "ACTIVITY" ~ dir_ls(fdr_write,
                                                type = "directory",
                                                regexp = fld_act),
          df_info$schema == "POSTURE" ~ dir_ls(fdr_write,
                                               type = "directory",
                                               regexp = fld_pos)
        ),
        fnm_write
      )
    data.table::fwrite(
      df_cln,
      file = fpa_write,
      sep = ",",
      showProgress = FALSE
    )
    
    if(!is_empty(fdr_project)) {
      fpa_project <- 
        fs::path(
          dplyr::case_when(
            df_info$schema == "ACTIVITY" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_act,
                                         ignore_case = TRUE)),
            df_info$schema == "POSTURE" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_pos,
                                         ignore_case = TRUE))
          ),
          fnm_write
        )
      data.table::fwrite(
        df_cln,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
    }
    
    cli_progress_update()
    cnt <- 
      cnt + 1
    
  }
  
  cli_progress_done()
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
clean_oxford_v1 <- function(fdr_raw,
                            fdr_cln,
                            vec_subject,
                            vec_coder_initials) {
  
  # # CHANGES:
  
  # -include cli code for messages
  
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NA
  
  # ARG: fdr_raw
  #      File directory of timestamp file.
  # ARG: fdr_cln
  #      File name of timestamp file.
  # ARG: vec_subject
  #      Optional vector of subjects to subset from fdr_raw.
  # ARG: vec_coder_initials
  #      Named vector where names(vector) = lastnames & elements = initials
  
  # # TESTING
  
  # key_coder_initials <- 
  #   c("bayer"        = "AB",
  #     "smith"        = "AS",
  #     "peraltawerns" = "AP",
  #     "miller"       = "EM",
  #     "martinez"     = "JM",
  #     "chang"        = "LC",
  #     "almanza"      = "MA")
  # vec_subject <- 
  #   c(
  #     # Normal
  #     "1052", "1053", "1068", "1074", "1085", "1087", "1095", "1096", "1100",
  #     "1115", "1125", "1127", "1160", "1162", "1163", "1164", "1167", "1183",
  #     "1196",
  #     # Impaired
  #     "2129", "2176", "3070", "3172", "3177", "4054", "5034", "5035", "5036",
  #     "5040", "5057", "7059", "7064", "7174",
  #     # Limited
  #     "2038", "2123", "2139", "2143", "3065", "4055", "5076", "5079", "5086",
  #     "5090", "5093", "6180", "7092", "7170"
  #   )
  # fdr_raw <- 
  #   fs::path("S:/_V_PAHRL/FLAC/OxfordImageBrowser-win32-x64",
  #            "3_Downloaded Annotation Files")
  # fdr_clean <- 
  #   fs::path("3_data",
  #            "1_cleaned")
  
  vec_fpa_raw <- 
    fdr_raw %>% 
    fs::dir_ls(recurse = TRUE,
               # glob = "*/7Day/*",
               regexp = "d7"
    ) %>% 
    fs::path_filter(regexp = paste0(vec_subject,
                                    collapse = "|"))
  
  cli::cli_alert_info("Cleaning Oxford annotation files.")
  # cli::cli_progress_message("Cleaned 0 out of {length(vec_fpa_raw)} files.")
  counter <- 0
  
  for (i in seq_along(vec_fpa_raw)) {
    
    fpa_raw <- 
      vec_fpa_raw[i]
    
    fnm_raw <- 
      fpa_raw %>% 
      fs::path_file()
    
    # cli::cli_inform(
    #   message = c("i" = "Cleaning file #{i}: {fnm}...")
    #   # message = paste0("Cleaning file #", 1, ": ", fnm)
    # )
    cli::cli_progress_message(
      "Cleaning file #{counter + 1} out of {length(vec_fpa_raw)} files."
    )
    # message(
    #   fnm_raw, "...",
    #   # "Cleaning file #", i, ": ", fnm
    #   appendLF = FALSE
    # )
    
    fnm_split <- 
      fnm_raw %>% 
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      unlist()
    study <- 
      fnm_split[1]
    sbj_vst <- 
      fnm_split[2]
    subject <- 
      sbj_vst %>% 
      str_extract(pattern = "(?:(?!v|V|d).)*") %>% 
      as.integer()
    visit <- 
      sbj_vst %>% 
      str_extract(pattern = "([^v|V|d]*)$") %>% 
      as.integer()
    schema <- 
      fnm_split[3] %>% 
      str_to_lower()
    coder <- 
      fnm_split[4] %>% 
      str_to_lower()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                           CHECK #1                         ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    chk_study <- 
      !(fnm_split[1] %in% c("FLAC", "CO"))
    chk_subject <- 
      subject %>% 
      str_detect(pattern = "[1|2|3|4|5|6|7]\\d{3}",
                 negate = TRUE)
    chk_visit <- 
      visit %>% 
      str_detect(pattern = "1|2|3|7",
                 negate = TRUE)
    chk_schema <- 
      schema %>% 
      str_detect(pattern = regex("activity|posture",
                                 ignore_case = TRUE),
                 negate = TRUE)
    chk_coder <- 
      coder %>% 
      str_detect(pattern = 
                   key_coder_initials %>% 
                   names() %>% 
                   paste0(collapse = "|"),
                 negate = TRUE)
    
    if (any(chk_study, chk_subject,
            chk_schema, chk_visit, chk_coder)) {
      
      stop("iabfianfilnawelfinawieubf") # WARNING 1
      
    }
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    df_img_raw <- 
      data.table::fread(
        file = fpa_raw,
        sep = ",",
        header = TRUE, # category column doesnt have name in raw files.
        # col.names = c("start_time", "end_time", "duration", "annotation", "category"),
        fill = TRUE
      )
    
    # Consistency: make all lowercase and _ seperator.
    colnames(df_img_raw) <-
      colnames(df_img_raw) %>%
      str_to_lower() %>%
      str_replace(pattern = "time",
                  replacement = "_time")
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                           CHECK #2                         ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    chk_undefined <- 
      (visit == 7L & "undefined" %in% df_img_raw[["annotation"]]) |
      (visit %in% c(1L, 2L, 3L) &
         !any(which(df_img_raw$annotation == "undefined") %in% c(1L, nrow(df_img_raw))))
    
    if (chk_undefined) {
      ind_undefined <- 
        which(df_img_raw$annotation == "undefined")
      fdr_img_set <- 
        "S:/_V_PAHRL/FLAC/OxfordImageBrowser-win32-x64/2_Browser Images to Annotate"
      dtm_img_og <- 
        fs::path(fdr_img_set,
                 paste(str_to_lower(study),
                       sbj_vst,
                       sep = "_")) %>% 
        fs::dir_ls(pattern = "JPG") %>% 
        fs::path_file() %>% 
        str_sub(start = 18,
                end = 32) %>% 
        lubridate::ymd_hms(tz = "America/Chicago")
      dtm_end_strped <- 
        df_img_raw$end_time %>% 
        lubridate::parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                                   tz     = "UTC")
      dtm_start_strped <- 
        df_img_raw$start_time %>% 
        lubridate::parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                                   tz     = "UTC")
      if (visit == 7L) {
        vec_ind_mistake <- 
          ind_undefined
      } else {
        vec_ind_mistake <- 
          ind_undefined[!ind_undefined %in% c(1L, nrow(tib_img_raw))]
      }
      
      for (ii in seq_along(vec_ind_mistake)) {
        ind_mistake <- 
          vec_ind_mistake[ii]
        
        dtm_mis_start <- 
          dtm_start_strped[ind_mistake] %>% 
          with_tz(tzone = "America/Chicago")
        ind_mis_start <- 
          which(
            dtm_img_og %in% dtm_mis_start
          )
        ind_mis_start <- 
          ind_mis_start[length(ind_mis_start)] # I can't remember why I do this. Maybe cuz there are sometimes imgs with same date time???
        dtm_mis_start <- 
          dtm_mis_start %>% 
          format("%I:%M:%S %p")
        
        dtm_mis_end <- 
          dtm_end_strped[ind_mistake] %>% 
          with_tz(tzone = "America/Chicago")
        ind_mis_end <- 
          which(
            dtm_img_og %in% dtm_mis_end
          )
        ind_mis_end <- 
          ind_mis_end[1] # Or this.
        dtm_mis_end <- 
          dtm_mis_end %>% 
          format("%I:%M:%S %p")
        
        warning(
          "File #", i, ": ", fnm_raw, ": WARNING 2\n",
          "\n",
          "A sequence of IMGs were left uncoded; no annotation was applied.\n",
          "Inform coder to apply an annotation with following info:\n",
          "Uncoded Anno File Line: ", ind_mistake + 1, "\n",
          "Uncoded IMG Numbers   : ", ind_mis_start, " - ", ind_mis_end, "\n",
          "Uncoded Oxford Time   : ", dtm_mis_start, " - ", dtm_mis_end, "\n",
          call. = FALSE
        )
      }
      next()
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            CLEAN                         ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    if (ncol(df_img_raw) == 4) {
      
      # For activity files coded with Versions before 8 and if an activity file
      # was not opened beforehand. Posture files will also be changed but its ok
      # ...for now.
      df_img_cln <- 
        df_img_raw %>%
        # dtplyr doesnt work when separate results in same named column.
        as_tibble() %>%
        tidyr::separate(col = "annotation",
                        into = c("annotation",
                                 "category"),
                        remove = FALSE,
                        sep = ",",
                        fill = "right") %>% 
        mutate(category = as.integer(category)) %>% 
        as.data.table()
      
    } else {
      
      # For activity files coded with Version 8 and opened in excel at all.
      df_img_cln <-
        df_img_raw %>%
        rename(category = v5) %>% 
        as.data.table()
      # setnames(df_img_raw,
      #          new = "category",
      #          old = "v5")[]
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    # Move files into project folder and fix column names.
    fnm_cln <- 
      fnm_raw %>% 
      str_to_lower()
    fpa_cln <- 
      fs::path(
        fdr_cln,
        dplyr::case_when(schema == "activity" ~ "oxford_activity",
                         schema == "posture" ~ "oxford_posture"),
        fnm_cln
      )
    data.table::fwrite(
      df_img_cln,
      file = fpa_cln,
      sep = ","
    )
    
    # message(
    #   "DONE",
    #   appendLF = TRUE
    # )
    # cli::cli_inform(
    #   message = "DONE\n"
    # )
    counter <- 
      counter + 1
    # cli::cli_status("Cleaned 0 out of {length(vec_fpa_raw)} files.")
  }
  
  cli::cli_progress_step(msg = "DONE",
                         msg_done = "SUCCESS. Cleaned ")
  
}


clean_rmr <- function() {
  
  # RMR files manually checked through and cleaned.
  
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                             SHAPE FUNCTIONS                             ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shape_actigraph_raw_flac_v2 <- function() {
  
  # Decided to make a separate flac function as the files are manually cleaned
  # and dont follow the flow of other cleaning functions.
  # Piped head.data.
  # Piped start.time and made it streamlined.
  # Removed "start.time" object since it is only used in 100Hz function.
  # Go back to using data.table for reading and writing csvs. arrow error that popped up
  # with writing csv: "Error: NotImplemented: Casting a timestamp with time zone to 
  # string is not yet supported on Windows." Only use arrow for when you would've
  # wrote an rds (reference rowland function).
  # Piped data, keep it as a tibble and made it faster.
  
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  ag_model_loc <- "GT3X_LW"
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex(paste0(ag_model_loc, "_CSV_RAW"),
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write) {
    # Create a directory under fdr_write that contains "ag_model_loc"_CSV_RAW
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "{ag_model_loc}_CSV_RAW" found in WRITE directory.',
          "i" = 'Creating sub directory "{ag_model_loc}_CSV_RAW" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   paste0(ag_model_loc, "_CSV_RAW")))
  } 
  
  vct_fpt_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = paste0(ag_model_loc, "_CSV_RAW"),
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.csv")
  
  cli::cli_alert_info("Shaping chamber files.")
  counter <- 0
  
  for (i in seq_along(vct_fpt_read)) {
    
    fpt_read <-
      vct_fpt_read[63]
    
    fnm_read <- 
      fs::path_file(fpt_read)
    # If it isnt an AIM2 data file
    fnm_split <- 
      fnm_read %>% 
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop()
    chk_visit <- 
      str_detect(fnm_split[4],
                 pattern = "\\d{4}$")
    
    if (chk_visit) {
      
      # Should be a FLAC that has the subject_V#....csv.
      fli <- 
        fnm_split %>% 
        vec_chop() %>% 
        rlang::set_names(nm = 
                           c("study",
                             "model_fnm",
                             "location",
                             "subject",
                             "visit"))
      fli$epoch_fnm <- 
        str_remove(fli$visit,
                   pattern = "V\\d{1}")
      
      str_detect(fli$epoch_fnm,
                 pattern = "\\d*")
      str_extract(fli$epoch_fnm,
                  pattern = "\\d*") %>% 
        as.integer() %>%
        sprintf(fmt = "00:00:%02d")
      
      fcase(
        fli$epoch_fnm == "RAW", "00:00:00",
        fli$epoch_fnm %>% 
          str_detect(pattern = "\\d*"), 
        fli$epoch_fnm %>% 
          str_extract(pattern = "\\d*") %>% 
          as.integer(),
        default = NA
      )
      
    }
    fnm_split[4]
    str_remove(pattern = ".*_.*_.{2}_\\d{4}")
    fli$epoch_fnm <- "1sec"
    
    fli$epoch_fnm
    fcase(fli$epoch_fnm == "RAW", "00:00:00",
    )
    dplyr::case_when(
      fli$epoch_fnm == "RAW" ~ "00:00:00",
      fli$epoch_fnm == "1sec" ~ "00:00:01",
      fli$epoch_fnm == "60sec" ~ "00:00:60"
    )
    
    fli <- 
      readLines(fpt_read,
                n = 10) %>% 
      str_split(pattern = " ") %>% 
      vec_unchop() %>% 
      vec_chop(indices = list(7, 9, 11, 14, 16, 20, c(30, 27), 34)) %>% 
      rlang::set_names(nm =
                         c("model",
                           "actilife_version",
                           "model_firmware",
                           "date_format",
                           "sampling_rate",
                           "filter",
                           "dtm_start",
                           "epoch_df"))
    fli$dtm_start <- 
      fli$dtm_start %>% 
      paste0(collapse = " ") %>% 
      lubridate::mdy_hms() # make local timezone as thats what original code did.
    fli$sampling_rate <- 
      as.double(fli$sampling_rate)
    
    fnm_read %>% 
      fs::path_ext_remove() %>% 
      str_split(pattern = "_")
    
    
    # head.data <- 
    #   fpt_read %>% 
    #   # as.character() %>% 
    #   readLines(n = 10)
    # start.time <- 
    #   paste(str_extract(head.data[4],
    #                     pattern = "[^ ]*$"),
    #         str_extract(head.data[3],
    #                     pattern = "[^ ]*$")) %>% 
    #   lubridate::mdy_hms(tz = Sys.timezone()) # make local timezone as thats what original code did.
    df_cln <- 
      data.table::fread(
        fpt_read,
        skip = 10,
        sep = ",",
        header = TRUE,
        showProgress = FALSE,
      )
    colnames(df_cln) <- 
      colnames(df_cln) %>% 
      str_to_lower()
    
    chk_timestamp <- 
      !"timestamp" %in% colnames(df_cln)
    
    if (chk_timestamp) {
      
      df_shp <- 
        df_cln %>% 
        transmute(
          datetime = seq.POSIXt(from = fli$dtm_start,
                                by = 1 / fli$sampling_rate,
                                length.out = dim(df_cln)[1]),
          axis_x   = `accelerometer x`,
          axis_y   = `accelerometer y`,
          axis_z   = `accelerometer z`
        ) %>% 
        as.data.table()
      
    } else {
      
      df_shp <- 
        df_cln %>% 
        transmute(
          datetime = timestamp,
          axis_x   = `accelerometer x`,
          axis_y   = `accelerometer y`,
          axis_z   = `accelerometer z`
        ) %>% 
        as.data.table()
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fnm_write <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        fli$schema,
        fli$coder_intials,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    fpt_write <- 
      fs::path(
        fdr_write,
        dplyr::case_when(
          fli$schema == "ACTIVITY" ~ 
            str_subset(vct_fsb_write,
                       pattern = regex("noldus_activity",
                                       ignore_case = TRUE)),
          fli$schema == "POSTURE" ~ 
            str_subset(vct_fsb_write,
                       pattern = regex("noldus_posture",
                                       ignore_case = TRUE))
        ),
        fnm_write
      )
    data.table::fwrite(
      df_shp,
      file = fpt_write,
      sep = ","
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpt_write,
                              ext = "feather")
    )
    
    data.table::fwrite(
      df_shp
    )
    
  }
}
shape_actigraph_raw_flac_v3 <- function(fdr_read,
                                        fdr_write,
                                        fdr_project = NULL,
                                        folder = "NOLDUS_ACTIVITY",
                                        freq = 100,
                                        filter_sub = NULL,
                                        filter_loc = NULL,
                                        project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Incorporate get_fpa_read as it is the same for non Noldus/Oxford.
  # -   Incorporate filter_sub code.
  # -   Incorporate project code.
  # -   Incorporate initiate_wrangle as it is the same across all wrangle functions.
  # -   Have code be data.table centric as it saves a lot of memory.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of cleaned chamber files.
  # ARG: fdr_write
  #      File directory of shaped chamber files.
  # ARG: folder
  #      Folder name of shaped chamber files.
  # ARG: fdr_project
  #      File directory to where all the merged data resides in the project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: filter_loc
  #      Integer vector to subset from vct_fpa_read.
  # ARG: project_only
  #      Should merged files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  fdr_project <- 
    NULL
  folder <- 
    "GT3X_LW_CSV_RAW"
  freq <- 100
  filter_sub <- 
    NULL
  filter_loc <- 
    NULL
  project_only <- 
    FALSE
  
  ag_model_loc <-
    folder %>% 
    str_split(pattern = "_") %>% 
    vec_unchop() %>% 
    vec_slice(c(1, 2)) %>% 
    paste(collapse = "_")
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   filter_loc   = filter_loc,
                   project_only = project_only,
                   type         = "Shap",
                   file_source  = ag_model_loc)
  # ag_model <- 
  #   ag_model_loc %>% 
  #   str_split(pattern = "_") %>% 
  #   pluck(1, 1)
  # ag_loc <- 
  #   ag_model_loc %>% 
  #   str_split(pattern = "_") %>% 
  #   pluck(1, 2)
  freq <- 
    as.integer(freq)
  vct_fpa_read <- 
    get_fpa_read(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_source_1 = folder,
      filter_sub    = filter_sub
    )
  if (!is_empty(filter_loc)) {
    vct_fpa_read <- 
      vec_slice(vct_fpa_read,
                filter_loc)
  }
  df_start_stop <- 
    fdr_write %>% 
    dir_ls(
      type = "file",
      regexp = "df_start_stop\\.feather$"
    ) %>% 
    arrow::read_feather() %>% 
    add_row(study = "CO",
            subject = 3045L,
            visit = 1L,
            start = mdy_hms("10-02-2019 08:02:00",
                            tz = "America/Denver"),
            stop = mdy_hms("10-02-2019 20:07:00",
                           tz = "America/Denver"))
  
  for (i in seq_along(vct_fpa_read)) {
    # for (i in cli_progress_along(vct_fpa_read,
    #                              format = progress_format,
    #                              clear = FALSE)) {
    
    fpa_read <- 
      vct_fpa_read[i]
    fnm_read <- 
      fs::path_file(fpa_read)
    message(fnm_read)
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    df_info <- 
      fnm_read %>% 
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop() %>% 
      vec_chop()
    setDT(df_info)
    setnames(df_info,
             c("study",
               "model_fnm",
               "location",
               "sbj_epoch"))
    df_info[, `:=`(
      subject = 
        sbj_epoch %>% 
        str_extract(pattern = "\\d*") %>%
        as.integer(),
      epoch_fnm = 
        sbj_epoch %>% 
        str_remove(pattern = "\\d*"),
      visit = 
        fifelse(info_flac_aim == "AIM1",
                yes = 1L,
                no  = 
                  sbj_epoch %>% 
                  str_extract(pattern = "\\d*") %>% 
                  as.integer(),
                na  = NA_integer_),
      flac_aim = info_flac_aim,
      file_source = info_source,
      time_zone = fcase(info_flac_aim == "AIM1", "America/Denver",
                        info_flac_aim == "AIM2", "America/Chicago")
    )]
    df_info[, c(
      "model_df",
      "actilife_version",
      "model_firmware",
      "date_format",
      "sampling_rate",
      "filter",
      "dte_start",
      "tim_start",
      "epoch_df") := 
        readLines(fpa_read,
                  n = 10) %>%
        str_split(pattern = " ") %>%
        vec_unchop() %>%
        vec_chop(indices = list(7, 9, 11, 14, 16, 20, 30, 27, 34))]
    df_info[, `:=`(dtm_start =
                     paste(dte_start, tim_start, sep = " ") %>%
                     lubridate::mdy_hms(tz = df_info$time_zone),
                   sampling_rate = as.integer(sampling_rate),
                   dte_start = NULL,
                   tim_start = NULL)]
    setcolorder(df_info, c("study", "subject", "visit"))
    # chk_visit <- 
    #   fli %>% str_detect(pattern = "V\\d{1}") %>% any()
    # 
    # if (chk_visit) {
    #   
    #   # Should be a FLAC that has the subject_V#....csv.
    #   fli <- 
    #     fli %>% 
    #     rlang::set_names(nm = 
    #                        c("study",
    #                          "model_fnm",
    #                          "location",
    #                          "subject",
    #                          "visit",
    #                          "epoch_fnm"))
    #   fli$visit <- 
    #     fli$visit %>% 
    #     str_remove(pattern = "V") %>% 
    #     as.integer()
    #     # str_extract(pattern = "\\d*") %>% 
    #     # lubridate::seconds()
    #     # str_remove(pattern = "ec") %>% 
    #       # str_to_upper()
    #   # str_detect(fli$epoch_fnm,
    #   #             pattern = "\\d*")
    #   # str_extract(fli$epoch_fnm,
    #   #             pattern = "\\d*") %>%
    #   #   as.integer() %>%
    #   # sprintf(fmt = "00:00:%02d")
    #   # 
    #   #   fcase(
    #   #     fli$epoch_fnm == "RAW", "00:00:00",
    #   #     fli$epoch_fnm %>% 
    #   #       str_detect(pattern = "\\d*"), 
    #   #     fli$epoch_fnm %>% 
    #   #       str_extract(pattern = "\\d*") %>% 
    #   #       as.integer(),
    #   #     default = NA
    #   #   )
    #   
    # } else {
    #   
    #   fli <- 
    #     fli %>% 
    #     rlang::set_names(nm = 
    #                        c("study",
    #                          "model_fnm",
    #                          "location",
    #                          "subject",
    #                          "epoch_fnm"))
    #   fli$visit <- 
    #     1L
    #   
    # }
    #          model_fnm     = fifelse(test = model_fnm == "AG",
    #                                  yes = "GT3X",
    #                                  no = model_fnm,
    #                                  na = NA_character_)
    
    # if(df_info$subject %in% vct_subject_filter) next()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            CHECKS                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
    # RAW files will always say that it was created from a GT3X+ file regardless
    # if the model was a GT3X or a LINK so it isnt used at all for checks.
    # df_chk <- 
    #   df_info %>% 
    #   transmute(
    #     study         = !study %in% c("CO", "FLAC", "DLW"),
    #     subject       = is.na(subject) | !is.integer(subject),
    #     visit         = str_length(visit) != 1L | !is.integer(visit),
    #     ag_model_fnm  = model_fnm != ag_model,
    #     ag_model_df   = FALSE,
    #     ag_location   = location != ag_loc,
    #     model_fnm     = !model_fnm %in% c("GT3X", "LINK"),
    #     model_df      = FALSE,
    #     location      = !location %in% c("LW", "RH", "RH"),
    #     epoch_fnm     = epoch_fnm != "RAW",
    #     epoch_df      = epoch_df != "00:00:00",
    #     date_format   = date_format != "M/d/yyyy",
    #     sampling_rate = sampling_rate != freq,
    #     filter        = filter != "Normal"
    #   ) %>% 
    #   as.data.table()
    # 
    # # # FOR EPOCH FILES.
    # # GT3XPlus
    # # wGT3XBT
    # 
    # if (any(df_chk)) {
    #   
    #   chk <- 
    #     df_chk %>% 
    #     unlist() %>% 
    #     which() %>% 
    #     names()
    #   
    #   stop(chk)
    #   
    # }
    
    df_shp <- 
      data.table::fread(
        fpa_read,
        skip = 10,
        sep = ",",
        header = TRUE,
        showProgress = FALSE,
      )
    # setnames(df_shp, str_to_lower)
    setnames(
      df_shp,
      old = c("Accelerometer X", "Accelerometer Y", "Accelerometer Z"),
      new = c("axis_x", "axis_y", "axis_z")
    )
    
    chk_timestamp <- 
      !"Timestamp" %in% colnames(df_shp)
    
    if (chk_timestamp) {
      df_shp[, datetime := seq.POSIXt(from = ..df_info$dtm_start, 
                                      by = 1/..df_info$sampling_rate, 
                                      length.out = dim(..df_shp)[1])]
    } else {
      setnames(df_shp,
               old = "Timestamp",
               new = "datetime")
    }
    
    # On & off
    df_on <- 
      df_start_stop[study == df_info$study &
                      subject == df_info$subject &
                      visit == df_info$visit][1]
    if (anyNA.data.frame(df_on)) {
      cli_warn(c(
        "{paste(df_info[, .(study, subject, visit)], 
                collapse = '_')}",
        "!" = "No entry in df_start_stop",
        "i" = "No on/off filtering done."
      ))
    } else {
      df_shp <- 
        df_shp[between(datetime,
                       lower = df_on$start,
                       upper = df_on$stop), ]
    }
    
    # Shape #2
    df_shp[, `:=`(study    = ..df_info$study,
                  subject  = ..df_info$subject,
                  visit    = ..df_info$visit,
                  datetime = lubridate::with_tz(datetime,
                                                tzone = "UTC"))]
    setcolorder(
      df_shp,
      c("study", "subject", "visit","datetime")
    )
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fnm_write <- 
      df_info %>% 
      unite(col = "file_name",
            study, subject, visit, file_source, epoch_fnm) %>% 
      pull(file_name) %>% 
      fs::path_ext_set(ext = "csv")
    
    if (project_only) {
      fpa_project <- 
        fs::path(
          dir_ls(fdr_project,
                 type = "directory",
                 regexp = folder),
          fnm_write
        )
      data.table::fwrite(
        df_shp,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
      arrow::write_feather(
        df_shp,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
      cnt <-
        cnt + 1
      next()
    }
    
    fpa_write <- 
      fs::path(
        dir_ls(fdr_write,
               type = "directory",
               regexp = folder),
        fnm_write
      )
    data.table::fwrite(
      df_shp,
      file = fpa_write,
      sep = ",",
      showProgress = FALSE
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpa_write,
                              ext = "feather")
    )
    
    if(!is_empty(fdr_project)) {
      fpa_project <- 
        fs::path(
          dir_ls(fdr_project,
                 type = "directory",
                 regexp = folder),
          fnm_write
        )
      data.table::fwrite(
        df_shp,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
      arrow::write_feather(
        df_shp,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
    }
    
    cnt <- 
      cnt + 1
    
  }
  
  # cli_progress_done()
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
shape_chamber_v1 <- function(fdr_cln,
                             fdr_shp,
                             fdr_rmr) {
  
  
  # Move getting intensity and second by second into DOINT specific stuff.
  # Match format to how Dr. Strath would like it.
  
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  fli_flac_aim <- 
    fdr_read %>% 
    fs::path_dir() %>% 
    str_extract(pattern = "AIM\\d{1}")
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write_chm <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("chamber",
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write_chm) {
    # Create a directory under fdr_write that contains "chamber"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "chamber" found in WRITE directory.',
          "i" = 'Creating sub directory "CHAMBER" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "CHAMBER"))
  } 
  
  vct_fpt_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "chamber",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.csv")
  
  cli::cli_alert_info("Shaping chamber files.")
  counter <- 0
  
  for (i in seq_along(vct_fpt_read)) {
    
    fpt_read <- 
      vct_fpt_read[1]
    fnm_read <- 
      fs::path_file(fpt_read)
    
    cli::cli_progress_message(
      "Shaping file #{counter + 1}: {fnm_read}"
    )
    
    cli::cli_progress_step(
      msg = "Shaping file #{counter + 1}: {fnm_read}...",
      msg_done = "Shaping file #{counter}: {fnm_read}...DONE",
      msg_failed = "Shaping file #{counter}: {fnm_read}...WARNING"
    )
    
    fli <- 
      fnm_read %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop()
    fli <- 
      list(fli[1] %>% 
             str_extract(pattern = "\\w{2}"),
           fli[1] %>% 
             str_extract(pattern = "\\d{4}") %>% 
             as.integer(),
           1L) %>% 
      rlang::set_names(nm = c("study",
                              "subject",
                              "visit"))
    fli$file_source <- "CHAMBER"
    fli$flac_aim <- fli_flac_aim
    
    df_cln <- 
      data.table::fread(
        input = fpt_read,
        sep = ",",
        header = TRUE
      ) %>% 
      rename_with(.cols = everything(),
                  .fn = str_to_lower)
    df_shp <- 
      df_cln %>% 
      transmute(
        study = fli$study,
        subject = fli$subject,
        visit = fli$visit,
        datetime = lubridate::mdy_hms(datetime,
                                      tz = "America/Chicago"),
        date = lubridate::date(datetime),
        time = format(datetime,
                      "%H:%M:%S"),
        datetime = lubridate::with_tz(datetime,
                                      tzone = "UTC"),
        vo2_ml_min = vo2 * 1000,
        vco2_ml_min = vco2 * 1000,
        rq,
        kcal_min
      ) %>% 
      rename_with(.cols = !study:time,
                  .fn = ~paste(str_to_lower(fli$file_source),
                               .x,
                               sep = "_")) %>% 
      as.data.table()
    
    fnm_write <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    fpt_write <- 
      fs::path(
        fdr_write,
        str_subset(vct_fsb_write,
                   pattern = regex(fli$file_source,
                                   ignore_case = TRUE)),
        fnm_write
      )
    data.table::fwrite(
      df_shp,
      file = fpt_write,
      sep = ","
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpt_write,
                              ext = "feather")
    )
    
    
    
  }
  
  message(
    "------------------------------------COMPLETE------------------------------------\n",
    appendLF = TRUE
  )
  
}
shape_img_irr_v1 <- function(fls_irr_schema) {
  
  # # Make annotations 
  # tib_img_raw$annotation <- 
  #   tib_img_raw$annotation %>% 
  #   str_extract(pattern = "(?<=;).*?(?=,)") # after first semicolon but before first comma.
  # 
  # # str_extract(pattern = "([^;]*)$") %>% # Extract everything after last semicolon.
  # # str_extract(pattern = "(?:(?!,).)*") %>%  # Extract everything before first space.
  # # str_extract(pattern = "(?:(?!\\().)*") %>%  # Extract everything before first (.
  # # str_extract(pattern = "([^\\d]*)$") %>% # Extract everything after last digit.
  # # as.numeric()
  
  # Merge Activity files first.
  fls_irr_schema <- 
    fls_irr_act
  
  # Now merge Posture files.
  fls_irr_schema <- 
    fls_irr_pos
  for (i in seq_along(fls_irr_schema)) {
    
    fnm_irr <- fls_irr_schema[i]
    
    coder_name <- 
      fnm_irr %>% 
      str_extract(pattern = "(?:(?!_SPLIT).)*") %>% # Captures everything before "_SPLIT".
      str_extract(pattern = "([^_]*)$") # Captures everything after last "_".
    schema <- 
      fnm_irr %>% 
      str_extract(pattern = "ACTIVITY|POSTURE")
    # str_extract(pattern = "(?:(?!_SPLIT).)*") %>% # Captures everything before "_SPLIT".
    # str_extract(pattern = "([^_]*)$")
    
    message("Reading ", coder_name, "...",
            appendLF = FALSE)
    
    tib_img_raw <- suppressMessages(
      paste(fdr_irr,
            fld_irr,
            fnm_irr,
            sep = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    )
    
    # Consistency: make all lowercase and _ seperator.
    colnames(tib_img_raw) <-
      colnames(tib_img_raw) %>%
      str_to_lower() %>%
      str_replace(pattern = "time",
                  replacement = "_time")
    
    # Make start time into Oxford time for merge files later.
    tib_img_raw$start_time <- 
      tib_img_raw$start_time %>% 
      strptime(format = "%Y-%m-%d %H:%M:%S") %>% 
      as.POSIXct(tz = "UTC") %>% 
      with_tz(tzone = "America/Chicago") %>% 
      format("%H:%M:%S")
    
    if (schema == "POSTURE") {
      
      tib_img_raw$category <- 
        as.numeric(NA)
      
      # Make annotations numeric by extracting Taylor Code.
      tib_img_raw$taylor_code <-
        tib_img_raw$annotation %>%
        str_extract(pattern = "([^;]*)$") %>% # Extract everything after last semicolon.
        str_extract(pattern = "(?:(?! ).)*") %>%  # Extract everything before first space.
        as.numeric()
      
      # POSTURE SPECIFIC: Extract posture.
      tib_img_raw$annotation <- 
        tib_img_raw$annotation %>% 
        str_remove(pattern = "(?:(?! ).)*") %>%  # Remove everything before first space.
        str_trim()
      
    } else if (ncol(tib_img_raw) == 4) {
      
      # For UNALTERED ACTIVITY annotation files.
      tib_img_raw <- suppressWarnings(
        tib_img_raw %>%
          tidyr::separate(col = annotation,
                          into = c("annotation",
                                   "category"),
                          sep = ",")
      )
      tib_img_raw$category <- 
        tib_img_raw$category %>% 
        as.numeric()
      
      # Make annotations numeric by extracting Taylor Code.
      tib_img_raw$taylor_code <-
        tib_img_raw$annotation %>%
        str_extract(pattern = "\\d{4,5}") %>%  # Extract first occurrence of 4-5 digits.
        as.numeric()
      
      # ACTIVITY SPECIFIC: sub-domain + behavior
      tib_img_raw$annotation <- 
        tib_img_raw$annotation %>% 
        str_remove(pattern = "^([^;]*)(.;*)") %>%  # Remove everything before first semicolon INCLUDING first semicolon
        str_remove(pattern = "\\d{4,5}") %>%  # Remove first occurrence of 4-5 digits.
        str_trim()
      
    } else {
      
      # For ALTERED ACTIVITY annotation files.
      colnames(tib_img_raw)[colnames(tib_img_raw) == "...5"] <- 
        "category"
      tib_img_raw$category <- 
        tib_img_raw$category %>% 
        as.numeric()
      
      # Make annotations numeric by extracting Taylor Code.
      tib_img_raw$taylor_code <-
        tib_img_raw$annotation %>%
        str_extract(pattern = "\\d{4,5}") %>%  # Extract first occurrence of 4-5 digits.
        as.numeric()
      
      # ACTIVITY SPECIFIC: sub-domain + behavior
      tib_img_raw$annotation <- 
        tib_img_raw$annotation %>% 
        str_remove(pattern = "^([^;]*)(.;*)") %>%  # Remove everything before first semicolon INCLUDING first semicolon
        str_remove(pattern = "\\d{4,5}") %>%  # Remove first occurrence of 4-5 digits.
        str_trim()
      
      # str_extract(pattern = "([^;]*)$") # Extract everything after last semicolon.
      # str_extract(pattern = "(?:(?! ).)*") # Extract everything before first space.
      # str_extract(pattern = "^([^;]*)") # extract everything before first semicolon
      # str_extract(pattern = "(?<=;).*?(?=,)") # Extract after first semicolon but before first comma.
      
    }
    
    # # Make annotations numeric by extracting Taylor Code.
    # tib_img_raw$taylor_code <-
    #   tib_img_raw$annotation %>%
    #   str_extract(pattern = "([^;]*)$") %>% # Extract everything after last semicolon.
    #   str_extract(pattern = "(?:(?! ).)*") %>%  # Extract everything before first space.
    #   as.numeric()
    # 
    # # ACTIVITY SPECIFIC: sub-domain + behavior
    # tib_img_raw$annotation <- 
    #   tib_img_raw$annotation %>% 
    #   str_extract(pattern = "(?<=;).*?(?=,)") # Extract after first semicolon but before first comma.
    
    # Rename annotation column with coder's name.
    colnames(tib_img_raw)[colnames(tib_img_raw) == "annotation"] <- 
      coder_name
    colnames(tib_img_raw)[colnames(tib_img_raw) == "taylor_code"] <- 
      paste(coder_name,
            "TC",
            sep = "_")
    
    # Remove uneeded columns.
    tib_img_raw <- 
      tib_img_raw[, !(colnames(tib_img_raw) %in% c("end_time", "duration", "category"))]
    
    # put anno files in one list
    if (i == 1) {
      
      list_merge <- list(tib_img_raw)
      # list_merge <- list(tib_img_raw[, c(ncol(tib_img_raw) - 1, ncol(tib_img_raw))])
      
    } else if (i > 1) {
      
      list_merge[[i]] <- tib_img_raw
      # list_merge[[i]] <- tib_img_raw[, c(ncol(tib_img_raw) - 1, ncol(tib_img_raw))]
      
    }
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  tib_irr_pos <- 
    bind_cols(list_merge)
  colnames(tib_irr_pos)[colnames(tib_irr_pos) == "start_time...1"] <- 
    "start_time"
  tib_irr_pos <- 
    tib_irr_pos %>% 
    dplyr::select(-contains("..."))
  
  # Reorder so Martinez is first.
  tib_irr_act <- 
    tib_irr_act %>% 
    dplyr::relocate(start_time, MARTINEZ, MARTINEZ_TC)
  tib_irr_pos <- 
    tib_irr_pos %>% 
    dplyr::relocate(start_time, MARTINEZ, MARTINEZ_TC)
  
  # Export merged files.
  tib_mer_act <- 
    tib_irr_act %>% 
    dplyr::select(-vroom::ends_with("_TC"))
  tib_mer_pos <- 
    tib_irr_pos %>% 
    dplyr::select(-vroom::ends_with("_TC"))
  vroom_write(
    tib_mer_act,
    path = paste0(fdr_merged,
                  "/",
                  dte_quality_check,
                  "_",
                  "ACTIVITY.csv"),
    delim = ",",
    progress = FALSE
  )
  vroom_write(
    tib_mer_pos,
    path = paste0(fdr_merged,
                  "/",
                  dte_quality_check,
                  "_",
                  "POSTURE.csv"),
    delim = ",",
    progress = FALSE
  )
  
}
shape_img_irr_v2 <- function(fls_irr_schema) {
  
  # # CHANGES:
  
  # -A LOT.
  # -Dependent on file lists that were okay-ed from read_img_irr_v1 function.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: [NONE]
  # ARG: fls_irr_schema
  #      
  
  # # TESTING
  # fls_irr_schema <-
  #   fls_irr_act
  # fls_irr_schema <-
  #   fls_irr_pos
  
  tib_irr_schema <- 
    tibble(start_time = character())
  
  for (i in seq_along(fls_irr_schema)) {
    
    fnm_irr <- 
      fls_irr_schema[i]
    
    coder_name <- 
      fnm_irr %>% 
      str_extract(pattern = "(?:(?!_SPLIT).)*") %>% # Captures everything before "_SPLIT".
      str_extract(pattern = "([^_]*)$") # Captures everything after last "_".
    schema <- 
      fnm_irr %>% 
      str_extract(pattern = "ACTIVITY|POSTURE")
    
    message("Reading ", coder_name, "...",
            appendLF = FALSE)
    
    tib_img_raw <- suppressMessages(
      paste(fdr_irr,
            fld_irr,
            fnm_irr,
            sep = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    )
    
    # Check the file has splits between all images.
    nrw_img_raw <- 
      nrow(tib_img_raw)
    
    if (nrw_img_raw != 250L) {
      
      message("",
              appendLF = TRUE)
      
      warning(
        coder_name, " does not have all IMGs split.",
        call. = FALSE
      )
      
    }
    
    # Consistency: make all lowercase and _ seperator.
    colnames(tib_img_raw) <-
      colnames(tib_img_raw) %>%
      str_to_lower() %>%
      str_replace(pattern = "time",
                  replacement = "_time")
    
    tib_img_raw <- 
      tib_img_raw %>% 
      mutate(
        # Make start time into Oxford time for merge files later.
        start_time = 
          start_time %>% 
          strptime(format = "%Y-%m-%d %H:%M:%S") %>% 
          as.POSIXct(tz = "UTC") %>% 
          with_tz(tzone = "America/Chicago") %>% 
          format("%H:%M:%S"),
        .before = 1
      )
    
    if (schema == "POSTURE") {
      
      tib_img_raw <- 
        tib_img_raw %>% 
        mutate(
          category = 
            NA_integer_,
          # Have undefined portions have the Taylor Code of 0000.
          annotation = 
            annotation %>% 
            if_else(condition = annotation == "undefined",
                    true = "uncodeable;0000 undefined",
                    false = annotation),
          # Extract Taylor Codes to have numeric comparisons for later.
          taylor_code = 
            annotation %>% 
            str_extract(pattern = "\\d{4,5}") %>% 
            as.integer(),
          # POSTURE SPECIFIC: Extract posture.
          annotation = 
            annotation %>% 
            str_remove(pattern = "(?:(?! ).)*") %>%  # Remove everything before first space.
            str_trim()
        )
      
    } else if (ncol(tib_img_raw) == 4) {
      
      # For UNALTERED ACTIVITY annotation files.
      tib_img_raw <- 
        tib_img_raw %>% 
        separate(col = annotation,
                 into = c("annotation",
                          "category"),
                 sep = ",",
                 fill = "right") %>% 
        mutate(
          # Have undefined portions have the Taylor Code of 0000.
          annotation = 
            annotation %>% 
            if_else(condition = annotation == "undefined",
                    true = "uncodeable;0000 undefined",
                    false = annotation),
          # Undefined categories = 0.
          category = 
            category %>% 
            as.integer() %>% 
            if_else(condition = annotation == "uncodeable;0000 undefined",
                    true = 0L,
                    false = as.integer(category)),
          # Extract Taylor Codes to have numeric comparisons for later.
          taylor_code = 
            annotation %>% 
            str_extract(pattern = "\\d{4,5}") %>% 
            as.integer(),
          # ACTIVITY SPECIFIC: sub-domain + behavior.
          annotation = 
            annotation %>% 
            str_remove(pattern = "^([^;]*)(.;*)") %>%  # Remove everything before first semicolon INCLUDING first semicolon
            str_remove(pattern = "\\d{4,5}") %>% # Remove first occurrence of 4-5 digits.
            str_trim()
        )
      
    } else {
      
      # For ALTERED ACTIVITY annotation files.
      tib_img_raw <- 
        tib_img_raw %>% 
        rename(category = ...5) %>% 
        mutate(
          # Have undefined portions have the Taylor Code of 0000.
          annotation = 
            annotation %>% 
            if_else(condition = annotation == "undefined",
                    true = "uncodeable;0000 undefined",
                    false = annotation),
          # Undefined categories = 0.
          category = 
            category %>% 
            as.integer() %>% 
            if_else(condition = annotation == "uncodeable;0000 undefined",
                    true = 0L,
                    false = as.integer(category)),
          # Extract Taylor Codes to have numeric comparisons for later.
          taylor_code = 
            annotation %>% 
            str_extract(pattern = "\\d{4,5}") %>% 
            as.integer(),
          # ACTIVITY SPECIFIC: sub-domain + behavior.
          annotation = 
            annotation %>% 
            str_remove(pattern = "^([^;]*)(.;*)") %>% 
            str_remove(pattern = "\\d{4,5}") %>%
            str_trim()
        )
      
    }
    
    tib_img_raw <- 
      tib_img_raw %>% 
      select(
        start_time,
        "{coder_name}" := annotation,
        "{coder_name}_TC" := taylor_code
      )
    
    tib_irr_schema <- 
      tib_img_raw %>% 
      left_join(tib_irr_schema,
                by = "start_time")
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  tib_irr_schema <- 
    tib_irr_schema %>% 
    relocate(MARTINEZ, MARTINEZ_TC,
             .after = start_time) %>% 
    mutate(schema = schema,
           .before = 1)
  tib_mer_schema <- 
    tib_irr_schema %>% 
    select(-c(ends_with("_TC")))
  fnm_mer <- 
    paste0(dte_quality_check, "_", schema, ".csv")
  vroom_write(
    tib_mer_schema,
    path = paste(fdr_merged,
                 fnm_mer,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
  return(tib_irr_schema)
  
}
shape_img_irr_v3 <- function(fdr_irr,
                             fdr_mer,
                             fld_irr,
                             fls_irr_schema) {
  
  # # CHANGES:
  
  # -Updated naming of objects.
  # -Dependent on file lists that were okay-ed from read_img_irr_v1 function.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: [NONE]
  # ARG: fls_irr_schema
  #      
  
  # # TESTING
  # fdr_irr <- 
  #   file_directory_irr
  # fdr_mer <- 
  #   file_directory_merged
  # fld_irr <- 
  #   folder_irr
  # fls_irr_schema <- 
  #   file_list_act
  file_directory_irr <- 
    "./OxfordImageBrowser-win32-x64/6_Training/Quality Check/annotation files"
  file_directory_merged <- 
    "./OxfordImageBrowser-win32-x64/6_Training/Quality Check/merged files"
  file_directoy_results <- 
    "./OxfordImageBrowser-win32-x64/6_Training/Quality Check"
  folder_irr <- 
    "FLAC_QC_2021-07-22"
  
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
  
  tib_irr_schema <- 
    tibble(start_time = character())
  
  for (i in seq_along(fls_irr_schema)) {
    
    fnm_irr <- 
      fls_irr_schema[i]
    
    coder_name <- 
      fnm_irr %>% 
      str_extract(pattern = "(?:(?!_SPLIT).)*") %>% # Captures everything before "_SPLIT".
      str_extract(pattern = "([^_]*)$") # Captures everything after last "_".
    schema <- 
      fnm_irr %>% 
      str_extract(pattern = "ACTIVITY|POSTURE")
    
    message("Reading ", coder_name, "...",
            appendLF = FALSE)
    
    tib_img_raw <- suppressMessages(
      paste(fdr_irr,
            fld_irr,
            fnm_irr,
            sep = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    )
    
    # Check the file has splits between all images.
    nrw_img_raw <- 
      nrow(tib_img_raw)
    
    if (nrw_img_raw != 250L) {
      
      message("",
              appendLF = TRUE)
      
      warning(
        coder_name, " does not have all IMGs split.",
        call. = FALSE
      )
      
    }
    
    # Consistency: make all lowercase and _ seperator.
    colnames(tib_img_raw) <-
      colnames(tib_img_raw) %>%
      str_to_lower() %>%
      str_replace(pattern = "time",
                  replacement = "_time")
    
    tib_img_raw <- 
      tib_img_raw %>% 
      mutate(
        # Make start time into Oxford time for merge files later.
        start_time = 
          start_time %>% 
          strptime(format = "%Y-%m-%d %H:%M:%S") %>% 
          as.POSIXct(tz = "UTC") %>% 
          with_tz(tzone = "America/Chicago") %>% 
          format("%H:%M:%S"),
        .before = 1
      )
    
    if (schema == "POSTURE") {
      
      tib_img_raw <- 
        tib_img_raw %>% 
        mutate(
          category = 
            NA_integer_,
          # Have undefined portions have the Taylor Code of 0000.
          annotation = 
            annotation %>% 
            if_else(condition = annotation == "undefined",
                    true = "uncodeable;0000 undefined",
                    false = annotation),
          # Extract Taylor Codes to have numeric comparisons for later.
          taylor_code = 
            annotation %>% 
            str_extract(pattern = "\\d{4,5}") %>% 
            as.integer(),
          # POSTURE SPECIFIC: Extract posture.
          annotation = 
            annotation %>% 
            str_remove(pattern = "(?:(?! ).)*") %>%  # Remove everything before first space.
            str_trim()
        )
      
    } else if (ncol(tib_img_raw) == 4) {
      
      # For UNALTERED ACTIVITY annotation files.
      tib_img_raw <- 
        tib_img_raw %>% 
        separate(col = annotation,
                 into = c("annotation",
                          "category"),
                 sep = ",",
                 fill = "right") %>% 
        mutate(
          # Have undefined portions have the Taylor Code of 0000.
          annotation = 
            annotation %>% 
            if_else(condition = annotation == "undefined",
                    true = "uncodeable;0000 undefined",
                    false = annotation),
          # Undefined categories = 0.
          category = 
            category %>% 
            as.integer() %>% 
            if_else(condition = annotation == "uncodeable;0000 undefined",
                    true = 0L,
                    false = as.integer(category)),
          # Extract Taylor Codes to have numeric comparisons for later.
          taylor_code = 
            annotation %>% 
            str_extract(pattern = "\\d{4,5}") %>% 
            as.integer(),
          # ACTIVITY SPECIFIC: sub-domain + behavior.
          annotation = 
            annotation %>% 
            str_remove(pattern = "^([^;]*)(.;*)") %>%  # Remove everything before first semicolon INCLUDING first semicolon
            str_remove(pattern = "\\d{4,5}") %>% # Remove first occurrence of 4-5 digits.
            str_trim()
        )
      
    } else {
      
      # For ALTERED ACTIVITY annotation files.
      tib_img_raw <- 
        tib_img_raw %>% 
        rename(category = ...5) %>% 
        mutate(
          # Have undefined portions have the Taylor Code of 0000.
          annotation = 
            annotation %>% 
            if_else(condition = annotation == "undefined",
                    true = "uncodeable;0000 undefined",
                    false = annotation),
          # Undefined categories = 0.
          category = 
            category %>% 
            as.integer() %>% 
            if_else(condition = annotation == "uncodeable;0000 undefined",
                    true = 0L,
                    false = as.integer(category)),
          # Extract Taylor Codes to have numeric comparisons for later.
          taylor_code = 
            annotation %>% 
            str_extract(pattern = "\\d{4,5}") %>% 
            as.integer(),
          # ACTIVITY SPECIFIC: sub-domain + behavior.
          annotation = 
            annotation %>% 
            str_remove(pattern = "^([^;]*)(.;*)") %>% 
            str_remove(pattern = "\\d{4,5}") %>%
            str_trim()
        )
      
    }
    
    tib_img_raw <- 
      tib_img_raw %>% 
      select(
        start_time,
        "{coder_name}" := annotation,
        "{coder_name}_TC" := taylor_code
      )
    
    tib_irr_schema <- 
      tib_img_raw %>% 
      left_join(tib_irr_schema,
                by = "start_time")
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  tib_irr_schema <- 
    tib_irr_schema %>% 
    relocate(MARTINEZ, MARTINEZ_TC,
             .after = start_time) %>% 
    mutate(schema = schema,
           .before = 1)
  tib_mer_schema <- 
    tib_irr_schema %>% 
    select(-c(ends_with("_TC")))
  fnm_mer <- 
    paste0(fld_irr, "_", schema, ".csv")
  vroom_write(
    tib_mer_schema,
    path = paste(fdr_mer,
                 fnm_mer,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
  return(tib_irr_schema)
  
}
shape_noldus_v2 <- function(fdr_vid_raw,
                            fdr_vid_clean,
                            project) {
  
  fdr_vid_raw <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/0_raw"
  #   "./3_data/0_raw/event_logs"
  fdr_vid_clean <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  #   "./3_data/1_cleaned/sbs"
  project <-
    "FLAC = Aim 1"  
  #   "DOCOMP"
  
  fls_vid_raw <-
    list.files(path = fdr_vid_raw,
               pattern = ".xlsx")
  
  for (i in seq_along(fls_vid_raw)) {
    
    fnm_vid_raw <-
      fls_vid_raw[i]
    
    # str_extract(pattern = "(?:(?!_SPLIT).)*")# Captures everything before first "_SPLIT".
    # str_extract(pattern = "([^_]*)$") # Captures everything after last "_".
    
    fnm_obs_len <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% 
      str_length()
    fnm_obs <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$")
    fnm_obs_split <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "Activity|Posture")
    
    message("Cleaning file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = FALSE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2]
    coder <- 
      fnm_obs_split[3]
    
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v).)*")
    
    if (study == "CO") {
      
      sub_vis <- 
        paste(sub_vis,
              "1",
              sep = "v")
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        sub_vis %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    tib_vid_raw <- 
      read_xlsx(path = paste(fdr_vid_raw,
                             fnm_vid_raw,
                             sep = "/"),
                progress = FALSE)
    
    # Make Posture Uncoded codes the same as Activity.
    if (schema == "Posture") {
      
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded - Dark/Obscured/OoF"] <- 
        "[U] Dark/Obscured/OoF"
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded; Start/Stop"] <- 
        "[U] Start/Stop"
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Start Time"] <- 
        "[U] Start Time"
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Stop Time"] <- 
        "[U] Stop Time"
      
    }
    
    if (project == "DOCOMP") {
      
      dtm_vid_start <- 
        tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Start Time"]
      dtm_vid_stop <- 
        tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Stop Time"]
      
    } else {
      
      # Get datetime start, stop after removing any NA's but before removing any
      # other rows. This is because if a boolean has NA when subsetting it will
      # include the NA within the subset.
      dtm_comment <-
        read_xlsx(
          path = paste(fdr_vid_raw,
                       fnm_vid_raw,
                       sep = "/"),
          range = cell_cols(12:24),
          col_types = "date",
          progress = FALSE,
          .name_repair = "minimal"
        ) %>%
        dplyr::pull("Comment")
      dtm_comment <-
        dtm_comment[is.na(tib_vid_raw$Behavior) == FALSE]
      tib_vid_raw <-
        tib_vid_raw[is.na(tib_vid_raw$Behavior) == FALSE, ]
      
      dtm_vid_start <-
        dtm_comment[tib_vid_raw$Behavior == "[U] Start Time"] #still UTC
      dtm_vid_stop <-
        dtm_comment[tib_vid_raw$Behavior == "[U] Stop Time"] #still UTC
      
    }
    
    # Some light cleaning.
    tib_vid_raw <- 
      tib_vid_raw[tib_vid_raw$Event_Type != "State stop", ]
    tib_vid_raw <-
      tib_vid_raw[is.na(tib_vid_raw$Behavior) == FALSE, ]
    tib_vid_raw <- 
      tib_vid_raw[tib_vid_raw$Behavior != "*General Placeholder*", ]
    tib_vid_raw <- 
      tib_vid_raw[str_detect(tib_vid_raw$Behavior,
                             pattern = "Deviant",
                             negate = TRUE), ]
    tib_vid_raw <- 
      tib_vid_raw[str_detect(tib_vid_raw$Behavior,
                             pattern = "UEM",
                             negate = TRUE), ]
    
    # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$ DOCOMP SPECIFIC $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    
    if (colnames(tib_vid_raw) %>% 
        str_detect(pattern = "Comment",
                   negate = TRUE) %>% 
        all()) {
      
      tib_vid_raw$Comment <- 
        NA
      
    }
    # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    if ((tib_vid_raw$Behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    dtm_relative_hmsf_end <- 
      tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)]
    
    dtm_relative_hms_start <- 
      tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      tib_vid_raw$Time_Relative_hmsf[tib_vid_raw$Behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      tib_vid_raw$Time_Relative_hmsf[tib_vid_raw$Behavior == "[U] Stop Time"]
    
    # # %%%%%%%%%%%%%%%%%%%%%%%% DONT NEED FOR DOCOMP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    # # Double check a behavior code has same relative time as the start and
    # # stop times.
    # 
    # # The comments column is parsed as strings. Date-times in Excel that are in the
    # # mdy_hms format are contained as # of days since the date 1899-12-30. Therefore,
    # # need to change the date-time string from the start and stop placeholders to 
    # # double class then to date class and then finally into POSIXct class (numbers
    # # after the decimal point is the hms time). Don't change the tz to Chicago or 
    # # Denver until the end...
    # 
    # # I was using the below code which I adapted from process_ap function from 
    # # activpalprocessing package but for some reason the start time was always 1 sec
    # # behind the time I placed (using CO_1001). This happened with my adapted code
    # # AND the OG code.
    # # tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Start Time"] %>% 
    # #   as.double() %>% 
    # #   lubridate::as_date(origin = "1899-12-30") %>% 
    # #   lubridate::as_datetime(tz = "UTC")
    # # as.POSIXlt(
    # #   as.POSIXct(
    # #     as.Date(
    # #       as.numeric(
    # #         sub(
    # #           "#", "", tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Start Time"]
    # #         )
    # #       ),
    # #       origin = "1899-12-30"
    # #     )
    # #   ),
    # #   tz = "UTC"
    # # )
    # 
    # # find a way to make sure these are always within 5 seconds of each other.
    # difftime(
    #   time1 = dtm_relative_hms_stop,
    #   time2 = dtm_relative_hms_start,
    #   units = "secs"
    # )
    # difftime(
    #   time1 = dtm_vid_stop,
    #   time2 = dtm_vid_start,
    #   units = "secs"
    # )
    # 
    # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # Remove "[U] Start/Stop" codes and State Points.
    tib_vid_raw <- 
      tib_vid_raw[(tib_vid_raw$Behavior == "[U] Start/Stop" |
                     tib_vid_raw$Behavior == "[U] Start Time" |
                     tib_vid_raw$Behavior == "[U] Stop Time") == FALSE, ]
    
    # # Faster way to do this???
    # tib_vid_raw$Behavior %>% 
    # str_detect(pattern = "[U] Start/Stop | [U] Start Time | [U] Stop Time")
    # tib_vid_raw$Behavior == "[U] Start/Stop|[U] Start Time|[U] Stop Time"
    
    
    
    # CHECK: Start & Stop -----------------------------------------------------
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    
    lubridate::seconds(tib_vid_raw$Time_Relative_hmsf[1]) != lubridate::seconds(dtm_relative_hmsf_start)
    
    if (tib_vid_raw$Time_Relative_hmsf[1] != dtm_relative_hmsf_start) {
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    }
    
    # # testing
    # (tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] +
    #     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>%
    #   strptime(format = "%Y-%m-%d %H:%M:%S") %>%
    #   as.POSIXct(tz = "UTC") ==
    #   dtm_relative_hms_stop
    # 
    # (tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] +
    #     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>%
    #   strptime(format = "%Y-%m-%d %H:%M:%S") %>%
    #   as.POSIXct(tz = "UTC") %>%
    #   lubridate::seconds()
    # dtm_relative_hms_stop %>%
    #   lubridate::seconds()
    # dtm_relative_hmsf_stop %>%
    #   lubridate::seconds()
    # 
    # tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] %>%
    #   lubridate::seconds()
    # tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] %>%
    #   lubridate::seconds()
    # tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] %>%
    #   strptime(format = "%Y-%m-%d %H:%M:%S")
    # tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] %>%
    #   strptime(format = "%Y-%m-%d %H:%M:%S")
    # 
    # dtm_relative_hms_start %>%
    #   lubridate::seconds()
    # dtm_relative_hmsf_start %>%
    #   lubridate::seconds()
    # 
    # (tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] +
    #     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>%
    #   strptime(format = "%Y-%m-%d %H:%M:%S") %>%
    #   as.POSIXct(tz = "UTC") %>%
    #   lubridate::seconds()
    # (tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] +
    #     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>%
    #   strptime(format = "%Y-%m-%d %H:%M:%S") %>%
    #   as.POSIXct(tz = "UTC") %>%
    #   lubridate::seconds()
    # 
    # (tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] +
    #     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>%
    #   seconds() ==
    #   dtm_relative_hmsf_end %>%
    #   seconds()
    # dplyr::near((tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] +
    #                tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>% 
    #               seconds(),
    #             dtm_relative_hmsf_end %>% 
    #               seconds(),
    #             tol = .001)
    # 
    # difftime(
    #   dtm_relative_hms_stop,
    #   dtm_relative_hms_start,
    #   units = "secs"
    # )
    # difftime(
    #   dtm_relative_hmsf_stop,
    #   dtm_relative_hmsf_start,
    #   units = "secs"
    # )
    # difftime(
    #   strptime(dtm_relative_hmsf_stop,
    #            format = "%Y-%m-%d %H:%M:%S"),
    #   strptime(dtm_relative_hmsf_start,
    #            format = "%Y-%m-%d %H:%M:%S"),
    #   units = "secs"
    # )
    # dtm_relative_hmsf_end %>% 
    #   seconds()
    # (tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] +
    #     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>%
    #   seconds()
    
    if (dplyr::near((tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] +
                     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>% 
                    seconds(),
                    dtm_relative_hmsf_end %>% 
                    seconds(),
                    tol = .01) == FALSE) {
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    }
    
    # if ((tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] +
    #      tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>% 
    #     strptime(format = "%Y-%m-%d %H:%M:%S") %>% 
    #     as.POSIXct(tz = "UTC") != 
    #     dtm_relative_hms_stop) {
    #   
    #   # The timestamp of [U] Stop Time was NOT placed at the same time as the last
    #   # [U] Start/Stop.
    #   stop("Stop time does not match last code timestamp + its duration.")
    #   
    # }
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(tib_vid_raw)[colnames(tib_vid_raw) == "Modifier_1"] <- 
      "Mod_1"
    
    if (schema == "Activity") {
      
      if ("Modifier_2" %in% colnames(tib_vid_raw)) {
        
        tib_vid_raw <- 
          tib_vid_raw %>% 
          unite(col = "Mod_2",
                contains("Modifier",
                         ignore.case = FALSE,
                         vars = NULL),
                remove = TRUE,
                na.rm = TRUE)
        
      } else {
        
        tib_vid_raw$Mod_2 <- 
          NA
        
      }
    } else if (schema == "Posture") {
      
      tib_vid_raw$Mod_2 <- 
        NA
      
    }
    
    # # %%%%%%%%%%%%%%%%%%%%%%%%% COLORADO SPECIFIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    # tib_vid_raw$Domain <- "Non-Domestic"
    # 
    # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # SecBySec ----------------------------------------------------------------
    
    nrw_vid_raw <- 
      nrow(tib_vid_raw)
    # int_duration <-  
    #   as.vector(
    #     difftime(
    #       tib_vid_raw$Time_Relative_hms[seq_len(nrw_vid_raw - 1) + 1],
    #       tib_vid_raw$Time_Relative_hms[seq_len(nrw_vid_raw - 1)],
    #       units = "secs"
    #     )
    #   )
    int_duration <-  
      diff.POSIXt(tib_vid_raw$Time_Relative_hms,
                  units = "secs") %>% 
      as.vector()
    
    if (difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
                 units = "secs") !=
        round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
              digits = 0)) {
      
      diff_duration_sf <- 
        round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
                 units = "secs")
      
      warning("File #", i, ": ", schema, " - ", fnm_obs, "\n",
              "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
              "Duration_sf is ", diff_duration_sf, " off.\n",
              "Using difference between the stop time and last code time.\n",
              call. = FALSE)
      
      # stop("The Duration_sf of the last code does not equal the difference between the stop time and last code time.")
      
    }
    
    int_duration <- 
      c(round(int_duration,
              digits = 0),
        round(difftime(time1 = dtm_relative_hms_stop,
                       time2 = tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
                       units = "secs"),
              digits = 0))
    # int_duration <- 
    #   c(round(int_duration,
    #           digits = 0),
    #     round(dtm_relative_hms_stop - tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
    #           digits = 0))
    # int_duration <-
    #   c(round(int_duration,
    #           digits = 0),
    #     round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
    #           digits = 0))
    
    if (is.na(int_duration) %>% 
        any()) {
      
      stop("Duration of a code is less than a second??? IDK man")
      # int_duration[is.na(int_duration)] <-
      #   1
      
    }
    
    int_duration <- 
      as.integer(int_duration)
    
    sbs_int_events <- 
      int_duration %>% 
      seq_along() %>% 
      rep(times = int_duration)
    sbs_str_behavior <- 
      tib_vid_raw$Behavior %>% 
      rep(times = int_duration)
    sbs_str_mod_1 <- 
      tib_vid_raw$Mod_1 %>% 
      rep(times = int_duration)
    sbs_str_mod_2 <- 
      tib_vid_raw$Mod_2 %>% 
      rep(times = int_duration)
    sbs_str_comment <- 
      tib_vid_raw$Comment %>% 
      rep(times = int_duration)  
    
    # sbs_int_events <- 
    #  rep(1:length(int_duration),
    #      times = int_duration)
    # sbs_str_behavior <- 
    #  rep(tib_vid_raw$Behavior,
    #       times = int_duration)
    # sbs_str_mod_1 <- 
    #   rep(tib_vid_raw$Mod_1,
    #       times = int_duration)
    # sbs_str_mod_2 <- 
    #   rep(tib_vid_raw$Mod_2,
    #       times = int_duration)
    # sbs_str_comment <- 
    #   rep(tib_vid_raw$Comment,
    #       times = int_duration)
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    
    # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$ DOCOMP SPECIFIC $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    if (project == "DOCOMP") {
      
      sbs_dtm_times <- 
        dtm_relative_hms_start + 
        (0:(nrw_vid_sbs - 1))
      sample_length <- 
        fnm_obs_split[4]
      
      if (schema == "Activity") {
        
        tib_vid_sbs <- tibble(
          # study          = study,
          subject        = subject,
          # visit          = visit,
          time           = sbs_dtm_times,
          # date           = sbs_dte_dates,
          behavior       = sbs_str_behavior,
          sub_activity   = sbs_str_mod_2,
          environment    = sbs_str_mod_1,
          # comment_act    = sbs_str_comment,
          num_events_act = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
        colnames(tib_vid_sbs)[colnames(tib_vid_sbs) %in% c("behavior",
                                                           "sub_activity",
                                                           "environment",
                                                           "num_events_act")] <- 
          paste(c("behavior",
                  "sub_activity",
                  "environment",
                  "num_events_act"),
                sample_length,
                sep = "_")
        # paste(sample_length,
        #         c("behavior",
        #           "sub_activity",
        #           "environment",
        #           "num_events_act"),
        #         sep = "_")
        
      } else if (schema == "Posture") {
        
        tib_vid_sbs <- tibble(
          # study          = study,
          subject        = subject,
          # visit          = visit,
          time           = sbs_dtm_times,
          # date          = sbs_dte_dates,
          posture        = sbs_str_behavior,
          intensity      = sbs_str_mod_1,
          # comment_pos    = sbs_str_comment,
          num_events_pos = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
        colnames(tib_vid_sbs)[colnames(tib_vid_sbs) %in% c("posture",
                                                           "intensity",
                                                           "num_events_pos")] <- 
          paste(c("posture",
                  "intensity",
                  "num_events_pos"),
                sample_length,
                sep = "_")
        # paste(sample_length,
        #         c("posture",
        #           "intensity",
        #           "num_events_pos"),
        #         sep = "_")
        
      }
      
      fnm_vid_clean <- 
        paste0(study, 
               "_", 
               subject, 
               "v",
               visit,
               "_",
               schema,
               "_",
               sample_length,
               ".csv")
      
    } else {
      
      # dttm_start <- strptime(data_ap_raw$time[1],
      #                        format = "%Y-%m-%d %H:%M:%S") %>% 
      #   as.actIXct(tz = "America/Chicago")
      if (schema == "Activity") {
        
        tib_vid_sbs <- tibble(
          study          = study,
          subject        = subject,
          visit          = visit,
          time           = sbs_dtm_times,
          date           = sbs_dte_dates,
          behavior       = sbs_str_behavior,
          sub_activity   = sbs_str_mod_2,
          environment    = sbs_str_mod_1,
          comment_act    = sbs_str_comment,
          num_events_act = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
      } else if (schema == "Posture") {
        
        tib_vid_sbs <- tibble(
          study          = study,
          subject        = subject,
          visit          = visit,
          time           = sbs_dtm_times,
          date          = sbs_dte_dates,
          posture        = sbs_str_behavior,
          intensity      = sbs_str_mod_1,
          comment_pos    = sbs_str_comment,
          num_events_pos = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
      }
    }
    
    if (project == "FLAC Aim 2") {
      
      fnm_vid_clean <- 
        paste0(fnm_obs,
               "_",
               schema,
               ".csv")
      
    }
    
    vroom_write(
      tib_vid_sbs,
      path = paste(fdr_vid_clean,
                   fnm_vid_clean,
                   sep = "/"),
      delim = ","
    )
    
    # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    message("DONE",
            appendLF = TRUE)
    
  }
}
shape_noldus_v3 <- function(project,
                            fdr_vid_raw,
                            fdr_vid_clean) {
  
  project <-
    "FLAC - Aim 1"
  fdr_vid_raw <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/0_raw"
  fdr_vid_clean <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  
  # project <-
  #   "DOCOMP"
  # fdr_vid_raw <-
  #   "./3_data/0_raw/event_logs"
  # fdr_vid_clean <-
  #   "./3_data/1_cleaned/sbs"
  
  fls_vid_raw <-
    list.files(path = fdr_vid_raw,
               pattern = ".xlsx")
  
  for (i in seq_along(fls_vid_raw)) {
    
    fnm_vid_raw <-
      fls_vid_raw[i]
    
    # str_extract(pattern = "(?:(?!_SPLIT).)*")# Captures everything before first "_SPLIT".
    # str_extract(pattern = "([^_]*)$") # Captures everything after last "_".
    
    fnm_obs_len <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% 
      str_length()
    fnm_obs <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$")
    fnm_obs_split <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "Activity|Posture")
    
    message("Cleaning file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = FALSE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2]
    coder <- 
      fnm_obs_split[3]
    
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v).)*")
    
    if (study == "CO") {
      
      sub_vis <- 
        paste(sub_vis,
              "1",
              sep = "v")
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        sub_vis %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    tib_vid_raw <- 
      read_xlsx(path = paste(fdr_vid_raw,
                             fnm_vid_raw,
                             sep = "/"),
                progress = FALSE)
    
    # Make Posture Uncoded codes the same as Activity.
    if (schema == "Posture") {
      
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded - Dark/Obscured/OoF"] <- 
        "[U] Dark/Obscured/OoF"
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded; Start/Stop"] <- 
        "[U] Start/Stop"
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Start Time"] <- 
        "[U] Start Time"
      tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Stop Time"] <- 
        "[U] Stop Time"
      
    }
    
    # Some light cleaning. Remove NA for Behavior column first as it will affect
    # Boolean subsetting.
    tib_vid_raw <-
      tib_vid_raw[is.na(tib_vid_raw$Behavior) == FALSE, ]
    
    if (project == "DOCOMP") {
      
      dtm_vid_start <- 
        tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Start Time"]
      dtm_vid_stop <- 
        tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Stop Time"]
      
    } else {
      
      # Change start/stop to corresponding time zone at the end.
      dtm_vid_start <- suppressWarnings(
        tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Start Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      dtm_vid_stop <- suppressWarnings(
        tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Stop Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      
      if (is.na(dtm_vid_start)) {
        
        warning(
          schema, " - ", fnm_obs,
          "Start Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
      
      if (is.na(dtm_vid_stop)) {
        
        warning(
          schema, " - ", fnm_obs,
          "Stop Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
      
      # dtm_comment <-
      #   read_xlsx(path = paste(fdr_vid_raw,
      #                          fnm_vid_raw,
      #                          sep = "/"),
      #             range = cell_cols(12:24),
      #             col_types = "date",
      #             progress = FALSE,
      #             .name_repair = "minimal") %>%
      #   dplyr::pull("Comment")
      # dtm_comment <-
      #   dtm_comment[is.na(tib_vid_raw$Behavior) == FALSE]
      # tib_vid_raw <-
      #   tib_vid_raw[is.na(tib_vid_raw$Behavior) == FALSE, ]
      # 
      # dtm_vid_start <-
      #   dtm_comment[tib_vid_raw$Behavior == "[U] Start Time"] #still UTC
      # dtm_vid_stop <-
      #   dtm_comment[tib_vid_raw$Behavior == "[U] Stop Time"] #still UTC
      
    }
    
    # Some light cleaning.
    # tib_vid_raw <-
    # tib_vid_raw[is.na(tib_vid_raw$Behavior) == FALSE, ]
    tib_vid_raw <- 
      tib_vid_raw[tib_vid_raw$Event_Type != "State stop", ]
    tib_vid_raw <- 
      tib_vid_raw[tib_vid_raw$Behavior != "*General Placeholder*", ]
    tib_vid_raw <- 
      tib_vid_raw[str_detect(tib_vid_raw$Behavior,
                             pattern = "Deviant",
                             negate = TRUE), ]
    tib_vid_raw <- 
      tib_vid_raw[str_detect(tib_vid_raw$Behavior,
                             pattern = "UEM",
                             negate = TRUE), ]
    
    # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$ DOCOMP SPECIFIC $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    
    if (colnames(tib_vid_raw) %>% 
        str_detect(pattern = "Comment",
                   negate = TRUE) %>% 
        all()) {
      
      tib_vid_raw$Comment <- 
        NA
      
    }
    # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    if ((tib_vid_raw$Behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    dtm_relative_hmsf_end <- 
      tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)]
    
    dtm_relative_hms_start <- 
      tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      tib_vid_raw$Time_Relative_hmsf[tib_vid_raw$Behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      tib_vid_raw$Time_Relative_hmsf[tib_vid_raw$Behavior == "[U] Stop Time"]
    
    # # %%%%%%%%%%%%%%%%%%%%%%%% DONT NEED FOR DOCOMP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # Double check a behavior code has same relative time as the start and
    # stop times.
    
    # # The below comment doesnt happen anymore so what the beep.
    
    # The comments column is parsed as strings. Date-times in Excel that are in the
    # mdy_hms format are contained as # of days since the date 1899-12-30. Therefore,
    # need to change the date-time string from the start and stop placeholders to
    # double class then to date class and then finally into POSIXct class (numbers
    # after the decimal point is the hms time). Don't change the tz to Chicago or
    # Denver until the end...
    
    # I was using the below code which I adapted from process_ap function from
    # activpalprocessing package but for some reason the start time was always 1 sec
    # behind the time I placed (using CO_1001). This happened with my adapted code
    # AND the OG code.
    # tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Start Time"] %>%
    #   as.double() %>%
    #   lubridate::as_date(origin = "1899-12-30") %>%
    #   lubridate::as_datetime(tz = "UTC")
    # as.POSIXlt(
    #   as.POSIXct(
    #     as.Date(
    #       as.numeric(
    #         sub(
    #           "#", "", tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Start Time"]
    #         )
    #       ),
    #       origin = "1899-12-30"
    #     )
    #   ),
    #   tz = "UTC"
    # )
    
    # find a way to make sure these are always within 5 seconds of each other.
    # difftime(
    #   time1 = dtm_relative_hms_stop,
    #   time2 = dtm_relative_hms_start,
    #   units = "secs"
    # )
    # difftime(
    #   time1 = dtm_relative_hmsf_stop,
    #   time2 = dtm_relative_hmsf_start,
    #   units = "secs"
    # )
    # difftime(
    #   time1 = dtm_vid_stop,
    #   time2 = dtm_vid_start,
    #   units = "secs"
    # )
    
    if (dplyr::near(difftime(time1 = dtm_vid_stop,
                             time2 = dtm_vid_start,
                             units = "secs"),
                    difftime(time1 = dtm_relative_hms_stop,
                             time2 = dtm_relative_hms_start,
                             units = "secs"),
                    tol = 5) == FALSE) {
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1 -
        t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    
    # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # Remove "[U] Start/Stop" codes and State Points.
    tib_vid_raw <- 
      tib_vid_raw[(tib_vid_raw$Behavior == "[U] Start/Stop" |
                     tib_vid_raw$Behavior == "[U] Start Time" |
                     tib_vid_raw$Behavior == "[U] Stop Time") == FALSE, ]
    
    # # Faster way to do this???
    # tib_vid_raw$Behavior %>% 
    # str_detect(pattern = "[U] Start/Stop | [U] Start Time | [U] Stop Time")
    # tib_vid_raw$Behavior == "[U] Start/Stop|[U] Start Time|[U] Stop Time"
    
    
    
    # CHECK: Start & Stop -----------------------------------------------------
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    
    lubridate::seconds(tib_vid_raw$Time_Relative_hmsf[1]) != lubridate::seconds(dtm_relative_hmsf_start)
    
    if (tib_vid_raw$Time_Relative_hmsf[1] != dtm_relative_hmsf_start) {
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    }
    
    
    if (dplyr::near((tib_vid_raw$Time_Relative_hmsf[nrow(tib_vid_raw)] +
                     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>% 
                    seconds(),
                    dtm_relative_hmsf_end %>% 
                    seconds(),
                    tol = .01) == FALSE) {
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    }
    
    # if ((tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] +
    #      tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>% 
    #     strptime(format = "%Y-%m-%d %H:%M:%S") %>% 
    #     as.POSIXct(tz = "UTC") != 
    #     dtm_relative_hms_stop) {
    #   
    #   # The timestamp of [U] Stop Time was NOT placed at the same time as the last
    #   # [U] Start/Stop.
    #   stop("Stop time does not match last code timestamp + its duration.")
    #   
    # }
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(tib_vid_raw)[colnames(tib_vid_raw) == "Modifier_1"] <- 
      "Mod_1"
    
    if (schema == "Activity") {
      
      if ("Modifier_2" %in% colnames(tib_vid_raw)) {
        
        tib_vid_raw <- 
          tib_vid_raw %>% 
          unite(col = "Mod_2",
                contains("Modifier",
                         ignore.case = FALSE,
                         vars = NULL),
                remove = TRUE,
                na.rm = TRUE)
        
      } else {
        
        tib_vid_raw$Mod_2 <- 
          NA
        
      }
    } else if (schema == "Posture") {
      
      tib_vid_raw$Mod_2 <- 
        NA
      
    }
    
    # # %%%%%%%%%%%%%%%%%%%%%%%%% COLORADO SPECIFIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 
    # tib_vid_raw$Domain <- "Non-Domestic"
    # 
    # # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # SecBySec ----------------------------------------------------------------
    
    nrw_vid_raw <- 
      nrow(tib_vid_raw)
    # int_duration <-  
    #   as.vector(
    #     difftime(
    #       tib_vid_raw$Time_Relative_hms[seq_len(nrw_vid_raw - 1) + 1],
    #       tib_vid_raw$Time_Relative_hms[seq_len(nrw_vid_raw - 1)],
    #       units = "secs"
    #     )
    #   )
    int_duration <-  
      diff.POSIXt(tib_vid_raw$Time_Relative_hms,
                  units = "secs") %>% 
      as.vector()
    
    if (difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
                 units = "secs") !=
        round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
              digits = 0)) {
      
      diff_duration_sf <- 
        round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
                 units = "secs")
      
      warning("File #", i, ": ", schema, " - ", fnm_obs, "\n",
              "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
              "Duration_sf is ", diff_duration_sf, " off.\n",
              "Using difference between the stop time and last code time.\n",
              call. = FALSE)
      
      # stop("The Duration_sf of the last code does not equal the difference between the stop time and last code time.")
      
    }
    
    int_duration <- 
      c(round(int_duration,
              digits = 0),
        round(difftime(time1 = dtm_relative_hms_stop,
                       time2 = tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
                       units = "secs"),
              digits = 0))
    # int_duration <- 
    #   c(round(int_duration,
    #           digits = 0),
    #     round(dtm_relative_hms_stop - tib_vid_raw$Time_Relative_hms[nrw_vid_raw],
    #           digits = 0))
    # int_duration <-
    #   c(round(int_duration,
    #           digits = 0),
    #     round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
    #           digits = 0))
    
    if (is.na(int_duration) %>% 
        any()) {
      
      stop("Duration of a code is less than a second??? IDK man")
      # int_duration[is.na(int_duration)] <-
      #   1
      
    }
    
    int_duration <- 
      as.integer(int_duration)
    
    # test <- 
    #   tib_vid_raw$Duration_sf %>% 
    #   round()
    # sbs_test <- 
    #   test %>% 
    #   seq_along() %>% 
    #   rep(times = test)
    
    sbs_int_events <- 
      int_duration %>% 
      seq_along() %>% 
      rep(times = int_duration)
    sbs_str_behavior <- 
      tib_vid_raw$Behavior %>% 
      rep(times = int_duration)
    sbs_str_mod_1 <- 
      tib_vid_raw$Mod_1 %>% 
      rep(times = int_duration)
    sbs_str_mod_2 <- 
      tib_vid_raw$Mod_2 %>% 
      rep(times = int_duration)
    sbs_str_comment <- 
      tib_vid_raw$Comment %>% 
      rep(times = int_duration)  
    
    # sbs_int_events <- 
    #  rep(1:length(int_duration),
    #      times = int_duration)
    # sbs_str_behavior <- 
    #  rep(tib_vid_raw$Behavior,
    #       times = int_duration)
    # sbs_str_mod_1 <- 
    #   rep(tib_vid_raw$Mod_1,
    #       times = int_duration)
    # sbs_str_mod_2 <- 
    #   rep(tib_vid_raw$Mod_2,
    #       times = int_duration)
    # sbs_str_comment <- 
    #   rep(tib_vid_raw$Comment,
    #       times = int_duration)
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    if (project == "DOCOMP") {
      
      sbs_dtm_times <- 
        dtm_relative_hms_start + 
        (0:(nrw_vid_sbs - 1))
      sample_length <- 
        fnm_obs_split[4]
      
      if (schema == "Activity") {
        
        tib_vid_sbs <- tibble(
          # study          = study,
          subject        = subject,
          # visit          = visit,
          time           = sbs_dtm_times,
          # date           = sbs_dte_dates,
          behavior       = sbs_str_behavior,
          activity       = sbs_str_mod_2,
          environment    = sbs_str_mod_1,
          # comment_act    = sbs_str_comment,
          num_events_act = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
        colnames(tib_vid_sbs)[colnames(tib_vid_sbs) %in% c("behavior",
                                                           "activity",
                                                           "environment",
                                                           "num_events_act")] <- 
          paste(c("behavior",
                  "activity",
                  "environment",
                  "num_events_act"),
                sample_length,
                sep = "_")
        # paste(sample_length,
        #         c("behavior",
        #           "sub_activity",
        #           "environment",
        #           "num_events_act"),
        #         sep = "_")
        
      } else if (schema == "Posture") {
        
        tib_vid_sbs <- tibble(
          # study          = study,
          subject        = subject,
          # visit          = visit,
          time           = sbs_dtm_times,
          # date          = sbs_dte_dates,
          posture        = sbs_str_behavior,
          intensity      = sbs_str_mod_1,
          # comment_pos    = sbs_str_comment,
          num_events_pos = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
        colnames(tib_vid_sbs)[colnames(tib_vid_sbs) %in% c("posture",
                                                           "intensity",
                                                           "num_events_pos")] <- 
          paste(c("posture",
                  "intensity",
                  "num_events_pos"),
                sample_length,
                sep = "_")
        # paste(sample_length,
        #         c("posture",
        #           "intensity",
        #           "num_events_pos"),
        #         sep = "_")
        
      }
      
      fnm_vid_clean <- 
        paste0(study, 
               "_", 
               subject, 
               "v",
               visit,
               "_",
               schema,
               "_",
               sample_length,
               ".csv")
      
    } else {
      
      # dttm_start <- 
      #   strptime(data_ap_raw$time[1],
      #            format = "%Y-%m-%d %H:%M:%S") %>%
      #   as.actIXct(tz = "America/Chicago")
      
      sbs_dtm_times <- 
        seq.POSIXt(from = dtm_vid_start,
                   by = 1,
                   length.out = nrw_vid_sbs)
      sbs_dte_dates <- 
        sbs_dtm_times %>% 
        lubridate::as_date()
      
      # sbs_dtm_times <- 
      #   dtm_vid_start +
      #   (0:(nrw_vid_sbs - 1))
      # all(
      #   seq.POSIXt(from = dtm_vid_start,
      #              by = 1,
      #              length.out = nrw_vid_sbs) == 
      #     (dtm_vid_start +
      #        (0:(nrw_vid_sbs - 1)))
      # )
      
      if (schema == "Activity") {
        
        tib_vid_sbs <- tibble(
          study          = study,
          subject        = subject,
          visit          = visit,
          time           = sbs_dtm_times,
          date           = sbs_dte_dates,
          behavior       = sbs_str_behavior,
          activity       = sbs_str_mod_2,
          environment    = sbs_str_mod_1,
          comment_act    = sbs_str_comment,
          num_events_act = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
      } else if (schema == "Posture") {
        
        tib_vid_sbs <- tibble(
          study          = study,
          subject        = subject,
          visit          = visit,
          time           = sbs_dtm_times,
          date           = sbs_dte_dates,
          posture        = sbs_str_behavior,
          intensity      = sbs_str_mod_1,
          comment_pos    = sbs_str_comment,
          num_events_pos = sbs_int_events,
          .rows = nrw_vid_sbs
        )
        
      }
    }
    
    # Project-specific --------------------------------------------------------
    if (project == "FLAC - Aim 1") {
      
      tib_vid_sbs$time <- 
        tib_vid_sbs$time %>% 
        lubridate::with_tz(tzone = "America/Denver")
      
      if (schema == "Activity") {
        
        tib_vid_sbs$environment <- 
          "Non-Domestic"
        
      }
    } else if (project == "FLAC - Aim 2") {
      
      tib_vid_sbs$time <- 
        tib_vid_sbs$time %>% 
        lubridate::with_tz(tzone = "America/Chicago")
      
    }
    
    fnm_vid_clean <- 
      paste0(study,
             "_",
             subject,
             "V",
             visit,
             "_",
             schema,
             "_",
             coder,
             ".csv")
    
    vroom_write(
      tib_vid_sbs,
      path = paste(fdr_vid_clean,
                   fnm_vid_clean,
                   sep = "/"),
      delim = ","
    )
    
    message("DONE",
            appendLF = TRUE)
    
  }
}
shape_noldus_v4 <- function(project,
                            fdr_vid_raw,
                            fdr_vid_clean,
                            key_behavior_bucket,
                            key_behavior_domain,
                            key_posture_bucket,
                            key_posture_domain) {
  # CHANGES:
  # -Include dplyr code to make subsetting readable
  # -Remove old code from v3
  # -change [P} Lying to [P] Lying
  # -If DOCOMP, make sure to change caring grooming - self TO caring/grooming - self
  # -Push more project specific stuff till the end.
  # -Include new event code and replace NA's to allow rle be more accurate.
  
  # project <-
  #   "FLAC - Aim 1"
  # fdr_vid_raw <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/0_raw"
  # fdr_vid_clean <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  
  # project <-
  #   "DOCOMP"
  # fdr_vid_raw <-
  #   "./3_data/0_raw/event_logs"
  # fdr_vid_clean <-
  #   "./3_data/1_cleaned/sbs"
  # key_behavior_bucket <-
  #   c(
  #     "Sports/Exercise"              = 1L,
  #     "Eating/Drinking"              = 2L,
  #     "Transportation"               = 3L,
  #     "Electronics"                  = 2L,
  #     "Other - Manipulating Objects" = 4L,
  #     "Other - Carrying Load w/ UE"  = 4L,
  #     "Other - Pushing Cart"         = 4L,
  #     "Talking - Person"             = 2L,
  #     "Talking - Phone"              = 2L,
  #     "Caring/Grooming - Adult"      = 5L,
  #     "Caring/Grooming - Animal/Pet" = 5L,
  #     "Caring/Grooming - Child"      = 5L,
  #     "Caring/Grooming - Self"       = 2L,
  #     "Cleaning"                     = 6L,
  #     "C/F/R/M"                      = 6L,
  #     "Cooking/Meal Preperation"     = 6L,
  #     "Laundry"                      = 6L,
  #     "Lawn&Garden"                  = 6L,
  #     "Leisure Based"                = 2L,
  #     "Only [P/M] Code"              = 7L,
  #     "Talking - Researchers"        = 7L,
  #     "Intermittent Activity"        = 7L,
  #     "Dark/Obscured/OoF"            = 8L
  #   )
  # key_behavior_domain <-
  #   c(
  #     "1" = "Sport&Exercise",
  #     "2" = "Leisure",
  #     "3" = "Transportation",
  #     "4" = "Other",
  #     "5" = "Caring&Grooming",
  #     "6" = "Household",
  #     "7" = "Transition",
  #     "8" = "Uncoded"
  #   )
  # key_posture_bucket <-
  #   c(
  #     "Lying"                            = 1L,
  #     "Sitting"                          = 1L,
  #     "Crouching / Kneeling / Squating"  = 2L,
  #     "Standing"                         = 3L,
  #     "Other - Posture"                  = 4L,
  #     "Intermittent Posture"             = 4L,
  #     "Walking"                          = 5L,
  #     "Stepping"                         = 5L,
  #     "Running"                          = 5L,
  #     "Ascending Stairs"                 = 5L,
  #     "Descending Stairs"                = 5L,
  #     "Crouching / Squating"             = 6L,
  #     "Cycling"                          = 6L,
  #     "Other - Movement"                 = 7L,
  #     "Intermittent Movement"            = 7L,
  #     "Intermittent P/M"                 = 8L,
  #     "Dark/Obscured/OoF"                = 8L
  #   )
  # key_posture_domain <-
  #   c(
  #     "1" = "Sit",
  #     "2" = "Crouching",
  #     "3" = "Stand",
  #     "4" = "Posture Other",
  #     "5" = "Movement",
  #     "6" = "Movement Stationary",
  #     "7" = "Movement Other",
  #     "8" = "Transition/Uncoded"
  #   )
  
  fls_vid_raw <-
    list.files(path = fdr_vid_raw,
               pattern = ".xlsx")
  
  for (i in seq_along(fls_vid_raw)) {
    
    fnm_vid_raw <-
      fls_vid_raw[i]
    
    # Save this code for another function that will check files are
    # named correctly.
    # fnm_obs_len <- 
    #   fnm_vid_raw %>% 
    #   str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
    #   str_extract(pattern = "([^ - ]*)$") %>% 
    #   str_length()
    fnm_obs <-
      fnm_vid_raw %>%
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$")
    fnm_obs_split <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "Activity|Posture")
    
    message("Cleaning file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = TRUE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2]
    coder <- 
      fnm_obs_split[3]
    
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v).)*")
    
    if (study == "CO") {
      
      sub_vis <- 
        paste(sub_vis,
              "1",
              sep = "v")
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        sub_vis %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    tib_vid_raw <- 
      read_xlsx(path = paste(fdr_vid_raw,
                             fnm_vid_raw,
                             sep = "/"),
                progress = FALSE)
    
    # Consistency: Make all column names lowercase
    colnames(tib_vid_raw) <- 
      colnames(tib_vid_raw) %>% 
      str_to_lower()
    
    # Clean #1 ----------------------------------------------------------------
    tib_vid_raw <- 
      tib_vid_raw %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior =="*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      )
    
    # tib_vid_raw <- 
    #   tib_vid_raw[tib_vid_raw$Event_Type != "State stop", ]
    # tib_vid_raw <- 
    #   tib_vid_raw[tib_vid_raw$Behavior != "*General Placeholder*", ]
    # tib_vid_raw <- 
    #   tib_vid_raw[str_detect(tib_vid_raw$Behavior,
    #                          pattern = "Deviant",
    #                          negate = TRUE), ]
    # tib_vid_raw <- 
    #   tib_vid_raw[str_detect(tib_vid_raw$Behavior,
    #                          pattern = "UEM",
    #                          negate = TRUE), ]
    
    
    # Start & Stop Times ------------------------------------------------------
    message("Start/Stop...",
            appendLF = FALSE)
    # Make Posture Uncoded codes the same as Activity.
    if (schema == "Posture") {
      
      tib_vid_raw$behavior <- 
        tib_vid_raw$behavior %>% 
        dplyr::recode(
          "Uncoded - Dark/Obscured/OoF" = "[U] Dark/Obscured/OoF",
          "Uncoded; Start/Stop"         = "[U] Start/Stop",
          "Start Time"                  = "[U] Start Time",
          "Stop Time"                   = "[U] Stop Time",
          "[P} Lying"                   = "[P] Lying",
          "[P] Other"                   = "[P] Other - Posture",
          "[M] Other"                   = "[M] Other - Movement"
        )
      
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded - Dark/Obscured/OoF"] <- 
      #   "[U] Dark/Obscured/OoF"
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded; Start/Stop"] <- 
      #   "[U] Start/Stop"
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Start Time"] <- 
      #   "[U] Start Time"
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Stop Time"] <- 
      #   "[U] Stop Time"
      
    }
    
    if (project == "DOCOMP") {
      
      dtm_vid_start <- 
        tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Start Time"]
      dtm_vid_stop <- 
        tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Stop Time"]
      
    } else {
      
      # Change start/stop to corresponding time zone at the end.
      dtm_vid_start <- suppressWarnings(
        tib_vid_raw$comment[tib_vid_raw$behavior == "[U] Start Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      dtm_vid_stop <- suppressWarnings(
        tib_vid_raw$comment[tib_vid_raw$behavior == "[U] Stop Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      
      if (is.na(dtm_vid_start)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Start Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
      
      if (is.na(dtm_vid_stop)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Stop Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
    }
    
    if ((tib_vid_raw$behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      message("",
              appendLF = TRUE)
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    dtm_relative_hmsf_end <- 
      tib_vid_raw$time_relative_hmsf[nrow(tib_vid_raw)]
    
    dtm_relative_hms_start <- 
      tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      tib_vid_raw$time_relative_hmsf[tib_vid_raw$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      tib_vid_raw$time_relative_hmsf[tib_vid_raw$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    tib_vid_raw <- 
      tib_vid_raw %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      )
    # tib_vid_raw <- 
    #   tib_vid_raw[(tib_vid_raw$behavior == "[U] Start/Stop" |
    #                  tib_vid_raw$behavior == "[U] Start Time" |
    #                  tib_vid_raw$behavior == "[U] Stop Time") == FALSE, ]
    
    # CHECK: Start & Stop -----------------------------------------------------
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    
    # lubridate::seconds(tib_vid_raw$time_relative_hmsf[1]) != 
    #   lubridate::seconds(dtm_relative_hmsf_start)
    
    if (tib_vid_raw$time_relative_hmsf[1] != dtm_relative_hmsf_start) {
      
      message("",
              appendLF = TRUE)
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    }
    
    if (dplyr::near((tib_vid_raw$time_relative_hmsf[nrow(tib_vid_raw)] +
                     tib_vid_raw$duration_sf[nrow(tib_vid_raw)]) %>% 
                    seconds(),
                    dtm_relative_hmsf_end %>% 
                    seconds(),
                    tol = .01) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    }
    
    if (dplyr::near(difftime(time1 = dtm_vid_stop,
                             time2 = dtm_vid_start,
                             units = "secs"),
                    difftime(time1 = dtm_relative_hms_stop,
                             time2 = dtm_relative_hms_start,
                             units = "secs"),
                    tol = 5) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1 -
        t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    
    # Clean #2 ----------------------------------------------------------------
    
    # DOCOMP specific...I think?...No its not.
    tib_vid_raw$behavior <- 
      tib_vid_raw$behavior %>% 
      dplyr::recode(
        "[Q] Caring Grooming - Self" = "[Q] Caring/Grooming - Self",
        "[LQ] Caring Grooming - Self" = "[LQ] Caring/Grooming - Self",
        "[HQ] Caring Grooming - Self" = "[HQ] Caring/Grooming - Self"
      )
    # tib_vid_raw$behavior[tib_vid_raw$behavior == "[Q] Caring Grooming - Self"] <- 
    #   "[Q] Caring/Grooming - Self"
    
    
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    if (colnames(tib_vid_raw) %>% 
        str_detect(pattern = "comment",
                   negate = TRUE) %>% 
        all()) {
      
      tib_vid_raw$comment <- 
        NA
      
    }
    
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(tib_vid_raw)[colnames(tib_vid_raw) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(tib_vid_raw)) {
      
      tib_vid_raw <-
        tib_vid_raw %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              remove = TRUE,
              na.rm = TRUE)
      
    } else {
      
      tib_vid_raw$mod_2 <- 
        NA
      
    }
    
    # colnames(tib_vid_raw)[colnames(tib_vid_raw) == "Modifier_1"] <- 
    #   "Mod_1"
    # 
    # if (schema == "Activity") {
    #   
    #   if ("Modifier_2" %in% colnames(tib_vid_raw)) {
    #     
    #     tib_vid_raw <- 
    #       tib_vid_raw %>% 
    #       unite(col = "Mod_2",
    #             contains("Modifier",
    #                      ignore.case = FALSE,
    #                      vars = NULL),
    #             remove = TRUE,
    #             na.rm = TRUE)
    #     
    #   } else {
    #     
    #     tib_vid_raw$Mod_2 <- 
    #       NA
    #     
    #   }
    # } else if (schema == "Posture") {
    #   
    #   tib_vid_raw$Mod_2 <- 
    #     NA
    #   
    # }
    
    # SecBySec ----------------------------------------------------------------
    message("SBS...",
            appendLF = FALSE)
    nrw_vid_raw <- 
      nrow(tib_vid_raw)
    int_duration <-  
      diff.POSIXt(tib_vid_raw$time_relative_hms,
                  units = "secs") %>% 
      as.vector()
    
    if (difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$time_relative_hms[nrw_vid_raw],
                 units = "secs") !=
        round(tib_vid_raw$duration_sf[nrow(tib_vid_raw)],
              digits = 0)) {
      
      diff_duration_sf <- 
        round(tib_vid_raw$duration_sf[nrow(tib_vid_raw)],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$time_relative_hms[nrw_vid_raw],
                 units = "secs")
      
      warning("File #", i, ": ", schema, " - ", fnm_obs, "\n",
              "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
              "Duration_sf is ", diff_duration_sf, " off.\n",
              "Using difference between the stop time and last code time.\n",
              call. = FALSE)
      
      # stop("The Duration_sf of the last code does not equal the difference between the stop time and last code time.")
      
    }
    
    int_duration <- 
      c(round(int_duration,
              digits = 0),
        round(difftime(time1 = dtm_relative_hms_stop,
                       time2 = tib_vid_raw$time_relative_hms[nrw_vid_raw],
                       units = "secs"),
              digits = 0))
    
    if (is.na(int_duration) %>% 
        any()) {
      
      message("",
              appendLF = TRUE)
      
      stop("Duration of a code is less than a second??? IDK man")
      
    }
    
    int_duration <- 
      as.integer(int_duration)
    
    sbs_int_events <- 
      int_duration %>% 
      seq_along() %>% 
      rep(times = int_duration)
    sbs_str_behavior <- 
      tib_vid_raw$behavior %>% 
      rep(times = int_duration)
    sbs_str_mod_1 <- 
      tib_vid_raw$mod_1 %>% 
      rep(times = int_duration)
    sbs_str_mod_2 <- 
      tib_vid_raw$mod_2 %>% 
      rep(times = int_duration)
    sbs_str_comment <- 
      tib_vid_raw$comment %>% 
      rep(times = int_duration)  
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    sbs_datetime <- 
      seq.POSIXt(from = dtm_vid_start,
                 by = 1,
                 length.out = nrw_vid_sbs)
    # dtm_vid_start == dtm_relative_hms_start
    sbs_date <- 
      sbs_datetime %>% 
      lubridate::as_date()
    sbs_time <- 
      sbs_datetime %>% 
      format("%H:%M:%S")
    
    tib_vid_sbs <- 
      tibble(
        study      = study,
        subject    = subject,
        visit      = visit,
        datetime   = sbs_datetime,
        date       = sbs_date,
        time       = sbs_time,
        behavior   = sbs_str_behavior,
        mod_2      = sbs_str_mod_2,
        mod_1      = sbs_str_mod_1,
        comment    = sbs_str_comment,
        events_raw = sbs_int_events,
        .rows      = nrw_vid_sbs
      )
    
    # Clean #3 ----------------------------------------------------------------
    message("Domain & Events...",
            appendLF = FALSE)
    
    # Remove category "[P/M/G/LQ]" from behavior. and fill NA's.
    tib_vid_sbs <- 
      tib_vid_sbs %>% 
      mutate(behavior = str_remove(behavior,
                                   pattern = "\\[[A-Z]{1,2}\\] ")) %>% 
      replace_na(replace = list(mod_1 = "Dark/Obscured/OoF"))
    
    # Replace NA's as rle function counts each NA as its own encoding.
    
    if (schema == "Activity") {
      
      # Put NA's for unite function.
      tib_vid_sbs$mod_2 <- 
        tib_vid_sbs$mod_2 %>% 
        na_if("")
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        unite(col = beh_act,
              behavior, mod_2,
              sep = " ~ ",
              remove = FALSE,
              na.rm = TRUE) %>% 
        rename(environment      = mod_1,
               activity         = mod_2,
               comment_behavior = comment,
               events_beh_raw   = events_raw) %>% 
        mutate(
          beh_bucket = dplyr::recode(behavior, !!!key_behavior_bucket),
          beh_domain = dplyr::recode(beh_bucket, !!!key_behavior_domain),
          events_behavior = 
            rle(behavior)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(behavior)$lengths),
          events_beh_act = 
            rle(beh_act)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(beh_act)$lengths),
          events_beh_domain = 
            rle(beh_bucket)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(beh_bucket)$lengths),
          events_environment = 
            rle(environment)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(environment)$lengths)
        ) %>% 
        select(
          study:time,
          behavior, activity, beh_act, beh_domain, beh_bucket, environment,
          events_behavior:events_environment, events_beh_raw, comment_behavior
        )
      
    } else if (schema == "Posture") {
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename(posture         = behavior,
               intensity       = mod_1,
               comment_posture = comment,
               events_pos_raw  = events_raw) %>% 
        mutate(
          pos_bucket = dplyr::recode(posture, !!!key_posture_bucket),
          pos_domain = dplyr::recode(pos_bucket, !!!key_posture_domain),
          events_posture = 
            rle(posture)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(posture)$lengths),
          events_pos_domain = 
            rle(pos_bucket)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(pos_bucket)$lengths),
          events_intensity = 
            rle(intensity)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(intensity)$lengths)
        ) %>% 
        select(
          study:time,
          posture, intensity, pos_domain, pos_bucket,
          events_posture:events_intensity, events_pos_raw, comment_posture
        )
      
    }
    
    # Project-specific --------------------------------------------------------
    message("Project Specific...",
            appendLF = FALSE)
    if (project == "FLAC - Aim 1") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Denver")
      fnm_vid_clean <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
      if (schema == "Activity") {
        
        tib_vid_sbs$environment <- 
          "Non-Domestic"
        
      }
      
    } else if (project == "FLAC - Aim 2") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Chicago")
      fnm_vid_clean <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
    } else if (project == "DOCOMP") {
      
      sample_length <- 
        fnm_obs_split[4] %>% 
        str_remove(pattern = "sec")
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename_with(.fn = paste0, "_", sample_length,
                    .cols = 7:last_col())
      # rename_with(.fn = paste0, "_", sample_length,
      #             .cols = starts_with(regex("behavior|posture")):last_col())
      fnm_vid_clean <- 
        paste0(
          study, "_", subject, "v", visit, "_", schema, "_", sample_length, ".csv"
        )
      
    }
    
    vroom_write(
      tib_vid_sbs,
      path = paste(fdr_vid_clean,
                   fnm_vid_clean,
                   sep = "/"),
      delim = ","
    )
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
}
shape_noldus_v5 <- function(project,
                            fdr_vid_raw,
                            fdr_vid_clean,
                            key_behavior_bucket,
                            key_behavior_domain,
                            key_posture_bucket,
                            key_posture_domain) {
  # CHANGES:
  # -Make domains include full name of posture/behavior.
  # -Remove old code from v4
  
  # project <-
  #   "FLAC - Aim 1"
  # fdr_vid_raw <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/0_raw"
  # fdr_vid_clean <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  
  # project <-
  #   "DOCOMP"
  # fdr_vid_raw <-
  #   "./3_data/0_raw/event_logs"
  # fdr_vid_clean <-
  #   "./3_data/1_cleaned/sbs"
  # key_behavior_bucket <-
  #   c(
  #     "Sports/Exercise"              = 1L,
  #     "Eating/Drinking"              = 2L,
  #     "Transportation"               = 3L,
  #     "Electronics"                  = 2L,
  #     "Other - Manipulating Objects" = 4L,
  #     "Other - Carrying Load w/ UE"  = 4L,
  #     "Other - Pushing Cart"         = 4L,
  #     "Talking - Person"             = 2L,
  #     "Talking - Phone"              = 2L,
  #     "Caring/Grooming - Adult"      = 5L,
  #     "Caring/Grooming - Animal/Pet" = 5L,
  #     "Caring/Grooming - Child"      = 5L,
  #     "Caring/Grooming - Self"       = 2L,
  #     "Cleaning"                     = 6L,
  #     "C/F/R/M"                      = 6L,
  #     "Cooking/Meal Preperation"     = 6L,
  #     "Laundry"                      = 6L,
  #     "Lawn&Garden"                  = 6L,
  #     "Leisure Based"                = 2L,
  #     "Only [P/M] Code"              = 7L,
  #     "Talking - Researchers"        = 7L,
  #     "Intermittent Activity"        = 7L,
  #     "Dark/Obscured/OoF"            = 8L
  #   )
  # key_behavior_domain <-
  #   c(
  #     "1" = "Sport&Exercise",
  #     "2" = "Leisure",
  #     "3" = "Transportation",
  #     "4" = "Other",
  #     "5" = "Caring&Grooming",
  #     "6" = "Household",
  #     "7" = "Transition",
  #     "8" = "Uncoded"
  #   )
  # key_posture_bucket <-
  #   c(
  #     "Lying"                            = 1L,
  #     "Sitting"                          = 1L,
  #     "Crouching / Kneeling / Squating"  = 2L,
  #     "Standing"                         = 3L,
  #     "Other - Posture"                  = 4L,
  #     "Intermittent Posture"             = 4L,
  #     "Walking"                          = 5L,
  #     "Stepping"                         = 5L,
  #     "Running"                          = 5L,
  #     "Ascending Stairs"                 = 5L,
  #     "Descending Stairs"                = 5L,
  #     "Crouching / Squating"             = 6L,
  #     "Cycling"                          = 6L,
  #     "Other - Movement"                 = 7L,
  #     "Intermittent Movement"            = 7L,
  #     "Intermittent P/M"                 = 8L,
  #     "Dark/Obscured/OoF"                = 8L
  #   )
  # key_posture_domain <-
  #   c(
  #     "1" = "Sit",
  #     "2" = "Crouching",
  #     "3" = "Stand",
  #     "4" = "Posture Other",
  #     "5" = "Movement",
  #     "6" = "Movement Stationary",
  #     "7" = "Movement Other",
  #     "8" = "Transition/Uncoded"
  #   )
  
  fls_vid_raw <-
    list.files(path = fdr_vid_raw,
               pattern = ".xlsx")
  
  for (i in seq_along(fls_vid_raw)) {
    
    fnm_vid_raw <-
      fls_vid_raw[i]
    
    # Save this code for another function that will check files are
    # named correctly.
    # fnm_obs_len <- 
    #   fnm_vid_raw %>% 
    #   str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
    #   str_extract(pattern = "([^ - ]*)$") %>% 
    #   str_length()
    fnm_obs <-
      fnm_vid_raw %>%
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$")
    fnm_obs_split <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_vid_raw %>% 
      str_extract(pattern = "Activity|Posture")
    
    message("Cleaning file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = TRUE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2]
    coder <- 
      fnm_obs_split[3]
    
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v).)*")
    
    if (study == "CO") {
      
      sub_vis <- 
        paste(sub_vis,
              "1",
              sep = "v")
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        sub_vis %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    tib_vid_raw <- 
      read_xlsx(path = paste(fdr_vid_raw,
                             fnm_vid_raw,
                             sep = "/"),
                progress = FALSE)
    
    # Consistency: Make all column names lowercase
    colnames(tib_vid_raw) <- 
      colnames(tib_vid_raw) %>% 
      str_to_lower()
    
    # Clean #1 ----------------------------------------------------------------
    tib_vid_raw <- 
      tib_vid_raw %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior =="*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      )
    
    # tib_vid_raw <- 
    #   tib_vid_raw[tib_vid_raw$Event_Type != "State stop", ]
    # tib_vid_raw <- 
    #   tib_vid_raw[tib_vid_raw$Behavior != "*General Placeholder*", ]
    # tib_vid_raw <- 
    #   tib_vid_raw[str_detect(tib_vid_raw$Behavior,
    #                          pattern = "Deviant",
    #                          negate = TRUE), ]
    # tib_vid_raw <- 
    #   tib_vid_raw[str_detect(tib_vid_raw$Behavior,
    #                          pattern = "UEM",
    #                          negate = TRUE), ]
    
    
    # Start & Stop Times ------------------------------------------------------
    message("Start/Stop...",
            appendLF = FALSE)
    # Make Posture Uncoded codes the same as Activity.
    if (schema == "Posture") {
      
      tib_vid_raw$behavior <- 
        tib_vid_raw$behavior %>% 
        dplyr::recode(
          "Uncoded - Dark/Obscured/OoF" = "[U] Dark/Obscured/OoF",
          "Uncoded; Start/Stop"         = "[U] Start/Stop",
          "Start Time"                  = "[U] Start Time",
          "Stop Time"                   = "[U] Stop Time",
          "[P} Lying"                   = "[P] Lying",
          "[P] Other"                   = "[P] Other - Posture",
          "[M] Other"                   = "[M] Other - Movement"
        )
      
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded - Dark/Obscured/OoF"] <- 
      #   "[U] Dark/Obscured/OoF"
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded; Start/Stop"] <- 
      #   "[U] Start/Stop"
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Start Time"] <- 
      #   "[U] Start Time"
      # tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Stop Time"] <- 
      #   "[U] Stop Time"
      
    }
    
    if (project == "DOCOMP") {
      
      dtm_vid_start <- 
        tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Start Time"]
      dtm_vid_stop <- 
        tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Stop Time"]
      
    } else {
      
      # Change start/stop to corresponding time zone at the end.
      dtm_vid_start <- suppressWarnings(
        tib_vid_raw$comment[tib_vid_raw$behavior == "[U] Start Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      dtm_vid_stop <- suppressWarnings(
        tib_vid_raw$comment[tib_vid_raw$behavior == "[U] Stop Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      
      if (is.na(dtm_vid_start)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Start Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
      
      if (is.na(dtm_vid_stop)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Stop Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
    }
    
    if ((tib_vid_raw$behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      message("",
              appendLF = TRUE)
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    dtm_relative_hmsf_end <- 
      tib_vid_raw$time_relative_hmsf[nrow(tib_vid_raw)]
    
    dtm_relative_hms_start <- 
      tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      tib_vid_raw$time_relative_hms[tib_vid_raw$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      tib_vid_raw$time_relative_hmsf[tib_vid_raw$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      tib_vid_raw$time_relative_hmsf[tib_vid_raw$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    tib_vid_raw <- 
      tib_vid_raw %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      )
    # tib_vid_raw <- 
    #   tib_vid_raw[(tib_vid_raw$behavior == "[U] Start/Stop" |
    #                  tib_vid_raw$behavior == "[U] Start Time" |
    #                  tib_vid_raw$behavior == "[U] Stop Time") == FALSE, ]
    
    # CHECK: Start & Stop -----------------------------------------------------
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    
    # lubridate::seconds(tib_vid_raw$time_relative_hmsf[1]) != 
    #   lubridate::seconds(dtm_relative_hmsf_start)
    
    if (tib_vid_raw$time_relative_hmsf[1] != dtm_relative_hmsf_start) {
      
      message("",
              appendLF = TRUE)
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    }
    
    if (dplyr::near((tib_vid_raw$time_relative_hmsf[nrow(tib_vid_raw)] +
                     tib_vid_raw$duration_sf[nrow(tib_vid_raw)]) %>% 
                    seconds(),
                    dtm_relative_hmsf_end %>% 
                    seconds(),
                    tol = .01) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    }
    
    if (dplyr::near(difftime(time1 = dtm_vid_stop,
                             time2 = dtm_vid_start,
                             units = "secs"),
                    difftime(time1 = dtm_relative_hms_stop,
                             time2 = dtm_relative_hms_start,
                             units = "secs"),
                    tol = 5) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1 -
        t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    
    # Clean #2 ----------------------------------------------------------------
    
    # DOCOMP specific...I think?...No its not.
    tib_vid_raw$behavior <- 
      tib_vid_raw$behavior %>% 
      dplyr::recode(
        "[Q] Caring Grooming - Self" = "[Q] Caring/Grooming - Self",
        "[LQ] Caring Grooming - Self" = "[LQ] Caring/Grooming - Self",
        "[HQ] Caring Grooming - Self" = "[HQ] Caring/Grooming - Self"
      )
    # tib_vid_raw$behavior[tib_vid_raw$behavior == "[Q] Caring Grooming - Self"] <- 
    #   "[Q] Caring/Grooming - Self"
    
    
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    if (colnames(tib_vid_raw) %>% 
        str_detect(pattern = "comment",
                   negate = TRUE) %>% 
        all()) {
      
      tib_vid_raw$comment <- 
        NA
      
    }
    
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(tib_vid_raw)[colnames(tib_vid_raw) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(tib_vid_raw)) {
      
      tib_vid_raw <-
        tib_vid_raw %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              remove = TRUE,
              na.rm = TRUE)
      
    } else {
      
      tib_vid_raw$mod_2 <- 
        NA
      
    }
    
    # colnames(tib_vid_raw)[colnames(tib_vid_raw) == "Modifier_1"] <- 
    #   "Mod_1"
    # 
    # if (schema == "Activity") {
    #   
    #   if ("Modifier_2" %in% colnames(tib_vid_raw)) {
    #     
    #     tib_vid_raw <- 
    #       tib_vid_raw %>% 
    #       unite(col = "Mod_2",
    #             contains("Modifier",
    #                      ignore.case = FALSE,
    #                      vars = NULL),
    #             remove = TRUE,
    #             na.rm = TRUE)
    #     
    #   } else {
    #     
    #     tib_vid_raw$Mod_2 <- 
    #       NA
    #     
    #   }
    # } else if (schema == "Posture") {
    #   
    #   tib_vid_raw$Mod_2 <- 
    #     NA
    #   
    # }
    
    # SecBySec ----------------------------------------------------------------
    message("SBS...",
            appendLF = FALSE)
    nrw_vid_raw <- 
      nrow(tib_vid_raw)
    int_duration <-  
      diff.POSIXt(tib_vid_raw$time_relative_hms,
                  units = "secs") %>% 
      as.vector()
    
    if (difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$time_relative_hms[nrw_vid_raw],
                 units = "secs") !=
        round(tib_vid_raw$duration_sf[nrow(tib_vid_raw)],
              digits = 0)) {
      
      diff_duration_sf <- 
        round(tib_vid_raw$duration_sf[nrow(tib_vid_raw)],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_raw$time_relative_hms[nrw_vid_raw],
                 units = "secs")
      
      warning("File #", i, ": ", schema, " - ", fnm_obs, "\n",
              "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
              "Duration_sf is ", diff_duration_sf, " off.\n",
              "Using difference between the stop time and last code time.\n",
              call. = FALSE)
      
      # stop("The Duration_sf of the last code does not equal the difference between the stop time and last code time.")
      
    }
    
    int_duration <- 
      c(round(int_duration,
              digits = 0),
        round(difftime(time1 = dtm_relative_hms_stop,
                       time2 = tib_vid_raw$time_relative_hms[nrw_vid_raw],
                       units = "secs"),
              digits = 0))
    
    if (is.na(int_duration) %>% 
        any()) {
      
      message("",
              appendLF = TRUE)
      
      stop("Duration of a code is less than a second??? IDK man")
      
    }
    
    int_duration <- 
      as.integer(int_duration)
    
    sbs_int_events <- 
      int_duration %>% 
      seq_along() %>% 
      rep(times = int_duration)
    sbs_str_behavior <- 
      tib_vid_raw$behavior %>% 
      rep(times = int_duration)
    sbs_str_mod_1 <- 
      tib_vid_raw$mod_1 %>% 
      rep(times = int_duration)
    sbs_str_mod_2 <- 
      tib_vid_raw$mod_2 %>% 
      rep(times = int_duration)
    sbs_str_comment <- 
      tib_vid_raw$comment %>% 
      rep(times = int_duration)  
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    sbs_datetime <- 
      seq.POSIXt(from = dtm_vid_start,
                 by = 1,
                 length.out = nrw_vid_sbs)
    # dtm_vid_start == dtm_relative_hms_start
    sbs_date <- 
      sbs_datetime %>% 
      lubridate::as_date()
    sbs_time <- 
      sbs_datetime %>% 
      format("%H:%M:%S")
    
    tib_vid_sbs <- 
      tibble(
        study      = study,
        subject    = subject,
        visit      = visit,
        datetime   = sbs_datetime,
        date       = sbs_date,
        time       = sbs_time,
        behavior   = sbs_str_behavior,
        mod_2      = sbs_str_mod_2,
        mod_1      = sbs_str_mod_1,
        comment    = sbs_str_comment,
        events_raw = sbs_int_events,
        .rows      = nrw_vid_sbs
      )
    
    # Clean #3 ----------------------------------------------------------------
    message("Domain & Events...",
            appendLF = FALSE)
    
    # Remove category "[P/M/G/LQ]" from behavior. and fill NA's.
    tib_vid_sbs <- 
      tib_vid_sbs %>% 
      mutate(behavior = str_remove(behavior,
                                   pattern = "\\[[A-Z]{1,2}\\] ")) %>% 
      replace_na(replace = list(mod_1 = "Dark/Obscured/OoF"))
    
    # Replace NA's as rle function counts each NA as its own encoding.
    
    if (schema == "Activity") {
      
      # Put NA's for unite function.
      tib_vid_sbs$mod_2 <- 
        tib_vid_sbs$mod_2 %>% 
        na_if("")
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        unite(col = beh_act,
              behavior, mod_2,
              sep = " ~ ",
              remove = FALSE,
              na.rm = TRUE) %>% 
        rename(environment      = mod_1,
               activity         = mod_2,
               comment_behavior = comment,
               events_beh_raw   = events_raw) %>% 
        mutate(
          beh_bucket = dplyr::recode(behavior, !!!key_behavior_bucket),
          beh_domain = dplyr::recode(beh_bucket, !!!key_behavior_domain),
          events_behavior = 
            rle(behavior)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(behavior)$lengths),
          events_beh_act = 
            rle(beh_act)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(beh_act)$lengths),
          events_beh_domain = 
            rle(beh_bucket)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(beh_bucket)$lengths),
          events_environment = 
            rle(environment)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(environment)$lengths)
        ) %>% 
        select(
          study:time,
          behavior, activity, beh_act, beh_domain, beh_bucket, environment,
          events_behavior:events_environment, events_beh_raw, comment_behavior
        )
      
    } else if (schema == "Posture") {
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename(posture         = behavior,
               intensity       = mod_1,
               comment_posture = comment,
               events_pos_raw  = events_raw) %>% 
        mutate(
          pos_bucket = dplyr::recode(posture, !!!key_posture_bucket),
          pos_domain = dplyr::recode(pos_bucket, !!!key_posture_domain),
          events_posture = 
            rle(posture)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(posture)$lengths),
          events_pos_domain = 
            rle(pos_bucket)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(pos_bucket)$lengths),
          events_intensity = 
            rle(intensity)$lengths %>% 
            seq_along() %>% 
            rep(times = rle(intensity)$lengths)
        ) %>% 
        select(
          study:time,
          posture, intensity, pos_domain, pos_bucket,
          events_posture:events_intensity, events_pos_raw, comment_posture
        )
      
    }
    
    # Project-specific --------------------------------------------------------
    message("Project Specific...",
            appendLF = FALSE)
    if (project == "FLAC - Aim 1") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Denver")
      fnm_vid_clean <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
      if (schema == "Activity") {
        
        tib_vid_sbs$environment <- 
          "Non-Domestic"
        
      }
      
    } else if (project == "FLAC - Aim 2") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Chicago")
      fnm_vid_clean <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
    } else if (project == "DOCOMP") {
      
      sample_length <- 
        fnm_obs_split[4] %>% 
        str_remove(pattern = "sec")
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename_with(.fn = paste0, "_", sample_length,
                    .cols = 7:last_col())
      # rename_with(.fn = paste0, "_", sample_length,
      #             .cols = starts_with(regex("behavior|posture")):last_col())
      fnm_vid_clean <- 
        paste0(
          study, "_", subject, "v", visit, "_", schema, "_", sample_length, ".csv"
        )
      
    }
    
    vroom_write(
      tib_vid_sbs,
      path = paste(fdr_vid_clean,
                   fnm_vid_clean,
                   sep = "/"),
      delim = ","
    )
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
}
shape_noldus_v7 <- function(project,
                            fdr_vid_clean,
                            fdr_vid_shape,
                            key_behavior_domain,
                            key_posture_bucket,
                            key_posture_domain) {
  # # CHANGES:
  
  # -Change function name from clean to shape. Match data flow of FLAC folders.
  # -Rename raw   TO clean in ARG and object names
  # -Rename clean TO shape in ARG and object names
  # -For fls_vid_clean, get list of noldus files from both {study}_AIM1_Noldus_Activity
  #  and {study}_AIM1_Noldus_Posture
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: get_domain_behavior
  # FUNCTION: get_domain_posture
  # ARG: project
  #      Self-explanatory
  # ARG: fdr_vid_clean
  #      File directory of clean files.
  # ARG: fdr_vid_shape
  #      File directory of shaped files.
  # ARG: key_behavior_domain
  #      For "key_domain" ARG of get_domain_behavior
  # ARG: key_posture_bucket
  #      For "key_bucket" ARG of get_domain_posture
  # ARG: key_posture_domain
  #      For "key_domain" ARG of get_domain_posture
  
  # # TESTING
  
  project <-
    "FLAC - Aim 1"
  fdr_vid_clean <-
    "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA"
  fdr_vid_shape <-
    "./FLAC_AIM1_DATA/3_AIM1_SHAPED_DATA"
  key_behavior_domain <-
    beh_domain_key_v2
  key_posture_bucket <-
    pos_bucket_key_v2
  key_posture_domain <-
    pos_domain_key_v2
  
  
  # project <-
  #   "DOCOMP"
  # fdr_vid_raw <-
  #   "./3_data/0_raw/event_logs"
  # fdr_vid_clean <-
  #   "./3_data/1_cleaned/sbs"
  # key_behavior_domain <-
  #   beh_domain_key_v2
  # key_posture_bucket <-
  #   pos_bucket_key_v2
  # key_posture_domain <-
  #   pos_domain_key_v2
  
  fld_fls_vid_clean <- 
    list.files(path = fdr_vid_clean,
               recursive = TRUE,
               pattern = ".xlsx") %>% 
    str_subset(pattern = "Noldus")
  fld_activity_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(pattern = "Activity") %>% 
    unique()
  fld_posture_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(pattern = "Posture") %>% 
    unique()
  fls_vid_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "([^/]*)$")
  # fls_vid_clean <-
  #   list.files(path = fdr_vid_clean,
  #              pattern = ".xlsx")
  
  for (i in seq_along(fls_vid_clean)) {
    
    fnm_vid_clean <-
      fls_vid_clean[i]
    
    # Save this code for another function that will check files are
    # named correctly.
    # fnm_obs_len <- 
    #   fnm_vid_clean %>% 
    #   str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
    #   str_extract(pattern = "([^ - ]*)$") %>% 
    #   str_length()
    fnm_obs <-
      fnm_vid_clean %>%
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$")
    fnm_obs_split <- 
      fnm_vid_clean %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_vid_clean %>% 
      str_extract(pattern = "Activity|Posture")
    
    message("Cleaning file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = TRUE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2]
    coder <- 
      fnm_obs_split[3]
    
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v).)*")
    
    if (study == "CO") {
      
      sub_vis <- 
        paste(sub_vis,
              "1",
              sep = "v")
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        sub_vis %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    switch(
      schema,
      "Activity" = {
        tib_vid_clean <- 
          read_xlsx(path = paste(fdr_vid_clean,
                                 fld_activity_clean,
                                 fnm_vid_clean,
                                 sep = "/"),
                    progress = FALSE)
      },
      "Posture" = {
        tib_vid_clean <- 
          read_xlsx(path = paste(fdr_vid_clean,
                                 fld_posture_clean,
                                 fnm_vid_clean,
                                 sep = "/"),
                    progress = FALSE)
      }
    )
    # tib_vid_clean <- 
    #   read_xlsx(path = paste(fdr_vid_clean,
    #                          fnm_vid_clean,
    #                          sep = "/"),
    #             progress = FALSE)
    
    # Consistency: Make all column names lowercase
    colnames(tib_vid_clean) <- 
      colnames(tib_vid_clean) %>% 
      str_to_lower()
    
    # Clean #1 ----------------------------------------------------------------
    tib_vid_clean <- 
      tib_vid_clean %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior == "*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      )
    
    # Start & Stop Times ------------------------------------------------------
    message("Start/Stop...",
            appendLF = FALSE)
    
    # Make Posture Uncoded codes the same as Activity and fix small stuff.
    tib_vid_clean$behavior <- 
      tib_vid_clean$behavior %>% 
      dplyr::recode(
        "Uncoded - Dark/Obscured/OoF" = "[U] Dark/Obscured/OoF",
        "Uncoded; Start/Stop"         = "[U] Start/Stop",
        "Start Time"                  = "[U] Start Time",
        "Stop Time"                   = "[U] Stop Time",
        # posture
        "[P] Other"                           = "[P] Other - Posture",
        "[P} Lying"                           = "[P] Lying",
        "[P] Crouching / Kneeling / Squating" = "[P] Crouching/Kneeling/Squatting",
        "[M] Other"                           = "[M] Other - Movement",
        "[M] Crouching / Squating"            = "[M] Crouching/Squatting",
        # behavior
        "[Q] Cooking/Meal Preperation"  = "[Q] Cooking/Meal Preparation",
        "[Q] Caring Grooming - Self"    = "[Q] Caring/Grooming - Self", # DOCOMP
        "[LQ] Caring Grooming - Self"   = "[LQ] Caring/Grooming - Self",
        "[LQ] Cooking/Meal Preperation" = "[LQ] Cooking/Meal Preparation",
        "[HQ] Caring Grooming - Self"   = "[HQ] Caring/Grooming - Self",
        "[HQ] Cooking/Meal Preperation" = "[HQ] Cooking/Meal Preparation"
      )
    
    if (project == "DOCOMP") {
      
      dtm_vid_start <- 
        tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Start Time"]
      dtm_vid_stop <- 
        tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Stop Time"]
      
    } else {
      
      # Change start/stop to corresponding time zone at the end.
      dtm_vid_start <- suppressWarnings(
        tib_vid_clean$comment[tib_vid_clean$behavior == "[U] Start Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      dtm_vid_stop <- suppressWarnings(
        tib_vid_clean$comment[tib_vid_clean$behavior == "[U] Stop Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      
      if (is.na(dtm_vid_start)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Start Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
      
      if (is.na(dtm_vid_stop)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Stop Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
    }
    
    if ((tib_vid_clean$behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      message("",
              appendLF = TRUE)
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    dtm_relative_hmsf_end <- 
      tib_vid_clean$time_relative_hmsf[nrow(tib_vid_clean)]
    
    dtm_relative_hms_start <- 
      tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      tib_vid_clean$time_relative_hmsf[tib_vid_clean$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      tib_vid_clean$time_relative_hmsf[tib_vid_clean$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    tib_vid_clean <- 
      tib_vid_clean %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      )
    
    # CHECK: Start & Stop -----------------------------------------------------
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    
    # lubridate::seconds(tib_vid_clean$time_relative_hmsf[1]) != 
    #   lubridate::seconds(dtm_relative_hmsf_start)
    
    if (tib_vid_clean$time_relative_hmsf[1] != dtm_relative_hmsf_start) {
      
      message("",
              appendLF = TRUE)
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    }
    
    if (dplyr::near((tib_vid_clean$time_relative_hmsf[nrow(tib_vid_clean)] +
                     tib_vid_clean$duration_sf[nrow(tib_vid_clean)]) %>% 
                    seconds(),
                    dtm_relative_hmsf_end %>% 
                    seconds(),
                    tol = .01) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    }
    
    if (dplyr::near(difftime(time1 = dtm_vid_stop,
                             time2 = dtm_vid_start,
                             units = "secs"),
                    difftime(time1 = dtm_relative_hms_stop,
                             time2 = dtm_relative_hms_start,
                             units = "secs"),
                    tol = 5) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1 -
        t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    
    # Clean #2 ----------------------------------------------------------------
    
    # DOCOMP specific...I think?...No its not.
    tib_vid_clean$behavior <- 
      tib_vid_clean$behavior %>% 
      dplyr::recode(
        "[Q] Caring Grooming - Self" = "[Q] Caring/Grooming - Self",
        "[LQ] Caring Grooming - Self" = "[LQ] Caring/Grooming - Self",
        "[HQ] Caring Grooming - Self" = "[HQ] Caring/Grooming - Self"
      )
    
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    if (colnames(tib_vid_clean) %>% 
        str_detect(pattern = "comment",
                   negate = TRUE) %>% 
        all()) {
      
      tib_vid_clean$comment <- 
        NA
      
    }
    
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(tib_vid_clean)[colnames(tib_vid_clean) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(tib_vid_clean)) {
      
      tib_vid_clean <-
        tib_vid_clean %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              remove = TRUE,
              na.rm = TRUE)
      
    } else {
      
      tib_vid_clean$mod_2 <- 
        NA
      
    }
    
    # SecBySec ----------------------------------------------------------------
    message("SBS...",
            appendLF = FALSE)
    nrw_vid_clean <- 
      nrow(tib_vid_clean)
    int_duration <-  
      diff.POSIXt(tib_vid_clean$time_relative_hms,
                  units = "secs") %>% 
      as.vector()
    
    if (difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                 units = "secs") !=
        round(tib_vid_clean$duration_sf[nrow(tib_vid_clean)],
              digits = 0)) {
      
      diff_duration_sf <- 
        round(tib_vid_clean$duration_sf[nrow(tib_vid_clean)],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                 units = "secs")
      
      warning("File #", i, ": ", schema, " - ", fnm_obs, "\n",
              "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
              "Duration_sf is ", diff_duration_sf, " off.\n",
              "Using difference between the stop time and last code time.\n",
              call. = FALSE)
      
      # stop("The Duration_sf of the last code does not equal the difference between the stop time and last code time.")
      
    }
    
    int_duration <- 
      c(round(int_duration,
              digits = 0),
        round(difftime(time1 = dtm_relative_hms_stop,
                       time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                       units = "secs"),
              digits = 0))
    
    if (is.na(int_duration) %>% 
        any()) {
      
      message("",
              appendLF = TRUE)
      
      stop("Duration of a code is less than a second??? IDK man")
      
    }
    
    int_duration <- 
      as.integer(int_duration)
    
    sbs_int_events <- 
      int_duration %>% 
      seq_along() %>% 
      rep(times = int_duration)
    sbs_str_behavior <- 
      tib_vid_clean$behavior %>% 
      rep(times = int_duration)
    sbs_str_mod_1 <- 
      tib_vid_clean$mod_1 %>% 
      rep(times = int_duration)
    sbs_str_mod_2 <- 
      tib_vid_clean$mod_2 %>% 
      rep(times = int_duration)
    sbs_str_comment <- 
      tib_vid_clean$comment %>% 
      rep(times = int_duration)  
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    sbs_datetime <- 
      seq.POSIXt(from = dtm_vid_start,
                 by = 1,
                 length.out = nrw_vid_sbs)
    sbs_date <- 
      sbs_datetime %>% 
      lubridate::as_date()
    sbs_time <- 
      sbs_datetime %>% 
      format("%H:%M:%S")
    
    tib_vid_sbs <- 
      tibble(
        study      = study,
        subject    = subject,
        visit      = visit,
        datetime   = sbs_datetime,
        date       = sbs_date,
        time       = sbs_time,
        behavior   = sbs_str_behavior,
        mod_2      = sbs_str_mod_2,
        mod_1      = sbs_str_mod_1,
        comment    = sbs_str_comment,
        .rows      = nrw_vid_sbs
      )
    
    # Clean #3 ----------------------------------------------------------------
    message("Domain & Events...",
            appendLF = FALSE)
    
    # Remove category "[P/M/G/LQ]" from behavior, make values lowercase
    # and fill NA's.
    tib_vid_sbs <- 
      tib_vid_sbs %>% 
      mutate(behavior = str_remove(behavior,
                                   pattern = "\\[[A-Z]{1,2}\\] "),
             behavior = str_to_lower(behavior),
             mod_2    = str_to_lower(mod_2),
             mod_1    = str_to_lower(mod_1)) %>% 
      replace_na(replace = list(mod_1 = "dark/obscured/oof"))
    
    if (schema == "Activity") {
      
      # Put NA's for unite function.
      tib_vid_sbs$mod_2 <- 
        tib_vid_sbs$mod_2 %>% 
        na_if("")
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        unite(col = behavior_activity,
              behavior, mod_2,
              sep = " ~ ",
              remove = FALSE,
              na.rm = TRUE) %>% 
        rename(environment      = mod_1,
               activity         = mod_2,
               comment_behavior = comment) %>% 
        mutate(
          # Fix spelling.
          environment = 
            dplyr::recode(
              environment,
              "organizational/civic/religiious" = "organizational/civic/religious"
            ),
          behavior_domain = get_domain_behavior(
            vec_behavior    = behavior,
            vec_environment = environment,
            key_domain      = key_behavior_domain)
        ) %>% 
        select(
          study:time,
          behavior, behavior_activity, behavior_domain, environment,
          comment_behavior
        )
      
    } else if (schema == "Posture") {
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename(posture         = behavior,
               intensity       = mod_1,
               comment_posture = comment) %>% 
        mutate(
          posture_domain = get_domain_posture(
            vec_posture = posture,
            key_bucket  = key_posture_bucket,
            key_domain  = key_posture_domain
          ),
          # Change mod-vig to mvpa.
          intensity = dplyr::recode(intensity,
                                    "mod-vig" = "mvpa")
        ) %>% 
        select(
          study:time,
          posture, intensity, posture_domain,
          comment_posture
        )
      
    }
    
    # Project-specific --------------------------------------------------------
    message("Project Specific...",
            appendLF = FALSE)
    if (project == "FLAC - Aim 1") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Denver")
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
      if (schema == "Activity") {
        
        tib_vid_sbs$environment <- 
          "Non-Domestic"
        
      }
      
    } else if (project == "FLAC - Aim 2") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Chicago")
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
    } else if (project == "DOCOMP") {
      
      sample_length <- 
        fnm_obs_split[4] %>% 
        str_remove(pattern = "sec")
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename_with(.fn = ~ paste0(.x, "_", sample_length),
                    .cols = 7:last_col())
      # rename_with(.fn = paste0, "_", sample_length,
      #             .cols = starts_with(regex("behavior|posture")):last_col())
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "v", visit, "_", schema, "_", sample_length, ".csv"
        )
      
    }
    
    
    # Don't know if this is projet specific yet but we shall see.
    fld_activity_shape <- 
      list.files(path = fdr_vid_shape) %>% 
      str_subset(pattern = "Activity")
    fld_posture_shape <- 
      list.files(path = fdr_vid_shape) %>% 
      str_subset(pattern = "Posture")
    
    switch(
      schema,
      "Activity" = {
        vroom_write(
          tib_vid_sbs,
          path = paste(fdr_vid_shape,
                       fld_activity_shape,
                       fnm_vid_shape,
                       sep = "/"),
          delim = ","
        )
      },
      "Posture" = {
        vroom_write(
          tib_vid_sbs,
          path = paste(fdr_vid_shape,
                       fld_posture_shape,
                       fnm_vid_shape,
                       sep = "/"),
          delim = ","
        )
      }
    )
    
    # vroom_write(
    #   tib_vid_sbs,
    #   path = paste(fdr_vid_shape,
    #                fnm_vid_shape,
    #                sep = "/"),
    #   delim = ","
    # )
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
}
shape_noldus_v8 <- function(project,
                            fdr_vid_clean,
                            fdr_vid_shape,
                            key_behavior_domain,
                            key_posture_bucket,
                            key_posture_domain) {
  # # CHANGES:
  
  # -Change function name from clean to shape. Match data flow of FLAC folders.
  # -Rename raw   TO clean in ARG and object names
  # -Rename clean TO shape in ARG and object names
  # -For fls_vid_clean, get list of noldus files from both {study}_AIM1_Noldus_Activity
  #  and {study}_AIM1_Noldus_Posture
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: get_domain_behavior
  # FUNCTION: get_domain_posture
  # ARG: project
  #      Self-explanatory
  # ARG: fdr_vid_clean
  #      File directory of clean files.
  # ARG: fdr_vid_shape
  #      File directory of shaped files.
  # ARG: key_behavior_domain
  #      For "key_domain" ARG of get_domain_behavior
  # ARG: key_posture_bucket
  #      For "key_bucket" ARG of get_domain_posture
  # ARG: key_posture_domain
  #      For "key_domain" ARG of get_domain_posture
  
  # # TESTING
  
  project <-
    "FLAC - Aim 1"
  fdr_vid_clean <-
    "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA"
  fdr_vid_shape <-
    "./FLAC_AIM1_DATA/3_AIM1_SHAPED_DATA"
  key_behavior_domain <-
    beh_domain_key_v2
  key_posture_bucket <-
    pos_bucket_key_v2
  key_posture_domain <-
    pos_domain_key_v2
  
  
  fld_fls_vid_clean <- 
    list.files(path = fdr_vid_clean,
               recursive = TRUE,
               pattern = ".xlsx") %>% 
    str_subset(pattern = "Noldus")
  fld_activity_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(pattern = "Activity") %>% 
    unique()
  fld_posture_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(pattern = "Posture") %>% 
    unique()
  fls_vid_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "([^/]*)$")
  # fls_vid_clean <-
  #   list.files(path = fdr_vid_clean,
  #              pattern = ".xlsx")
  
  for (i in seq_along(fls_vid_clean)) {
    
    fnm_vid_clean <-
      fls_vid_clean[i]
    
    # Save this code for another function that will check files are
    # named correctly.
    # fnm_obs_len <- 
    #   fnm_vid_clean %>% 
    #   str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
    #   str_extract(pattern = "([^ - ]*)$") %>% 
    #   str_length()
    fnm_obs <-
      fnm_vid_clean %>%
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$")
    fnm_obs_split <- 
      fnm_vid_clean %>% 
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_vid_clean %>% 
      str_extract(pattern = "Activity|Posture")
    
    message("Cleaning file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = TRUE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2]
    coder <- 
      fnm_obs_split[3]
    
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v).)*")
    
    if (study == "CO") {
      
      sub_vis <- 
        paste(sub_vis,
              "1",
              sep = "v")
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        sub_vis %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    switch(
      schema,
      "Activity" = {
        tib_vid_clean <- 
          read_xlsx(path = paste(fdr_vid_clean,
                                 fld_activity_clean,
                                 fnm_vid_clean,
                                 sep = "/"),
                    progress = FALSE)
      },
      "Posture" = {
        tib_vid_clean <- 
          read_xlsx(path = paste(fdr_vid_clean,
                                 fld_posture_clean,
                                 fnm_vid_clean,
                                 sep = "/"),
                    progress = FALSE)
      }
    )
    # tib_vid_clean <- 
    #   read_xlsx(path = paste(fdr_vid_clean,
    #                          fnm_vid_clean,
    #                          sep = "/"),
    #             progress = FALSE)
    
    # Consistency: Make all column names lowercase
    colnames(tib_vid_clean) <- 
      colnames(tib_vid_clean) %>% 
      str_to_lower()
    
    # Clean #1 ----------------------------------------------------------------
    tib_vid_clean <- 
      tib_vid_clean %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior == "*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      )
    
    # Start & Stop Times ------------------------------------------------------
    message("Start/Stop...",
            appendLF = FALSE)
    
    # Make Posture Uncoded codes the same as Activity and fix small stuff.
    tib_vid_clean$behavior <- 
      tib_vid_clean$behavior %>% 
      dplyr::recode(
        "Uncoded - Dark/Obscured/OoF" = "[U] Dark/Obscured/OoF",
        "Uncoded; Start/Stop"         = "[U] Start/Stop",
        "Start Time"                  = "[U] Start Time",
        "Stop Time"                   = "[U] Stop Time",
        # posture
        "[P] Other"                           = "[P] Other - Posture",
        "[P} Lying"                           = "[P] Lying",
        "[P] Crouching / Kneeling / Squating" = "[P] Crouching/Kneeling/Squatting",
        "[M] Other"                           = "[M] Other - Movement",
        "[M] Crouching / Squating"            = "[M] Crouching/Squatting",
        # behavior
        "[Q] Cooking/Meal Preperation"  = "[Q] Cooking/Meal Preparation",
        "[Q] Caring Grooming - Self"    = "[Q] Caring/Grooming - Self", # DOCOMP
        "[LQ] Caring Grooming - Self"   = "[LQ] Caring/Grooming - Self",
        "[LQ] Cooking/Meal Preperation" = "[LQ] Cooking/Meal Preparation",
        "[HQ] Caring Grooming - Self"   = "[HQ] Caring/Grooming - Self",
        "[HQ] Cooking/Meal Preperation" = "[HQ] Cooking/Meal Preparation"
      )
    
    if (project == "DOCOMP") {
      
      dtm_vid_start <- 
        tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Start Time"]
      dtm_vid_stop <- 
        tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Stop Time"]
      
    } else {
      
      # Change start/stop to corresponding time zone at the end.
      dtm_vid_start <- suppressWarnings(
        tib_vid_clean$comment[tib_vid_clean$behavior == "[U] Start Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      dtm_vid_stop <- suppressWarnings(
        tib_vid_clean$comment[tib_vid_clean$behavior == "[U] Stop Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      
      if (is.na(dtm_vid_start)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Start Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
      
      if (is.na(dtm_vid_stop)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Stop Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
    }
    
    if ((tib_vid_clean$behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      message("",
              appendLF = TRUE)
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    dtm_relative_hmsf_end <- 
      tib_vid_clean$time_relative_hmsf[nrow(tib_vid_clean)]
    
    dtm_relative_hms_start <- 
      tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      tib_vid_clean$time_relative_hmsf[tib_vid_clean$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      tib_vid_clean$time_relative_hmsf[tib_vid_clean$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    tib_vid_clean <- 
      tib_vid_clean %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      )
    
    # CHECK: Start & Stop -----------------------------------------------------
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    
    # lubridate::seconds(tib_vid_clean$time_relative_hmsf[1]) != 
    #   lubridate::seconds(dtm_relative_hmsf_start)
    
    if (tib_vid_clean$time_relative_hmsf[1] != dtm_relative_hmsf_start) {
      
      message("",
              appendLF = TRUE)
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    }
    
    if (dplyr::near((tib_vid_clean$time_relative_hmsf[nrow(tib_vid_clean)] +
                     tib_vid_clean$duration_sf[nrow(tib_vid_clean)]) %>% 
                    seconds(),
                    dtm_relative_hmsf_end %>% 
                    seconds(),
                    tol = .01) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    }
    
    if (dplyr::near(difftime(time1 = dtm_vid_stop,
                             time2 = dtm_vid_start,
                             units = "secs"),
                    difftime(time1 = dtm_relative_hms_stop,
                             time2 = dtm_relative_hms_start,
                             units = "secs"),
                    tol = 5) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1 -
        t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    
    # Clean #2 ----------------------------------------------------------------
    
    # DOCOMP specific...I think?...No its not.
    tib_vid_clean$behavior <- 
      tib_vid_clean$behavior %>% 
      dplyr::recode(
        "[Q] Caring Grooming - Self" = "[Q] Caring/Grooming - Self",
        "[LQ] Caring Grooming - Self" = "[LQ] Caring/Grooming - Self",
        "[HQ] Caring Grooming - Self" = "[HQ] Caring/Grooming - Self"
      )
    
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    if (colnames(tib_vid_clean) %>% 
        str_detect(pattern = "comment",
                   negate = TRUE) %>% 
        all()) {
      
      tib_vid_clean$comment <- 
        NA
      
    }
    
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(tib_vid_clean)[colnames(tib_vid_clean) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(tib_vid_clean)) {
      
      tib_vid_clean <-
        tib_vid_clean %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              remove = TRUE,
              na.rm = TRUE)
      
    } else {
      
      tib_vid_clean$mod_2 <- 
        NA
      
    }
    
    # SecBySec ----------------------------------------------------------------
    message("SBS...",
            appendLF = FALSE)
    nrw_vid_clean <- 
      nrow(tib_vid_clean)
    int_duration <-  
      diff.POSIXt(tib_vid_clean$time_relative_hms,
                  units = "secs") %>% 
      as.vector()
    
    if (difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                 units = "secs") !=
        round(tib_vid_clean$duration_sf[nrow(tib_vid_clean)],
              digits = 0)) {
      
      diff_duration_sf <- 
        round(tib_vid_clean$duration_sf[nrow(tib_vid_clean)],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                 units = "secs")
      
      warning("File #", i, ": ", schema, " - ", fnm_obs, "\n",
              "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
              "Duration_sf is ", diff_duration_sf, " off.\n",
              "Using difference between the stop time and last code time.\n",
              call. = FALSE)
      
      # stop("The Duration_sf of the last code does not equal the difference between the stop time and last code time.")
      
    }
    
    int_duration <- 
      c(round(int_duration,
              digits = 0),
        round(difftime(time1 = dtm_relative_hms_stop,
                       time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                       units = "secs"),
              digits = 0))
    
    if (is.na(int_duration) %>% 
        any()) {
      
      message("",
              appendLF = TRUE)
      
      stop("Duration of a code is less than a second??? IDK man")
      
    }
    
    int_duration <- 
      as.integer(int_duration)
    
    sbs_int_events <- 
      int_duration %>% 
      seq_along() %>% 
      rep(times = int_duration)
    sbs_str_behavior <- 
      tib_vid_clean$behavior %>% 
      rep(times = int_duration)
    sbs_str_mod_1 <- 
      tib_vid_clean$mod_1 %>% 
      rep(times = int_duration)
    sbs_str_mod_2 <- 
      tib_vid_clean$mod_2 %>% 
      rep(times = int_duration)
    sbs_str_comment <- 
      tib_vid_clean$comment %>% 
      rep(times = int_duration)  
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    sbs_datetime <- 
      seq.POSIXt(from = dtm_vid_start,
                 by = 1,
                 length.out = nrw_vid_sbs)
    sbs_date <- 
      sbs_datetime %>% 
      lubridate::as_date()
    sbs_time <- 
      sbs_datetime %>% 
      format("%H:%M:%S")
    
    tib_vid_sbs <- 
      tibble(
        study      = study,
        subject    = subject,
        visit      = visit,
        datetime   = sbs_datetime,
        date       = sbs_date,
        time       = sbs_time,
        behavior   = sbs_str_behavior,
        mod_2      = sbs_str_mod_2,
        mod_1      = sbs_str_mod_1,
        comment    = sbs_str_comment,
        .rows      = nrw_vid_sbs
      )
    
    # Clean #3 ----------------------------------------------------------------
    message("Domain & Events...",
            appendLF = FALSE)
    
    # Remove category "[P/M/G/LQ]" from behavior, make values lowercase
    # and fill NA's.
    tib_vid_sbs <- 
      tib_vid_sbs %>% 
      mutate(behavior = str_remove(behavior,
                                   pattern = "\\[[A-Z]{1,2}\\] "),
             behavior = str_to_lower(behavior),
             mod_2    = str_to_lower(mod_2),
             mod_1    = str_to_lower(mod_1)) %>% 
      replace_na(replace = list(mod_1 = "dark/obscured/oof"))
    
    if (schema == "Activity") {
      
      # Put NA's for unite function.
      tib_vid_sbs$mod_2 <- 
        tib_vid_sbs$mod_2 %>% 
        na_if("")
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        unite(col = behavior_activity,
              behavior, mod_2,
              sep = " ~ ",
              remove = FALSE,
              na.rm = TRUE) %>% 
        rename(environment      = mod_1,
               activity         = mod_2,
               comment_behavior = comment) %>% 
        mutate(
          # Fix spelling.
          environment = 
            dplyr::recode(
              environment,
              "organizational/civic/religiious" = "organizational/civic/religious"
            ),
          behavior_domain = get_domain_behavior(
            vec_behavior    = behavior,
            vec_environment = environment,
            key_domain      = key_behavior_domain)
        ) %>% 
        select(
          study:time,
          behavior, behavior_activity, behavior_domain, environment,
          comment_behavior
        )
      
    } else if (schema == "Posture") {
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename(posture         = behavior,
               intensity       = mod_1,
               comment_posture = comment) %>% 
        mutate(
          posture_domain = get_domain_posture(
            vec_posture = posture,
            key_bucket  = key_posture_bucket,
            key_domain  = key_posture_domain
          ),
          # Change mod-vig to mvpa.
          intensity = dplyr::recode(intensity,
                                    "mod-vig" = "mvpa")
        ) %>% 
        select(
          study:time,
          posture, intensity, posture_domain,
          comment_posture
        )
      
    }
    
    # Project-specific --------------------------------------------------------
    message("Project Specific...",
            appendLF = FALSE)
    if (project == "FLAC - Aim 1") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Denver")
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
      if (schema == "Activity") {
        
        tib_vid_sbs$environment <- 
          "Non-Domestic"
        
      }
      
    } else if (project == "FLAC - Aim 2") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Chicago")
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
    } else if (project == "DOCOMP") {
      
      sample_length <- 
        fnm_obs_split[4] %>% 
        str_remove(pattern = "sec")
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename_with(.fn = ~ paste0(.x, "_", sample_length),
                    .cols = 7:last_col())
      # rename_with(.fn = paste0, "_", sample_length,
      #             .cols = starts_with(regex("behavior|posture")):last_col())
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "v", visit, "_", schema, "_", sample_length, ".csv"
        )
      
    }
    
    
    # Don't know if this is projet specific yet but we shall see.
    fld_activity_shape <- 
      list.files(path = fdr_vid_shape) %>% 
      str_subset(pattern = "Activity")
    fld_posture_shape <- 
      list.files(path = fdr_vid_shape) %>% 
      str_subset(pattern = "Posture")
    
    switch(
      schema,
      "Activity" = {
        vroom_write(
          tib_vid_sbs,
          path = paste(fdr_vid_shape,
                       fld_activity_shape,
                       fnm_vid_shape,
                       sep = "/"),
          delim = ","
        )
      },
      "Posture" = {
        vroom_write(
          tib_vid_sbs,
          path = paste(fdr_vid_shape,
                       fld_posture_shape,
                       fnm_vid_shape,
                       sep = "/"),
          delim = ","
        )
      }
    )
    
    # vroom_write(
    #   tib_vid_sbs,
    #   path = paste(fdr_vid_shape,
    #                fnm_vid_shape,
    #                sep = "/"),
    #   delim = ","
    # )
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
}
shape_noldus_v9 <- function(project,
                            fdr_clean,
                            fdr_shape,
                            key_behavior_domain,
                            key_posture_bucket,
                            key_posture_domain) {
  # # CHANGES:
  
  # -changed names of arguments "fdr" to be shorter.
  # -Moved around things that could be combined into sections better.
  # -Renamed sections.
  # -Update to latest vroom_write.
  # -Don't have activity files have the "behavior_activity" column anymore.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: get_domain_behavior
  # FUNCTION: get_domain_posture
  # ARG: project
  #      Self-explanatory
  # ARG: fdr_vid_clean
  #      File directory of clean files.
  # ARG: fdr_vid_shape
  #      File directory of shaped files.
  # ARG: key_behavior_domain
  #      For "key_domain" ARG of get_domain_behavior
  # ARG: key_posture_bucket
  #      For "key_bucket" ARG of get_domain_posture
  # ARG: key_posture_domain
  #      For "key_domain" ARG of get_domain_posture
  
  # # TESTING
  
  project <-
    "FLAC - Aim 1"
  # fdr_vid_clean <-
  #   "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA"
  # fdr_vid_shape <-
  #   "./FLAC_AIM1_DATA/3_AIM1_SHAPED_DATA"
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
  fdr_clean <-
    "./3_data/1_cleaned/"
  fdr_shape <-
    "./3_data/2_shaped/"
  key_behavior_domain <-
    beh_domain_key_v2
  key_posture_bucket <-
    pos_bucket_key_v2
  key_posture_domain <-
    pos_domain_key_v2
  
  
  fld_fls_vid_clean <- 
    list.files(path = fdr_clean,
               recursive = TRUE,
               pattern = ".xlsx") %>% 
    str_subset(regex(pattern = "noldus",
                     ignore_case = TRUE))
  fld_activity_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(regex(pattern = "activity",
                     ignore_case = TRUE)) %>% 
    unique()
  fld_posture_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(regex(pattern = "posture",
                     ignore_case = TRUE)) %>% 
    unique()
  fls_vid_clean <- 
    fld_fls_vid_clean %>% 
    str_extract(pattern = "([^/]*)$")
  
  for (i in seq_along(fls_vid_clean)) {
    
    fnm_vid_clean <-
      fls_vid_clean[i]
    
    fnm_obs <-
      fnm_vid_clean %>%
      str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      str_extract(pattern = "([^ - ]*)$")
    fnm_obs_split <- 
      fnm_obs %>% 
      # str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
      # str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_vid_clean %>% 
      str_extract(pattern = "Activity|Posture")
    
    message("Shaping file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = TRUE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2]
    coder <- 
      fnm_obs_split[3]
    
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v).)*")
    
    if (study == "CO") {
      
      # sub_vis <- 
      #   paste(sub_vis,
      #         "1",
      #         sep = "v")
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        sub_vis %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    switch(
      schema,
      "Activity" = {
        tib_vid_clean <- 
          read_xlsx(path = paste(fdr_clean,
                                 fld_activity_clean,
                                 fnm_vid_clean,
                                 sep = "/"),
                    progress = FALSE)
      },
      "Posture" = {
        tib_vid_clean <- 
          read_xlsx(path = paste(fdr_clean,
                                 fld_posture_clean,
                                 fnm_vid_clean,
                                 sep = "/"),
                    progress = FALSE)
      }
    )
    # tib_vid_clean <- 
    #   read_xlsx(path = paste(fdr_vid_clean,
    #                          fnm_vid_clean,
    #                          sep = "/"),
    #             progress = FALSE)
    
    # Consistency: Make all column names lowercase
    colnames(tib_vid_clean) <- 
      colnames(tib_vid_clean) %>% 
      str_to_lower()
    
    # Shape #1: Start & Stop --------------------------------------------------
    message("Start/Stop...",
            appendLF = FALSE)
    
    tib_vid_clean <- 
      tib_vid_clean %>% 
      filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior == "*P Deviant*" |
            behavior == "*M Deviant*" |
            behavior == "*No UEM*" |
            behavior == "*Yes UEM*" |
            is.na(behavior))
      )
    
    # Make Posture Uncoded codes the same as Activity.
    tib_vid_clean$behavior <- 
      tib_vid_clean$behavior %>% 
      dplyr::recode(
        "Uncoded; Start/Stop"         = "[U] Start/Stop",
        "Start Time"                  = "[U] Start Time",
        "Stop Time"                   = "[U] Stop Time"
      )
    
    if (project == "DOCOMP") {
      
      dtm_vid_start <- 
        tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Start Time"]
      dtm_vid_stop <- 
        tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Stop Time"]
      
    } else {
      
      # Change start/stop to corresponding time zone at the end.
      dtm_vid_start <- suppressWarnings(
        tib_vid_clean$comment[tib_vid_clean$behavior == "[U] Start Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      dtm_vid_stop <- suppressWarnings(
        tib_vid_clean$comment[tib_vid_clean$behavior == "[U] Stop Time"] %>% 
          lubridate::mdy_hms(tz = "UTC")
      )
      
      if (is.na(dtm_vid_start)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Start Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
      
      if (is.na(dtm_vid_stop)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          schema, " - ", fnm_obs,
          "Stop Time is not in MM-DD-YYYY hh:mm:ss",
          call. = FALSE
        )
        
      }
    }
    
    if ((tib_vid_clean$behavior == "[U] Start/Stop") %>% 
        sum() != 2) {
      
      message("",
              appendLF = TRUE)
      
      # IDK man.
      stop("STAAAAAAAAAAAAAAAAAHP. [U] Start/Stop was applied more than twice or only once.")
      
    }
    
    dtm_relative_hmsf_end <- 
      tib_vid_clean$time_relative_hmsf[nrow(tib_vid_clean)]
    
    dtm_relative_hms_start <- 
      tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Start Time"]
    dtm_relative_hms_stop <- 
      tib_vid_clean$time_relative_hms[tib_vid_clean$behavior == "[U] Stop Time"]
    dtm_relative_hmsf_start <- 
      tib_vid_clean$time_relative_hmsf[tib_vid_clean$behavior == "[U] Start Time"]
    dtm_relative_hmsf_stop <- 
      tib_vid_clean$time_relative_hmsf[tib_vid_clean$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    tib_vid_clean <- 
      tib_vid_clean %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      )
    
    # CHECK: Start & Stop -----------------------------------------------------
    # Check to see if the timestamp of [U] Stop Time was placed at the same time as
    # the last [U] Start/Stop. Use Relative_hmsf to make sure it was placed at
    # exactly the same frame.
    
    # lubridate::seconds(tib_vid_clean$time_relative_hmsf[1]) != 
    #   lubridate::seconds(dtm_relative_hmsf_start)
    
    if (tib_vid_clean$time_relative_hmsf[1] != dtm_relative_hmsf_start) {
      
      message("",
              appendLF = TRUE)
      
      # The timestmap of [U] Start Time was NOT placed at the same time as the first
      # annotation code.
      stop("First code does not align with start time.")
      
    }
    
    if (dplyr::near((tib_vid_clean$time_relative_hmsf[nrow(tib_vid_clean)] +
                     tib_vid_clean$duration_sf[nrow(tib_vid_clean)]) %>% 
                    seconds(),
                    dtm_relative_hmsf_end %>% 
                    seconds(),
                    tol = .01) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      # The timestamp of [U] Stop Time was NOT placed at the same time as the last
      # [U] Start/Stop.
      stop("Stop time does not match last code timestamp + its duration.")
      
    }
    
    if (dplyr::near(difftime(time1 = dtm_vid_stop,
                             time2 = dtm_vid_start,
                             units = "secs"),
                    difftime(time1 = dtm_relative_hms_stop,
                             time2 = dtm_relative_hms_start,
                             units = "secs"),
                    tol = 5) == FALSE) {
      
      message("",
              appendLF = TRUE)
      
      t1 <- 
        difftime(time1 = dtm_vid_stop,
                 time2 = dtm_vid_start,
                 units = "secs")
      t2 <- 
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = dtm_relative_hms_start,
                 units = "secs")
      diff_abs_rel <- 
        t1 -
        t2
      
      stop(
        "Difference between absolute and relative times are not < 5 seconds.\n",
        "Difference = ", diff_abs_rel, " seconds" 
      )
      
    }
    
    # Shape #2 ----------------------------------------------------------------
    
    # Finish making Posture codes the same as Activity.
    tib_vid_clean$behavior <- 
      tib_vid_clean$behavior %>% 
      dplyr::recode(
        # posture
        "Uncoded - Dark/Obscured/OoF" = "[U] Dark/Obscured/OoF",
        "[P] Other"                           = "[P] Other - Posture",
        "[P} Lying"                           = "[P] Lying",
        "[P] Crouching / Kneeling / Squating" = "[P] Crouching/Kneeling/Squatting",
        "[M] Other"                           = "[M] Other - Movement",
        "[M] Crouching / Squating"            = "[M] Crouching/Squatting",
        # behavior
        "[Q] Cooking/Meal Preperation"  = "[Q] Cooking/Meal Preparation", # DOCOMP
        "[Q] Caring Grooming - Self"    = "[Q] Caring/Grooming - Self", # DOCOMP
        "[LQ] Caring Grooming - Self"   = "[LQ] Caring/Grooming - Self",
        "[LQ] Cooking/Meal Preperation" = "[LQ] Cooking/Meal Preparation",
        "[HQ] Caring Grooming - Self"   = "[HQ] Caring/Grooming - Self",
        "[HQ] Cooking/Meal Preperation" = "[HQ] Cooking/Meal Preparation"
      )
    
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    if (colnames(tib_vid_clean) %>% 
        str_detect(pattern = "comment",
                   negate = TRUE) %>% 
        all()) {
      
      tib_vid_clean$comment <- 
        NA
      
    }
    # DOCOMP specific...I think?...No its not. BUT I ALREADY DO THIS IN SHAPE #1
    # tib_vid_clean$behavior <- 
    #   tib_vid_clean$behavior %>% 
    #   dplyr::recode(
    #     "[Q] Caring Grooming - Self" = "[Q] Caring/Grooming - Self",
    #     "[LQ] Caring Grooming - Self" = "[LQ] Caring/Grooming - Self",
    #     "[HQ] Caring Grooming - Self" = "[HQ] Caring/Grooming - Self"
    #   )
    
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(tib_vid_clean)[colnames(tib_vid_clean) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(tib_vid_clean)) {
      
      tib_vid_clean <-
        tib_vid_clean %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              remove = TRUE,
              na.rm = TRUE)
      
    } else {
      
      tib_vid_clean$mod_2 <- 
        NA
      
    }
    
    # Remove category "[P/M/G/LQ]" from behavior, make values lowercase
    # and fill NA's.
    tib_vid_clean <- 
      tib_vid_clean %>% 
      mutate(behavior = str_remove(behavior,
                                   pattern = "\\[[A-Z]{1,2}\\] "),
             behavior = str_to_lower(behavior),
             mod_2    = str_to_lower(mod_2),
             mod_1    = str_to_lower(mod_1)) %>% 
      replace_na(replace = list(mod_1 = "dark/obscured/oof"))
    
    
    # Shape #3: Second By Second ----------------------------------------------
    message("SBS...",
            appendLF = FALSE)
    
    nrw_vid_clean <- 
      nrow(tib_vid_clean)
    int_duration <-  
      diff.POSIXt(tib_vid_clean$time_relative_hms,
                  units = "secs") %>% 
      as.vector()
    
    if (difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                 units = "secs") !=
        round(tib_vid_clean$duration_sf[nrow(tib_vid_clean)],
              digits = 0)) {
      
      diff_duration_sf <- 
        round(tib_vid_clean$duration_sf[nrow(tib_vid_clean)],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                 units = "secs")
      
      warning("File #", i, ": ", schema, " - ", fnm_obs, "\n",
              "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
              "Duration_sf is ", diff_duration_sf, " off.\n",
              "Using difference between the stop time and last code time.\n",
              call. = FALSE)
      
      # stop("The Duration_sf of the last code does not equal the difference between the stop time and last code time.")
      
    }
    
    int_duration <- 
      c(round(int_duration,
              digits = 0),
        round(difftime(time1 = dtm_relative_hms_stop,
                       time2 = tib_vid_clean$time_relative_hms[nrw_vid_clean],
                       units = "secs"),
              digits = 0))
    
    if (is.na(int_duration) %>% 
        any()) {
      
      message("",
              appendLF = TRUE)
      
      stop("Duration of a code is less than a second??? IDK man")
      
    }
    
    int_duration <- 
      as.integer(int_duration)
    
    sbs_int_events <- 
      int_duration %>% 
      seq_along() %>% 
      rep(times = int_duration)
    sbs_str_behavior <- 
      tib_vid_clean$behavior %>% 
      rep(times = int_duration)
    sbs_str_mod_1 <- 
      tib_vid_clean$mod_1 %>% 
      rep(times = int_duration)
    sbs_str_mod_2 <- 
      tib_vid_clean$mod_2 %>% 
      rep(times = int_duration)
    sbs_str_comment <- 
      tib_vid_clean$comment %>% 
      rep(times = int_duration)  
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    sbs_datetime <- 
      seq.POSIXt(from = dtm_vid_start,
                 by = 1,
                 length.out = nrw_vid_sbs)
    sbs_date <- 
      sbs_datetime %>% 
      lubridate::as_date()
    sbs_time <- 
      sbs_datetime %>% 
      format("%H:%M:%S")
    
    tib_vid_sbs <- 
      tibble(
        study      = study,
        subject    = subject,
        visit      = visit,
        datetime   = sbs_datetime,
        date       = sbs_date,
        time       = sbs_time,
        behavior   = sbs_str_behavior,
        mod_2      = sbs_str_mod_2,
        mod_1      = sbs_str_mod_1,
        comment    = sbs_str_comment,
        .rows      = nrw_vid_sbs
      )
    
    # Shape #3 ----------------------------------------------------------------
    message("Domain & Events...",
            appendLF = FALSE)
    
    if (schema == "Activity") {
      
      # Replace NA's with empty strings for unite function.
      tib_vid_sbs$mod_2 <- 
        tib_vid_sbs$mod_2 %>% 
        na_if("")
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        unite(col = behavior_activity,
              behavior, mod_2,
              sep = " ~ ",
              remove = FALSE,
              na.rm = TRUE) %>% 
        rename(environment      = mod_1,
               activity         = mod_2,
               comment_behavior = comment) %>% 
        mutate(
          # Fix spelling.
          environment = 
            dplyr::recode(
              environment,
              "organizational/civic/religiious" = "organizational/civic/religious"
            ),
          behavior_domain = get_domain_behavior(
            vec_behavior    = behavior,
            vec_environment = environment,
            key_domain      = key_behavior_domain)
        ) %>% 
        select(
          study:time,
          behavior, 
          activity,
          # behavior_activity,
          behavior_domain,
          environment,
          comment_behavior
        )
      
    } else if (schema == "Posture") {
      
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename(posture         = behavior,
               intensity       = mod_1,
               comment_posture = comment) %>% 
        mutate(
          posture_domain = get_domain_posture(
            vec_posture = posture,
            key_bucket  = key_posture_bucket,
            key_domain  = key_posture_domain
          ),
          # Change mod-vig to mvpa.
          intensity = dplyr::recode(intensity,
                                    "mod-vig" = "mvpa")
        ) %>% 
        select(
          study:time,
          posture, intensity, posture_domain,
          comment_posture
        )
      
    }
    
    # Project-specific --------------------------------------------------------
    message("Project Specific...",
            appendLF = FALSE)
    if (project == "FLAC - Aim 1") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Denver")
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "_VID_", str_to_upper(schema), "_", coder, ".csv"
        )
      
      if (schema == "Activity") {
        
        tib_vid_sbs$environment <- 
          "Non-Domestic"
        
      }
      
    } else if (project == "FLAC - Aim 2") {
      
      tib_vid_sbs$datetime <- 
        tib_vid_sbs$datetime %>% 
        lubridate::force_tz(tzone = "America/Chicago")
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
        )
      
    } else if (project == "DOCOMP") {
      
      sample_length <- 
        fnm_obs_split[4] %>% 
        str_remove(pattern = "sec")
      tib_vid_sbs <- 
        tib_vid_sbs %>% 
        rename_with(.fn = ~ paste0(.x, "_", sample_length),
                    .cols = 7:last_col())
      # rename_with(.fn = paste0, "_", sample_length,
      #             .cols = starts_with(regex("behavior|posture")):last_col())
      fnm_vid_shape <- 
        paste0(
          study, "_", subject, "v", visit, "_", schema, "_", sample_length, ".csv"
        )
      
    }
    
    fld_activity_shape <- 
      list.files(path = fdr_shape) %>% 
      str_subset(regex(pattern = "activity",
                       ignore_case = TRUE))
    fld_posture_shape <- 
      list.files(path = fdr_shape) %>% 
      str_subset(regex(pattern = "posture",
                       ignore_case = TRUE))
    
    switch(
      schema,
      "Activity" = {
        vroom_write(
          tib_vid_sbs,
          file = paste(fdr_shape,
                       fld_activity_shape,
                       fnm_vid_shape,
                       sep = "/"),
          delim = ","
        )
      },
      "Posture" = {
        vroom_write(
          tib_vid_sbs,
          file = paste(fdr_shape,
                       fld_posture_shape,
                       fnm_vid_shape,
                       sep = "/"),
          delim = ","
        )
      }
    )
    
    # vroom_write(
    #   tib_vid_sbs,
    #   path = paste(fdr_vid_shape,
    #                fnm_vid_shape,
    #                sep = "/"),
    #   delim = ","
    # )
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
}
shape_noldus_v10 <- function(project,
                             fdr_clean,
                             fdr_shape,
                             key_behavior_domain,
                             key_posture_bucket,
                             key_posture_domain) {
  # # CHANGES:
  
  # -Update to lastest naming schema
  # -Change read/write to data.table since it is faster
  # -Renamed sections.
  # -Cleaned up file path objects to now have fpt to be the full file path to 
  #  file.
  # -Add in duration column.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: get_domain_behavior
  # FUNCTION: get_domain_posture
  # ARG: project
  #      Self-explanatory
  # ARG: fdr_vid_clean
  #      File directory of clean files.
  # ARG: fdr_vid_shape
  #      File directory of shaped files.
  # ARG: key_behavior_domain
  #      For "key_domain" ARG of get_domain_behavior
  # ARG: key_posture_bucket
  #      For "key_bucket" ARG of get_domain_posture
  # ARG: key_posture_domain
  #      For "key_domain" ARG of get_domain_posture
  
  # # TESTING
  
  project <-
    "FLAC - Aim 1"
  fdr_cln <-
    "./3_data/1_cleaned/"
  fdr_shp <-
    "./3_data/2_shaped/"
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
  key_behavior_domain <-
    beh_domain_key_v2
  key_posture_bucket <-
    pos_bucket_key_v2
  key_posture_domain <-
    pos_domain_key_v2
  
  vct_fpt_cln <- 
    fs::dir_ls(
      path        = fdr_cln,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "noldus",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.csv")
  
  for (i in seq_along(vct_fpt_cln)) {
    
    fpt_cln <-
      vct_fpt_cln[i]
    
    fnm_cln <- 
      fpt_cln %>% 
      fs::path_file()
    
    message("Shaping file #", i , ": ", fnm_cln, "...",
            appendLF = TRUE)
    
    fli <- 
      fnm_cln %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      unlist() %>% 
      as.list() %>% 
      rlang::set_names(nm = c("study",
                              "subject",
                              "visit",
                              "file_source",
                              "schema",
                              "coder"))
    switch(
      fli$schema,
      "activity" = {
        fli$col_annotation <- "behavior"
        fli$col_mod_1 <- "activity"
        fli$col_mod_2 <- "environment"
      },
      "posture" = {
        fli$col_annotation <- "posture"
        fli$col_mod_1 <- "intensity"
      }
    )
    
    df_cln <- 
      data.table::fread(
        input = fpt_cln,
        sep = ",",
        header = TRUE
      )
    
    # Shape #1: Start & Stop --------------------------------------------------
    message("Start/Stop...",
            appendLF = FALSE)
    
    
    # Change start/stop to corresponding time zone at the end.
    fli$start <- suppressWarnings(
      df_raw$comment[df_raw$behavior == "[U] Start Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    fli$stop <- suppressWarnings(
      df_raw$comment[df_raw$behavior == "[U] Stop Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    
    dtm_relative_hms_stop <- 
      df_cln$time_relative_hms[df_cln$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    df_cln <- 
      df_cln %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      ) %>% 
      as.data.table()
    
    # Shape #2 ----------------------------------------------------------------
    
    # DOCOMP files should be the only ones that may not have the Comment column.
    # All other files should have it as it is used for getting vid start/stop time.
    if (colnames(df_cln) %>% 
        str_detect(pattern = "comment",
                   negate = TRUE) %>% 
        all()) {
      
      df_cln$comment <- 
        NA
      
    }
    
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(df_cln)[colnames(df_cln) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(df_cln)) {
      
      df_cln <-
        df_cln %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              sep = "",
              remove = TRUE,
              na.rm = TRUE)
      
    } else {
      
      df_cln$mod_2 <- 
        NA_character_
      
    }
    
    # Remove category "[P/M/G/LQ]" from behavior, make values lowercase
    # and fill NA's.
    df_cln <- 
      df_cln %>% 
      mutate(behavior = str_remove(behavior,
                                   pattern = "\\[[A-Z]{1,2}\\] "),
             behavior = str_to_lower(behavior),
             mod_2    = str_to_lower(mod_2),
             mod_1    = str_to_lower(mod_1)) %>% 
      replace_na(replace = list(mod_1 = "dark/obscured/oof")) %>% 
      as.data.table()
    
    # Shape #3: Second By Second ----------------------------------------------
    message("SBS...",
            appendLF = FALSE)
    
    nrw_cln <- 
      nrow(df_cln)
    wrn_duration_last <- 
      difftime(time1 = dtm_relative_hms_stop,
               time2 = df_cln$time_relative_hms[nrw_cln],
               units = "secs") !=
      round(df_cln$duration_sf[nrw_cln],
            digits = 0)
    
    if (wrn_duration_last) {
      
      diff_duration_sf <- 
        round(df_cln$duration_sf[nrw_cln],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_cln$time_relative_hms[nrw_cln],
                 units = "secs")
      
      warning(
        "File #", i, ": ", fli$schema, " - ", fnm_cln, "\n",
        "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
        "Duration_sf is ", diff_duration_sf, " off.\n",
        "Using difference between the stop time and last code time.\n",
        call. = FALSE
      )
      
    }
    
    vct_duration <- 
      c(diff.POSIXt(df_cln$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_cln$time_relative_hms[nrw_cln],
                 units = "secs")) %>% 
      as.integer()
    
    # difftime(
    #   time1 = df_cln$time_relative_hms[nrw_cln] + 
    #     round(df_cln$duration_sf[nrw_cln],
    #           digits = 0),
    #   time2 = df_cln$time_relative_hms[1],
    #   units = "secs"
    # )
    # data.table(
    #   round(df_cln$duration_sf,
    #         digits = 0),
    #   vct_duration
    # )
    # round(df_cln$duration_sf,
    #       digits = 0) %>% 
    #   sum()
    # sum(vct_duration)
    # vct_run <- 
    #   c(vct_duration[1],
    #     vct_duration[-1] - 1) %>% 
    #   as.integer()
    
    sbs_int_events <- 
      vct_duration %>% 
      seq_along() %>% 
      rep(times = vct_duration)
    sbs_str_behavior <- 
      df_cln$behavior %>% 
      rep(times = vct_duration)
    sbs_str_mod_1 <- 
      df_cln$mod_1 %>% 
      rep(times = vct_duration)
    sbs_str_mod_2 <- 
      df_cln$mod_2 %>% 
      rep(times = vct_duration)
    sbs_str_comment <- 
      df_cln$comment %>% 
      rep(times = vct_duration)  
    
    nrw_vid_sbs <- 
      length(sbs_int_events)
    
    sbs_datetime <- 
      seq.POSIXt(from = fli$start,
                 by = 1,
                 length.out = nrw_vid_sbs)
    # difftime(
    #   time1 = sbs_datetime[nrw_vid_sbs],
    #   time2 = sbs_datetime[1],
    #   units = "secs"
    # )
    sbs_date <- 
      sbs_datetime %>% 
      lubridate::as_date()
    sbs_time <- 
      sbs_datetime %>% 
      format("%H:%M:%S")
    
    df_shp <- 
      tibble(
        study      = fli$study,
        subject    = fli$subject,
        visit      = fli$visit,
        datetime   = sbs_datetime,
        date       = sbs_date,
        time       = sbs_time,
        behavior   = sbs_str_behavior,
        mod_2      = sbs_str_mod_2,
        mod_1      = sbs_str_mod_1,
        comment    = sbs_str_comment,
        .rows      = nrw_vid_sbs
      )
    
    # Shape #3 ----------------------------------------------------------------
    message("Domain & Events...",
            appendLF = FALSE)
    
    if (fli$schema == "activity") {
      
      # # Replace NA's with empty strings for unite function.
      # df_shp$mod_2 <- 
      #   df_shp$mod_2 %>% 
      #   na_if("")
      
      df_shp <- 
        df_shp %>% 
        rename(environment      = mod_1,
               activity         = mod_2,
               comment_behavior = comment) %>% 
        mutate(
          # Replace NA's with empty strings for unite function.
          environment = na_if(environment,
                              ""),
          # Fix spelling.
          environment = 
            dplyr::recode(
              environment,
              "organizational/civic/religiious" = "organizational/civic/religious"
            ),
          behavior_domain = get_domain_behavior(
            vec_behavior    = behavior,
            vec_environment = environment,
            key_domain      = key_behavior_domain)
        ) %>% 
        select(
          study:time,
          behavior, 
          activity,
          behavior_domain,
          environment,
          comment_behavior
        ) %>% 
        rename_with(.cols = !study:time,
                    .fn   = function(.x) paste(fli$file_source,
                                               .x,
                                               sep = "_")) %>% 
        as.data.table()
      
    } else if (fli$schema == "posture") {
      
      df_shp <- 
        df_shp %>% 
        rename(posture         = behavior,
               intensity       = mod_1,
               comment_posture = comment) %>% 
        mutate(
          posture_domain = get_domain_posture(
            vec_posture = posture,
            key_bucket  = key_posture_bucket,
            key_domain  = key_posture_domain
          ),
          # Change mod-vig to mvpa.
          intensity = dplyr::recode(intensity,
                                    "mod-vig" = "mvpa")
        ) %>% 
        select(
          study:time,
          posture, intensity, posture_domain,
          comment_posture
        ) %>% 
        rename_with(.cols = !study:time,
                    .fn   = function(.x) paste(fli$file_source,
                                               .x,
                                               sep = "_")) %>% 
        as.data.table()
      
    }
    
    # df_shp
    
    # Project-specific --------------------------------------------------------
    message("Project Specific...",
            appendLF = FALSE)
    
    if (project == "FLAC - Aim 1") {
      
      df_shp$datetime <- 
        df_shp$datetime %>% 
        lubridate::force_tz(tzone = "America/Denver")
      # fnm_vid_shape <- 
      #   paste0(
      #     study, "_", subject, "_VID_", str_to_upper(schema), "_", coder, ".csv"
      #   )
      
      if (fli$schema == "Activity") {
        
        df_shp$environment <- 
          "non-domestic"
        
      }
      
    } else if (project == "FLAC - Aim 2") {
      
      df_shp$datetime <- 
        df_shp$datetime %>% 
        lubridate::force_tz(tzone = "America/Chicago")
      # fnm_vid_shape <- 
      #   paste0(
      #     study, "_", subject, "V", visit, "_", schema, "_", coder, ".csv"
      #   )
      
    } else if (project == "DOCOMP") {
      
      sample_length <- 
        fnm_obs_split[4] %>% 
        str_remove(pattern = "sec")
      df_shp <- 
        df_shp %>% 
        rename_with(.fn = ~ paste0(.x, "_", sample_length),
                    .cols = 7:last_col())
      # rename_with(.fn = paste0, "_", sample_length,
      #             .cols = starts_with(regex("behavior|posture")):last_col())
      # fnm_vid_shape <- 
      #   paste0(
      #     study, "_", subject, "v", visit, "_", schema, "_", sample_length, ".csv"
      #   )
      
    }
    
    fnm_shp <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        fli$schema,
        fli$coder,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    
    fpt_shp <- 
      fs::path(
        fdr_shp,
        dplyr::case_when(fli$schema == "activity" ~ "noldus_activity",
                         fli$schema == "posture" ~ "noldus_posture"),
        fnm_shp
      )    
    data.table::fwrite(
      df_shp,
      file = fpt_shp,
      sep = ","
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpt_shp,
                              ext = "feather")
    )
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
}
shape_noldus_v11 <- function(fdr_read,
                             fdr_write,
                             fdr_project = NULL,
                             vct_subject_filter = NULL) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -Make read and write arguments universal.
  # -Leave the get_domain functions out until project-specific functions.
  # -Move DOCOMP comment check to clean function.
  # -Fix sbs code to make sure durations are correct. Make sbs by treating first
  #  row ONLY as an anchor.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -NULL
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of clean files.
  # ARG: fdr_write
  #      File directory of shaped files.
  # ARG: fdr_project
  #      Project that is a fork of the main study. If this is supplied, it needs
  #      to be to the file directory. Sub-directories under the data directory
  #      need to at least contain one of the following words:
  #      raw, cleaned, shaped, merged, processed.
  # ARG: vct_subject_filter
  #      Vector of subjects to filter the fpt_read 
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  fli_flac_aim <- 
    fdr_read %>% 
    fs::path_dir() %>% 
    str_extract(pattern = "AIM\\d{1}")
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write_nld_act <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("noldus_activity",
                                 ignore_case = TRUE))
    )
  chk_fsb_write_nld_pos <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("noldus_posture",
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write_nld_act) {
    # Create a directory under fdr_write that contains "noldus_activity"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "noldus_activity" found in WRITE directory.',
          "i" = 'Creating sub directory "NOLDUS_ACTIVITY" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "NOLDUS_ACTIVITY"))
  } 
  if (chk_fsb_write_nld_pos) {
    # Create a directory under fdr_write that contains "noldus_posture"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "noldus_posture" found in WRITE directory.',
          "i" = 'Creating sub directory "NOLDUS_POSTURE" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "NOLDUS_POSTURE"))
  }
  
  vct_fpt_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "noldus",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.csv")
  
  cli::cli_alert_info("Shaping Oxford annotation files.")
  counter <- 0
  
  for (i in seq_along(vct_fpt_read)) {
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fpt_read <-
      vct_fpt_read[i]
    
    fnm_read <- 
      fpt_read %>% 
      fs::path_file()
    
    cli::cli_progress_step(
      msg = "Shaping file #{counter + 1}: {fnm_read}...",
      msg_done = "Shaping file #{counter}: {fnm_read}...DONE",
      msg_failed = "Shaping file #{counter}: {fnm_read}...WARNING"
    )
    # cli::cli_progress_message(
    #   "Shaping file #{counter + 1}: {fnm_read}"
    # )
    # message("Shaping file #", i , ": ", fnm_read, "...",
    #         appendLF = TRUE)
    
    fli <- 
      fnm_read %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop() %>% 
      vec_chop() %>% 
      rlang::set_names(nm = c("study",
                              "subject",
                              "visit",
                              "file_source",
                              "schema",
                              "coder_intials"))
    fli$flac_aim <- fli_flac_aim
    switch(
      fli$schema,
      "ACTIVITY" = {
        fli$col_duration <- "duration_behavior"
        fli$col_annotation <- "behavior"
        fli$col_mod_1 <- "activity"
        fli$col_mod_2 <- "environment"
        fli$col_comment <- "comment_behavior"
      },
      "POSTURE" = {
        fli$col_duration <- "duration_posture"
        fli$col_annotation <- "posture"
        fli$col_mod_1 <- "intensity"
        fli$col_comment <- "comment_posture"
      }
    )
    
    df_cln <- 
      data.table::fread(
        input = fpt_read,
        sep = ",",
        header = TRUE
      )
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                    SHAPE #1: START & STOP                  ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # message("Start/Stop...",
    #         appendLF = FALSE)
    # Change start/stop to corresponding time zone at the end.
    fli$start <- suppressWarnings(
      df_cln$comment[df_cln$behavior == "[U] Start Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    fli$stop <- suppressWarnings(
      df_cln$comment[df_cln$behavior == "[U] Stop Time"] %>% 
        lubridate::mdy_hms(tz = "UTC")
    )
    
    dtm_relative_hms_stop <- 
      df_cln$time_relative_hms[df_cln$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    df_cln <- 
      df_cln %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      ) %>% 
      as.data.table()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                SHAPE #2: VARIABLE CONSISTENCY              ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(df_cln)[colnames(df_cln) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(df_cln)) {
      
      df_cln <-
        df_cln %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              sep = "",
              remove = TRUE,
              na.rm = TRUE) %>% 
        mutate(mod_2 = na_if(mod_2,
                             "")) %>% 
        as.data.table()
      
    } else {
      
      df_cln$mod_2 <- 
        NA_character_
      
    }
    
    # Remove category "[P/M/G/LQ]" from behavior, make values lowercase,
    # fill NA's and place NA's.
    df_cln <- 
      df_cln %>% 
      mutate(
        behavior = 
          behavior %>% 
          str_remove(pattern = "\\[[A-Z]{1,2}\\] ") %>% 
          str_to_lower(),
        mod_2    = str_to_lower(mod_2),
        mod_1    = str_to_lower(mod_1),
        comment  = na_if(comment,
                         "")
      ) %>% 
      replace_na(replace = list(mod_1 = "dark/obscured/oof")) %>% 
      as.data.table()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                  SHAPE #3: SECOND-BY-SECOND                ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    # message("SBS...",
    #         appendLF = FALSE)
    nrw_cln <- 
      nrow(df_cln)
    wrn_duration_last <- 
      difftime(time1 = dtm_relative_hms_stop,
               time2 = df_cln$time_relative_hms[nrw_cln],
               units = "secs") !=
      round(df_cln$duration_sf[nrw_cln],
            digits = 0)
    
    if (wrn_duration_last) {
      
      message("\n",
              appendLF = TRUE)
      # LEFT OFF HERE -----------------------------------------------------------
      
      cli_progress_update("Shaping file #{counter + 1}: {fnm_read}...WARNING")
      
      diff_duration_sf <- 
        round(df_cln$duration_sf[nrw_cln],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_cln$time_relative_hms[nrw_cln],
                 units = "secs")
      
      warning(
        "File #", i, ": ", fli$schema, " - ", fnm_read, "\n",
        "Duration_sf of the last code does not equal the difference between the stop time and last code time.\n",
        "Duration_sf is ", diff_duration_sf, " off.\n",
        "Using difference between the stop time and last code time.\n",
        call. = FALSE
      )
      
    }
    
    vct_duration <- 
      c(diff.POSIXt(df_cln$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_cln$time_relative_hms[nrw_cln],
                 units = "secs"))
    
    # This code treats the time1 value as what occurred between 
    # time1 - 1 second UP TO time1!! Do this for every value EXCEPT the
    # start time. We leave the start time in the data frame only as an anchor.
    vct_run <-
      c(vct_duration[1] + 1,
        vct_duration[-1]) %>%
      as.integer()
    vct_run_seq <- 
      vct_run %>% 
      seq_along() %>% 
      rep(times = vct_run)
    df_shp <- 
      tibble(
        study    = fli$study,
        subject  = fli$subject,
        visit    = fli$visit,
        datetime = seq.POSIXt(from = fli$start,
                              by = 1,
                              length.out = length(vct_run_seq)),
        date     = lubridate::as_date(datetime),
        time     = format(datetime,
                          "%H:%M:%S"),
        map_dfc(
          .x = df_cln[, .(duration = vct_duration,
                          behavior,
                          # run = seq_along(vct_run),
                          mod_1,
                          mod_2,
                          comment)],
          .f = ~rep(.x,
                    times = vct_run)
        )
      ) %>% 
      as.data.table()
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                  SHAPE #4: SCHEMA SPECIFIC                ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    if (fli$schema == "ACTIVITY") {
      
      # # Replace NA's with empty strings for unite function.
      # df_shp$mod_2 <- 
      #   df_shp$mod_2 %>% 
      #   na_if("")
      
      df_shp <- 
        df_shp %>% 
        rename("{fli$col_duration}"   := duration,
               "{fli$col_annotation}" := behavior,
               "{fli$col_mod_1}"      := mod_1,
               "{fli$col_mod_2}"      := mod_2,
               "{fli$col_comment}"    := comment) %>% 
        mutate(
          # Fix spelling.
          environment = 
            dplyr::recode(
              environment,
              "organizational/civic/religiious" = "organizational/civic/religious"
            )
          # behavior_domain = get_domain_behavior(
          #   vec_behavior    = behavior,
          #   vec_environment = environment,
          #   key_domain      = key_behavior_domain)
        ) %>% 
        rename_with(
          .cols = !study:time,
          .fn = ~paste(.x,
                       str_to_lower(fli$file_source),
                       sep = "_")
        ) %>% 
        as.data.table()
      
    } else if (fli$schema == "POSTURE") {
      
      df_shp <- 
        df_shp %>% 
        rename("{fli$col_duration}"   := duration,
               "{fli$col_annotation}" := behavior,
               "{fli$col_mod_1}"      := mod_1,
               "{fli$col_comment}"    := comment) %>% 
        mutate(
          # posture_domain = get_domain_posture(
          #   vec_posture = posture,
          #   key_bucket  = key_posture_bucket,
          #   key_domain  = key_posture_domain
          # ),
          # Change mod-vig to mvpa.
          intensity = dplyr::recode(intensity,
                                    "mod-vig" = "mvpa"),
          mod_2 = NULL
        ) %>% 
        rename_with(
          .cols = !study:time,
          .fn = ~paste(.x,
                       str_to_lower(fli$file_source),
                       sep = "_")
        ) %>% 
        as.data.table()
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                 SHAPE #5: FLAC AIM SPECIFIC               ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    if (fli$flac_aim == "AIM1") {
      
      df_shp$datetime <- 
        df_shp$datetime %>% 
        lubridate::force_tz(tzone = "America/Denver")
      
      if (fli$schema == "Activity") {
        
        df_shp$environment <- 
          "non-domestic"
        
      }
      
    } else if (fli$flac_aim == "AIM2") {
      
      df_shp$datetime <- 
        df_shp$datetime %>% 
        lubridate::force_tz(tzone = "America/Chicago")
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fnm_write <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        fli$schema,
        fli$coder_intials,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    fpt_write <- 
      fs::path(
        fdr_write,
        dplyr::case_when(
          fli$schema == "ACTIVITY" ~ 
            str_subset(vct_fsb_write,
                       pattern = regex("noldus_activity",
                                       ignore_case = TRUE)),
          fli$schema == "POSTURE" ~ 
            str_subset(vct_fsb_write,
                       pattern = regex("noldus_posture",
                                       ignore_case = TRUE))
        ),
        fnm_write
      )
    data.table::fwrite(
      df_shp,
      file = fpt_write,
      sep = ","
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpt_write,
                              ext = "feather")
    )
    
    counter <- 
      counter + 1
    # message("DONE\n",
    #         appendLF = TRUE)
    # cli::cli_progress_step(
    #   msg = "DONE",
    #   msg_done = "SUCCESS. Cleaned {counter} files out of {length(vct_fpt_read)} files."
    # )
    
  }
  
  cli::cli_progress_step(
    msg = "DONE",
    msg_done = "SUCCESS. Cleaned {counter} files out of {length(vct_fpt_read)} files."
  )
  
}
shape_noldus_v12 <- function(fdr_read,
                             fdr_write,
                             fdr_project = NULL,
                             fld_act = "NOLDUS_ACTIVITY",
                             fld_pos = "NOLDUS_POSTURE",
                             filter_sub = NULL,
                             project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Include code for fdr_project
  # -   Include code for filter_sub
  # -   Include code for exporting start and stop times as we are using noldus
  #     as the criterion.
  # -   Make subject and visit integers.
  # -   Change "fli" to "df_info" to make it easier to access.
  # -   Incorporate get_fpa_read_noldus as it is the same across all functions.
  # -   Incorporate initiate_wrangle as it is the same across all wrangle functions.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read_noldus
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of clean noldus files.
  # ARG: fdr_write
  #      File directory of shaped noldus files.
  # ARG: fdr_project
  #      File directory to where all the shaped data resides in the project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  #      Unless project only is TRUE.
  # ARG: fld_act
  #      Folder name of shaped noldus activity files.
  # ARG: fld_pos
  #      Folder name of shaped noldus posture files.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: project_only
  #      Should shaped files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "2_AIM1_CLEANED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_project <-
  #   NULL
  # fld_act <-
  #   "NOLDUS_ACTIVITY"
  # fld_pos <-
  #   "NOLDUS_POSTURE"
  # filter_sub <-
  #   NULL
  # project_only <- FALSE
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   project_only = project_only,
                   type         = "Shap",
                   file_source  = "NOLDUS")
  vct_fpa_read <- 
    get_fpa_read_noldus(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_activity = fld_act,
      name_posture  = fld_pos,
      name_merged   = NULL,
      filter_sub    = filter_sub
    )
  lst_start_stop <- 
    list()
  
  for (i in cli_progress_along(vct_fpa_read,
                               format = progress_format,
                               clear = FALSE)) {
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fpa_read <-
      vct_fpa_read[i]
    
    fnm_read <- 
      fpa_read %>% 
      fs::path_file()
    
    df_info <- 
      fnm_read %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop() %>% 
      vec_chop() %>%
      as.data.table() %>% 
      rlang::set_names(nm = c("study",
                              "subject",
                              "visit",
                              "file_source",
                              "schema",
                              "coder_initials")) %>% 
      mutate(subject = as.integer(subject),
             visit   = as.integer(visit),
             flac_aim = info_flac_aim,
             time_zone = fcase(info_flac_aim == "AIM1", "America/Denver",
                               info_flac_aim == "AIM2", "America/Chicago")) %>% 
      as.data.table()
    switch(
      df_info$schema,
      "ACTIVITY" = {
        df_info$col_duration <- "duration_behavior"
        df_info$col_annotation <- "behavior"
        df_info$col_mod_1 <- "environment"
        df_info$col_mod_2 <- "activity"
        df_info$col_comment <- "comment_behavior"
      },
      "POSTURE" = {
        df_info$col_duration <- "duration_posture"
        df_info$col_annotation <- "posture"
        df_info$col_mod_1 <- "intensity"
        df_info$col_comment <- "comment_posture"
      }
    )
    
    df_cln <- 
      data.table::fread(
        input = fpa_read,
        sep = ",",
        header = TRUE
      )
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                    SHAPE #1: START & STOP                  ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    df_info$start <- suppressWarnings(
      df_cln$comment[df_cln$behavior == "[U] Start Time"] %>% 
        lubridate::mdy_hms(tz = df_info$time_zone)
    )
    df_info$stop <- suppressWarnings(
      df_cln$comment[df_cln$behavior == "[U] Stop Time"] %>% 
        lubridate::mdy_hms(tz = df_info$time_zone)
    )
    
    dtm_relative_hms_stop <- 
      df_cln$time_relative_hms[df_cln$behavior == "[U] Stop Time"]
    
    # Remove "[U] Start/Stop" codes and State Points.
    df_cln <- 
      df_cln %>% 
      filter(
        !(behavior == "[U] Start/Stop" |
            behavior == "[U] Start Time" |
            behavior == "[U] Stop Time")
      ) %>% 
      as.data.table()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                SHAPE #2: VARIABLE CONSISTENCY              ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # So the unite function does not combine the environment column when cleaning
    # an activity file.
    colnames(df_cln)[colnames(df_cln) == "modifier_1"] <- 
      "mod_1"
    
    if ("modifier_2" %in% colnames(df_cln)) {
      
      df_cln <-
        df_cln %>%
        unite(col = "mod_2",
              contains("modifier",
                       ignore.case = FALSE,
                       vars = NULL),
              sep = "",
              remove = TRUE,
              na.rm = TRUE) %>% 
        mutate(mod_2 = na_if(mod_2,
                             "")) %>% 
        as.data.table()
      
    } else {
      
      df_cln$mod_2 <- 
        NA_character_
      
    }
    
    # Remove category "[P/M/G/LQ]" from behavior, make values lowercase,
    # fill NA's and place NA's.
    df_cln <- 
      df_cln %>% 
      mutate(
        behavior = 
          behavior %>% 
          str_remove(pattern = "\\[[A-Z]{1,2}\\] ") %>% 
          str_to_lower(),
        mod_2    = str_to_lower(mod_2),
        mod_1    = str_to_lower(mod_1),
        comment  = na_if(comment,
                         "")
      ) %>% 
      replace_na(replace = list(mod_1 = "dark/obscured/oof")) %>% 
      as.data.table()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                  SHAPE #3: SECOND-BY-SECOND                ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    nrw_cln <- 
      nrow(df_cln)
    wrn_duration_last <- 
      difftime(time1 = dtm_relative_hms_stop,
               time2 = df_cln$time_relative_hms[nrw_cln],
               units = "secs") !=
      round(df_cln$duration_sf[nrw_cln],
            digits = 0)
    
    if (wrn_duration_last) {
      diff_duration_sf <- 
        round(df_cln$duration_sf[nrw_cln],
              digits = 0) -
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_cln$time_relative_hms[nrw_cln],
                 units = "secs")
      cli_warn(c(
        "File #{i}: {fnm_read}",
        "!" = "Duration_sf of the last code does not equal the difference between the stop time and last code time.",
        "!" = "Duration_sf is {diff_duration_sf} off.",
        "i" = "Using difference between the stop time and last code time."
      ))
    }
    
    vct_duration <- 
      c(diff.POSIXt(df_cln$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = df_cln$time_relative_hms[nrw_cln],
                 units = "secs"))
    
    # This code treats the time1 value as what occurred between 
    # time1 - 1 second UP TO time1!! Do this for every value EXCEPT the
    # start time. We leave the start time in the data frame only as an anchor.
    vct_run <-
      c(vct_duration[1] + 1,
        vct_duration[-1]) %>%
      as.integer()
    vct_run_seq <- 
      vct_run %>% 
      seq_along() %>% 
      rep(times = vct_run)
    df_shp <- 
      tibble(
        study    = df_info$study,
        subject  = df_info$subject,
        visit    = df_info$visit,
        datetime = seq.POSIXt(from = df_info$start,
                              by = 1,
                              length.out = length(vct_run_seq)),
        date     = lubridate::as_date(datetime),
        time     = format(datetime,
                          "%H:%M:%S"),
        map_dfc(
          .x = df_cln[, .(duration = vct_duration,
                          behavior,
                          mod_1,
                          mod_2,
                          comment)],
          .f = ~rep(.x,
                    times = vct_run)
        )
      ) %>% 
      as.data.table()
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                  SHAPE #4: SCHEMA SPECIFIC                ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    if (df_info$schema == "ACTIVITY") {
      
      df_shp <- 
        df_shp %>% 
        rename("{df_info$col_duration}"   := duration,
               "{df_info$col_annotation}" := behavior,
               "{df_info$col_mod_1}"      := mod_1,
               "{df_info$col_mod_2}"      := mod_2,
               "{df_info$col_comment}"    := comment) %>% 
        mutate(
          # Fix spelling.
          environment = 
            dplyr::recode(
              environment,
              "organizational/civic/religiious" = "organizational/civic/religious"
            )
        ) %>% 
        rename_with(
          .cols = !study:time,
          .fn = ~paste(.x,
                       str_to_lower(df_info$file_source),
                       sep = "_")
        ) %>% 
        as.data.table()
      
    } else if (df_info$schema == "POSTURE") {
      
      df_shp <- 
        df_shp %>% 
        rename("{df_info$col_duration}"   := duration,
               "{df_info$col_annotation}" := behavior,
               "{df_info$col_mod_1}"      := mod_1,
               "{df_info$col_comment}"    := comment) %>% 
        mutate(
          # Change mod-vig to mvpa.
          intensity = dplyr::recode(intensity,
                                    "mod-vig" = "mvpa"),
          mod_2 = NULL
        ) %>% 
        rename_with(
          .cols = !study:time,
          .fn = ~paste(.x,
                       str_to_lower(df_info$file_source),
                       sep = "_")
        ) %>% 
        as.data.table()
      
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                 SHAPE #5: FLAC AIM SPECIFIC               ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    if (df_info$flac_aim == "AIM1") {
      
      # FOR SOME REASON IT IS ALWAYS ONE HOUR AHEAD WHEN CHANGING TO UTC.
      df_shp$datetime <-
        df_shp$datetime - 3600
      lst_start_stop[[i]] <- 
        df_info %>% 
        select(study, subject, visit, start, stop) %>% 
        as.data.table()
      if (df_info$schema == "Activity") {
        
        df_shp$environment <- 
          "non-domestic"
        
      }
    } else if (df_info$flac_aim == "AIM2") {
      lst_start_stop[[i]] <-
        df_info %>%
        select(study, subject, visit, start, stop) %>%
        as.data.table()
    }
    
    df_shp$datetime <- 
      with_tz(df_shp$datetime,
              tzone = "UTC")
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fnm_write <- 
      df_info %>% 
      unite(col = "file_name",
            study, subject, visit, file_source, schema, coder_initials) %>% 
      pull(file_name) %>% 
      fs::path_ext_set(ext = "csv")
    
    if (project_only) {
      fpa_project <- 
        fs::path(
          dplyr::case_when(
            df_info$schema == "ACTIVITY" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_act,
                                         ignore_case = TRUE)),
            df_info$schema == "POSTURE" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_pos,
                                         ignore_case = TRUE))
          ),
          fnm_write
        )
      data.table::fwrite(
        df_shp,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
      arrow::write_feather(
        df_shp,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
      cnt <-
        cnt + 1
      next()
    }
    
    fpa_write <- 
      fs::path(
        dplyr::case_when(
          df_info$schema == "ACTIVITY" ~ dir_ls(fdr_write,
                                                type = "directory",
                                                regexp = fld_act),
          df_info$schema == "POSTURE" ~ dir_ls(fdr_write,
                                               type = "directory",
                                               regexp = fld_pos)
        ),
        fnm_write
      )
    data.table::fwrite(
      df_shp,
      file = fpa_write,
      sep = ",",
      showProgress = FALSE
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpa_write,
                              ext = "feather")
    )
    
    if(!is_empty(fdr_project)) {
      fpa_project <- 
        fs::path(
          dplyr::case_when(
            df_info$schema == "ACTIVITY" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_act,
                                         ignore_case = TRUE)),
            df_info$schema == "POSTURE" ~ 
              str_subset(dir_ls(fdr_project),
                         pattern = regex(fld_pos,
                                         ignore_case = TRUE))
          ),
          fnm_write
        )
      data.table::fwrite(
        df_shp,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
      arrow::write_feather(
        df_shp,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
    }
    
    cnt <- 
      cnt + 1
    
  }
  
  cli_progress_done()
  df_start_stop <- 
    bind_rows(lst_start_stop)
  
  if (project_only) {
    fwrite(
      df_start_stop,
      file = path(fdr_project,
                  "df_start_stop.csv"),
      sep = ",",
      showProgress = FALSE
    )
    arrow::write_feather(
      df_start_stop,
      sink = path(fdr_project,
                  "df_start_stop.feather")
    )
    return()
  }
  fwrite(
    df_start_stop,
    file = path(fdr_write,
                "df_start_stop.csv"),
    sep = ",",
    showProgress = FALSE
  )
  arrow::write_feather(
    df_start_stop,
    sink = path(fdr_write,
                "df_start_stop.feather")
  )
  if (!is_empty(fdr_project)) {
    fwrite(
      df_start_stop,
      file = path(fdr_project,
                  "df_start_stop.csv"),
      sep = ",",
      showProgress = FALSE
    )
    arrow::write_feather(
      df_start_stop,
      sink = path(fdr_project,
                  "df_start_stop.feather")
    )
  }
  
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
shape_oxford_v5 <- function(type,
                            fpa_img_raw,
                            fpa_img_clean,
                            fdr_img_raw,
                            tib_cor_time) {
  
  # OLD: NEWEST VERSION IN:
  # "S:/_R_CHS_Research/PAHRL/Student Access/0_Students/MARTINEZ/2_Conferences/2022_SURF"
  
  # # CHANGES:
  
  # -Change name of function from "clean_annotation_files"  to "shape_oxford"
  # -"clean_annotation_files is v4 from PPAQ.
  # -Bring out beginning of v4 into clean_oxford_v1
  # -Have it work for act and pos files in one folder and in separate folders.
  # -Change ARG "fpa_img_raw" to "fdr_img_raw"
  # -Change ARG "fpa_img_clean" to "fdr_img_clean"  
  
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NA
  
  # ARG: fdr_timestamps
  #      File directory of timestamp file.
  # ARG: fnm_timestamps
  #      File name of timestamp file.
  # ARG: tib_timestamps
  #      Tibble from read_img_timestamps_v5
  
  # # TESTING
  
  # type <- "POS"
  fdr_img_raw <- 
    "./3_data/0_raw"
  fdr_img_clean <- 
    "./3_data/1_cleaned"
  fdr_img_shp <- 
    "./3_data/2_shaped"
  tib_timestamps <- 
    tib_img_stamps
  key_coder_initials <- 
    c("bayer"        = "AB",
      "smith"        = "AS",
      "peraltawerns" = "AP",
      "miller"       = "EM",
      "martinez"     = "JM",
      "chang"        = "LC",
      "almanza"      = "MA")
  vct_coder_initials <- 
    key_coder_initials %>% 
    vec_set_names(names = NULL)
  # vct_coder_initials <- 
  #   c("AB",
  #     "AS",
  #     "AP",
  #     "EM",
  #     "JM",
  #     "LC",
  #     "MA")
  # coder_names <- 
  #   fls_img_raw %>% 
  #   str_extract(pattern = "([^_]*)$") %>% 
  #   str_remove(pattern = ".csv") %>% 
  #   str_to_lower() %>% 
  #   unique()
  
  processed_files <- 0
  
  fld_fls_img_raw <- 
    list.files(path = fdr_img_raw,
               recursive = TRUE,
               pattern = ".csv") %>% 
    str_subset(pattern = regex("oxford",
                               ignore_case = TRUE))
  
  # In the scenario that activity and posture files are kept in separate folders.
  fld_act_raw <- 
    fld_fls_img_raw %>% 
    str_subset(pattern = regex("activity",
                               ignore_case = TRUE)) %>%
    str_extract(pattern = "(?:(?!/).)*") %>% 
    unique()
  fld_pos_raw <- 
    fld_fls_img_raw %>% 
    str_subset(pattern = regex("posture",
                               ignore_case = TRUE)) %>%
    str_extract(pattern = "(?:(?!/).)*") %>% 
    unique()
  fls_img_raw <- 
    fld_fls_img_raw %>% 
    str_extract(pattern = "([^/]*)$")
  
  for (i in seq_along(fls_img_raw)) {
    
    fnm_img_raw <-
      fls_img_raw[i]
    
    fnm_obs <- 
      fnm_img_raw %>%
      str_extract(pattern = "(?:(?!.csv).)*") %>%  # Capture everything before first.
      str_remove(pattern = regex("_activity|_posture",
                                 ignore_case = TRUE))
    fnm_obs_split <- 
      fnm_img_raw %>%
      str_extract(pattern = "(?:(?!.csv).)*") %>%  # Capture everything before first.
      str_remove(pattern = regex("_activity|_posture",
                                 ignore_case = TRUE)) %>% 
      str_split(pattern = "_") %>% 
      unlist()
    schema <- 
      fnm_img_raw %>% 
      str_extract(pattern = regex("activity|posture",
                                  ignore_case = TRUE)) %>% 
      str_to_title()
    
    message("rawing file #", i , ": ", schema, " - ", fnm_obs, "...",
            appendLF = TRUE)
    
    study <- 
      fnm_obs_split[1]
    sub_vis <- 
      fnm_obs_split[2] %>% 
      str_to_lower()
    coder <- 
      fnm_obs_split[3] %>% 
      str_to_lower()
    subject <- 
      sub_vis %>% 
      str_extract(pattern = "(?:(?!v|V).)*")
    visit <- 
      sub_vis %>% 
      str_extract(pattern = "([^v|V]*)$")
    coder_initals <- 
      coder %>% 
      recode(!!!key_coder_initials)
    
    # Check #1 ----------------------------------------------------------------
    chk_study <- 
      study != "FLAC"
    chk_subject <- 
      subject %>% 
      str_detect(pattern = "\\d{4}",
                 negate = TRUE)
    chk_visit <- 
      visit %>% 
      str_detect(pattern = "\\d{1}",
                 negate = TRUE)
    chk_coder <- 
      !(coder_initals %in% vct_coder_initials)
    
    if (any(chk_study, chk_subject,
            chk_visit, chk_coder)) {
      
      stop("iabfianfilnawelfinawieubf") # WARNING 1
      
    }
    
    switch(
      schema,
      "Activity" = {
        tib_img_raw <- suppressMessages(
          paste(fdr_img_raw,
                fld_act_raw,
                fnm_img_raw,
                sep = "/") %>% 
            vroom(delim = ",",
                  progress = FALSE)
        )
      },
      "Posture" = {
        tib_img_raw <- suppressMessages(
          paste(fdr_img_raw,
                fld_pos_raw,
                fnm_img_raw,
                sep = "/") %>% 
            vroom(delim = ",",
                  progress = FALSE)
        )
      }
    )
    
    
    # Shape #1 ----------------------------------------------------------------
    # Consistency: make all lowercase and _ seperator.
    colnames(tib_img_raw) <-
      colnames(tib_img_raw) %>%
      str_to_lower() %>%
      str_replace(pattern = "time",
                  replacement = "_time")
    
    # Oxford "FLAC_DOMAIN_ACTIVITY_NIH_R01CA215318_08" schema includes a "category".
    # variable that previous versions do not have. This was used in PPAQ but
    # there is no plan to use them in any FLAC aim...yet. Regardless, a phenomena
    # that occurs with Version 8 of the schema is sometimes the category will be
    # collapsed under the annotation column. Need to separate it out if it is.
    
    if (ncol(tib_img_raw) == 4) {
      
      # For activity files coded with Versions before 8 and if an activity file
      # was not opened beforehand. Posture files will also be changed but its ok
      # ...for now.
      tib_img_raw <- 
        tib_img_raw %>%
        tidyr::separate(col = annotation,
                        into = c("annotation",
                                 "category"),
                        remove = FALSE,
                        sep = ",",
                        fill = "right") %>% 
        mutate(category = as.integer(category))
      
    } else {
      
      # For activity files coded with Version 8 and opened in excel at all.
      tib_img_raw <- 
        tib_img_raw %>% 
        mutate(category = as.integer(`...5`),
               .keep = "unused")
      
    }
    
    if (study == "FLAC") {
      
      tib_img_raw <- 
        tib_img_raw %>% 
        select(!category)
      
    }
    
    
    # Check #2 ----------------------------------------------------------------
    ## CHECK: If annotation has the uncoded annotation, make sure it is ONLY the
    ##        first and/or last annotation. There shouldn't be "undefined" anywhere
    ##        else.
    
    ind_undefined <- 
      which(tib_img_raw$annotation == "undefined")
    
    chk_undefined <- 
      (!ind_undefined %in% c(1L, nrow(tib_img_raw))) %>% 
      any()
    
    if (chk_undefined) {
      
      message("\n",
              appendLF = TRUE)
      
      lst_ind_mistake <- 
        ind_undefined[!ind_undefined %in% c(1L, nrow(tib_img_raw))]
      
      fdr_img_set <- 
        "S:/_V_PAHRL/FLAC/OxfordImageBrowser-win32-x64/2_Browser Images to Annotate"
      # "./OxfordImageBrowser/2_Browser Images to Annotate"
      stu_sub_vis <- 
        paste(str_to_lower(study),
              sub_vis,
              sep = "_")
      dtm_img_og <- 
        paste(fdr_img_set,
              stu_sub_vis,
              sep = "/") %>% 
        list.files(pattern = "JPG") %>% 
        str_sub(start = 18,
                end = 32) %>% 
        lubridate::ymd_hms(tz = "America/Chicago")
      dtm_end_raw_strped <- 
        tib_img_raw$end_time %>% 
        lubridate::parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                                   tz     = "UTC")
      dtm_start_raw_strped <- 
        tib_img_raw$start_time %>% 
        lubridate::parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                                   tz     = "UTC")
      
      for (ii in seq_along(lst_ind_mistake)) {
        
        ind_mistake <- 
          lst_ind_mistake[ii]
        
        dtm_mis_start <- 
          dtm_start_raw_strped[ind_mistake] %>% 
          with_tz(tzone = "America/Chicago")
        ind_mis_start <- 
          which(
            dtm_img_og %in% dtm_mis_start
          )
        ind_mis_start <- 
          ind_mis_start[length(ind_mis_start)] # I can't remember why I do this. Maybe cuz there are sometimes imgs with same date time???
        dtm_mis_start <- 
          dtm_mis_start %>% 
          format("%I:%M:%S %p")
        
        dtm_mis_end <- 
          dtm_end_raw_strped[ind_mistake] %>% 
          with_tz(tzone = "America/Chicago")
        ind_mis_end <- 
          which(
            dtm_img_og %in% dtm_mis_end
          )
        ind_mis_end <- 
          ind_mis_end[1] # Or this.
        dtm_mis_end <- 
          dtm_mis_end %>% 
          format("%I:%M:%S %p")
        
        warning(
          "File #", i, ": ", fnm_img_raw, ": WARNING 2\n",
          "\n",
          "A sequence of IMGs were left uncoded; no annotation was applied.\n",
          "Inform coder to apply an annotation with following info:\n",
          "Uncoded Anno File Line: ", ind_mistake + 1, "\n",
          "Uncoded IMG Numbers   : ", ind_mis_start, " - ", ind_mis_end, "\n",
          "Uncoded Oxford Time   : ", dtm_mis_start, " - ", dtm_mis_end, "\n",
          call. = FALSE
        )
        
      }
      
      # warning(
      #   "File #", i, ": ", schema, " - ", fnm_obs, ": STOP 2\n",
      #   "FUUUUUUUUUUU undefined in the middle of the annotation.\n",
      #   call. = FALSE
      # )
      
      next()
      
    }
    
    # Times are in UTC but Oxford thinks the time taken from the filename of the
    # IMGs are in America/Chicago (since thats the timezone the IMGs are coded
    # in).
    tib_img_raw <- 
      tib_img_raw %>% 
      mutate(start_time =
               start_time %>% 
               lubridate::with_tz(tzone = "America/Chicago"),
             end_time   = 
               end_time %>% 
               lubridate::with_tz(tzone = "America/Chicago"),
             # For merging.
             id         = as.integer(subject),
             visit      = as.integer(visit))
    
    # STEP 2: MERGE ----
    message("Merging...",
            appendLF = FALSE)
    tib_img_mer <- 
      left_join(tib_img_raw,
                tib_timestamps %>% 
                  select(!c(stopwatch_ymd_hms, picture_ymd_hms)),
                by = c("id", "visit"))
    
    ## CHECK: See if timestamp was entered.
    if (all(is.na(tib_img_mer$difference))) {
      
      message("\n",
              appendLF = TRUE)
      
      warning(
        "IMG File ", fnm_img_raw, ":\n",
        "\n",
        "Annotation file does not have an entry in Timestamps.csv\n",
        call. = FALSE
      )
      
      next()
      
    }
    
    # Add diff to times.
    tib_img_mer <- 
      tib_img_mer %>% 
      mutate(new_start_time = 
               (start_time + difference) %>% 
               # Need to remove fractional seconds for new_duration and padding.
               parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                               tz = "America/Chicago"),
             new_end_time   = 
               (end_time + difference) %>% 
               parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                               tz = "America/Chicago"),
             # new_start_date = 
             #   new_start_time %>% 
             #   lubridate::date(),
             # new_end_date = 
             #   new_end_time %>% 
             #   lubridate::date()
      )
    # tib_img_mer$new_start_time <-
    #   tib_img_mer$start_time +
    #   tib_img_mer$difference
    # tib_img_mer$new_end_time <-
    #   tib_img_mer$end_time +
    #   tib_img_mer$difference
    # tib_img_mer$new_start_date <- 
    #   tib_img_mer$new_start_time %>% 
    #   lubridate::date()
    # tib_img_mer$new_end_date <- 
    #   tib_img_mer$new_end_time %>% 
    #   lubridate::date()
    
    
    # # write a "check" csv file to see if stopwatch matches NEW start time
    # write.table(mer_anno,
    #             file = paste0("./3_data/processed/anno_check/",id, "V", visit, ".csv"),
    #             sep = ",",
    #             row.names = F)
    
    # sbs -----
    # test <- 
    #   tib_img_mer %>% 
    #   mutate(new_duration = new_end_time - new_start_time,
    #          .after = duration)
    
    tib_img_sbs <- 
      lapply(seq_len(nrow(tib_img_mer)), 
             function(i) {
               tib <- 
                 tibble(
                   datetime =
                     seq.POSIXt(from = tib_img_mer$new_start_time[i],
                                to   = tib_img_mer$new_end_time[i],
                                by   = "sec"),
                   date     = NA_Date_,
                   time     = NA_character_,
                   behavior = 
                     tib_img_mer$annotation[i] %>% 
                     rep.int(length(datetime))
                 )
             }) %>% 
      bind_rows() %>% 
      # There are gaps between Oxford annotations. Need to "pad" gaps to make sbs.
      pad(interval = "sec",
          by = "datetime") %>% 
      fill(behavior,
           .direction = "down") %>% 
      # For merging with other data types.
      mutate(study    = study,
             subject  = subject,
             visit    = visit,
             date     = lubridate::date(datetime),
             time     = format(datetime,
                               "%H:%M:%S"),
             .before = 1)
    
    # Only do the below for unlisting POSIXct lists.
    # attributes(sbs_dttm) <- 
    #   attributes(test$new_start_time)
    
    
    # POST-ABSTRACT -----------------------------------------------------------
    # ADD IN CODE TO HAVE behavior_imggap, behavior_imgup, behavior_imgdown.
    # RIGHT NOW ITS FILLED DOWN
    
    if (schema == "Posture") {
      
      tib_img_sbs <- 
        tib_img_sbs %>% 
        rename(posture = behavior)
      # colnames(tib_img_sbs)[colnames(tib_img_sbs) == "behavior_img"] <- "posture_img"
      
    }
    
    tib_img_sbs <- 
      tib_img_sbs %>% 
      rename_with(.cols = !study:time,
                  .fn   = ~ paste0(.x, "_img"))
    
    # # changing NA's to transition;gap
    # levels <- levels(sbs_anno$annotation)
    # levels[length(levels) + 1] <- "gap"
    # sbs_anno$annotation <- factor(sbs_anno$annotation,
    #                               levels = levels)
    # sbs_anno$annotation[is.na(sbs_anno$annotation)] <- "gap"
    # 
    # # on off times
    # on_off <- on_off_log[on_off_log$ID == id & on_off_log$Visit == visit, ]
    # on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
    # off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")
    # 
    # #	label off times
    # sbs_anno$off <- 1
    # n <- dim(sbs_anno)[1]
    # index <- (1:n)[(sbs_anno$time >= on) & (sbs_anno$time <= off)]
    # sbs_anno$off[index] <- 0
    # 
    # # check#2: see if off times were actually labeled
    # inds_worn <- (1:(dim(sbs_anno)[1]))[sbs_anno$off==0]
    # i <- length(inds_worn)
    # if(i == 0) {
    #   
    #   warning("\n",
    #           "\n",
    #           file_name, "timestamp or on_off_log entry incorrect")
    #   
    # }
    
    # write table
    fnm_img_shp <- 
      paste0(
        study, "_", subject, "V", visit, "_", schema, "_", coder_initals, ".csv"
      )
    
    # Find separate or single shape directories.Assumes that if there is a single
    # clean directory then there will be a single shape directory.
    if (fld_act_raw == fld_pos_raw) {
      
      fld_act_shp <- 
        list.files(path = fdr_img_shp,
                   pattern = regex("oxford",
                                   ignore_case = TRUE))
      fld_pos_shp <- 
        list.files(path = fdr_img_shp,
                   pattern = regex("oxford",
                                   ignore_case = TRUE))
      
    } else {
      
      fld_act_shp <- 
        list.files(path = fdr_img_shp,
                   pattern = regex("oxford",
                                   ignore_case = TRUE)) %>% 
        str_subset(pattern = "Activity")
      fld_pos_shp <- 
        list.files(path = fdr_img_shp,
                   pattern = regex("oxford",
                                   ignore_case = TRUE)) %>% 
        str_subset(pattern = "Posture")
      
    }
    
    switch(
      schema,
      "Activity" = {
        vroom_write(
          tib_img_sbs,
          path = paste(fdr_img_shp,
                       fld_act_shp,
                       fnm_img_shp,
                       sep = "/"),
          delim = ","
        )
      },
      "Posture" = {
        vroom_write(
          tib_img_sbs,
          path = paste(fdr_img_shp,
                       fld_pos_shp,
                       fnm_img_shp,
                       sep = "/"),
          delim = ","
        )
      }
    )
    
    message("DONE\n",
            appendLF = TRUE)
    processed_files <- 
      processed_files + 1
    
  } 
  
  num_input <- 
    fls_img_raw %>% 
    length() %>% 
    as.character()
  
  message(
    "\n",
    "---------------------------------Done Processing--------------------------------\n",
    "                    ",
    processed_files,
    " out of ",
    num_input,
    # " ", 
    # type,
    " annotation files were processed."
  )
  
}
shape_oxford_v6 <- function(type,
                            fpa_img_raw,
                            fpa_img_clean,
                            fdr_img_raw,
                            tib_cor_time) {
  
  # # CHANGES:
  
  # -update naming scheme.
  # -move checks and cleaning to clean_oxford_v1.
  # -dont make it dependent on schema anymore.
  # -clean up preliminary such that fpt is the full filepath to the file in question.
  #  Don't have "fld" objects anymore.
  
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NA
  
  # ARG: fdr_timestamps
  #      File directory of timestamp file.
  # ARG: fnm_timestamps
  #      File name of timestamp file.
  # ARG: tib_timestamps
  #      Tibble from read_img_timestamps_v5
  
  # # TESTING
  
  # type <- "POS"
  fdr_cln <- 
    "./3_data/1_cleaned"
  fdr_shp <- 
    "./3_data/2_shaped"
  df_timestamps <-
    NULL
  # df_timestamps <-
  #   tib_img_stamps
  key_coder_initials <- 
    c("bayer"        = "AB",
      "smith"        = "AS",
      "peraltawerns" = "AP",
      "miller"       = "EM",
      "martinez"     = "JM",
      "chang"        = "LC",
      "almanza"      = "MA")
  # vct_coder_initials <- 
  #   key_coder_initials %>% 
  #   vec_set_names(names = NULL)
  
  if (is_null(df_timestamps)) {
    
    lgl_timestamp <- FALSE
    
    message("No timestamp data frame found.\n",
            "start and stop times will not be adjusted.")
    
    df_timestamps <- 
      tibble(
        subject           = integer(),
        visit             = integer(),
        stopwatch_ymd_hms = lubridate::POSIXct(),
        picture_ymd_hms   = lubridate::POSIXct(),
        timestamp         = lubridate::POSIXct(), 
        picture           = lubridate::POSIXct(),
        difference        = double()
      )
    
  } else {
    lgl_timestamp <- TRUE
  }
  
  processed_files <- 0
  
  vct_fpa_cln <- 
    fs::dir_ls(
      path        = fdr_cln,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "oxford",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    )
  
  for (i in seq_along(vct_fpa_cln)) {
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    fpa_cln <-
      vct_fpa_cln[i]
    
    fnm_cln <- 
      fpa_cln %>% 
      fs::path_file()
    
    message("rawing file #", i , ": ", # schema, " - ",
            fnm_cln, "...",
            appendLF = TRUE)
    
    fli <- 
      fnm_cln %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      unlist() %>% 
      as.list() %>% 
      rlang::set_names(nm = c("study",
                              "sbj_vst",
                              "schema",
                              "coder"))
    fli$subject <- 
      fli$sbj_vst %>% 
      str_extract(pattern = "(?:(?!v|V|d).)*") %>% 
      as.integer()
    fli$visit <- 
      fli$sbj_vst %>% 
      str_extract(pattern = "([^v|V|d]*)$") %>% 
      as.integer()
    fli$coder_initials = 
      fli$coder %>% 
      str_to_lower() %>% 
      recode(!!!key_coder_initials)
    fli$file_source <- 
      "oxf"
    switch(
      fli$schema,
      "activity" = {
        # col_annotation <- as.symbol("behavior")
        fli$col_annotation <- "behavior"
      },
      "posture" = {
        # col_annotation <- as.symbol("posture")
        fli$col_annotation <- "posture"
      }
    )
    
    df_img_cln <- 
      data.table::fread(
        file = fpa_cln,
        sep = ",",
        header = TRUE,
        # fill = TRUE
      )
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                     CHECK #1: TIMESTAMP                   ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    # Times are in UTC but Oxford thinks the time taken from the filename of the
    # IMGs are in America/Chicago (since thats the timezone the IMGs are coded
    # in).
    df_img_cln <- 
      df_img_cln %>% 
      mutate(start_time =
               start_time %>% 
               lubridate::with_tz(tzone = "America/Chicago"),
             end_time   = 
               end_time %>% 
               lubridate::with_tz(tzone = "America/Chicago"),
             # For df_timestamps.
             subject    = fli$subject,
             visit      = fli$visit,
             .before = 1) %>% 
      # Merge df_timestamp to 1) see if a timestamp was entered for the sbj_vst
      # and 2) see if the picture time is the picture time is between the first.
      # and last photo in the dataset
      left_join(df_timestamps %>% 
                  select(!c(stopwatch_ymd_hms, picture_ymd_hms)),
                by = c("subject", "visit")) %>% 
      as.data.table()
    
    if (lgl_timestamp) {
      
      chk_timestamp_entry <- 
        all(is.na(df_img_cln$difference))
      chk_picture_included <- 
        df_img_cln$picture[1] %within%
        lubridate::interval(start = df_img_cln$start_time[1],
                            end = df_img_cln$end_time[nrow(df_img_cln)])
      
      if (chk_timestamp_entry) {
        
        message("\n",
                appendLF = TRUE)
        warning(
          "IMG File ", fnm_img_raw, ":\n",
          "\n",
          "Annotation file does not have an entry in Timestamps.csv\n",
          call. = FALSE
        )
        next()
        
      } else if (chk_picture_included) {
        
        stop("poop")
        
      }
    } else {
      df_img_cln$difference <- 0
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                       SHAPE #1: TIMES                     ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    # Add diff to times.
    df_img_cln <- 
      df_img_cln %>% 
      mutate(new_start_time = 
               (start_time + difference) %>% 
               # Need to remove fractional seconds for new duration and padding.
               parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                               tz = "America/Chicago"),
             new_end_time   = 
               (end_time + difference) %>% 
               parse_date_time(orders = "%Y-%m-%d %H:%M:%S",
                               tz = "America/Chicago"),
             # new_start_date = 
             #   new_start_time %>% 
             #   lubridate::date(),
             # new_end_date = 
             #   new_end_time %>% 
             #   lubridate::date()
      ) %>% 
      as.data.table()
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                  SHAPE #2: SECOND-BY-SECOND                ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    df_img_shp <- 
      df_img_cln %>% 
      mutate(row = seq_len(n())) %>% 
      group_by(row) %>% 
      summarise(
        study = fli$study,
        subject,
        visit,
        datetime = 
          seq.POSIXt(from = new_start_time,
                     to   = new_end_time,
                     by   = "sec"),
        annotation,
        category,
        .groups = "drop"
      ) %>% 
      # There are gaps between Oxford annotations. Need to "pad" gaps to make sbs.
      as_tibble() %>%
      padr::pad(interval = "sec",
                by = "datetime") %>% 
      # fill(study:visit,
      #      .direction = "down") %>%
      replace_na(replace = list(study = fli$study,
                                subject = fli$subject,
                                visit = fli$visit,
                                annotation = "gap",
                                category = 6L)) %>% 
      mutate(date     = lubridate::date(datetime),
             time     = format(datetime,
                               "%H:%M:%S"),
             duration = seq_duration(vct_datetime = datetime,
                                     vct_value = annotation),
             "{fli$col_annotation}" := annotation,
             annotation = NULL,
             row = NULL,
             .after = datetime) %>% 
      rename_with(.cols = !study:time,
                  .fn   = function(.x) paste(fli$file_source,
                                             .x,
                                             sep = "_")) %>% 
      as.data.table()
    
    # POST-ABSTRACT ------------------------------------------------------------
    # ADD IN CODE TO HAVE behavior_imggap, behavior_imgup, behavior_imgdown.
    # RIGHT NOW ITS FILLED DOWN
    
    # # on off times
    # on_off <- on_off_log[on_off_log$ID == id & on_off_log$Visit == visit, ]
    # on <- strptime(on_off$date_time_on,"%Y-%m-%d %H:%M:%S")
    # off <- strptime(on_off$date_time_off,"%Y-%m-%d %H:%M:%S")
    # 
    # #	label off times
    # sbs_anno$off <- 1
    # n <- dim(sbs_anno)[1]
    # index <- (1:n)[(sbs_anno$time >= on) & (sbs_anno$time <= off)]
    # sbs_anno$off[index] <- 0
    # 
    # # check#2: see if off times were actually labeled
    # inds_worn <- (1:(dim(sbs_anno)[1]))[sbs_anno$off==0]
    # i <- length(inds_worn)
    # if(i == 0) {
    #   
    #   warning("\n",
    #           "\n",
    #           file_name, "timestamp or on_off_log entry incorrect")
    #   
    # }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    fnm_shp <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        fli$schema,
        fli$coder_initials,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    
    fpa_shp <- 
      fs::path(
        fdr_shp,
        dplyr::case_when(fli$schema == "activity" ~ "oxford_activity",
                         fli$schema == "posture" ~ "oxford_posture"),
        fnm_shp
      )
    data.table::fwrite(
      df_img_shp,
      file = fpa_shp,
      sep = ","
    )
    
    message(
      "DONE",
      appendLF = TRUE
    )
    
    message("DONE\n",
            appendLF = TRUE)
    processed_files <- 
      processed_files + 1
    
  } 
  
  num_input <- 
    fpa_cln %>% 
    length() %>% 
    as.character()
  
  message(
    "\n",
    "---------------------------------Done Processing--------------------------------\n",
    "                    ",
    processed_files,
    " out of ",
    num_input,
    # " ", 
    # type,
    " annotation files were processed."
  )
  
}
shape_rmr_v1 <- function() {
  
  # # CHANGES:
  
  # -NA
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NONE
  # ARG: fdr_chm_clean
  #      File directory of clean chamber files.
  # ARG: fdr_rmr_clean
  #      File directory of clean rmr files.
  # ARG: fdr_merge
  #      File directory of merged files.
  
  # # TESTING
  
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  fli_flac_aim <- 
    fdr_read %>% 
    fs::path_dir() %>% 
    str_extract(pattern = "AIM\\d{1}")
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write_rmr <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("rmr",
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write_rmr) {
    # Create a directory under fdr_write that contains "rmr"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "rmr" found in WRITE directory.',
          "i" = 'Creating sub directory "RMR" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "RMR"))
  } 
  
  vct_fpt_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "rmr",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*xlsx")
  
  cli::cli_alert_info("Shaping chamber annotation files.")
  counter <- 0
  
  for (i in seq_along(vct_fpt_read)) {
    
    fpt_read <- 
      vct_fpt_read[1]
    fnm_read <- 
      fs::path_file(fpt_read)
    
    cli::cli_progress_message(
      "Shaping file #{counter + 1}: {fnm_read}"
    )
    
    cli::cli_progress_step(
      msg = "Shaping file #{counter + 1}: {fnm_read}...",
      msg_done = "Shaping file #{counter}: {fnm_read}...DONE",
      msg_failed = "Shaping file #{counter}: {fnm_read}...WARNING"
    )
    
    fli <- 
      fnm_read %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop()
    fli <- 
      list(fli[1] %>% 
             str_extract(pattern = "\\w{2}"),
           fli[1] %>% 
             str_extract(pattern = "\\d{4}") %>% 
             as.integer(),
           1L,
           str_to_upper(fli[2])) %>% 
      rlang::set_names(nm = c("study",
                              "subject",
                              "visit",
                              "file_source"))
    fli$flac_aim <- fli_flac_aim
    
    df_cln <- 
      readxl::read_xlsx(
        path = fpt_read,
        sheet = 2,
        range = "G1:H3",
        progress = FALSE
      ) %>% 
      rename_with(.cols = everything(),
                  .fn = str_to_lower)
    
    lgl_wt_kg <- 
      df_cln$wt_kg[1] %>% 
      is.na()
    lgl_vo2_l_min <- 
      df_cln$vo2_l_min[1] %>% 
      is.na()
    lgl_vo2_ml_kg_min <- 
      df_cln$vo2_l_min[2] %>% 
      is.na()
    
    if (rlang::is_empty(df_cln) |
        lgl_wt_kg |
        lgl_vo2_l_min) {
      
      warning(
        "RMR file ", i,": ", fnm_read, " not formatted correctly.\n",
        "Please make sure:\n",
        '  - Variables "wt_kg" and "VO2_L_min" are in the second sheet.\n',
        '  - "wt_kg" is column G and value is in row 2.\n',
        '  - "VO2_L_min" is column H and value is in row 2.\n',
        call. = FALSE
      )
      
      next()
      
    } else if (lgl_vo2_ml_kg_min) {
      
      # Cleaned file does not have vo2 ml/kg/min calculated. Do so now.
      df_cln$vo2_l_min[2] <- 
        (df_cln$vo2_l_min[1] * 1000) / df_cln$wt_kg[1]
      
    }
    
    df_shp <- 
      df_cln %>% 
      summarise(
        study = fli$study,
        subject = fli$subject,
        visit = fli$visit,
        rmr_mass_kg       = as.double(wt_kg[1]),
        rmr_vo2_l_min     = as.double(vo2_l_min[1]),
        rmr_vo2_ml_kg_min = as.double(vo2_l_min[2])
      )
    
    fnm_write <- 
      paste(
        fli$study,
        fli$subject,
        fli$visit,
        fli$file_source,
        sep = "_"
      ) %>% 
      fs::path_ext_set(ext = "csv")
    fpt_write <- 
      fs::path(
        fdr_write,
        str_subset(vct_fsb_write,
                   pattern = regex("rmr",
                                   ignore_case = TRUE)),
        fnm_write
      )
    data.table::fwrite(
      df_shp,
      file = fpt_write,
      sep = ","
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpt_write,
                              ext = "feather")
    )
    
  }
  
  
}
shape_rmr_v2 <- function(fdr_read,
                         fdr_write,
                         fdr_project = NULL,
                         folder = "RMR",
                         filter_sub = NULL,
                         project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Incorporate get_fpa_read as it is the same for non Noldus/Oxford.
  # -   Incorporate filter_sub code.
  # -   Incorporate project code.
  # -   Incorporate initiate_wrangle as it is the same across all wrangle functions.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of cleaned rmr files.
  # ARG: fdr_write
  #      File directory of shaped rmr files.
  # ARG: folder
  #      Folder name of shaped rmr files.
  # ARG: fdr_project
  #      File directory to where all the merged data resides in the project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: project_only
  #      Should merged files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "2_AIM1_CLEANED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  fdr_project <-
    NULL
  folder <-
    "RMR"
  filter_sub <-
    NULL
  project_only <- FALSE
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   project_only = project_only,
                   type         = "Shap",
                   file_source  = "RMR")
  vct_fpa_read <- 
    get_fpa_read(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_source_1 = folder,
      filter_sub    = filter_sub
    )
  
  for (i in cli_progress_along(vct_fpa_read,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_read <- 
      vct_fpa_read[1]
    fnm_read <- 
      fs::path_file(fpa_read)
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    df_info <- 
      fnm_read %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop() %>% 
      vec_slice(1) %>% 
      as.data.table() %>% 
      rlang::set_names(nm = "stu_sbj") %>% 
      as_tibble() %>% 
      mutate(study = str_extract(stu_sbj,
                                 pattern = "\\w{2}"),
             subject =
               stu_sbj %>% 
               str_extract(pattern = "\\d{4}") %>% 
               as.integer(),
             visit = 1L,
             file_source = info_source,
             flac_aim = info_flac_aim) %>% 
      as.data.table()
    
    df_cln <- 
      readxl::read_xlsx(
        path = fpa_read,
        sheet = 2,
        range = "G1:H3",
        progress = FALSE
      ) %>% 
      rename_with(.cols = everything(),
                  .fn = str_to_lower)
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            CHECKS                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
    chk_wt_kg <- 
      df_cln$wt_kg[1] %>% 
      is.na()
    chk_vo2_l_min <- 
      df_cln$vo2_l_min[1] %>% 
      is.na()
    chk_vo2_ml_kg_min <- 
      df_cln$vo2_l_min[2] %>% 
      is.na()
    
    if (rlang::is_empty(df_cln) |
        chk_wt_kg |
        chk_vo2_l_min) {
      
      cli_warn(c(
        "!" = "File {i}: {fnm_read} not formatted correctly.",
        "i" = "Please make sure:",
        ">" =  'Variables "wt_kg" and "VO2_L_min" are in the second sheet.',
        ">" =  '"wt_kg" is column G and value is in row 2.',
        ">" =  '"VO2_L_min" is column H and value is in row 2.'
      ))
      
      next()
      
    } else if (chk_vo2_ml_kg_min) {
      # Cleaned file does not have vo2 ml/kg/min calculated. Do so now.
      df_cln$vo2_l_min[2] <- 
        (df_cln$vo2_l_min[1] * 1000) / df_cln$wt_kg[1]
    }
    # chk_value <- 
    #   !near(x = df_cln$vo2_l_min[2],
    #         y = ((df_cln$vo2_l_min[1] * 1000) / df_cln$wt_kg[1]),
    #         tol = 0.0001)
    # if(chk_value) {
    #   difference <- 
    #     df_cln$vo2_l_min[2] - (df_cln$vo2_l_min[1] * 1000) / df_cln$wt_kg[1]
    #   cli_warn(c(
    #     "!" = "File {i}: {fnm_read}",
    #     "!" = "vo2_ml_kg_min from file does not = vo2_l_min * 1000 / wt_kg",
    #     "i" = "File vo2_ml_kg_min is {difference} from calculated.",
    #     "i" =  "Using calculated instead of file's value."
    #   ))
    #   df_cln$vo2_l_min[2] <- 
    #     (df_cln$vo2_l_min[1] * 1000) / df_cln$wt_kg[1]
    # }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            SHAPE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    df_shp <- 
      df_cln %>% 
      summarise(rmr_mass_kg       = as.double(wt_kg[1]),
                rmr_vo2_l_min     = as.double(vo2_l_min[1]),
                rmr_vo2_ml_kg_min = as.double(vo2_l_min[2])) %>% 
      bind_cols(df_info[, .(study, subject, visit)],
                .)
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fnm_write <- 
      df_info %>% 
      unite(col = "file_name",
            study, subject, visit, file_source) %>% 
      pull(file_name) %>% 
      fs::path_ext_set(ext = "csv")
    
    if (project_only) {
      fpa_project <- 
        fs::path(
          dir_ls(fdr_project,
                 type = "directory",
                 regexp = folder),
          fnm_write
        )
      data.table::fwrite(
        df_shp,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
      arrow::write_feather(
        df_shp,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
      cnt <-
        cnt + 1
      next()
    }
    
    fpa_write <- 
      fs::path(
        dir_ls(fdr_write,
               type = "directory",
               regexp = folder),
        fnm_write
      )
    data.table::fwrite(
      df_shp,
      file = fpa_write,
      sep = ",",
      showProgress = FALSE
    )
    arrow::write_feather(
      df_shp,
      sink = fs::path_ext_set(path = fpa_write,
                              ext = "feather")
    )
    
    if(!is_empty(fdr_project)) {
      fpa_project <- 
        fs::path(
          dir_ls(fdr_project,
                 type = "directory",
                 regexp = folder),
          fnm_write
        )
      data.table::fwrite(
        df_shp,
        file = fpa_project,
        sep = ",",
        showProgress = FALSE
      )
      arrow::write_feather(
        df_shp,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
    }
    
    cnt <- 
      cnt + 1
    
  }
  
  cli_progress_done()
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                             MERGE FUNCTIONS                             ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merge_chamber_rmr_v1 <- function(fdr_chm_raw,
                                 fdr_chm_clean) {
  
  fdr_chm_raw <- 
    "./Colorado/Chamber data"
  fdr_chm_clean <- 
    "./Colorado/Chamber data/clean"
  
  fls_chm_raw <- 
    list.files(path = fdr_chm_raw,
               pattern = ".txt")
  # fls_chm_raw %>% 
  #   # str_extract(pattern = "(?:(?!_).)*") %>% 
  #   str_extract(pattern = "[0-9][0-9][0-9][0-9]") %>% 
  #   paste0("V1")
  
  for (i in seq_along(fls_chm_raw)) {
    
    fnm_chm_raw <- 
      fls_chm_raw[1]
    
    study <- 
      fnm_chm_raw %>% 
      str_extract(pattern = "(?:(?!_).)*") %>%
      str_extract(pattern = "[A-Z][A-Z]")
    subject <- 
      fnm_chm_raw %>% 
      # str_extract(pattern = "(?:(?!_).)*") %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]")
    visit <- 
      1L
    subject_visit <- 
      paste(subject,
            visit,
            sep = "V")
    
    message("Cleaning file #", i, ": ", fnm_chm_raw, "...", 
            appendLF = FALSE)
    
    tib_chm_raw <- suppressMessages(
      paste(fdr_chm_raw,
            fnm_chm_raw,
            sep = "/") %>% 
        vroom(delim = "\t",
              progress = FALSE)
    )
    
    tib_chm_raw$datetime <- 
      tib_chm_raw$datetime %>% 
      lubridate::mdy_hms(tz = "America/Denver")
    
    message("sbs...",
            appendLF = FALSE)
    
    tib_chm_sbs <- 
      tib_chm_raw %>% 
      padr::pad(interval = "sec") %>% 
      tidyr::fill(!datetime,
                  .direction = "down")
    
    tib_chm_clean <- 
      tib_chm_sbs %>% 
      rename(time = datetime) %>% 
      mutate(study = study,
             subject = subject,
             visit = visit,
             date = lubridate::date(time)) %>% 
      relocate(study,
               subject,
               visit,
               .before = time) %>% 
      relocate(date,
               .after = time)
    
    message("writing...",
            appendLF = FALSE)
    
    fnm_chm_clean <- 
      paste0(
        study,
        "_",
        subject_visit,
        "_chamber",
        ".rds"
      )
    
    readr::write_rds(
      tib_chm_clean,
      file = paste(fdr_chm_clean,
                   fnm_chm_clean,
                   sep = "/"),
      compress = "none"
    )
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  message(
    "------------------------------------COMPLETE------------------------------------\n",
    appendLF = TRUE
  )
  
}
merge_chamber_rmr_v2 <- function(fdr_chm_raw,
                                 fdr_chm_clean,
                                 fdr_rmr) {
  
  # Implement RMR data into chamber tibble.
  # Calculate absolute and relative MET/intensity.
  # Add warinings when the RMR data is not in the correct format.
  # Write a csv verion of clean data as well.
  
  fdr_chm_raw <- 
    "./Colorado/Chamber data"
  fdr_chm_clean <- 
    "./Colorado/Chamber data/clean"
  fdr_rmr <- 
    "./Colorado/Raw RMR"
  
  fls_chm_raw <- 
    list.files(path = fdr_chm_raw,
               pattern = ".txt")
  fls_rmr <- 
    list.files(path = fdr_rmr,
               pattern = ".xlsx")
  
  for (i in seq_along(fls_chm_raw)) {
    
    fnm_chm_raw <- 
      fls_chm_raw[1]
    
    study <- 
      fnm_chm_raw %>% 
      str_extract(pattern = "(?:(?!_).)*") %>%
      str_extract(pattern = "[A-Z][A-Z]")
    subject <- 
      fnm_chm_raw %>% 
      # str_extract(pattern = "(?:(?!_).)*") %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]")
    visit <- 
      1L
    subject_visit <- 
      paste(subject,
            visit,
            sep = "V")
    
    fnm_rmr <- 
      fnm_chm_raw %>% 
      str_extract(pattern = "(?:(?!_).)*") %>% 
      str_subset(fls_rmr,
                 pattern = .) %>% 
      str_subset(pattern = "^CO\\d{4}_rmr.xlsx")
    
    message("Cleaning file #", i, ": ", fnm_chm_raw, "...", 
            appendLF = FALSE)
    
    tib_chm_raw <- suppressMessages(
      paste(fdr_chm_raw,
            fnm_chm_raw,
            sep = "/") %>% 
        vroom(delim = "\t",
              progress = FALSE)
    )
    
    # if (length(fnm_rmr) == 0) {
    if (rlang::is_empty(fnm_rmr)) {
      
      fnm_rmr_wrong <- 
        fnm_chm_raw %>% 
        str_extract(pattern = "(?:(?!_).)*") %>% 
        str_subset(fls_rmr,
                   pattern = .)
      
      if (rlang::is_empty(fnm_rmr_wrong)) {
        
        warning(
          "RMR file for chamber file ", i,": ", fnm_chm_raw, " not found.\n",
          'Setting "wt_kg" and "VO2_L_min" to 0.\n',
          call. = FALSE
        )
        
      } else {
        
        warning(
          "RMR file #", i,": ", fnm_rmr_wrong, " named incorrectly.\n",
          'Make sure RMR file follows "CO####_rmr.xlsx" format\n',
          'Setting "wt_kg" and "VO2_L_min" to 0.\n',
          call. = FALSE
        )
        
      }
      
      tib_rmr <- 
        tibble(
          wt_kg = 0,
          VO2_L_min = 0
        )
      
    } else {
      
      tib_rmr <- 
        readxl::read_xlsx(
          path = paste(fdr_rmr,
                       fnm_rmr,
                       sep = "/"),
          sheet = 2,
          range = "G1:H3",
          progress = FALSE
        ) 
      
      if (rlang::is_empty(tib_rmr)) {
        
        warning(
          "RMR file ", i,": ", fnm_rmr, " not formatted correctly.\n",
          "Please make sure:\n",
          '  - Variables "wt_kg" and "VO2_L_min" are in the second sheet.\n',
          '  - "wt_kg" is column G and value is in row 2.\n',
          '  - "VO2_L_min" is column H and value is in row 3.\n',
          call. = FALSE
        )
        
        tib_rmr <- 
          tibble(
            wt_kg = 0,
            VO2_L_min = 0
          )
        
      } else {
        
        tib_rmr <- 
          tib_rmr %>% 
          summarise(wt_kg     = as.double(wt_kg[1]),
                    VO2_L_min = as.double(VO2_L_min[2]))
        
      }
    }
    
    tib_chm_raw
    test <- 
      tib_chm_raw %>% 
      bind_cols(tib_rmr) %>% 
      mutate(
        datetime          = lubridate::mdy_hms(datetime,
                                               tz = "America/Denver"),
        date              = lubridate::date(datetime),
        time              = format(datetime,
                                   "%H:%M:%S %p"),
        met_absolute      = (VO2 * 1000) / wt_kg / 3.5,
        met_relative      = (VO2 * 1000) / wt_kg / VO2_L_min,
        intensity_chamber_absolute = case_when(
          met_absolute < 1.5                         ~ "Sedentary",
          (met_absolute >= 1.5 & met_absolute < 3.0) ~ "Light",
          met_absolute >= 3.0                        ~ "Mod-Vig"
        ),
        intensity_chamber_relative = case_when(
          met_relative < 1.5                         ~ "Sedentary",
          (met_relative >= 1.5 & met_relative < 3.0) ~ "Light",
          met_relative >= 3.0                        ~ "Mod-Vig"
        )
      )
    
    # tib_chm_raw$datetime <- 
    #   tib_chm_raw$datetime %>% 
    #   lubridate::mdy_hms(tz = "America/Denver")
    # 
    message("sbs...",
            appendLF = FALSE)
    
    tib_chm_sbs <- 
      tib_chm_raw %>% 
      padr::pad(interval = "sec",
                by = "datetime") %>% 
      tidyr::fill(!datetime,
                  .direction = "down") %>% 
      mutate(study = study,
             subject = subject,
             visit = visit,
             .before = 1)
    
    tib_chm_clean <- 
      tib_chm_sbs %>% 
      select(study:datetime,
             date,
             VO2, wt_kg:intensity_chamber_relative,
             O21:kcal_min)
    # rename(time = datetime) %>% 
    # mutate(study = study,
    #        subject = subject,
    #        visit = visit,
    #        .before = 1) %>% 
    # mutate(date = lubridate::date(datetime),
    #        .after = datetime)
    # date = lubridate::date(time)) %>% 
    # relocate(study,
    #          subject,
    #          visit,
    #          .before = time) %>% 
    # relocate(date,
    #          .after = time)
    
    message("writing...",
            appendLF = FALSE)
    
    fnm_chm_clean <- 
      paste0(
        study,
        "_",
        subject_visit,
        "_chamber",
        ".csv"
      )
    vroom_write(
      tib_chm_clean,
      path = paste(fdr_chm_clean,
                   fnm_chm_clean,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    fnm_chm_clean_rds <- 
      paste0(
        study,
        "_",
        subject_visit,
        "_chamber",
        ".rds"
      )
    readr::write_rds(
      tib_chm_clean,
      file = paste(fdr_chm_clean,
                   fnm_chm_clean_rds,
                   sep = "/"),
      compress = "none"
    )
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  message(
    "------------------------------------COMPLETE------------------------------------\n",
    appendLF = TRUE
  )
  
}
merge_chamber_rmr_v3 <- function(fdr_chm_raw,
                                 fdr_chm_clean,
                                 fdr_rmr) {
  
  
  # Move getting intensity and second by second into DOINT specific stuff.
  # Match format to how Dr. Strath would like it.
  
  fdr_chm_raw <- 
    "./FLAC_AIM1_DATA/1_AIM1_RAW_DATA/CO AIM 1 Chamber Data"
  fdr_chm_clean <- 
    "./FLAC_AIM1_DATA/3_AIM1_MERGED_DATA/AIM1_Merged_Chamber_RMR"
  fdr_rmr <- 
    "./FLAC_AIM1_DATA/1_AIM1_RAW_DATA/CO AIM 1 RMR"
  
  fls_chm_raw <- 
    list.files(path = fdr_chm_raw,
               pattern = ".txt")
  fls_rmr <- 
    list.files(path = fdr_rmr,
               pattern = ".xlsx")
  
  for (i in seq_along(fls_chm_raw)) {
    
    fnm_chm_raw <- 
      fls_chm_raw[1]
    
    study <- 
      fnm_chm_raw %>% 
      str_extract(pattern = "(?:(?!_).)*") %>%
      str_extract(pattern = "[A-Z][A-Z]")
    subject <- 
      fnm_chm_raw %>% 
      # str_extract(pattern = "(?:(?!_).)*") %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]")
    visit <- 
      1L
    subject_visit <- 
      paste(subject,
            visit,
            sep = "V")
    
    fnm_rmr <- 
      fnm_chm_raw %>% 
      str_extract(pattern = "(?:(?!_).)*") %>% 
      str_subset(fls_rmr,
                 pattern = .) %>% 
      str_subset(pattern = "^CO\\d{4}_rmr.xlsx")
    
    message("Cleaning file #", i, ": ", fnm_chm_raw, "...", 
            appendLF = FALSE)
    
    tib_chm_raw <- suppressMessages(
      paste(fdr_chm_raw,
            fnm_chm_raw,
            sep = "/") %>% 
        vroom(delim = "\t",
              progress = FALSE)
    )
    
    # if (length(fnm_rmr) == 0) {
    if (rlang::is_empty(fnm_rmr)) {
      
      fnm_rmr_wrong <- 
        fnm_chm_raw %>% 
        str_extract(pattern = "(?:(?!_).)*") %>% 
        str_subset(fls_rmr,
                   pattern = .)
      
      if (rlang::is_empty(fnm_rmr_wrong)) {
        
        warning(
          "RMR file for chamber file ", i,": ", fnm_chm_raw, " not found.\n",
          'Setting "wt_kg" and "VO2_L_min" to 0.\n',
          call. = FALSE
        )
        
      } else {
        
        warning(
          "RMR file #", i,": ", fnm_rmr_wrong, " named incorrectly.\n",
          'Make sure RMR file follows "CO####_rmr.xlsx" format\n',
          'Setting "wt_kg" and "VO2_L_min" to 0.\n',
          call. = FALSE
        )
        
      }
      
      tib_rmr <- 
        tibble(
          Body_Wgt_KG       = 0,
          RMR_V02_L_Min     = 0,
          RMR_V02_ml_kg_min = 0
        )
      
    } else {
      
      tib_rmr <- 
        readxl::read_xlsx(
          path = paste(fdr_rmr,
                       fnm_rmr,
                       sep = "/"),
          sheet = 2,
          range = "G1:H3",
          progress = FALSE
        ) 
      
      if (rlang::is_empty(tib_rmr)) {
        
        warning(
          "RMR file ", i,": ", fnm_rmr, " not formatted correctly.\n",
          "Please make sure:\n",
          '  - Variables "wt_kg" and "VO2_L_min" are in the second sheet.\n',
          '  - "wt_kg" is column G and value is in row 2.\n',
          '  - "VO2_L_min" is column H and value is in row 3.\n',
          call. = FALSE
        )
        
        tib_rmr <- 
          tibble(
            wt_kg = 0,
            VO2_L_min = 0
          )
        
      } else {
        
        tib_rmr <- 
          tib_rmr %>% 
          summarise(Body_Wgt_KG       = as.double(wt_kg[1]),
                    RMR_V02_L_Min     = as.double(VO2_L_min[1]),
                    RMR_V02_ml_kg_min = as.double(VO2_L_min[2]))
        
      }
    }
    
    tib_chm_clean <- 
      tib_chm_raw %>% 
      bind_cols(tib_rmr) %>% 
      mutate(
        datetime          = lubridate::mdy_hms(datetime,
                                               tz = "America/Denver"),
        datetime          = as.character(datetime),
        # date              = lubridate::date(datetime),
        # Think this should be named as "time". Bring it up later.
        # datetime          = format(datetime,
        #                            "%H:%M:%S %p"),
        Chamber_V02_ml_kg_min = (VO2 * 1000) / Body_Wgt_KG,
        Chamber_METs          = Chamber_V02_ml_kg_min / RMR_V02_ml_kg_min
        # met_absolute      = (VO2 * 1000) / Body_Wgt_KG / 3.5,
        # met_relative      = (VO2 * 1000) / Body_Wgt_KG / RMR_V02_ml_kg_min
        # intensity_chamber_absolute = case_when(
        #   met_absolute < 1.5                         ~ "Sedentary",
        #   (met_absolute >= 1.5 & met_absolute < 3.0) ~ "Light",
        #   met_absolute >= 3.0                        ~ "Mod-Vig"
        # ),
        # intensity_chamber_relative = case_when(
        #   met_relative < 1.5                         ~ "Sedentary",
        #   (met_relative >= 1.5 & met_relative < 3.0) ~ "Light",
        #   met_relative >= 3.0                        ~ "Mod-Vig"
        # )
      )
    
    # tib_chm_raw$datetime <- 
    #   tib_chm_raw$datetime %>% 
    #   lubridate::mdy_hms(tz = "America/Denver")
    # 
    # message("sbs...",
    #         appendLF = FALSE)
    
    # tib_chm_sbs <- 
    #   tib_chm_raw %>% 
    #   padr::pad(interval = "sec",
    #             by = "datetime") %>% 
    #   tidyr::fill(!datetime,
    #               .direction = "down") %>% 
    #   mutate(study = study,
    #          subject = subject,
    #          visit = visit,
    #          .before = 1)
    # 
    # tib_chm_clean <- 
    #   tib_chm_sbs %>% 
    #   select(study:datetime,
    #          date,
    #          VO2, wt_kg:intensity_chamber_relative,
    #          O21:kcal_min)
    # rename(time = datetime) %>% 
    # mutate(study = study,
    #        subject = subject,
    #        visit = visit,
    #        .before = 1) %>% 
    # mutate(date = lubridate::date(datetime),
    #        .after = datetime)
    # date = lubridate::date(time)) %>% 
    # relocate(study,
    #          subject,
    #          visit,
    #          .before = time) %>% 
    # relocate(date,
    #          .after = time)
    
    message("writing...",
            appendLF = FALSE)
    
    fnm_chm_clean <- 
      paste0(
        # study,
        # "_",
        subject,
        # "_test",
        # "_chamber",
        ".csv"
      )
    vroom_write(
      tib_chm_clean,
      path = paste(fdr_chm_clean,
                   fnm_chm_clean,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    # fnm_chm_clean_rds <- 
    #   paste0(
    #     study,
    #     "_",
    #     subject_visit,
    #     "_chamber",
    #     ".rds"
    #   )
    # readr::write_rds(
    #   tib_chm_clean,
    #   file = paste(fdr_chm_clean,
    #                fnm_chm_clean_rds,
    #                sep = "/"),
    #   compress = "none"
    # )
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  message(
    "------------------------------------COMPLETE------------------------------------\n",
    appendLF = TRUE
  )
  
}
merge_chamber_rmr_v4 <- function(fdr_chm_clean,
                                 fdr_rmr_clean,
                                 fdr_merge) {
  
  # # CHANGES:
  
  # -Change function name from clean to merge Match data flow of FLAC folders.
  # -Rename raw   TO clean in ARG and object names
  # -Rename clean TO merge in ARG and object names
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NONE
  # ARG: fdr_chm_clean
  #      File directory of clean chamber files.
  # ARG: fdr_rmr_clean
  #      File directory of clean rmr files.
  # ARG: fdr_merge
  #      File directory of merged files.
  
  # # TESTING
  
  # fdr_chm_clean <-
  #   "./FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_Raw_Chamber"
  # fdr_rmr_clean <-
  #   "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA/AIM1_Cleaned_RMR"
  # fdr_merge <-
  #   "./FLAC_AIM1_DATA/4_AIM1_MERGED_DATA/AIM1_Merged_Chamber_RMR"
  
  fls_chm_clean <- 
    list.files(path = fdr_chm_clean,
               pattern = ".txt") # Change this when clean fdr is actually clean
  fls_rmr_clean <- 
    list.files(path = fdr_rmr_clean,
               pattern = ".xlsx")
  
  for (i in seq_along(fls_chm_clean)) {
    
    fnm_chm_clean <- 
      fls_chm_clean[11]
    
    study <- 
      fnm_chm_clean %>% 
      str_extract(pattern = "(?:(?!_).)*") %>%
      str_extract(pattern = "[A-Z][A-Z]")
    subject <- 
      fnm_chm_clean %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]")
    visit <- 
      1L
    subject_visit <- 
      paste(subject,
            visit,
            sep = "v")
    
    fnm_rmr_clean <- 
      fnm_chm_clean %>% 
      str_extract(pattern = "(?:(?!_).)*") %>% 
      str_subset(fls_rmr_clean,
                 pattern = .) %>% 
      str_subset(pattern = "^CO\\d{4}_rmr.xlsx")
    
    message("Merging file #", i, ": ", fnm_chm_clean, "...", 
            appendLF = FALSE)
    
    if (rlang::is_empty(fnm_rmr_clean)) {
      
      message("",
              appendLF = TRUE)
      
      warning(
        "RMR file for chamber file ", i,": ", fnm_chm_clean, " not found.\n",
        'Skipping this file. No merge file will be written.\n',
        call. = FALSE
      )
      
      next()
      
      # fnm_rmr_wrong <- 
      #   fnm_chm_clean %>% 
      #   str_extract(pattern = "(?:(?!_).)*") %>% 
      #   str_subset(fls_rmr_clean,
      #              pattern = .)
      # 
      # if (rlang::is_empty(fnm_rmr_wrong)) {
      #   
      #   warning(
      #     "RMR file for chamber file ", i,": ", fnm_chm_clean, " not found.\n",
      #     'Setting "wt_kg" and "VO2_L_min" to 0.\n',
      #     call. = FALSE
      #   )
      #   
      # } else {
      #   
      #   warning(
      #     "RMR file #", i,": ", fnm_rmr_wrong, " named incorrectly.\n",
      #     'Make sure RMR file follows "CO####_rmr.xlsx" format\n',
      #     'Setting "wt_kg" and "VO2_L_min" to 0.\n',
      #     call. = FALSE
      #   )
      #   
      # }
      # 
      # tib_rmr_clean <- 
      #   tibble(
      #     Body_Wgt_KG       = 0,
      #     RMR_V02_L_Min     = 0,
      #     RMR_V02_ml_kg_min = 0
      #   )
      
    }
    
    tib_chm_clean <- suppressMessages(
      paste(fdr_chm_clean,
            fnm_chm_clean,
            sep = "/") %>% 
        vroom(delim = "\t",
              progress = FALSE)
    )
    tib_rmr_clean <- 
      readxl::read_xlsx(
        path = paste(fdr_rmr_clean,
                     fnm_rmr_clean,
                     sep = "/"),
        sheet = 2,
        range = "G1:H3",
        progress = FALSE
      ) 
    no_wt_kg <- 
      tib_rmr_clean$wt_kg[1] %>% 
      is.na()
    no_vo2_ml_kg_min <- 
      tib_rmr_clean$VO2_L_min[2] %>% 
      is.na()
    
    if (rlang::is_empty(tib_rmr_clean)) {
      
      warning(
        "RMR file ", i,": ", fnm_rmr_clean, " not formatted correctly.\n",
        "Please make sure:\n",
        '  - Variables "wt_kg" and "VO2_L_min" are in the second sheet.\n',
        '  - "wt_kg" is column G and value is in row 2.\n',
        '  - "VO2_L_min" is column H and value is in row 3.\n',
        call. = FALSE
      )
      
      tib_rmr_clean <- 
        tibble(
          wt_kg    = 0,
          VO2_L_min = 0
        )
      
    } else {
      
      tib_rmr_clean <- 
        tib_rmr_clean %>% 
        summarise(Body_Wgt_KG       = as.double(wt_kg[1]),
                  RMR_V02_L_Min     = as.double(VO2_L_min[1]),
                  RMR_V02_ml_kg_min = as.double(VO2_L_min[2]))
      
    }
    
    
    tib_merge <- 
      tib_chm_clean %>% 
      bind_cols(tib_rmr_clean) %>% 
      mutate(
        datetime              = lubridate::mdy_hms(datetime,
                                                   tz = "America/Denver"),
        datetime              = as.character(datetime),
        Chamber_V02_ml_kg_min = (VO2 * 1000) / Body_Wgt_KG,
        Chamber_METs          = Chamber_V02_ml_kg_min / RMR_V02_ml_kg_min
      )
    
    message("writing...",
            appendLF = FALSE)
    
    fnm_merge <- 
      paste0(
        # study,
        # "_",
        subject,
        ".csv"
      )
    
    vroom_write(
      tib_merge,
      path = paste(fdr_merge,
                   fnm_merge,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  message(
    "------------------------------------COMPLETE------------------------------------\n",
    appendLF = TRUE
  )
  
}
merge_chamber_rmr_v5 <- function(fdr_chm_shape,
                                 fdr_rmr_shape,
                                 fdr_merge) {
  
  # # CHANGES:
  
  # -Moved some of the cleaning stuff to function clean_chamber
  # -Rename clean to shape in ARG and object names
  # -Update to lowercase column names
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NONE
  # ARG: fdr_chm_shape
  #      File directory of shaped chamber files.
  # ARG: fdr_rmr_shape
  #      File directory of shaped rmr files.
  # ARG: fdr_merge
  #      File directory of merged files.
  
  # # TESTING
  
  # fdr_chm_shape <-
  #   "./FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_Raw_Chamber"
  # fdr_rmr_shape <-
  #   "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA/AIM1_Cleaned_RMR"
  # fdr_merge <-
  #   "./FLAC_AIM1_DATA/4_AIM1_MERGED_DATA/AIM1_Merged_Chamber_RMR"
  fdr_chm_shape <-
    "./3_data/2_shaped/chamber"
  fdr_rmr_shape <-
    "./3_data/2_shaped/rmr"
  fdr_merge <-
    "./3_data/3_merged/chamber_rmr"
  
  fls_chm_shape <- 
    list.files(path = fdr_chm_shape,
               pattern = ".csv") # Change this when clean fdr is actually clean
  fls_rmr_shape <- 
    list.files(path = fdr_rmr_shape,
               pattern = ".csv")
  
  for (i in seq_along(fls_chm_shape)) {
    
    fnm_chm_shape <- 
      fls_chm_shape[i]
    
    study <- 
      fnm_chm_shape %>% 
      str_extract(pattern = "(?:(?!_).)*") %>%
      str_extract(pattern = "[A-Z][A-Z]")
    subject <- 
      fnm_chm_shape %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]")
    # visit <- 
    #   1L
    # subject_visit <- 
    #   paste(subject,
    #         visit,
    #         sep = "v")
    
    fnm_rmr_shape <- 
      str_subset(fls_rmr_shape,
                 pattern = subject)
    
    message("Merging file #", i, ": ", fnm_chm_shape, "...", 
            appendLF = FALSE)
    
    if (rlang::is_empty(fnm_rmr_shape)) {
      
      message("",
              appendLF = TRUE)
      
      warning(
        "RMR file for chamber file ", i,": ", fnm_chm_shape, " not found.\n",
        'Skipping this file. No merge file will be written.\n',
        call. = FALSE
      )
      
      next()
      
    }
    
    tib_chm_shape <- suppressMessages(
      paste(fdr_chm_shape,
            fnm_chm_shape,
            sep = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    )
    tib_rmr_shape <- suppressMessages(
      paste(fdr_rmr_shape,
            fnm_rmr_shape,
            sep = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    ) 
    
    tib_merge <- 
      tib_chm_shape %>% 
      bind_cols(tib_rmr_shape) %>% 
      mutate(
        datetime              = lubridate::mdy_hms(datetime,
                                                   tz = "America/Denver"),
        chamber_vo2_ml_kg_min = (vo2 * 1000) / body_wgt_kg,
        chamber_mets          = chamber_vo2_ml_kg_min / rmr_vo2_ml_kg_min
      )
    
    message("writing...",
            appendLF = FALSE)
    
    fnm_merge <- 
      paste0(
        study,
        "_",
        subject,
        "_CHM_RMR.csv"
      )
    fnm_merge_rds <- 
      paste0(
        study,
        "_",
        subject,
        "_CHM_RMR.rds"
      )
    
    vroom_write(
      tib_merge %>% 
        mutate(datetime = as.character(datetime)),
      file = paste(fdr_merge,
                   fnm_merge,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    readr::write_rds(
      tib_merge,
      file = paste(fdr_merge,
                   fnm_merge_rds,
                   sep = "/"),
      compress = "none"
    )
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  message(
    "------------------------------------COMPLETE------------------------------------\n",
    appendLF = TRUE
  )
  
}
merge_chamber_rmr_v6 <- function(fdr_chm_shape,
                                 fdr_rmr_shape,
                                 fdr_merge) {
  
  # # CHANGES:
  
  # -Moved some of the cleaning stuff to function clean_chamber
  # -Rename clean to shape in ARG and object names
  # -Update to lowercase column names
  
  # # FUNCTIONS & ARGUMENTS:
  
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "4_AIM1_MERGED_DATA")
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  fsb_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file()
  
  # n_fsb_read <- length(fsb_read)
  # n_str_fsb_read <- 
  #   map_int(test,
  #           .f = ~str_length(.x)) %>% 
  #   max()
  # fsb_read_split <- 
  #   str_split(fsb_read,
  #             pattern = "")
  # fsb_read_split_equal <- 
  #   replicate(n_fsb_read,
  #             character(length = n_str_fsb_read),
  #             simplify = FALSE) %>% 
  #   rlang::set_names(nm = fsb_read) %>% 
  # map(.f = ~dplyr::na_if(.x, y = ""))
  # 
  # for (i in seq_along(fsb_read_split_equal)) {
  #   fsb_read_split_equal[[i]][seq_along(fsb_read_split[[i]])] <- fsb_read_split[[i]]
  # }
  # 
  # poo2 <- 
  #   tibble(
  #     x = fsb_read,
  #     y = fsb_read
  #   ) %>% 
  #   expand(x, y) %>%
  #   filter(x != y) %>% 
  #   unite(col = "xy",
  #         x, y,
  #         sep = "-",
  #         remove = FALSE) %>% 
  #   mutate(
  #     run_x = vec_identify_runs(x),
  #     run_y = vec_match(needles = y,
  #                       haystack = fsb_read),
  #     run_xy = paste(run_x, run_y, sep = ""),
  #     run_dup = 
  #       if_else(run_x < run_y,
  #               true = run_xy,
  #               false = stringi::stri_reverse(run_xy),
  #               missing = NULL)
  #   ) %>% 
  #   filter(run_xy == run_dup) %>% 
  #   select(x, y, xy)
  # poo3 <- 
  #   matrix(nrow = n_str_fsb_read,
  #          ncol = nrow(poo2)) %>% 
  #   as.data.table() %>% 
  #   rlang::set_names(nm = poo2$xy)
  # 
  # for (i in seq_len(nrow(poo2))) {
  #   poo3[[i]] <- 
  #     fsb_read_split_equal[[poo2$x[i]]] == fsb_read_split_equal[[poo2$y[i]]]
  # }
  # 
  # poo4 <- 
  #   poo3 %>% 
  #   rowwise() %>% 
  #   mutate(common = all(c_across(cols = everything())))
  # fsb_str_common <- 
  #   fsb_read_split[[1]][which(poo4$common)] %>% 
  #   paste0(collapse = "")
  # poo5 <- 
  #   poo4 %>% 
  #   filter(!common)
  
  fli_flac_aim <- 
    fdr_read %>% 
    fs::path_dir() %>% 
    str_extract(pattern = "AIM\\d{1}")
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write_chm_rmr <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("chamber_rmr",
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write_chm_rmr) {
    # Create a directory under fdr_write that contains "chamber_rmr"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "chamber_rmr" found in WRITE directory.',
          "i" = 'Creating sub directory "CHAMBER_RMR" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "CHAMBER_RMR"))
  } 
  
  vct_fpt_read_chm <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "chamber",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*feather")
  vct_fpt_read_rmr <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "rmr",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*feather")
  
  cli::cli_alert_info("Merging chamber & rmr files.")
  counter <- 0
  
  for (i in seq_along(vct_fpt_read_chm)) {
    
    fpt_read_chm <- 
      vct_fpt_read_chm[1]
    fnm_read_chm <- 
      fs::path_file(fpt_read_chm)
    
    cli::cli_progress_message(
      "Merging file #{counter + 1}: {fnm_read}"
    )
    
    cli::cli_progress_step(
      msg = "Merging file #{counter + 1}: {fnm_read}...",
      msg_done = "Merging file #{counter}: {fnm_read}...DONE",
      msg_failed = "Merging file #{counter}: {fnm_read}...WARNING"
    )
    
    fli_std_sbj_vst <- 
      fnm_read_chm %>%
      str_remove(pattern = "_(?!.*_).*")
    
    fpt_read_rmr <- 
      vct_fpt_read_rmr %>% 
      str_subset(pattern = fli_std_sbj_vst)
    
    df_mer <- 
      left_join(
        arrow::read_feather(fpt_read_chm,
                            col_select = c(study:time, chamber_vo2_ml_min)),
        arrow::read_feather(fpt_read_rmr,
                            col_select = c(study:rmr_mass_kg, rmr_vo2_ml_kg_min)),
        by = c("study", "subject", "visit")
      ) %>% 
      mutate(chamber_vo2_ml_kg_min = chamber_vo2_ml_min / rmr_mass_kg,
             mets_rmr = chamber_vo2_ml_kg_min / rmr_vo2_ml_kg_min,
             mets_standard = chamber_vo2_ml_kg_min / 3.5) %>% 
      select(study:time,
             chamber_vo2_ml_kg_min,
             rmr_vo2_ml_kg_min:last_col()) %>% 
      as.data.table()
    
    fnm_write <- 
      paste(fli_std_sbj_vst,
            "CHAMBER_RMR",
            sep = "_") %>% 
      fs::path_ext_set(ext = "csv")
    fpt_write <- 
      fs::path(
        fdr_write,
        str_subset(vct_fsb_write,
                   pattern = regex("chamber_rmr",
                                   ignore_case = TRUE)),
        fnm_write
      )
    data.table::fwrite(
      df_mer,
      file = fpt_write,
      sep = ","
    )
    arrow::write_feather(
      df_mer,
      sink = fs::path_ext_set(path = fpt_write,
                              ext = "feather")
    )
    # if (rlang::is_empty(fnm_rmr)) {
    #   
    #   fnm_rmr_wrong <- 
    #     fnm_chm_raw %>% 
    #     str_extract(pattern = "(?:(?!_).)*") %>% 
    #     str_subset(fls_rmr,
    #                pattern = .)
    #   
    #   if (rlang::is_empty(fnm_rmr_wrong)) {
    #     
    #     warning(
    #       "RMR file for chamber file ", i,": ", fnm_chm_raw, " not found.\n",
    #       'Setting "wt_kg" and "VO2_L_min" to 0.\n',
    #       call. = FALSE
    #     )
    #     
    #   } else {
    #     
    #     warning(
    #       "RMR file #", i,": ", fnm_rmr_wrong, " named incorrectly.\n",
    #       'Make sure RMR file follows "CO####_rmr.xlsx" format\n',
    #       'Setting "wt_kg" and "VO2_L_min" to 0.\n',
    #       call. = FALSE
    #     )
    #     
    #   }
    #   
    #   tib_rmr <- 
    #     tibble(
    #       Body_Wgt_KG       = 0,
    #       RMR_V02_L_Min     = 0,
    #       RMR_V02_ml_kg_min = 0
    #     )
    #   
    # }
    
    counter <- 
      counter + 1
    # message("DONE\n",
    #         appendLF = TRUE)
    # cli::cli_progress_step(
    #   msg = "DONE",
    #   msg_done = "SUCCESS. Cleaned {counter} files out of {length(vct_fpt_read)} files."
    # )
    
  }
  
  cli::cli_progress_step(
    msg = "DONE",
    msg_done = "SUCCESS. Cleaned {counter} files out of {length(vct_fpt_read)} files."
  )
  
}
docomp_merge_noldus_v2 <- function(fdr_vid_clean,
                                   fdr_vid_merged) {
  
  # fdr_vid_clean <- 
  #   "./3_data/1_cleaned/sbs"
  # fdr_vid_merged <- 
  #   "./3_data/1_cleaned/merged"
  
  fls_vid_clean <-
    list.files(path = fdr_vid_clean,
               pattern = ".csv")
  subjects <- 
    fls_vid_clean %>% 
    str_extract(pattern = "[0-9][0-9][0-9][0-9]") %>% 
    unique()
  lst_vid_subject <- 
    fls_vid_clean %>% 
    str_extract(pattern = "[0-9][0-9][0-9][0-9]")
  
  for (i in seq_along(subjects)) {
    
    subject <- 
      subjects[1]
    
    message("Merging files for subject ", subject, "...", "\n",
            "  Files: ",
            appendLF = FALSE)
    
    lgl_sub_unique <- 
      lst_vid_subject %in% subject
    
    fls_sub_unique <- 
      fls_vid_clean[lgl_sub_unique]
    
    for (ii in seq_along(fls_sub_unique)) {
      
      fnm_vid_clean <- fls_sub_unique[ii]
      
      message(ii, " ",
              appendLF = FALSE)
      
      tib_vid_clean <- suppressMessages(
        paste(fdr_vid_clean,
              fnm_vid_clean,
              sep = "/") %>% 
          vroom(delim = ",",
                progress = FALSE)
      )
      
      if (ii == 1) {
        
        tib_vid_mer <- 
          tib_vid_clean
        # lst_vid_mer <- 
        #   list(tib_vid_clean)
        
      } else if (ii > 1) {
        
        tib_vid_mer <- 
          left_join(tib_vid_mer,
                    tib_vid_clean,
                    by = c("subject", "time"))
        # lst_vid_mer[[ii]] <-
        #   tib_vid_clean
        
      }
    }
    
    # tib_vid_mer <- 
    #   lst_vid_mer[[1]] %>% 
    #   left_join(lst_vid_mer[[2]],
    #             by = c("subject", "time")) %>% 
    #   left_join(lst_vid_mer[[3]],
    #             by = c("subject", "time")) %>% 
    #   left_join(lst_vid_mer[[4]],
    #             by = c("subject", "time")) %>% 
    #   left_join(lst_vid_mer[[5]],
    #             by = c("subject", "time")) %>% 
    #   left_join(lst_vid_mer[[6]],
    #             by = c("subject", "time"))
    # 
    # tib_vid_mer <- 
    #   tib_vid_mer[, !colnames(tib_vid_mer) %in% c("subject", "time")]
    
    # tib_vid_mer <- 
    #   tib_vid_mer[, !colnames(tib_vid_mer) %in% c("subject")]
    
    fnm_vid_merged <- 
      paste(subject,
            "csv",
            sep = ".")
    
    vroom_write(
      tib_vid_mer,
      path = paste(fdr_vid_merged,
                   fnm_vid_merged,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    message("\n",
            "---------------DONE--------------" ,
            appendLF = TRUE)
    
  }
}
merge_noldus_v4 <- function(project,
                            fdr_vid_clean,
                            fdr_vid_merged) {
  
  # If project = "FLAC - Aim 1", also include chamber data.
  project <- 
    "FLAC - Aim 1"
  fdr_vid_clean <-
    "./Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  fdr_vid_merged <-
    "./Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
  
  fls_vid_clean <-
    list.files(path = fdr_vid_clean,
               pattern = ".csv")
  study <- 
    fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!_).)*") %>% 
    unique()
  subjects_visits <- 
    fls_vid_clean %>% 
    str_extract(pattern = "[0-9][0-9][0-9][0-9]V[0-9]") %>% 
    unique()
  lst_vid_subjects_visits <- 
    fls_vid_clean %>% 
    str_extract(pattern = "[0-9][0-9][0-9][0-9]V[0-9]")
  
  for (i in seq_along(subjects_visits)) {
    
    subject_visit <- 
      subjects_visits[i]
    subject <- 
      subject_visit %>% 
      str_sub(start = 1,
              end = 4)
    visit <- 
      subject_visit %>% 
      str_sub(start = 6,
              end = 6)
    
    message("Merging files for subject ", subject, ", visit ", visit, "...", "\n",
            "  Files: ",
            appendLF = FALSE)
    
    lgl_sub_vis_unique <- 
      lst_vid_subjects_visits %in% subject_visit
    
    fls_sub_vis_unique <- 
      fls_vid_clean[lgl_sub_vis_unique]
    
    for (ii in seq_along(fls_sub_vis_unique)) {
      
      fnm_vid_clean <- 
        fls_sub_vis_unique[ii]
      
      message(ii, " ",
              appendLF = FALSE)
      
      tib_vid_clean <- suppressMessages(
        paste(fdr_vid_clean,
              fnm_vid_clean,
              sep = "/") %>% 
          vroom(delim = ",",
                progress = FALSE)
      )
      
      if (ii == 1) {
        
        tib_vid_mer <- 
          tib_vid_clean
        
      } else if (ii > 1) {
        
        tib_vid_mer <- 
          left_join(tib_vid_mer,
                    tib_vid_clean,
                    by = c("study", "subject", "visit", "time", "date"))
        
      }
    }
    
    tib_vid_mer <- 
      tib_vid_mer[, c("study",
                      "subject",
                      "visit",
                      "time",
                      "date",
                      "posture",
                      "behavior",
                      "activity",
                      "intensity",
                      "environment",
                      "comment_act",
                      "comment_pos",
                      "num_events_act",
                      "num_events_pos")]
    
    if (project == "FLAC - Aim 1") {
      
      # Merge clean chamber data as well.
      message(ii + 1,
              appendLF = FALSE)
      
      fdr_chm_clean <- 
        "./Colorado/Chamber data/clean"
      fls_chm_clean <- 
        list.files(path = fdr_chm_clean,
                   pattern = ".rds")
      
      fnm_chm_clean <- 
        fls_chm_clean %>% 
        str_subset(pattern = subject_visit)
      
      tib_chm_clean <- 
        readr::read_rds(paste(fdr_chm_clean,
                              fnm_chm_clean,
                              sep = "/"))
      
      tib_vid_mer$subject <- 
        tib_vid_mer$subject %>% 
        as.character()
      tib_vid_mer$visit <- 
        tib_vid_mer$visit %>% 
        as.integer()
      tib_vid_mer$time <- 
        tib_vid_mer$time %>% 
        force_tz(tzone = "America/Denver")
      
      tib_vid_mer <- 
        full_join(tib_vid_mer,
                  tib_chm_clean,
                  by = c("study", "subject", "visit", "time", "date")) %>% 
        dplyr::arrange(time)
      
    }
    
    tib_vid_mer <- 
      tib_vid_mer %>% 
      mutate(datetime = time,
             time     = format(time,
                               "%H:%M:%S")) %>% 
      dplyr::relocate(datetime,
                      date,
                      .before = time)
    
    fnm_vid_merged <- 
      paste0(study,
             "_",
             subject_visit,
             ".csv")
    
    vroom_write(
      tib_vid_mer,
      path = paste(fdr_vid_merged,
                   fnm_vid_merged,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    message("\n",
            "---------------DONE--------------" ,
            appendLF = TRUE)
    
  }
}







# tib_mer_sbs <- left_join(tib_pos_sbs,
#                          tib_act_sbs,
#                          by = c("study",
#                                 "subject",
#                                 # "visit",
#                                 "time",
#                                 "date"))
# 
# tib_mer_sbs <- tib_mer_sbs[, c("study", 
#                                "subject", 
#                                # "visit", 
#                                "time", 
#                                "date",
#                                "posture",
#                                "behavior",
#                                "activity",
#                                "domain",
#                                "intensity",
#                                "pos_comment",
#                                "act_comment",
#                                "num_pos_events",
#                                "num_act_events")]

# fnm_clean <- 
#   str_sub(fnm_vid_raw_pos,
#           start = 26,
#           end = 36)

# vroom_write(
#   tib_mer_sbs,
#   path = paste(fdr_clean,
#                fnm_clean,
#                sep = "/"),
#   delim = ","
# )
merge_noldus_v6 <- function(project, # Previously merge_data_v6
                            fdr_vid_clean,
                            fdr_vid_merged,
                            lvl_behavior,
                            lvl_posture,
                            lvl_intensity,
                            lvl_environment) {
  
  # project <-
  #   "DOCOMP"
  # fdr_vid_clean <-
  #   "./3_data/1_cleaned/sbs"
  # fdr_vid_merged <-
  #   "./3_data/1_cleaned/merged"
  
  project <-
    "DOINT"
  fdr_vid_clean <-
    "./Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  fdr_merged
  fdr_img_clean
  fdr_acc_clean
  
  fdr_vid_merged <-
    "./Colorado/UWM_Modified CO DATA/DOINT"
  
  # project <-
  #   "FLAC - Aim 1"
  # fdr_vid_clean <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  # fdr_vid_merged <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
  
  # lvl_behavior <-
  #   c("Sports/Exercise",
  #     "Eating/Drinking",
  #     "Transportation",
  #     "Electronics",
  #     "Other - Manipulating Objects",
  #     "Other - Carrying Load w/ UE",
  #     "Other - Pushing Cart",
  #     "Talking - Person",
  #     "Talking - Phone",
  #     "Caring/Grooming - Adult",
  #     "Caring/Grooming - Animal/Pet",
  #     "Caring/Grooming - Child",
  #     "Caring/Grooming - Self",
  #     "Cleaning",
  #     "C/F/R/M",
  #     "Cooking/Meal Preperation",
  #     "Laundry" ,
  #     "Lawn&Garden",
  #     "Leisure Based",
  #     "Only [P/M] Code",
  #     "Talking - Researchers",
  #     "Intermittent Activity",
  #     "Dark/Obscured/OoF")
  # lvl_posture <-
  #   c("Lying",
  #     "Sitting",
  #     "Crouching / Kneeling / Squating",
  #     "Standing",
  #     "Other - Posture",
  #     "Intermittent Posture",
  #     "Walking",
  #     "Stepping",
  #     "Running",
  #     "Ascending Stairs",
  #     "Descending Stairs",
  #     "Crouching / Squating",
  #     "Cycling",
  #     "Other - Movement",
  #     "Intermittent Movement",
  #     "Intermittent P/M",
  #     "Dark/Obscured/OoF")
  # lvl_intensity <-
  #   c("Sedentary",
  #     "Light",
  #     "Mod-Vig",
  #     "Dark/Obscured/OoF")
  # lvl_environment <-
  #   c("Domestic",
  #     "Non-Domestic",
  #     "Errands/Shopping",
  #     "Occupation",
  #     "Organizational/Civic/Religiious",
  #     "Dark/Obscured/OoF")
  
  fls_vid_clean <-
    list.files(path = fdr_vid_clean,
               pattern = ".csv")
  
  # Not sure if I will want to merge all visits in one or seperate merge file for
  # each file. Do it by subject visit for now.
  study <- 
    fls_vid_clean %>% 
    str_extract(pattern = "(?:(?!_).)*") %>% 
    unique()
  subjects_visits <- 
    fls_vid_clean %>% 
    str_extract(pattern = "[0-9][0-9][0-9][0-9][A-z][0-9]") %>% 
    unique()
  lst_vid_subjects_visits <- 
    fls_vid_clean %>% 
    str_extract(pattern = "[0-9][0-9][0-9][0-9][A-z][0-9]")
  # subjects <- 
  #   fls_vid_clean %>% 
  #   str_extract(pattern = "[0-9][0-9][0-9][0-9]") %>% 
  #   unique()
  # lst_vid_subject <- 
  #   fls_vid_clean %>% 
  #   str_extract(pattern = "[0-9][0-9][0-9][0-9]")
  
  lst_merged_all <-
    list()
  
  for (i in seq_along(subjects_visits)) {
    # for (i in seq_along(subjects)) {
    
    subject_visit <- 
      subjects_visits[i]
    subject <- 
      subject_visit %>% 
      str_sub(start = 1,
              end = 4)
    visit <- 
      subject_visit %>% 
      str_sub(start = 6,
              end = 6)
    # subject <- 
    #   subjects[i]
    
    message("Merging files for subject ", subject, ", visit ", visit, "...", "\n",
            "  Files: ",
            appendLF = FALSE)
    # message("Merging files for subject ", subject, "...", "\n",
    #         "  Files: ",
    #         appendLF = FALSE)
    
    lgl_sub_vis_unique <- 
      lst_vid_subjects_visits %in% subject_visit
    
    fls_sub_vis_unique <- 
      fls_vid_clean[lgl_sub_vis_unique]
    
    # lgl_sub_unique <- 
    #   lst_vid_subject %in% subject
    # 
    # fls_sub_unique <- 
    #   fls_vid_clean[lgl_sub_unique]
    
    for (ii in seq_along(fls_sub_vis_unique)) {
      # for (ii in seq_along(fls_sub_unique)) {
      
      fnm_vid_clean <- 
        fls_sub_vis_unique[ii]
      # fnm_vid_clean <- 
      #   fls_sub_unique[ii]
      
      message(ii, " ",
              appendLF = FALSE)
      
      tib_vid_clean <- suppressMessages(
        paste(fdr_vid_clean,
              fnm_vid_clean,
              sep = "/") %>% 
          vroom(delim = ",",
                progress = FALSE)
      )
      
      if (ii == 1) {
        
        tib_vid_mer <- 
          tib_vid_clean
        
      } else if (ii > 1) {
        
        # left_join(tib_vid_mer,
        #           tib_vid_clean)
        tib_vid_mer <- 
          left_join(
            tib_vid_mer,
            tib_vid_clean,
            by = c("study",
                   "subject",
                   "visit",
                   "datetime",
                   "date",
                   "time")
          )
        
      }
    }
    
    # colnames(tib_vid_mer)
    chr_posture <- 
      tib_vid_mer %>% 
      colnames() %>% 
      str_subset(pattern = "comment|bucket|domain|events|activity|intensity|environment",
                 negate = TRUE) %>%
      str_subset(pattern = "posture")
    chr_behavior <- 
      tib_vid_mer %>% 
      colnames() %>% 
      str_subset(pattern = "comment|bucket|domain|events|activity|intensity|environment",
                 negate = TRUE) %>%
      str_subset(pattern = "behavior")
    
    tib_vid_mer <- 
      tib_vid_mer %>% 
      select(study:time,
             all_of(chr_posture),
             all_of(chr_behavior),
             starts_with("activity"),
             starts_with("behavior_activity"),
             starts_with("intensity"),
             starts_with("posture_domain"),
             starts_with("behavior_domain"),
             starts_with("environment"),
             starts_with("comment"),
             starts_with("posture_bucket"),
             starts_with("behavior_bucket"),
             starts_with("events_posture"),
             starts_with("events_behavior"),
             starts_with("events_behavior_activity"),
             starts_with("events_intensity"),
             starts_with("events_posture_domain"),
             starts_with("events_behavior_domain"),
             starts_with("events_environment")) %>% 
      relocate(contains("raw"),
               .after = last_col())
    
    if (project == "DOINT") {
      
      # Merge clean chamber data as well.
      message(ii + 1,
              appendLF = FALSE)
      
      # fdr_chm_clean <- 
      #   "./Colorado/Chamber data/clean"
      # fls_chm_clean <- 
      #   list.files(path = fdr_chm_clean,
      #              pattern = ".rds")
      fdr_chm_clean <- 
        "./Colorado/UWM_Modified CO DATA/Chamber_RMR_MET_Data_CALC"
      fls_chm_clean <- 
        list.files(path = fdr_chm_clean,
                   pattern = "test.csv")
      
      fnm_chm_clean <- 
        fls_chm_clean %>% 
        str_subset(pattern = subject)
      
      if (rlang::is_empty(fnm_chm_clean)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          "No cleaned chamber data for ", subject,
          call. = FALSE
        )
        
        next()
        
      }
      
      tib_chm_clean <- suppressMessages(
        # readr::read_rds(
        #   file = paste(fdr_chm_clean,
        #                fnm_chm_clean,
        #                sep = "/")
        # ) %>% 
        vroom(
          file = paste(fdr_chm_clean,
                       fnm_chm_clean,
                       sep = "/"),
          delim = ",",
          progress = FALSE
        )) %>%
        mutate(datetime = force_tz(datetime,
                                   tzone = "America/Denver")) %>% 
        select(datetime,
               Chamber_V02_ml_kg_min,
               Chamber_METs
        ) %>% 
        padr::pad(interval = "1 sec",
                  by = "datetime") %>% 
        tidyr::fill(!datetime,
                    .direction = "down")
      
      tib_vid_mer <- 
        tib_vid_mer %>% 
        mutate(
          visit    = as.integer(visit),
          # Double check clean code forces america chicago time on it.
          datetime = with_tz(datetime,
                             tzone = "America/Denver")
        )
      
      tib_vid_mer <- 
        full_join(tib_vid_mer,
                  tib_chm_clean,
                  by = "datetime") %>% 
        arrange(datetime) %>% 
        mutate(
          METs_absolute = Chamber_V02_ml_kg_min / 3.5,
          METs_relative = Chamber_METs,
          intensity_chamber_absolute = case_when(
            METs_absolute < 1.5 & (posture == "Sitting" |
                                     posture == "Lying") ~ "Sedentary",
            METs_absolute < 1.5 & (posture != "Sitting" |
                                     posture != "Lying") ~ "Light",
            METs_absolute >= 1.5 & METs_absolute < 3.0   ~ "Light",
            METs_absolute >= 3.0                         ~ "Mod-Vig"
          ),
          intensity_chamber_relative = case_when(
            METs_relative < 1.5 & (posture == "Sitting" |
                                     posture == "Lying") ~ "Sedentary",
            METs_relative < 1.5 & (posture != "Sitting" |
                                     posture != "Lying") ~ "Light",
            METs_relative >= 1.5 & METs_relative < 3.0   ~ "Light",
            METs_relative >= 3.0                         ~ "Mod-Vig"
          )
        ) %>% 
        tidyr::fill(study:time,
                    .direction = "updown")
      
    }
    
    if (project == "FLAC - Aim 1") {
      
      # Merge clean chamber data as well.
      message(ii + 1,
              appendLF = FALSE)
      
      fdr_chm_clean <- 
        "./Colorado/UWM_Modified CO DATA/Chamber_RMR_MET_Data_CALC"
      fls_chm_clean <- 
        list.files(path = fdr_chm_clean,
                   pattern = ".csv")
      
      fnm_chm_clean <- 
        fls_chm_clean %>% 
        str_subset(pattern = subject)
      
      fnm_chm_clean <- 
        fnm_chm_clean[2]
      
      tib_chm_clean <- 
        readr::read_rds(paste(fdr_chm_clean,
                              fnm_chm_clean,
                              sep = "/")) %>% 
        mutate(subject = as.integer(subject),
               # datetime = time,
               time = format(datetime,
                             "%H:%M:%S")) %>% 
        relocate(time,
                 .after = date)
      
      tib_vid_mer <- 
        tib_vid_mer %>% 
        mutate(
          visit    = as.integer(visit),
          # Double check clean code forces america chicago time on it.
          datetime = with_tz(datetime,
                             tzone = "America/Denver")
        )
      
      tib_vid_mer <- 
        full_join(tib_vid_mer,
                  tib_chm_clean,
                  by = c("study",
                         "subject",
                         "visit",
                         "datetime",
                         "date",
                         "time")) %>% 
        dplyr::arrange(time)
      # relocate(O21:last_col(),
      #          .after = comment_posture)
      
    }
    
    lst_merged_all[[i]] <- 
      tib_vid_mer
    
    fnm_vid_merged <- 
      paste0(study,
             "_",
             subject_visit,
             ".csv")
    
    vroom_write(
      tib_vid_mer,
      path = paste(fdr_vid_merged,
                   fnm_vid_merged,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    
    message("\n",
            "---------------DONE--------------" ,
            appendLF = TRUE)
    
  }
  
  message("\n",
          "Concatenating all merged files...",
          appendLF = FALSE)
  
  tib_vid_mer_all <- 
    bind_rows(lst_merged_all) %>% 
    mutate(across(.cols = c(subject,
                            visit,
                            contains("bucket"),
                            contains("events")),
                  .fns = as.integer)) %>% 
    mutate(across(starts_with("comment"),
                  as.character))
  # tib_vid_mer_all_fct <- 
  #   bind_rows(lst_merged_all) %>% 
  #   mutate(across(.cols = c(subject,
  #                           visit,
  #                           contains("bucket"),
  #                           contains("events")),
  #                 .fns = as.integer)) %>% 
  #   mutate(across(starts_with("comment"),
  #                 as.character)) %>% 
  #   mutate(across(starts_with("posture"), 
  #                 factor,
  #                 levels = lvl_posture)) %>% 
  #   mutate(across(starts_with("behavior"),
  #                 factor,
  #                 levels = lvl_behavior)) %>% 
  #   mutate(across(starts_with("intensity"),
  #                 factor,
  #                 levels = lvl_intensity)) %>% 
  #   mutate(across(starts_with("environment"),
  #                 factor,
  #                 levels = lvl_environment))
  
  message("writing files...",
          appendLF = FALSE)
  
  readr::write_rds(
    tib_vid_mer_all,
    paste(fdr_vid_merged,
          "merged_all.rds",
          sep = "/"),
    compress = "none"
  )
  vroom_write(
    tib_vid_mer_all,
    paste(fdr_vid_merged,
          "merged_all.csv",
          sep = "/"),
    delim = ",",
    progress = FALSE
  )
  # readr::write_rds(
  #   tib_vid_mer_all_fct,
  #   paste(fdr_vid_merged,
  #         "merged_all_factor.rds",
  #         sep = "/"),
  #   compress = "none"
  # )
  # vroom_write(
  #   tib_vid_mer_all_fct,
  #   paste(fdr_vid_merged,
  #         "merged_all_factor.csv",
  #         sep = "/"),
  #   delim = ",",
  #   progress = FALSE
  # )
  
  message(
    "DONE\n",
    "\n",
    "------------------------------MERGING DONE--------------------------------------",
    appendLF = TRUE)
  
}
merge_noldus_v7 <- function(project,
                            fdr_vid_clean,
                            fdr_vid_merged,
                            lvl_behavior,
                            lvl_posture,
                            lvl_intensity,
                            lvl_environment) {
  
  # # CHANGES:
  
  # -Go back to naming function merge_noldus instead of merge_data
  # -Rename clean to shape in ARG and object names.
  # -Have project specific stuff happen in a separate function.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NONE
  # ARG: fdr_chm_shape
  #      File directory of shaped chamber files.
  # ARG: fdr_rmr_shape
  #      File directory of shaped rmr files.
  # ARG: fdr_merge
  #      File directory of merged files.
  
  # # TESTING
  
  # fdr_chm_shape <-
  #   "./FLAC_AIM1_DATA/1_AIM1_RAW_DATA/AIM1_Raw_Chamber"
  # fdr_rmr_shape <-
  #   "./FLAC_AIM1_DATA/2_AIM1_CLEANED_DATA/AIM1_Cleaned_RMR"
  # fdr_merge <-
  #   "./FLAC_AIM1_DATA/4_AIM1_MERGED_DATA/AIM1_Merged_Chamber_RMR"
  fdr_shape <-
    "./3_data/2_shaped"
  # fdr_rmr_shape <-
  #   "./3_data/2_shaped/rmr"
  fdr_merge <-
    "./3_data/3_merged"
  fdr_chm_rmr <- 
    "./3_data/3_merged/chamber_rmr"
  
  # project <-
  #   "DOCOMP"
  # fdr_vid_clean <-
  #   "./3_data/1_cleaned/sbs"
  # fdr_vid_merged <-
  #   "./3_data/1_cleaned/merged"
  
  project <-
    "FLAC - Aim 1"
  # fdr_vid_clean <-
  #   "./Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  # fdr_merged
  # fdr_img_clean
  # fdr_acc_clean
  # 
  # fdr_vid_merged <-
  #   "./Colorado/UWM_Modified CO DATA/DOINT"
  
  # project <-
  #   "FLAC - Aim 1"
  # fdr_vid_clean <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"
  # fdr_vid_merged <-
  #   "Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
  
  # lvl_behavior <-
  #   c("Sports/Exercise",
  #     "Eating/Drinking",
  #     "Transportation",
  #     "Electronics",
  #     "Other - Manipulating Objects",
  #     "Other - Carrying Load w/ UE",
  #     "Other - Pushing Cart",
  #     "Talking - Person",
  #     "Talking - Phone",
  #     "Caring/Grooming - Adult",
  #     "Caring/Grooming - Animal/Pet",
  #     "Caring/Grooming - Child",
  #     "Caring/Grooming - Self",
  #     "Cleaning",
  #     "C/F/R/M",
  #     "Cooking/Meal Preperation",
  #     "Laundry" ,
  #     "Lawn&Garden",
  #     "Leisure Based",
  #     "Only [P/M] Code",
  #     "Talking - Researchers",
  #     "Intermittent Activity",
  #     "Dark/Obscured/OoF")
  # lvl_posture <-
  #   c("Lying",
  #     "Sitting",
  #     "Crouching / Kneeling / Squating",
  #     "Standing",
  #     "Other - Posture",
  #     "Intermittent Posture",
  #     "Walking",
  #     "Stepping",
  #     "Running",
  #     "Ascending Stairs",
  #     "Descending Stairs",
  #     "Crouching / Squating",
  #     "Cycling",
  #     "Other - Movement",
  #     "Intermittent Movement",
  #     "Intermittent P/M",
  #     "Dark/Obscured/OoF")
  # lvl_intensity <-
  #   c("Sedentary",
  #     "Light",
  #     "Mod-Vig",
  #     "Dark/Obscured/OoF")
  # lvl_environment <-
  #   c("Domestic",
  #     "Non-Domestic",
  #     "Errands/Shopping",
  #     "Occupation",
  #     "Organizational/Civic/Religiious",
  #     "Dark/Obscured/OoF")
  
  fld_fls_vid_shape <-
    list.files(path = fdr_shape,
               pattern = ".csv",
               recursive = TRUE,
               ignore.case = TRUE) %>% 
    str_subset(regex(pattern = "noldus",
                     ignore_case = TRUE))
  fld_activity_shape <- 
    fld_fls_vid_shape %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(regex(pattern = "activity",
                     ignore_case = TRUE)) %>% 
    unique()
  fld_posture_shape <- 
    fld_fls_vid_shape %>% 
    str_extract(pattern = "(?:(?!/).)*") %>% 
    str_subset(regex(pattern = "posture",
                     ignore_case = TRUE)) %>% 
    unique()
  fls_vid_shape <- 
    fld_fls_vid_shape %>% 
    str_extract(pattern = "([^/]*)$")
  # fls_obs <-
  #   fls_vid_shape %>%
  #   str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
  #   str_extract(pattern = "([^ - ]*)$")
  study <- 
    fls_vid_shape %>% 
    str_extract(pattern = "(?:(?!_).)*") %>% 
    unique()
  
  if (study == "CO") {
    
    vec_subject_visit <- 
      fls_vid_shape %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]")
    vec_subject_visit_unique <- 
      vec_subject_visit %>% 
      unique()
    
  } else {
    
    vec_subject_visit <- 
      fls_vid_shape %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9][A-z][0-9]")
    vec_subject_visit_unique <- 
      vec_subject_visit %>% 
      unique()
    
  }
  
  lst_merged_all <-
    list()
  
  for (i in seq_along(vec_subject_visit_unique)) {
    
    subject_visit <- 
      vec_subject_visit_unique[i]
    subject <- 
      subject_visit %>% 
      str_sub(start = 1,
              end = 4)
    if (study == "CO") {
      
      visit <- 
        1L
      
    } else {
      
      visit <- 
        subject_visit %>% 
        str_extract(pattern = "([^v]*)$")
      
    }
    
    message("Merging files for subject ", subject, ", visit ", visit, "...", "\n",
            "  Files: ",
            appendLF = FALSE)
    
    lgl_sub_vis_unique <- 
      vec_subject_visit %in% subject_visit
    
    fls_sub_vis_unique <- 
      fls_vid_shape[lgl_sub_vis_unique]
    
    for (ii in seq_along(fls_sub_vis_unique)) {
      
      fnm_vid_shape <- 
        fls_sub_vis_unique[ii]
      
      schema <- 
        fnm_vid_shape %>% 
        str_extract(regex(pattern = "activity|posture",
                          ignore_case = TRUE)) %>% 
        str_to_lower()
      
      message(ii, " ",
              appendLF = FALSE)
      
      switch(
        schema,
        "activity" = {
          tib_vid_shape <- suppressMessages(
            vroom(
              file = paste(fdr_shape,
                           fld_activity_shape,
                           fnm_vid_shape,
                           sep = "/"),
              delim = ",",
              progress = FALSE
            )
          )
        },
        "posture" = {
          tib_vid_shape <- suppressMessages(
            vroom(
              file = paste(fdr_shape,
                           fld_posture_shape,
                           fnm_vid_shape,
                           sep = "/"),
              delim = ",",
              progress = FALSE
            )
          )
        }
      )
      
      if (ii == 1) {
        
        tib_vid_mer <- 
          tib_vid_shape
        
      } else if (ii > 1) {
        
        # left_join(tib_vid_mer,
        #           tib_vid_shape)
        tib_vid_mer <- 
          left_join(
            tib_vid_mer,
            tib_vid_shape,
            by = c("study",
                   "subject",
                   "visit",
                   "datetime",
                   "date",
                   "time")
          )
        
      }
    }
    
    # colnames(tib_vid_mer)
    chr_posture <- 
      tib_vid_mer %>% 
      colnames() %>% 
      str_subset(pattern = "comment|bucket|domain|events|activity|intensity|environment",
                 negate = TRUE) %>%
      str_subset(pattern = "posture")
    chr_behavior <- 
      tib_vid_mer %>% 
      colnames() %>% 
      str_subset(pattern = "comment|bucket|domain|events|activity|intensity|environment",
                 negate = TRUE) %>%
      str_subset(pattern = "behavior")
    
    tib_vid_mer <- 
      tib_vid_mer %>% 
      select(
        study:time,
        all_of(chr_posture),
        all_of(chr_behavior),
        starts_with("activity"),
        # starts_with("behavior_activity"),
        starts_with("intensity"),
        starts_with("posture_domain"),
        starts_with("behavior_domain"),
        starts_with("environment"),
        starts_with("comment")
        # starts_with("posture_bucket"),
        # starts_with("behavior_bucket"),
        # starts_with("events_posture"),
        # starts_with("events_behavior"),
        # starts_with("events_behavior_activity"),
        # starts_with("events_intensity"),
        # starts_with("events_posture_domain"),
        # starts_with("events_behavior_domain"),
        # starts_with("events_environment")
      ) %>% 
      relocate(contains("raw"),
               .after = last_col())
    
    # Writing -----------------------------------------------------------------
    
    lst_merged_all[[i]] <- 
      tib_vid_mer
    
    fld_mer <- 
      list.files(fdr_merge,
                 pattern = "noldus",
                 ignore.case = TRUE) %>% 
      str_subset(regex(pattern = "chamber|rmr",
                       ignore_case = TRUE),
                 negate = TRUE)
    fnm_vid_mer <- 
      paste0(
        study,
        "_",
        subject_visit,
        "_VID.csv"
      )
    fnm_vid_mer_rds <- 
      paste0(
        study,
        "_",
        subject_visit,
        "_VID.rds"
      )
    
    vroom_write(
      tib_vid_mer,
      file = paste(fdr_merge,
                   fld_mer,
                   fnm_vid_mer,
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    readr::write_rds(
      tib_vid_mer,
      file = paste(fdr_merge,
                   fld_mer,
                   fnm_vid_mer_rds,
                   sep = "/"),
      compress = "none"
    )
    
    
    # Aim Specific ------------------------------------------------------------
    
    if (project == "DOINT") {
      
      # Merge shape chamber data as well.
      message(ii + 1,
              appendLF = FALSE)
      
      # fdr_chm_shape <- 
      #   "./Colorado/Chamber data/shape"
      # fls_chm_shape <- 
      #   list.files(path = fdr_chm_shape,
      #              pattern = ".rds")
      fdr_chm_shape <- 
        "./Colorado/UWM_Modified CO DATA/Chamber_RMR_MET_Data_CALC"
      fls_chm_shape <- 
        list.files(path = fdr_chm_shape,
                   pattern = "test.csv")
      
      fnm_chm_shape <- 
        fls_chm_shape %>% 
        str_subset(pattern = subject)
      
      if (rlang::is_empty(fnm_chm_shape)) {
        
        message("",
                appendLF = TRUE)
        
        warning(
          "No shapeed chamber data for ", subject,
          call. = FALSE
        )
        
        next()
        
      }
      
      tib_chm_clean <- suppressMessages(
        # readr::read_rds(
        #   file = paste(fdr_chm_clean,
        #                fnm_chm_clean,
        #                sep = "/")
        # ) %>% 
        vroom(
          file = paste(fdr_chm_clean,
                       fnm_chm_clean,
                       sep = "/"),
          delim = ",",
          progress = FALSE
        )) %>%
        mutate(datetime = force_tz(datetime,
                                   tzone = "America/Denver")) %>% 
        select(datetime,
               Chamber_V02_ml_kg_min,
               Chamber_METs
        ) %>% 
        padr::pad(interval = "1 sec",
                  by = "datetime") %>% 
        tidyr::fill(!datetime,
                    .direction = "down")
      
      tib_vid_mer <- 
        tib_vid_mer %>% 
        mutate(
          visit    = as.integer(visit),
          # Double check clean code forces america chicago time on it.
          datetime = with_tz(datetime,
                             tzone = "America/Denver")
        )
      
      tib_vid_mer <- 
        full_join(tib_vid_mer,
                  tib_chm_clean,
                  by = "datetime") %>% 
        arrange(datetime) %>% 
        mutate(
          METs_absolute = Chamber_V02_ml_kg_min / 3.5,
          METs_relative = Chamber_METs,
          intensity_chamber_absolute = case_when(
            METs_absolute < 1.5 & (posture == "Sitting" |
                                     posture == "Lying") ~ "Sedentary",
            METs_absolute < 1.5 & (posture != "Sitting" |
                                     posture != "Lying") ~ "Light",
            METs_absolute >= 1.5 & METs_absolute < 3.0   ~ "Light",
            METs_absolute >= 3.0                         ~ "Mod-Vig"
          ),
          intensity_chamber_relative = case_when(
            METs_relative < 1.5 & (posture == "Sitting" |
                                     posture == "Lying") ~ "Sedentary",
            METs_relative < 1.5 & (posture != "Sitting" |
                                     posture != "Lying") ~ "Light",
            METs_relative >= 1.5 & METs_relative < 3.0   ~ "Light",
            METs_relative >= 3.0                         ~ "Mod-Vig"
          )
        ) %>% 
        tidyr::fill(study:time,
                    .direction = "updown")
      
    }
    
    if (project == "FLAC - Aim 1") {
      
      # Merge chamber_rmr data as well.
      message(ii + 1,
              appendLF = FALSE)
      
      if (i == 1) {
        
        lst_vid_chm_rmr_all <-
          list()
        
      }
      
      fnm_chm_rmr <- 
        list.files(path = fdr_chm_rmr,
                   pattern = ".rds") %>% 
        str_subset(pattern = subject)
      
      tib_chm_rmr <- 
        readr::read_rds(paste(fdr_chm_rmr,
                              fnm_chm_rmr,
                              sep = "/"))
      # Have to separate it to make sure nrow works in pad function.
      tib_chm_rmr <- 
        tib_chm_rmr %>% 
        mutate(study   = study,
               subject = as.integer(subject),
               visit   = 1,
               .before = 1) %>% 
        # Go from minute level to second level, adding a minute to the end as
        # I am assuming the minute value covers the seconds UP to the next minute.
        padr::pad(interval = "1 sec",
                  by = "datetime",
                  end_val = tib_chm_rmr$datetime[nrow(tib_chm_rmr)] + 59) %>% 
        tidyr::fill(!datetime,
                    .direction = "down")
      #        # datetime = time,
      #        time = format(datetime,
      #                      "%H:%M:%S")) %>% 
      # relocate(time,
      #          .after = date)
      
      tib_vid_chm_rmr <- 
        tib_vid_mer %>% 
        mutate(
          subject  = as.integer(subject),
          visit    = as.integer(visit),
          # Double check clean code forces america chicago time on it.
          datetime = with_tz(datetime,
                             tzone = "America/Denver")
        )
      
      tib_vid_chm_rmr <- 
        full_join(tib_vid_chm_rmr,
                  tib_chm_rmr,
                  by = c("study",
                         "subject",
                         "visit",
                         "datetime")
                  # "date",
                  # "time")
        ) %>% 
        dplyr::arrange(datetime)
      # relocate(O21:last_col(),
      #          .after = comment_posture)
      
      lst_vid_chm_rmr_all[[i]] <- 
        tib_vid_chm_rmr
      
      fld_vid_chm_rmr <- 
        list.files(fdr_merge,
                   pattern = "noldus",
                   ignore.case = TRUE) %>% 
        str_subset(regex(pattern = "chamber|rmr",
                         ignore_case = TRUE),
                   negate = FALSE)
      fnm_vid_chm_rmr <- 
        paste0(
          study,
          "_",
          subject_visit,
          "_VID_CHM_RMR.csv"
        )
      fnm_vid_chm_rmr_rds <- 
        paste0(
          study,
          "_",
          subject_visit,
          "_VID_CHM_RMR.rds"
        )
      
      vroom_write(
        tib_vid_chm_rmr,
        file = paste(fdr_merge,
                     fld_vid_chm_rmr,
                     fnm_vid_chm_rmr,
                     sep = "/"),
        delim = ",",
        progress = FALSE
      )
      readr::write_rds(
        tib_vid_chm_rmr,
        file = paste(fdr_merge,
                     fld_vid_chm_rmr,
                     fnm_vid_chm_rmr_rds,
                     sep = "/"),
        compress = "none"
      )
      
    }
    
    message("\n",
            "---------------DONE--------------" ,
            appendLF = TRUE)
    
  }
  
  # Concatenating -----------------------------------------------------------
  
  message("\n",
          "Concatenating all merged files...",
          appendLF = FALSE)
  
  tib_vid_mer_all <- 
    bind_rows(lst_merged_all) %>% 
    mutate(across(.cols = c(subject,
                            visit,
                            contains("bucket"),
                            contains("events")),
                  .fns = as.integer)) %>% 
    mutate(across(starts_with("comment"),
                  as.character))
  vroom_write(
    tib_vid_mer_all,
    file = paste(fdr_merge,
                 fld_mer,
                 paste0(study, "_ALL_VID.csv"),
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  readr::write_rds(
    tib_vid_mer_all,
    paste(fdr_merge,
          fld_mer,
          paste0(study, "_ALL_VID.rds"),
          sep = "/"),
    compress = "none"
  )
  
  if(project == "FLAC - Aim 1") {
    
    tib_vid_chm_rmr_all <- 
      bind_rows(lst_vid_chm_rmr_all) %>% 
      mutate(across(.cols = c(subject,
                              visit,
                              contains("bucket"),
                              contains("events")),
                    .fns = as.integer)) %>% 
      mutate(across(starts_with("comment"),
                    as.character))
    vroom_write(
      tib_vid_chm_rmr_all,
      file = paste(fdr_merge,
                   fld_vid_chm_rmr,
                   paste0(study, "_ALL_VID_CHM_RMR.csv"),
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    readr::write_rds(
      tib_vid_chm_rmr_all,
      paste(fdr_merge,
            fld_vid_chm_rmr,
            paste0(study, "_ALL_VID_CHM_RMR.rds"),
            sep = "/"),
      compress = "none"
    )
    
  }
  
  # tib_vid_mer_all_fct <- 
  #   bind_rows(lst_merged_all) %>% 
  #   mutate(across(.cols = c(subject,
  #                           visit,
  #                           contains("bucket"),
  #                           contains("events")),
  #                 .fns = as.integer)) %>% 
  #   mutate(across(starts_with("comment"),
  #                 as.character)) %>% 
  #   mutate(across(starts_with("posture"), 
  #                 factor,
  #                 levels = lvl_posture)) %>% 
  #   mutate(across(starts_with("behavior"),
  #                 factor,
  #                 levels = lvl_behavior)) %>% 
  #   mutate(across(starts_with("intensity"),
  #                 factor,
  #                 levels = lvl_intensity)) %>% 
  #   mutate(across(starts_with("environment"),
  #                 factor,
  #                 levels = lvl_environment))
  # readr::write_rds(
  #   tib_vid_mer_all_fct,
  #   paste(fdr_vid_merged,
  #         "merged_all_factor.rds",
  #         sep = "/"),
  #   compress = "none"
  # )
  # vroom_write(
  #   tib_vid_mer_all_fct,
  #   paste(fdr_vid_merged,
  #         "merged_all_factor.csv",
  #         sep = "/"),
  #   delim = ",",
  #   progress = FALSE
  # )
  
  message(
    "DONE\n",
    "\n",
    "------------------------------MERGING DONE--------------------------------------",
    appendLF = TRUE)
  
}
merge_noldus_v8 <- function(project,
                            fdr_vid_clean,
                            fdr_vid_merged,
                            lvl_behavior,
                            lvl_posture,
                            lvl_intensity,
                            lvl_environment) {
  
  # # CHANGES:
  
  # -Go back to naming function merge_noldus instead of merge_data
  # -Rename clean to shape in ARG and object names.
  # -Have project specific stuff happen in a separate function.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: NONE
  # ARG: fdr_chm_shape
  #      File directory of shaped chamber files.
  # ARG: fdr_rmr_shape
  #      File directory of shaped rmr files.
  # ARG: fdr_merge
  #      File directory of merged files.
  
  # # TESTING
  
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "4_AIM1_MERGED_DATA")
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  fsb_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file()
  
  fli_flac_aim <- 
    fdr_read %>% 
    fs::path_dir() %>% 
    str_extract(pattern = "AIM\\d{1}")
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write_nld_act_pos <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex("noldus_activity_posture",
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write_nld_act_pos) {
    # Create a directory under fdr_write that contains "chamber_rmr"
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "noldus_activity_posture" found in WRITE directory.',
          "i" = 'Creating sub directory "NOLDUS_ACTIVITY_POSTURE" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   "NOLDUS_ACTIVITY_POSTURE"))
  } 
  
  vct_fpt_read_act <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "noldus_activity",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*feather")
  vct_fpt_read_pos <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = "noldus_posture",
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*feather")
  
  cli::cli_alert_info("Merging noldus activity & posture files.")
  
  lst_all <- 
    list()
  counter <- 0
  
  for (i in seq_along(vct_fpt_read_act)) {
    
    fpt_read_act <- 
      vct_fpt_read_act[1]
    fnm_read_act <- 
      fs::path_file(fpt_read_act)
    fli <- 
      fnm_read_act %>%
      fs::path_ext_remove() %>% 
      str_split(pattern = "_") %>% 
      vec_unchop() %>% 
      vec_slice(1:3) %>% 
      # paste0(collapse = "_")
      vec_chop() %>% 
      rlang::set_names(nm = c("study",
                              "subject",
                              "visit"))
    
    cli::cli_progress_message(
      "Merging files for pair #{counter + 1}: subject {fli$subject}, visit {fli$visit}"
    )
    
    cli::cli_progress_step(
      msg = "Merging files for pair #{counter + 1}: subject {fli$subject}, visit {fli$visit}...",
      msg_done = "Merging files for pair #{counter + 1}: subject {fli$subject}, visit {fli$visit}...DONE",
      msg_failed = "Merging files for pair #{counter + 1}: subject {fli$subject}, visit {fli$visit}...WARNING"
    )
    fli_std_sbj_vst <- 
      fli %>% 
      vec_unchop() %>% 
      paste0(collapse = "_")
    
    fpt_read_pos <- 
      vct_fpt_read_pos %>% 
      str_subset(pattern = fli_std_sbj_vst)
    
    
    # FIX ME ------------------------------------------------------------------
    # go back to shape_noldus and make subject and visit integers. Reference shape_chamber_v1
    
    df_mer <- 
      left_join(
        arrow::read_feather(fpt_read_act),
        arrow::read_feather(fpt_read_pos),
        by = c("study", "subject", "visit", "datetime", "date", "time")
      ) %>% 
      select(study:time,
             starts_with("behavior"),
             starts_with("activity"),
             starts_with("posture"),
             starts_with("intensity"),
             starts_with("environment"),
             starts_with("duration"),
             starts_with("comment")) %>% 
      as.data.table()
    
    fnm_write <- 
      paste(fli_std_sbj_vst,
            "NOLDUS_ACTIVITY_POSTURE",
            sep = "_") %>% 
      fs::path_ext_set(ext = "csv")
    fpt_write <- 
      fs::path(
        fdr_write,
        str_subset(vct_fsb_write,
                   pattern = regex("noldus_activity_posture",
                                   ignore_case = TRUE)),
        fnm_write
      )
    data.table::fwrite(
      df_mer,
      file = fpt_write,
      sep = ","
    )
    arrow::write_feather(
      df_mer,
      sink = fs::path_ext_set(path = fpt_write,
                              ext = "feather")
    )
    # if (rlang::is_empty(fnm_pos)) {
    #   
    #   fnm_pos_wrong <- 
    #     fnm_chm_raw %>% 
    #     str_extract(pattern = "(?:(?!_).)*") %>% 
    #     str_subset(fls_rmr,
    #                pattern = .)
    #   
    #   if (rlang::is_empty(fnm_rmr_wrong)) {
    #     
    #     warning(
    #       "RMR file for chamber file ", i,": ", fnm_chm_raw, " not found.\n",
    #       'Setting "wt_kg" and "VO2_L_min" to 0.\n',
    #       call. = FALSE
    #     )
    #     
    #   } else {
    #     
    #     warning(
    #       "RMR file #", i,": ", fnm_rmr_wrong, " named incorrectly.\n",
    #       'Make sure RMR file follows "CO####_rmr.xlsx" format\n',
    #       'Setting "wt_kg" and "VO2_L_min" to 0.\n',
    #       call. = FALSE
    #     )
    #     
    #   }
    #   
    #   tib_rmr <- 
    #     tibble(
    #       Body_Wgt_KG       = 0,
    #       RMR_V02_L_Min     = 0,
    #       RMR_V02_ml_kg_min = 0
    #     )
    #   
    # }
    
    counter <- 
      counter + 1
    # message("DONE\n",
    #         appendLF = TRUE)
    # cli::cli_progress_step(
    #   msg = "DONE",
    #   msg_done = "SUCCESS. Cleaned {counter} files out of {length(vct_fpt_read)} files."
    # )
    
    if (counter == 1) {
      lst_all <- 
        df_mer
    } else if (counter > 1) {
      lst_all[[counter]] <- 
        df_mer
    }
    
    
  }
  
  cli::cli_progress_step(
    msg = "DONE",
    msg_done = "SUCCESS. Cleaned {counter} files out of {length(vct_fpt_read)} files."
  )
  
  # Concatenating -----------------------------------------------------------
  
  message("\n",
          "Concatenating all merged files...",
          appendLF = FALSE)
  
  tib_vid_mer_all <- 
    bind_rows(lst_merged_all) %>% 
    mutate(across(.cols = c(subject,
                            visit,
                            contains("bucket"),
                            contains("events")),
                  .fns = as.integer)) %>% 
    mutate(across(starts_with("comment"),
                  as.character))
  vroom_write(
    tib_vid_mer_all,
    file = paste(fdr_merge,
                 fld_mer,
                 paste0(study, "_ALL_VID.csv"),
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  readr::write_rds(
    tib_vid_mer_all,
    paste(fdr_merge,
          fld_mer,
          paste0(study, "_ALL_VID.rds"),
          sep = "/"),
    compress = "none"
  )
  
  if(project == "FLAC - Aim 1") {
    
    tib_vid_chm_rmr_all <- 
      bind_rows(lst_vid_chm_rmr_all) %>% 
      mutate(across(.cols = c(subject,
                              visit,
                              contains("bucket"),
                              contains("events")),
                    .fns = as.integer)) %>% 
      mutate(across(starts_with("comment"),
                    as.character))
    vroom_write(
      tib_vid_chm_rmr_all,
      file = paste(fdr_merge,
                   fld_vid_chm_rmr,
                   paste0(study, "_ALL_VID_CHM_RMR.csv"),
                   sep = "/"),
      delim = ",",
      progress = FALSE
    )
    readr::write_rds(
      tib_vid_chm_rmr_all,
      paste(fdr_merge,
            fld_vid_chm_rmr,
            paste0(study, "_ALL_VID_CHM_RMR.rds"),
            sep = "/"),
      compress = "none"
    )
    
  }
  
  # tib_vid_mer_all_fct <- 
  #   bind_rows(lst_merged_all) %>% 
  #   mutate(across(.cols = c(subject,
  #                           visit,
  #                           contains("bucket"),
  #                           contains("events")),
  #                 .fns = as.integer)) %>% 
  #   mutate(across(starts_with("comment"),
  #                 as.character)) %>% 
  #   mutate(across(starts_with("posture"), 
  #                 factor,
  #                 levels = lvl_posture)) %>% 
  #   mutate(across(starts_with("behavior"),
  #                 factor,
  #                 levels = lvl_behavior)) %>% 
  #   mutate(across(starts_with("intensity"),
  #                 factor,
  #                 levels = lvl_intensity)) %>% 
  #   mutate(across(starts_with("environment"),
  #                 factor,
  #                 levels = lvl_environment))
  # readr::write_rds(
  #   tib_vid_mer_all_fct,
  #   paste(fdr_vid_merged,
  #         "merged_all_factor.rds",
  #         sep = "/"),
  #   compress = "none"
  # )
  # vroom_write(
  #   tib_vid_mer_all_fct,
  #   paste(fdr_vid_merged,
  #         "merged_all_factor.csv",
  #         sep = "/"),
  #   delim = ",",
  #   progress = FALSE
  # )
  
  message(
    "DONE\n",
    "\n",
    "------------------------------MERGING DONE--------------------------------------",
    appendLF = TRUE)
  
}
merge_noldus_chamber_rmr_v1 <- function(fdr_read,
                                        fdr_write,
                                        fdr_project = NULL,
                                        fld_nld = "NOLDUS_ACTIVITY_POSTURE",
                                        fld_chm = "CHAMBER_RMR",
                                        fld_merge = "NOLDUS_CHAMBER_RMR",
                                        filter_sub = NULL,
                                        project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Go back to naming function merge_noldus instead of merge_data.
  # -   Incorporate filter_sub code.
  # -   Incorporate project code.
  # -   Incorporate initiate_wrangle as it is the same across all wrangle functions.
  # -   Have project specific stuff happen in a separate function.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read_noldus
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of shaped noldus files.
  # ARG: fdr_write
  #      File directory of merged noldus files.
  # ARG: fld_act
  #      Folder name of shaped noldus activity files.
  # ARG: fld_pos
  #      Folder name of shaped noldus posture files.
  # ARG: fld_merge
  #      Folder name to save merged activity/posture files to.
  # ARG: fdr_project
  #      File directory to where all the merged data resides in the project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: project_only
  #      Should merged files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "4_AIM1_MERGED_DATA")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "4_AIM1_MERGED_DATA")
  fdr_project <-
    NULL
  fld_nld <- 
    "NOLDUS_ACTIVITY_POSTURE"
  fld_chm <- 
    "CHAMBER_RMR"
  fld_merge <- 
    "NOLDUS_CHAMBER_RMR"
  filter_sub <-
    NULL
  project_only <- FALSE
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   project_only = project_only,
                   type         = "Merg",
                   file_source  = "NOLDUS_ACTIVITY_POSTURE")
  lst_all <- 
    list()
  vct_fpa_read <- 
    get_fpa_read_noldus(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_activity = fld_nld,
      name_posture  = fld_chm,
      name_merged   = fld_merge,
      filter_sub    = filter_sub
    )
  vct_fpa_nld <- 
    vct_fpa_read %>% 
    fs::path_filter(glob = paste0("*/*", fld_nld, "/*")) %>% 
    fs::path_filter(regexp = "ALL",
                    invert = TRUE)
  vct_fpa_chm <- 
    vct_fpa_read %>% 
    fs::path_filter(glob = paste0("*/*", fld_chm, "/*")) %>% 
    fs::path_filter(regexp = "ALL",
                    invert = TRUE)
  info_study <- 
    vct_fpa_read %>% 
    fs::path_file() %>% 
    str_split(pattern = "_") %>% 
    pluck(1, 1)
  
  for (i in cli_progress_along(vct_fpa_nld,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_read <- 
      vct_fpa_nld[1]
    fnm_read <- 
      fs::path_file(fpa_read)
    fpa_chm <- 
      fnm_read %>% 
      fs::path_ext_remove() %>% 
      str_replace(pattern = fld_nld,
                  replacement = fld_chm) %>% 
      str_subset(vct_fpa_chm,
                 pattern = .)
    
    if(is_empty(fpa_chm)) {
      cli_warn(c(
        "File #{i}: {fnm_read}",
        "!" = "No accompanying {fld_chm} file found.",
        "i" = 'No merge file for {fnm_read %>% 
                                  str_split(pattern = "_") %>% 
                                  vec_unchop() %>% 
                                  vec_slice(c(1, 2, 3)) %>% 
                                  paste0(collapse = "_")}'
      ))
      next()
    }
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            MERGE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    df_mer <- 
      left_join(
        arrow::read_feather(fpa_read),
        arrow::read_feather(fpa_chm),
        by = c("study", "subject", "visit", "datetime", "date", "time")
      ) %>% 
      mutate(
        intensity_rmr = 
          fcase(
            mets_rmr < 1.5 & posture_noldus %in% c("sitting", "lying"), "sedentary",
            mets_rmr >= 1.5 & mets_rmr < 3.0, "light",
            mets_rmr >= 3.0, "mvpa"
          ),
        intensity_standard =
          fcase(
            mets_standard < 1.5 & posture_noldus %in% c("sitting", "lying"), "sedentary",
            mets_standard >= 1.5 & mets_standard < 3.0, "light",
            mets_standard >= 3.0, "mvpa"
          ),
        .after = intensity_noldus
      ) %>% 
      as.data.table()
    
    lst_all[[i]] <- 
      df_mer
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            WRITE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fnm_write <- 
      fnm_read %>% 
      str_split(pattern = "_") %>% 
      vec_unchop() %>% 
      vec_slice(c(1, 2, 3)) %>% 
      paste0(collapse = "_") %>% 
      paste(., fld_merge, sep = "_") %>% 
      fs::path_ext_set(ext = "csv")
    
    if (project_only) {
      fpa_project <- 
        fs::path(
          dir_ls(fdr_project,
                 type = "directory",
                 regexp = fld_merge),
          fnm_write
        )
      arrow::write_feather(
        df_mer,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
      next()
    }
    
    fpa_write <- 
      fs::path(
        dir_ls(fdr_write,
               type = "directory",
               regexp = fld_merge),
        fnm_write
      )
    data.table::fwrite(
      df_mer,
      file = fpa_write,
      sep = ",",
      showProgress = FALSE
    )
    arrow::write_feather(
      df_mer,
      sink = fs::path_ext_set(path = fpa_write,
                              ext = "feather")
    )
    
    if(!is_empty(fdr_project)) {
      fpa_project <- 
        fs::path(
          dir_ls(fdr_project,
                 type = "directory",
                 regexp = fld_merge),
          fnm_write
        )
      arrow::write_feather(
        df_mer,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
    }
    
    cnt <- 
      cnt + 1
    
  }
  
  cli_progress_done()
  df_all <- 
    bind_rows(lst_all)
  fnm_all <- 
    paste(
      info_study,
      "ALL",
      fld_merge,
      sep = "_"
    ) %>% 
    fs::path_ext_set(ext = "csv")
  
  if (project_only) {
    fpa_project <- 
      fs::path(
        dir_ls(fdr_project,
               type = "directory",
               regexp = fld_merge),
        fnm_all
      )
    arrow::write_feather(
      df_all,
      sink = fs::path_ext_set(path = fpa_project,
                              ext = "feather")
    )
    return()
  }
  
  fpa_write <- 
    fs::path(
      dir_ls(fdr_write,
             type = "directory",
             regexp = fld_merge),
      fnm_all
    )
  data.table::fwrite(
    df_all,
    file = fpa_write,
    sep = ",",
    showProgress = FALSE
  )
  arrow::write_feather(
    df_all,
    sink = fs::path_ext_set(path = fpa_write,
                            ext = "feather")
  )
  
  if (!is_empty(fdr_project)) {
    fpa_project <- 
      fs::path(
        dir_ls(fdr_project,
               type = "directory",
               regexp = fld_merge),
        fnm_all
      )
    arrow::write_feather(
      df_all,
      sink = fs::path_ext_set(path = fpa_project,
                              ext = "feather")
    )
  }
  
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                            PROCESS FUNCTIONS                            ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_actigraph_raw <- function() {
  
  fdr_read <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA",
             "AIM1_SHAPED_GT3X_LW_CSV_RAW")
  fdr_write <-
    fs::path("FLAC_AIM1_DATA",
             "3_AIM1_SHAPED_DATA")
  ag_model_loc <- "GT3X_LW"
  freq <- 100
  fdr_project <-
    NULL
  vct_subject_filter <- 
    NULL
  
  ag_model <- 
    ag_model_loc %>% 
    str_split(pattern = "_") %>% 
    pluck(1, 1)
  ag_loc <- 
    ag_model_loc %>% 
    str_split(pattern = "_") %>% 
    pluck(1, 2)
  freq <- 
    as.integer(freq)
  fdr_project <- 
    fs::as_fs_path(fdr_project)
  vct_subject_filter <- 
    as.integer(vct_subject_filter)
  
  vct_fsb_write <- 
    fs::dir_ls(
      path        = fdr_write,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = NULL,
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_file() # Actually just gets the base of the path.
  chk_fsb_write <- 
    !any(
      str_detect(vct_fsb_write,
                 pattern = regex(paste0(ag_model_loc, "_CSV_RAW"),
                                 ignore_case = TRUE))
    )
  if (chk_fsb_write) {
    # Create a directory under fdr_write that contains "ag_model_loc"_CSV_RAW
    cli::cli_inform(
      message = 
        c("!" = 'No sub directory with phrase "{ag_model_loc}_CSV_RAW" found in WRITE directory.',
          "i" = 'Creating sub directory "{ag_model_loc}_CSV_RAW" to house activity files.')
    )
    fs::dir_create(path = fs::path(fdr_write,
                                   paste0(ag_model_loc, "_CSV_RAW")))
  } 
  
  vct_fpa_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = TRUE,
      all         = TRUE,
      type        = "file",
      regexp      = paste0(ag_model_loc, "_CSV_RAW"),
      invert      = FALSE,
      fail        = TRUE,
      ignore.case = TRUE
    ) %>% 
    fs::path_filter(glob = "*.feather")
  
  progress_format <- 
    "Shaping {ag_model_loc}_RAW file {cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | [{cli::pb_elapsed}] | {cli::pb_eta_str}"
  
  fpa_read <- 
    vct_fpa_read[33]
  
  test <- 
    arrow::read_feather(fpa_read)
  
  test$axis_x %>% vec_unrep()
  
}
process_duration_files_v3 <- function(vec_source,
                                      fdr_vid_merged,
                                      fdr_vid_processed,
                                      fnm_merged_all,
                                      fnm_duration_rds,
                                      fnm_duration_csv) {
  
  # # CHANGES:
  
  # -Change argument names
  # -Dont use write_duration_type function anymore
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: get_duration_v2
  # ARG: vec_source
  #      Self-explanatory
  # ARG: fdr_vid_merged
  #      File directory of merged files.
  # ARG: fdr_vid_processed
  #      File directory of processed files.
  # ARG: fnm_merged_all
  #      File name of merged file with all subject, visit entries
  # ARG: fnm_duration_rds
  #      File name of processed duration file. Has rds file extension.
  # ARG: fnm_duration_csv
  #      File name of processed duration file. Has csv file extension.
  
  # # TESTING
  
  # vec_source <- 
  #   c("01",
  #     "05",
  #     "10")
  # fdr_vid_merged <- 
  #   "./3_data/1_cleaned/merged"
  # fdr_vid_processed <- 
  #   "./3_data/2_processed"
  # fnm_merged_all <- 
  #   "merged_all.rds"
  # fnm_duration_rds <- 
  #   "duration_all.rds"
  # fnm_duration_csv <-
  #   "duration_all.csv"
  
  tib_vid_mer_all <- 
    readr::read_rds(
      paste(fdr_vid_merged,
            fnm_merged_all,
            sep = "/")
    )
  
  tib_vid_mer_all <- 
    tib_vid_mer_all %>% 
    select(!c(date,
              time,
              starts_with("comment")))
  
  lst_dur_pos <- 
    list()
  lst_dur_beh <- 
    list()
  lst_dur_beh_act <- 
    list()
  lst_dur_pos_dom <- 
    list()
  lst_dur_beh_dom <- 
    list()
  lst_dur_int <- 
    list()
  lst_dur_env <- 
    list()
  
  for (i in seq_along(vec_source)) {
    
    .source <- 
      vec_source[i]
    
    message("Durations for source ", .source, ": ",
            appendLF = TRUE)
    
    tib_vid_source <- 
      tib_vid_mer_all %>% 
      select(subject:datetime, ends_with(.source)) %>% 
      rename_with(str_remove, pattern = paste0("_", .source))
    
    vec_code_type <- 
      c("posture",
        "behavior",
        "behavior_activity",
        "intensity",
        "posture_domain",
        "behavior_domain",
        "environment")
    
    for (ii in seq_along(vec_code_type)) {
      
      code_type <- 
        vec_code_type[ii]
      
      message(code_type, "...",
              appendLF = FALSE)
      
      assign(paste0("tib_dur_", code_type),
             value = get_duration_v2(tib = tib_vid_source,
                                     .code_type = code_type,
                                     .source = .source))
      
    }
    
    lst_dur_pos[[i]] <- 
      tib_dur_posture
    lst_dur_beh[[i]] <- 
      tib_dur_behavior
    lst_dur_beh_act[[i]] <- 
      tib_dur_behavior_activity
    lst_dur_pos_dom[[i]] <- 
      tib_dur_posture_domain
    lst_dur_beh_dom[[i]] <- 
      tib_dur_behavior_domain
    lst_dur_int[[i]] <- 
      tib_dur_intensity
    lst_dur_env[[i]] <- 
      tib_dur_environment
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
  
  lst_duration <- 
    c(lst_dur_pos,
      lst_dur_beh,
      lst_dur_beh_act,
      lst_dur_pos_dom,
      lst_dur_beh_dom,
      lst_dur_int,
      lst_dur_env)
  tib_duration <- 
    lst_duration %>% 
    bind_rows()
  readr::write_rds(
    tib_duration,
    file = paste(fdr_vid_processed,
                 fnm_duration_rds,
                 sep = "/"),
    compress = "none"
  )
  vroom_write(
    tib_duration,
    path = paste(fdr_vid_processed,
                 fnm_duration_csv,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
}
process_duration_files_v4 <- function(vec_source,
                                      fdr_vid_merged,
                                      fdr_vid_processed,
                                      fnm_merged_all,
                                      fnm_duration_rds,
                                      fnm_duration_csv) {
  
  # # CHANGES:
  
  # -Remove "vid" from object names
  # -Change "merged" in object names to "mer"
  # -vec_code_type based off column names.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: get_duration_v3
  # ARG: vec_source
  #      Self-explanatory
  # ARG: fdr_mer
  #      File directory of merged files.
  # ARG: fdr_processed
  #      File directory of processed files.
  # ARG: fnm_mer_all
  #      File name of merged file with all subject, visit entries
  # ARG: fnm_duration_rds
  #      File name of processed duration file. Has rds file extension.
  # ARG: fnm_duration_csv
  #      File name of processed duration file. Has csv file extension.
  
  # # TESTING
  
  # vec_source <-
  #   c("01",
  #     "05",
  #     "10")
  # fdr_mer <-
  #   "./3_data/1_cleaned/merged"
  # fdr_processed <-
  #   "./3_data/2_processed"
  # fnm_mer_all <-
  #   "merged_all.rds"
  # fnm_duration_rds <-
  #   "duration_all.rds"
  # fnm_duration_csv <-
  #   "duration_all.csv"
  
  # tib_mer_all <- 
  #   readr::read_rds(
  #     paste(fdr_mer,
  #           fnm_mer_all,
  #           sep = "/")
  #   )
  
  vec_source <-
    c("vid",
      "img")
  fdr_mer <-
    "./3_data/3_merged"
  fdr_pro <-
    "./3_data/4_processed"
  fnm_mer_all <-
    "merged_all_dowc.rds"
  fnm_duration_rds <-
    "duration_all_dowc.rds"
  fnm_duration_csv <-
    "duration_all_dowc.csv"
  
  
  tib_mer_all <- 
    readr::read_rds(file = paste(fdr_mer,
                                 fnm_mer_all,
                                 sep = "/"))
  
  lst_dur_pos <- 
    list()
  lst_dur_beh <- 
    list()
  # lst_dur_beh_act <- 
  #   list()
  # lst_dur_pos_dom <- 
  #   list()
  # lst_dur_beh_dom <- 
  #   list()
  lst_dur_int <-
    list()
  # lst_dur_env <- 
  #   list()
  
  for (i in seq_along(vec_source)) {
    
    .source <- 
      vec_source[i]
    
    message("Durations for source ", .source, ": ",
            appendLF = TRUE)
    
    tib_source <- 
      tib_mer_all %>% 
      select(1:datetime, ends_with(.source)) %>% 
      rename_with(str_remove, pattern = paste0("_", .source))
    
    vec_code_type <- 
      tib_source %>% 
      select(!1:datetime) %>% 
      colnames()
    # c("posture",
    #   "behavior",
    #   "behavior_activity",
    #   "intensity",
    #   "posture_domain",
    #   "behavior_domain",
    #   "environment")
    
    for (ii in seq_along(vec_code_type)) {
      
      code_type <- 
        vec_code_type[ii]
      
      message(code_type, "...",
              appendLF = FALSE)
      
      assign(paste0("tib_dur_", code_type),
             value = get_duration_v3(tib = tib_source,
                                     .code_type = code_type,
                                     .source = .source))
      
    }
    
    lst_dur_pos[[i]] <- 
      tib_dur_posture
    lst_dur_beh[[i]] <- 
      tib_dur_behavior
    # lst_dur_beh_act[[i]] <- 
    #   tib_dur_behavior_activity
    # lst_dur_pos_dom[[i]] <- 
    #   tib_dur_posture_domain
    # lst_dur_beh_dom[[i]] <- 
    #   tib_dur_behavior_domain
    lst_dur_int[[i]] <-
      tib_dur_intensity
    # lst_dur_env[[i]] <- 
    #   tib_dur_environment
    
    message("DONE\n",
            appendLF = TRUE)
    
  }
  
  lst_duration <- 
    c(
      lst_dur_pos,
      lst_dur_beh,
      # lst_dur_beh_act,
      # lst_dur_pos_dom,
      # lst_dur_beh_dom,
      lst_dur_int
      # lst_dur_env
    )
  tib_duration <- 
    lst_duration %>% 
    bind_rows()
  readr::write_rds(
    tib_duration,
    file = paste(fdr_pro,
                 fnm_duration_rds,
                 sep = "/"),
    compress = "none"
  )
  vroom_write(
    tib_duration,
    path = paste(fdr_pro,
                 fnm_duration_csv,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                           SUMMARIZE FUNCTIONS                           ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
summarize_duration_v3 <- function(sample_lengths,
                                  fdr_vid_merged,
                                  fdr_vid_processed,
                                  fnm_merged_all,
                                  fnm_duration) {
  
  # FROM DOCOMP AS results_duration_v3 - Table 3
  fdr_vid_merged <- 
    "./3_data/1_cleaned/merged"
  fdr_vid_processed <- 
    "./3_data/2_processed"
  fdr_results <-
    "./4_results/2_csv"
  fnm_merged_all <- 
    "merged_all.rds"
  fnm_duration <- 
    "duration_all.rds"
  fnm_results_duration <- 
    paste0("results_duration_", format(Sys.time(), "%F-%H%M"), ".csv")
  
  tib_duration_all <- 
    readr::read_rds(
      file = paste(fdr_vid_processed,
                   fnm_duration,
                   sep = "/")
    ) %>% 
    ungroup()
  
  tib_duration_long <- 
    tib_duration_all %>% 
    select(code_type, code, subject, source, duration)
  
  # Have to make a seperate tibble for each "by" as mean, stddev and median can't
  # just be summed up.
  tib_summary_duration_by_subject <- 
    tib_duration_long %>% 
    group_by(code_type, code, source, subject) %>% 
    summarise(
      frequency = n(),
      sum       = sum(duration),
      mean      = mean(duration),
      stddev    = sd(duration),
      median    = median(duration),
      .groups   = "drop"
    )
  tib_summary_duration_by_code <- 
    tib_duration_long %>% 
    group_by(code_type, code, source) %>% 
    summarise(
      frequency = n(),
      sum       = sum(duration),
      mean      = mean(duration),
      stddev    = sd(duration),
      median    = median(duration),
      .groups   = "drop"
    )
  tib_summary_duration_by_code_type <- 
    tib_duration_long %>% 
    group_by(code_type, source) %>% 
    summarise(
      frequency = n(),
      sum       = sum(duration),
      mean      = mean(duration),
      stddev    = sd(duration),
      median    = median(duration),
      .groups   = "drop"
    )
  
  # Make data presentable
  tbl_summary_duration <- 
    bind_rows(
      tib_summary_duration_by_code_type %>% 
        pivot_wider(names_from = source,
                    values_from = c("frequency",
                                    "sum",
                                    "mean",
                                    "stddev",
                                    "median")) %>% 
        mutate(code    = NA,
               subject = NA,
               .after  = 1),
      tib_summary_duration_by_code %>% 
        pivot_wider(names_from = source,
                    values_from = c("frequency",
                                    "sum",
                                    "mean",
                                    "stddev",
                                    "median")) %>% 
        mutate(subject = NA,
               .after  = code),
      tib_summary_duration_by_subject %>% 
        pivot_wider(names_from = source,
                    values_from = c("frequency",
                                    "sum",
                                    "mean",
                                    "stddev",
                                    "median"))
    ) %>% 
    # Reorder.
    mutate(code_type_id = factor(code_type,
                                 levels = c("posture",
                                            "behavior",
                                            "behavior_activity",
                                            "posture_domain",
                                            "behavior_domain",
                                            "intensity",
                                            "environment")),
           .before       = 1) %>% 
    arrange(code_type_id,
            !is.na(code),
            code,
            !is.na(subject),
            subject) %>% 
    # select(!code_type_id) %>% 
    # Make neat.
    mutate(across(.cols = code:subject,
                  .fns = vec_unfill)) %>% 
    mutate(across(.cols = code_type:subject,
                  .fns = ~ replace_na(.x, replace = ""))) %>% 
    mutate(across(.cols = matches("frequency|sum|mean|stddev|median"),
                  .fns = ~ replace_na(.x, replace = 0)))
  
  tbl_summary_duration_v2 <- 
    tbl_summary_duration %>% 
    mutate(subject = as.integer(subject)) %>% 
    filter(is.na(subject)) %>% 
    filter(code_type != "behavior_activity") %>% 
    select(!subject)
  
  # Taken from Figure 3
  key_code_graphics <- 
    c(
      # Posture
      "lying"                          = "Lying",
      "sitting"                        = "Sitting",
      "crouching/kneeling/squatting"   = "[P] Crouching",
      "standing"                       = "Standing",
      "other - posture"                = "[P] Other",
      "walking"                        = "Walking",
      "stepping"                       = "Stepping",
      "running"                        = "Running",
      "ascending stairs"               = "Ascending Stairs",
      "descending stairs"              = "Descending Stairs",
      "crouching/squatting"            = "[M] Crouching",
      "cycling"                        = "Cycling",
      "other - movement"               = "[M] Other",
      "intermittent posture"           = "[P] Intermittent",
      "intermittent movement"          = "[M] Intermittent",
      "intermittent p/m"               = "[P/M] Intermittent",
      "dark/obscured/oof"              = "Dark/Obscured/OOF",
      # Behavior
      "sports/exercise"                = "Sports/Exercise",
      "eating/drinking"                = "Eating/Drinking",
      "electronics"                    = "Electronics",
      "leisure based"                  = "Leisure Based",
      "talking - person"               = "Talking - Person",
      "talking - phone"                = "Talking - Phone",
      "cleaning"                       = "Cleaning",
      "c/f/r/m"                        = "CFRM",
      "cooking/meal preparation"       = "Cooking/Meal Preparation",
      "laundry"                        = "Laundry",
      "lawn&garden"                    = "Lawn & Garden",
      "caring/grooming - adult"        = "Caring/Grooming - Adult",
      "caring/grooming - animal/pet"   = "Caring/Grooming - Animal/Pet",
      "caring/grooming - child"        = "Caring/Grooming - Child",
      "caring/grooming - self"         = "Caring/Grooming - Self",
      "transportation"                 = "Transportation",
      "other - manipulating objects"   = "Manipulating Objects",
      "other - carrying load w/ ue"    = "Carrying Load w/ UE",
      "other - pushing cart"           = "Pushing Cart",
      "only [p/m] code"                = "Only [P/M] Code",
      "talking - researchers"          = "Talking - Researchers",
      "intermittent activity"          = "Intermittent Activity",
      "dark/obscured/oof"              = "Dark/Obscured/OOF",
      # Posture Domain
      "sit"                            = "Posture - Sedentary",
      "crouching"                      = "Posture - Crouch",
      "stand"                          = "Posture - Stand",
      "posture other"                  = "Posture - Other",
      "movement"                       = "Movement",
      "movement stationary"            = "Movement - Stationary",
      "movement other"                 = "Movement - Other",
      "nca_posture"                    = "No Clear [P/M]",
      "uncoded_posture"                = "Uncoded [P/M]",
      # Behavior Domain
      "sport&exercise"                 = "Sport & Exercise",
      "leisure"                        = "Leisure",
      "household"                      = "Household",
      "caring&grooming"                = "Caring & Grooming",
      "occupation"                     = "Occupation",
      "transportation"                 = "Transportation",
      "other"                          = "Other",
      "nca_behavior"                   = "No Clear Behavior",
      "uncoded_behavior"               = "Uncoded Behavior",
      # Intensity
      "sedentary"                      = "Sedentary",
      "light"                          = "Light",
      "mvpa"                           = "MVPA",
      "dark/obscured/oof_i"              = "Uncoded [P/M]",
      # Environment
      "domestic"                       = "Domestic",
      "non-domestic"                   = "Non Domestic",
      "occupation_e"                     = "Occupational",
      "errands/shopping"               = "Errands/Shopping",
      "social/leisure"                 = "Social/Leisure",
      "organizational/civic/religious" = "Organizational/Civic/Religious",
      "dark/obscured/oof_e"              = "Uncoded Behavior"
    )
  
  lvls_table_3_by_figure_1 <- 
    # Making them in order of schema. First means they appear first.
    c(
      # Posture
      "Lying",
      "Sitting",
      "[P] Crouching",
      "Standing",
      "[P] Other",
      "Walking",
      "Stepping",
      "Running",
      "Ascending Stairs",
      "Descending Stairs",
      "[M] Crouching",
      "Cycling",
      "[M] Other",
      "[P] Intermittent",
      "[M] Intermittent",
      "[P/M] Intermittent",
      # Behavior
      "Sports/Exercise",
      "Eating/Drinking",
      "Electronics",
      "Leisure Based",
      "Talking - Person",
      "Talking - Phone",
      "Cleaning",
      "CFRM",
      "Cooking/Meal Preparation",
      "Laundry",
      "Lawn & Garden",
      "Caring/Grooming - Adult",
      "Caring/Grooming - Animal/Pet",
      "Caring/Grooming - Child",
      "Caring/Grooming - Self",
      "Transportation",
      "Manipulating Objects",
      "Carrying Load w/ UE",
      "Pushing Cart",
      "Only [P/M] Code", # "Only Posture/Movement Code",
      "Talking - Researchers",
      "Intermittent Activity",
      # Intensity
      "Sedentary",                                  # 15
      "Light",                                      # 16
      "MVPA",                                       # 17
      "Uncoded [P/M]",
      # Environment
      "Domestic",
      "Non Domestic",
      "Occupational",
      "Errands/Shopping",                           # 20
      "Social Leisure",
      "Organizational/Civic/Religious",             # 22
      "Uncoded Behavior",
      "Dark/Obscured/OOF"
    )
  
  tbl_summary_duration_v3 <- 
    tbl_summary_duration_v2 %>% 
    filter(str_detect(code_type,
                      pattern = "domain",
                      negate = TRUE)) %>% 
    mutate(
      code_2 = 
        case_when(
          # code_type == "posture_domain" & code == "nca" ~ "nca_posture",
          # code_type == "posture_domain" & code == "uncoded" ~ "uncoded_posture",
          # code_type == "behavior_domain" & code == "nca" ~ "nca_behavior",
          # code_type == "behavior_domain" & code == "uncoded" ~ "uncoded_behavior",
          code_type == "intensity" & code == "dark/obscured/oof" ~ "dark/obscured/oof_i",
          code_type == "environment" & code == "dark/obscured/oof" ~ "dark/obscured/oof_e",
          code_type == "environment" & code == "occupation" ~ "occupation_e",
          TRUE ~ code
        ) %>% 
        recode(!!!key_code_graphics) %>% 
        na_if(""),
      code_type =
        code_type %>% 
        str_to_title(),
      .after = code
    ) %>% 
    filter(!is.na(code_2)) %>% 
    select(!contains("sum"))
  
  tbl_summary_duration_v3 <- 
    tbl_summary_duration_v3 %>% 
    mutate(
      code_2 = 
        code_2 %>% 
        factor(levels = lvls_table_3_by_figure_1)
    ) %>% 
    mutate(across(.cols = where(is.double),
                  .fns = ~ round(.x, digits = 1))) %>% 
    unite(col = mean_01,
          mean_01, stddev_01,
          sep = " \U00B1 ") %>% 
    unite(col = mean_05,
          mean_05, stddev_05,
          sep = " \U00B1 ") %>% 
    unite(col = mean_10,
          mean_10, stddev_10,
          sep = " \U00B1 ")
  
  tbl_summary_duration_v4_schema <- 
    tbl_summary_duration_v3 %>% 
    arrange(code_type_id, code_2) %>% 
    select(!c(code_type_id,
              code)) %>% 
    mutate(
      code_type =
        code_type %>% 
        vec_unfill() %>% 
        replace_na(replace = "")
    )
  
  tbl_summary_duration_v4_amount <- 
    tbl_summary_duration_v3 %>% 
    arrange(code_type_id, -frequency_01, -frequency_05) %>% 
    select(!c(code_type_id,
              code)) %>% 
    mutate(
      code_type =
        code_type %>% 
        vec_unfill() %>% 
        replace_na(replace = "")
    )
  
  fnm_results_duration <-
    paste0("table_3_by_schema_", format(Sys.time(), "%F-%H%M"), ".csv")
  vroom_write(
    tbl_summary_duration_v4_schema,
    path = paste(fdr_results,
                 fnm_results_duration,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  fnm_results_duration <-
    paste0("table_3_by_amount_", format(Sys.time(), "%F-%H%M"), ".csv")
  vroom_write(
    tbl_summary_duration_v4_amount,
    path = paste(fdr_results,
                 fnm_results_duration,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
}
summarize_value_distribution_v1 <- function() {
  
  # FROM DOCOMP AS "Figure 3 Domain Prevelance"
  
  # something something.
  
  fdr_vid_processed <- 
    "./3_data/2_processed"
  fdr_results <-
    "./4_results/2_csv"
  fnm_merged_all <- 
    "merged_all.rds"
  fnm_duration <- 
    "duration_all.rds"
  fnm_results_duration <- 
    paste0("results_duration_", format(Sys.time(), "%F-%H%M"), ".csv")
  
  tib_duration_all <- 
    readr::read_rds(
      file = paste(fdr_vid_processed,
                   fnm_duration,
                   sep = "/")
    ) %>% 
    ungroup()
  
  key_code_graphics <- 
    c(
      # Posture
      "lying"                          = "Lying",
      "sitting"                        = "Sitting",
      "crouching/kneeling/squatting"   = "[P] Crouching",
      "standing"                       = "Standing",
      "other - posture"                = "[P] Other",
      "walking"                        = "Walking",
      "stepping"                       = "Stepping",
      "running"                        = "Running",
      "ascending stairs"               = "Ascending Stairs",
      "descending stairs"              = "Descending Stairs",
      "crouching/squatting"            = "[M] Crouching",
      "cycling"                        = "Cycling",
      "other - movement"               = "[M] Other",
      "intermittent posture"           = "[P] Intermittent",
      "intermittent movement"          = "[M] Intermittent",
      "intermittent p/m"               = "[P/M] Intermittent",
      "dark/obscured/oof"              = "Dark/Obscured/OOF",
      # Behavior
      "sports/exercise"                = "Sports/Exercise",
      "eating/drinking"                = "Eating/Drinking",
      "electronics"                    = "Electronics",
      "leisure based"                  = "Leisure Based",
      "talking - person"               = "Talking - Person",
      "talking - phone"                = "Talking - Phone",
      "cleaning"                       = "Cleaning",
      "c/f/r/m"                        = "CFRM",
      "cooking/meal preparation"       = "Cooking/Meal Preparation",
      "laundry"                        = "Laundry",
      "lawn&garden"                    = "Lawn & Garden",
      "caring/grooming - adult"        = "Caring/Grooming - Adult",
      "caring/grooming - animal/pet"   = "Caring/Grooming - Animal/Pet",
      "caring/grooming - child"        = "Caring/Grooming - Child",
      "caring/grooming - self"         = "Caring/Grooming - Self",
      "transportation"                 = "Transportation",
      "other - manipulating objects"   = "Manipulating Objects",
      "other - carrying load w/ ue"    = "Carrying Load w/ UE",
      "other - pushing cart"           = "Pushing Cart",
      "only [p/m] code"                = "Only [P/M] Code",
      "talking - researchers"          = "Talking - Researchers",
      "intermittent activity"          = "Intermittent Activity",
      "dark/obscured/oof"              = "Dark/Obscured/OOF",
      # Posture Domain
      "sit"                            = "Posture - Sedentary",
      "crouching"                      = "Posture - Crouch",
      "stand"                          = "Posture - Stand",
      "posture other"                  = "Posture - Other",
      "movement"                       = "Movement",
      "movement stationary"            = "Movement - Stationary",
      "movement other"                 = "Movement - Other",
      "nca_posture"                    = "No Clear [P/M]",
      "uncoded_posture"                = "Uncoded [P/M]",
      # Behavior Domain
      "sport&exercise"                 = "Sport & Exercise",
      "leisure"                        = "Leisure",
      "household"                      = "Household",
      "caring&grooming"                = "Caring & Grooming",
      "occupation"                     = "Occupation",
      "transportation"                 = "Transportation",
      "other"                          = "Other",
      "nca_behavior"                   = "No Clear Behavior",
      "uncoded_behavior"               = "Uncoded Behavior",
      # Intensity
      "sedentary"                      = "Sedentary",
      "light"                          = "Light",
      "mvpa"                           = "MVPA",
      "dark/obscured/oof_i"              = "Uncoded [P/M]",
      # Environment
      "domestic"                       = "Domestic",
      "non-domestic"                   = "Non Domestic",
      "occupation_e"                     = "Occupational",
      "errands/shopping"               = "Errands/Shopping",
      "social/leisure"                 = "Social/Leisure",
      "organizational/civic/religious" = "Organizational/Civic/Religious",
      "dark/obscured/oof_e"              = "Uncoded Behavior"
    )
  lvls_figure_3_by_amount <- 
    # Making them in order of how often the codes appear. Last means they appear first.
    c(
      # Posture Domain
      "No Clear [P/M]",
      "Posture - Other",                            # 6
      "Uncoded [P/M]",
      "Movement - Stationary",                      # 5
      "Movement - Other",                           # 7
      "Posture - Crouch",                           # 2
      "Movement",                                   # 4
      "Posture - Sedentary",                        # 1
      "Posture - Stand",                            # 3
      # Behavior Domain
      "Social/Leisure",
      "Occupational",                               # 21
      "Uncoded Behavior",
      "Caring & Grooming",                          # 12
      "Occupation",                                 # 14
      "Sport & Exercise",                           # 8
      "Transportation",                             # 10
      "Other",                                      # 11
      "Household",                                  # 13
      "No Clear Behavior",
      "Leisure",                                    # 9
      # Intensity
      "Sedentary",                                  # 15
      "MVPA",                                       # 17
      "Light",                                      # 16
      # Environment
      "Errands/Shopping",                           # 20
      "Organizational/Civic/Religious",             # 22
      "Non Domestic",                               # 19
      "Domestic"                                    # 18
      # # Posture
      # "Lying",
      # "Sitting",
      # "[P] Crouching",
      # "Standing",
      # "[P] Other",
      # "Intermittent Posture",
      # "Walking",
      # "Stepping",
      # "Running",
      # "Ascending Stairs",
      # "Descending Stairs",
      # "[M] Crouching",
      # "Cycling",
      # "[M] Other",
      # "Intermittent Movement",
      # "Intermittent [P/M]",
      # "Dark/Obscured/OOF",
      # # Behavior
      # "Sports/Exercise",
      # "Eating/Drinking",
      # "Transportation",
      # "Electronics",
      # "Manipulating Objects",
      # "Carrying Load w/ UE",
      # "Pushing Cart",
      # "Talking - Person",
      # "Talking - Phone",
      # "Caring/Grooming - Adult",
      # "Caring/Grooming - Animal/Pet",
      # "Caring/Grooming - Child",
      # "Caring/Grooming - Self",
      # "Cleaning",
      # "CFRM",
      # "Cooking/Meal Preparation",
      # "Laundry",
      # "Lawn & Garden",
      # "Leisure Based",
      # "Only Posture/Movement Code",
      # "Talking - Researchers",
      # "Intermittent Activity",
      # "Dark/Obscured/OOF"
    )
  lvls_figure_3_by_figure_1 <- 
    # Making them in order of Figure 1.
    c(
      # Posture Domain
      "Uncoded [P/M]",
      "No Clear [P/M]",
      "Movement - Other",                           # 7
      "Movement - Stationary",                      # 5
      "Movement",                                   # 4
      "Posture - Other",                            # 6
      "Posture - Stand",                            # 3
      "Posture - Crouch",                           # 2
      "Posture - Sedentary",                        # 1
      # Behavior Domain
      "Uncoded Behavior",
      "No Clear Behavior",
      "Other",                                      # 11
      "Transportation",                             # 10
      "Occupation",                                 # 14
      "Caring & Grooming",                          # 12
      "Household",                                  # 13
      "Leisure",                                    # 9
      "Sport & Exercise",                           # 8
      # Intensity
      "MVPA",                                       # 17
      "Light",                                      # 16
      "Sedentary",                                  # 15
      # Environment
      "Organizational/Civic/Religious",             # 22
      "Social/Leisure",
      "Errands/Shopping",                           # 20
      "Occupational",                               # 21
      "Non Domestic",                               # 19
      "Domestic"                                    # 18
    )
  
  tib_something_something <- 
    tib_duration_all %>% 
    # Create new code that will appear in figures/tables.
    group_by(source, code_type) %>% 
    mutate(
      code_2 = case_when(
        code_type == "posture_domain" & code == "nca" ~ "nca_posture",
        code_type == "posture_domain" & code == "uncoded" ~ "uncoded_posture",
        code_type == "behavior_domain" & code == "nca" ~ "nca_behavior",
        code_type == "behavior_domain" & code == "uncoded" ~ "uncoded_behavior",
        code_type == "intensity" & code == "dark/obscured/oof" ~ "dark/obscured/oof_i",
        code_type == "environment" & code == "dark/obscured/oof" ~ "dark/obscured/oof_e",
        code_type == "environment" & code == "occupation" ~ "occupation_e",
        TRUE ~ code
      ),
      code_2 = recode(code_2, !!!key_code_graphics),
      .after = code
    ) %>% 
    group_by(source, code_type, code_2) %>% 
    summarise(
      total = sum(duration),
      freq = n(),
      .groups = "drop_last"
    ) %>% 
    pivot_wider(names_from = source,
                values_from = c(total, freq)) %>% 
    mutate(total = sum(total_01,
                       na.rm = TRUE),
           total_01_perc = total_01 / total,
           .after = total_01,
           total_01 = replace_na(total_01,
                                 replace = 0),
           total_01_perc = replace_na(total_01_perc,
                                      replace = 0)) %>% 
    ungroup()
  
  # Make into an IF statement later. For now, social/leisure is the only one missing,
  tib_something_something <- 
    tib_something_something %>% 
    add_row(
      code_type = "environment",
      code_2    = "Social/Leisure",
      total_01  = 0,
      total_01_perc = 0,
      .before = 1
    )
  
  tib_something_something <- 
    tib_something_something %>% 
    filter(code_type %in% c("behavior_domain",
                            "posture_domain",
                            "environment",
                            "intensity")) %>%
    mutate(
      code_type = 
        code_type %>%
        str_replace(pattern = "_",
                    replacement = " ") %>% 
        str_to_title(),
      code_type = factor(code_type,
                         levels = c("Posture Domain",
                                    "Behavior Domain",
                                    "Intensity",
                                    "Environment")),
      code_3 = factor(code_2,
                      levels = lvls_figure_3_by_amount),
      .after = code_2
    )
  
  
  # 1024 x 600 pixels
  tib_something_something %>% 
    ggplot() +
    geom_col(mapping = aes(y = code_3,
                           x = total_01 / 60)) +
    geom_text(mapping = aes(x = total_01 / 60,
                            y = code_3,
                            label = scales::percent(total_01_perc,
                                                    accuracy = 0.1)),
              hjust = -0.1) +
    facet_wrap(
      facets = vars(code_type),
      scales = "free",
      drop = TRUE
    ) + 
    scale_x_continuous(expand = expansion(mult = c(0.00, 0.2))) +
    scale_y_discrete(expand = expansion(mult = c(0, 0))) +
    theme_light() + 
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5),
      panel.border=element_blank(),
      # axis.line = element_line(),
      # axis.text.x.bottom = element_blank(),
      axis.ticks.y = element_blank(), # diff
      panel.grid.major.y = element_blank(),
      strip.background = element_rect(fill = "white",
                                      color = NULL),
      strip.text = element_text(color = "black")
    ) +
    labs(
      # title = "% of Total Minutes",
      x = "Minutes",
      # x = "Percentage",
      y = "Code"
    )
  
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                            COMPUTE FUNCTIONS                            ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compute_agreement_v1 <- function() {
  
  fdr_mer <- 
    "./3_data/3_merged"
  fnm_mer_rds <- 
    "merged_all_dowc.rds" # From getting tib_mer_all ready for process_duration_files_v4
  
  tib_mer <- 
    readr::read_rds(
      file = paste(fdr_mer,
                   fnm_mer_rds,
                   sep = "/")
    )
  
  # Remove first row from each subject_visit to treat each entry in tib_mer as a second
  # rather than a time anchor.
  tib_mer <- 
    tib_mer %>% 
    group_by(study, subject, visit) %>% 
    slice(-1) %>% 
    ungroup()
  
  tib_pos <- 
    tib_mer %>% 
    select(starts_with("posture")) %>% 
    rename_with(.cols = everything(),
                .fn   = ~ str_remove(.x,
                                     pattern = "posture_")) %>% 
    mutate(
      # Do this in an earlier version for manuscript by adding bucket column.
      vid = 
        vid %>% 
        recode("sitting"  = 1L,
               "standing"   = 2L,
               "movement"   = 3L,
               "transition" = 4L
               # "uncoded"    = 5L
        ),
      img = 
        img %>% 
        recode("sitting"  = 1L,
               "standing"   = 2L,
               "movement"   = 3L,
               "transition" = 4L
               # "uncoded"    = 5L
        )
    )
  tib_int <- 
    tib_mer %>% 
    select(starts_with("intensity")) %>% 
    rename_with(.cols = everything(),
                .fn   = ~ str_remove(.x,
                                     pattern = "intensity_")) %>% 
    mutate(
      # Do this in an earlier version for manuscript by adding bucket column.
      vid = 
        vid %>% 
        recode(
          # "dark/obscured/oof"    = 0L,
          "sedentary"  = 1L,
          "light"      = 2L,
          "mvpa"       = 3L),
      img = 
        img %>% 
        recode(
          # "uncoded"    = 0L,
          "sedentary"  = 1L,
          "light"      = 2L,
          "mvpa"       = 3L)
    )
  tib_beh <- 
    tib_mer %>% 
    select(starts_with("behavior")) %>% 
    rename_with(.cols = everything(),
                .fn   = ~ str_remove(.x,
                                     pattern = "behavior_")) %>% 
    mutate(
      # Do this in an earlier version for manuscript by adding bucket column.
      vid = 
        vid %>% 
        recode("sport & exercise" = 1L,
               "leisure"          = 2L,
               "transportation"   = 3L,
               "other"            = 4L,
               "caring&grooming"  = 5L,
               "household"        = 6L,
               "occupation"       = 7L,
               "nca"              = 8L,
               "uncoded"          = 9L,
               "errands & social" = 10L),
      img = 
        img %>% 
        recode("sport & exercise" = 1L,
               "leisure"          = 2L,
               "transportation"   = 3L,
               "other"            = 4L,
               "caring&grooming"  = 5L,
               "household"        = 6L,
               "occupation"       = 7L,
               "nca"              = 8L,
               "uncoded"          = 9L,
               "errands & social" = 10L),
    )
  
  irr::agree(tib_pos)
  tib_pos %>% 
    summarise(
      agree = sum(img == vid),
      percent_agreement = # Equal to irr::agree
        (agree / n()) %>% 
        scales::percent(accuracy = 0.1)
    )
  
  icc_variable <- 
    irr::icc(
      tib_pos,
      model = "oneway",
      type = "consistency",
      unit = "single",
      r0 = 0,
      conf.level = 0.95
    )
  tib_icc <- 
    tibble(
      variable = "posture",
      icc      = icc_variable$value,
      lower_ci = icc_variable$lbound,
      upper_ci = icc_variable$ubound
    )
  icc_variable <- 
    irr::icc(
      tib_int,
      model = "oneway",
      type = "consistency",
      unit = "single",
      r0 = 0,
      conf.level = 0.95
    )
  tib_icc <-
    tib_icc %>%
    add_row(
      variable = "intensity",
      icc      = icc_variable$value,
      lower_ci = icc_variable$lbound,
      upper_ci = icc_variable$ubound
    )
  # icc_variable <- 
  #   irr::icc(
  #     tib_beh,
  #     model = "oneway",
  #     type = "consistency",
  #     unit = "single",
  #     r0 = 0,
  #     conf.level = 0.95
  #   )
  # tib_icc <-
  #   tib_icc %>%
  #   add_row(
  #     variable = "behavior",
  #     icc      = icc_variable$value,
  #     lower_ci = icc_variable$lbound,
  #     upper_ci = icc_variable$ubound
  #   )
  
  vroom_write(
    tib_icc,
    path = paste("./4_results",
                 "2_csv",
                 "table_icc.csv",
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
  kappa_variable <- 
    irr::kappa2(tib_pos,
                weight = "equal",
                sort.levels = FALSE)
  tib_kappa <- 
    tibble(
      variable = "posture",
      method  = kappa_variable$method,
      kappa   = kappa_variable$value,
      p_value = kappa_variable$p.value
    )
  kappa_variable <- 
    irr::kappa2(tib_int,
                weight = "equal",
                sort.levels = FALSE)
  tib_kappa <- 
    tib_kappa %>% 
    add_row(
      variable = "intensity",
      method  = kappa_variable$method,
      kappa   = kappa_variable$value,
      p_value = kappa_variable$p.value
    )
  irr::kappa2(tib_beh,
              weight = "unweighted",
              sort.levels = FALSE)
  
  vroom_write(
    tib_kappa,
    path = paste("./4_results",
                 "2_csv",
                 "table_kappa.csv",
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
}
compute_classification_v8 <- function() {
  # FROM DOCOMP table_2_v8
  
  # lowest stratum is "BY DOMAIN CRITERION"
  
  # # CHANGES:
  
  # -Remove events code.
  # -Make "get_domain" functions and implement it.
  # -Remove buckets used for getting domain
  # -For activity files, remove activity column
  # -For posture files, change mod-vig to mvpa
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: vec_unfill
  # ARG: project
  #      Self-explanatory
  # ARG: fdr_vid_raw
  #      File directory of raw files.
  # ARG: fdr_vid_clean
  #      File directory of clean files.
  # ARG: key_behavior_domain
  #      For "key_domain" ARG of get_domain_behavior
  # ARG: key_posture_bucket
  #      For "key_bucket" ARG of get_domain_posture
  # ARG: key_posture_domain
  #      For "key_domain" ARG of get_domain_posture
  
  # # TESTING
  
  criterion <-
    "01"
  estimates <-
    c("05",
      "10")
  vec_code_type <-
    c("posture",
      "behavior",
      "intensity")
  vec_code_type <-
    c("posture",
      "behavior")
  fdr_vid_merged <- 
    "./3_data/1_cleaned/merged"
  fdr_vid_processed <- 
    "./3_data/2_processed"
  fnm_merged_all <- 
    "merged_all.rds"
  
  vec_stratum <- 
    c("OVERALL",
      "BY DOMAIN CRITERION",
      "BY DOMAIN ESTIMATE",
      "BY CODE",
      "BY SUBJECT")
  
  # vec_stratum_names <- 
  #   vec_stratum %>% 
  #   str_remove(pattern = "BY ") %>% 
  #   str_subset(pattern = "OVERALL",
  #              negate = TRUE) %>% 
  #   str_to_lower() %>% 
  #   str_replace_all(pattern = " ",
  #                   replacement = "_")
  
  tib_vid_mer_all <- 
    readr::read_rds(
      paste(fdr_vid_merged,
            fnm_merged_all,
            sep = "/")
    )
  tib_merged <-
    tib_vid_mer_all
  # mutate(posture_domain_05 = if_else(posture_domain_05 == "nca",
  #                                    true = "intermittent",
  #                                    false = posture_domain_05,
  #                                    missing = NULL),
  #        posture_domain_10 = if_else(posture_domain_10 == "nca",
  #                                    true = "intermittent",
  #                                    false = posture_domain_05,
  #                                    missing = NULL))
  
  tib_input_combinations <- 
    expand_grid(
      vec_code_type,
      criterion,
      estimates
    )
  
  lst_classification <- 
    list()
  # lst_tbl_class_all <- 
  #   list()
  
  for (i in seq_len(nrow(tib_input_combinations))) {
    
    combo_number <-
      1
    .criterion <-
      tib_input_combinations$criterion[combo_number]
    .estimate <-
      tib_input_combinations$estimates[combo_number]
    .code_type <-
      tib_input_combinations$vec_code_type[combo_number]
    
    message(
      "----------Getting classification results----------\n",
      "Combination #", combo_number, ": ", paste(.criterion,
                                                 .estimate,
                                                 .code_type,
                                                 sep = "-"), "...",
      appendLF = FALSE
    )
    
    if (.code_type == "intensity") {
      
      .domain_criterion <-
        paste(.code_type,
              .criterion,
              sep = "_")
      .domain_estimate <-
        paste(.code_type,
              .estimate,
              sep = "_")
      .code_criterion <- 
        paste(.code_type,
              .criterion,
              sep = "_")
      .code_estimate <- 
        paste(.code_type,
              .estimate,
              sep = "_")
      
    } else {
      
      .domain_criterion <-
        paste(.code_type,
              "domain",
              .criterion,
              sep = "_")
      .domain_estimate <-
        paste(.code_type,
              "domain",
              .estimate,
              sep = "_")
      .code_criterion <- 
        paste(.code_type,
              .criterion,
              sep = "_")
      .code_estimate <- 
        paste(.code_type,
              .estimate,
              sep = "_")
      
    }
    
    tib_overall <- 
      tib_merged %>% 
      summarise(
        stratum      = "OVERALL",
        code_type    = .code_type,
        criterion    = .criterion,
        estimate     = .estimate,
        total        = n(),
        agree_domain = sum(.data[[.domain_criterion]] == .data[[.domain_estimate]]),
        miss_domain  = sum(.data[[.domain_criterion]] != .data[[.domain_estimate]]),
        agree_code   = sum(.data[[.code_criterion]] == .data[[.code_estimate]]),
        miss_code    = sum(.data[[.code_criterion]] != .data[[.code_estimate]])
      )
    
    tib_count_domain_criterion <- 
      tib_merged %>% 
      group_by(.data[[.domain_criterion]]) %>% 
      summarise(
        total = n(),
        agree_domain = sum(.data[[.domain_criterion]] == .data[[.domain_estimate]]),
        miss_domain  = sum(.data[[.domain_criterion]] != .data[[.domain_estimate]]),
        agree_code   = sum(.data[[.code_criterion]] == .data[[.code_estimate]]),
        miss_code    = sum(.data[[.code_criterion]] != .data[[.code_estimate]]),
        .groups      = "drop"
      ) %>% 
      mutate(total_agree_domain = sum(agree_domain),
             total_miss_domain  = sum(miss_domain),
             total_agree_code   = sum(agree_code),
             total_miss_code    = sum(miss_code),
             stratum = "BY DOMAIN CRITERION",
             .before = 1)
    
    fail_check_1 <- 
      c(tib_overall$agree_domain == tib_tot_agr_miss_domain$total_agree_domain[1],
        tib_overall$miss_domain == tib_tot_agr_miss_domain$total_miss_domain[1],
        tib_overall$agree_code == tib_tot_agr_miss_domain$total_agree_code[1],
        tib_overall$miss_code == tib_tot_agr_miss_domain$total_miss_code[1]) %>% 
      all() == FALSE
    
    if (fail_check_1) {
      
      stop("asognaingfuiansgna")
      
    } else {
      
      tib_count_domain_criterion <- 
        tib_count_domain_criterion %>% 
        select(!contains("total_"))
      
    }
    
    tib_count <- 
      # Bind and rename.
      bind_rows(
        tib_overall,
        tib_count_domain_criterion
      ) %>% 
      rename_with(.cols = contains(paste(.code_type,
                                         "domain",
                                         sep = "_")),
                  .fn = ~ str_replace(.x,
                                      pattern = paste0(.code_type, "_"),
                                      replacement = "")) %>%
      rename_with(.cols = contains(.code_type),
                  .fn = ~ str_replace(.x,
                                      pattern = .code_type,
                                      replacement = "code")) %>% 
      rename_with(.cols = contains(.criterion),
                  .fn = ~ str_replace(.x,
                                      pattern = .criterion,
                                      replacement = "criterion")) %>% 
      rename_with(.cols = contains(.estimate),
                  .fn = ~ str_replace(.x,
                                      pattern = .estimate,
                                      replacement = "estimate")) %>% 
      # Create order columns to arrange the tib by.
      mutate(
        fct_domain_criterion = 
          factor(domain_criterion,
                 levels = lvls_posture_domain) %>% 
          as.integer()
      ) %>%
      # Rearrange.
      arrange(
        !is.na(fct_domain_criterion),
        fct_domain_criterion
      ) %>% 
      # Unfill criterion and estimate columns.
      mutate(across(.cols = contains("fct"),
                    .fns = ~ replace_na(.x,
                                        replace = 0L))) %>% 
      # Reorder and remove unneeded columns.
      select(stratum:estimate,
             domain_criterion,
             # domain_estimate,
             # code_criterion,
             # code_estimate,
             # subject,
             total,
             agree_domain,
             miss_domain,
             agree_code,
             miss_code)
    
    lst_classification[[i]] <- 
      tib_count
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  tib_classification <- 
    bind_rows(lst_classification) %>%
    mutate(
      agree_domain_percent_total = scales::percent(agree_domain / total,
                                                   accuracy = .1,
                                                   prefix = "",
                                                   suffix = "%"),
      agree_code_percent_domain  = scales::percent(agree_code / agree_domain,
                                                   accuracy = .1,
                                                   prefix = "",
                                                   suffix = "%")
    ) %>% 
    mutate(across(.cols = total:miss_code,
                  .fns = function(.x) .x / 60)) %>% 
    mutate(across(.cols = total:miss_code,
                  .fns = ~ round(.x,
                                 digits = 1))) %>% 
    select(
      type           = code_type,
      criterion,
      estimate,
      domain = domain_criterion,
      domain_criterion:subject,
      total,
      domain_agree   = agree_domain,
      percent_total  = agree_domain_percent_total,
      code_agree     = agree_code,
      percent_domain = agree_code_percent_domain
    ) %>% 
    mutate(
      domain = 
        domain %>% 
        replace_na(replace = "overall") %>%
        str_replace(pattern = "&",
                    replacement = " & ") %>% 
        str_to_title(),
      domain = if_else(domain == "Nca",
                       true = "NCA",
                       false = domain,
                       missing = NULL),
      domain = factor(domain,
                      levels = c("Overall", 
                                 "Sit",
                                 "Crouching",
                                 "Stand",
                                 "Posture Other",
                                 "Movement",
                                 "Movement Stationary",
                                 "Movement Other",
                                 "Sport & Exercise",
                                 "Leisure",
                                 "Transportation",
                                 "Other",
                                 "Caring & Grooming",
                                 "Household",
                                 "Occupation",
                                 "NCA",
                                 "Intermittent",
                                 "Uncoded")),
      type = str_to_title(type),
      group_id = 
        estimate %>% 
        vec_fill_missing(direction = "down") %>% 
        seq_events()
    ) %>% 
    mutate(across(.cols = type:estimate,
                  .fns = ~ replace_na(.x,
                                      replace = ""))) %>% 
    group_by(group_id) %>% 
    arrange(domain,
            .by_group = TRUE) %>% 
    ungroup() %>% 
    mutate(group_id = NULL)
  
  colnames(tib_classification) <- 
    colnames(tib_classification) %>% 
    str_replace(pattern = "_",
                replacement = " ") %>% 
    str_replace(pattern = "percent",
                replacement = "%") %>% 
    str_to_title()
  
  vroom_write(
    tib_classification,
    path = "./table_classification_v7.csv",
    delim = ",",
    progress = FALSE
  )
  
  tib_count_xlsx <- 
    tib_count %>% 
    mutate(across(.cols = stratum:domain,
                  # first group but idk by what yet. If I dont it "over" unfills for code_criterion and code_estimate.
                  # DONT HAVE TO WORRY ABOUT THIS ANYMORE. TAKEN CARE OF IN VERSION 7.
                  .fns = ~ vec_unfill(.x))) %>% 
    mutate(across(.cols = everything(),
                  .fns = ~ replace_na(.x,
                                      replace = "")))
  
  get_ind_table_space <- function(tib,
                                  .stratum) {
    
    # tib <-
    #   tib_count
    # .stratum <-
    #   "BY SUBJECT"
    
    ind_1 <- 
      tib$stratum %>% 
      vec_unfill() %>% 
      str_detect(pattern = .stratum) %>% 
      which()
    ind_2 <- 
      (tib$stratum %>% 
         vec_unrep() %>% 
         filter(key == .stratum) %>% 
         pull(times) + ind_1) %>% 
      sort(decreasing = TRUE)
    
    if (ind_2[1] > nrow(tib)) {
      
      # In case .stratum is the last stratum, do not include the last row.
      ind_1 <-
        ind_1[!(ind_1 == nrow(tib))]
      ind_2 <- 
        ind_2[-1]
      
    }
    
    return(ind_2)
    
  }
  
  ind_overall <- 
    get_ind_table_space(tib = tib_count,
                        .stratum = vec_stratum[1])
  ind_by_domain_criterion <- 
    get_ind_table_space(tib = tib_count,
                        .stratum = vec_stratum[2])
  ind_by_domain_estimate <- 
    get_ind_table_space(tib = tib_count,
                        .stratum = vec_stratum[3])
  ind_by_code <- 
    get_ind_table_space(tib = tib_count,
                        .stratum = vec_stratum[4])
  ind_by_subject <- 
    get_ind_table_space(tib = tib_count,
                        .stratum = vec_stratum[5])
  ind_space <- 
    c(ind_overall,
      ind_by_domain_criterion,
      ind_by_domain_estimate,
      ind_by_code,
      ind_by_subject)
  ind_space <- 
    ind_space %>% 
    sort(decreasing = TRUE)
  
  tib_count_format <- 
    tib_count
  # mutate(across(.cols = stratum:subject,
  #               .fns = ~ vec_unfill(.x)))
  
  for (i in seq_along(ind_space)) {
    
    message(i)
    
    tib_count_format <- 
      tib_count_format %>% 
      add_row(.before = ind_space[i])
    
  }
  
  tib_count_format <-
    tib_count_format %>% 
    # mutate(across(.cols = stratum:subject,
    #               .fns = ~ vec_unfill(.x))) %>% 
    mutate(across(.cols = everything(),
                  .fns = ~ replace_na(.x,
                                      replace = "")))
  
  formattable::formattable(
    tib_count_format,
    align = c(rep("l",
                  times = 8),
              rep("r",
                  times = ncol(tib_count_format) - 8))
  )
  
  vroom_write(tib_count2,
              path = "./test6.csv",
              delim = ",",
              progress = FALSE)
  
}
compute_confusion_matrix_v1 <- function() {
  
  # install.packages("janitor")
  library(janitor)
  fdr_mer <- 
    "./3_data/3_merged"
  fnm_mer_rds <- 
    "merged_all_dowc.rds" # From getting tib_mer_all ready for process_duration_files_v4
  
  tib_mer <- 
    readr::read_rds(
      file = paste(fdr_mer,
                   fnm_mer_rds,
                   sep = "/")
    )
  
  # Remove first row from each subject_visit to treat each entry in tib_mer as a second
  # rather than a time anchor.
  tib_mer <- 
    tib_mer %>% 
    group_by(study, subject, visit) %>% 
    slice(-1) %>% 
    ungroup()
  
  tib_pos <-
    tib_mer %>%
    select(starts_with("posture")) %>%
    rename_with(.cols = everything(),
                .fn   = ~ str_remove(.x,
                                     pattern = "posture_"))
  tib_int <-
    tib_mer %>%
    select(starts_with("intensity")) %>%
    rename_with(.cols = everything(),
                .fn   = ~ str_remove(.x,
                                     pattern = "intensity_"))
  tib_beh <- 
    tib_mer %>% 
    select(starts_with("behavior")) %>% 
    rename_with(.cols = everything(),
                .fn   = ~ str_remove(.x,
                                     pattern = "behavior_"))
  
  # tib_pos %>% 
  #   count(vid, img)
  table(tib_pos$vid, tib_pos$img) # Same as below but below is better.
  
  tib_confusion <- 
    tib_pos %>% 
    janitor::tabyl(vid, img) %>% 
    mutate(across(.cols = !vid,
                  .fns = ~ round(.x / 60,
                                 digits = 1))) %>% 
    as_tibble() # So the adorn_ns are based off of minutes.
  tib_confusion <- 
    tib_confusion %>% 
    adorn_totals(where = c("row", "col")) %>% 
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 1) %>% 
    adorn_ns(position = "front") %>% 
    adorn_title(placement = "combined")
  
  vroom_write(
    tib_confusion,
    path = paste("./4_results",
                 "2_csv",
                 "table_confusion_pos_minutes.csv",
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
  tib_confusion <- 
    tib_int %>% 
    janitor::tabyl(vid, img) %>% 
    mutate(across(.cols = !vid,
                  .fns = ~ round(.x / 60,
                                 digits = 1))) %>% 
    as_tibble() # So the adorn_ns are based off of minutes.
  tib_confusion <- 
    tib_confusion %>% 
    adorn_totals(where = c("row", "col")) %>% 
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 1) %>% 
    adorn_ns(position = "front") %>% 
    adorn_title(placement = "combined")
  
  vroom_write(
    tib_confusion,
    path = paste("./4_results",
                 "2_csv",
                 "table_confusion_int_minutes.csv",
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
  tib_confusion <- 
    tib_beh %>% 
    janitor::tabyl(vid, img) %>% 
    mutate(across(.cols = !vid,
                  .fns = ~ round(.x / 60,
                                 digits = 1))) %>% 
    as_tibble() # So the adorn_ns are based off of minutes.
  tib_confusion <- 
    tib_confusion %>% 
    adorn_totals(where = c("row", "col")) %>% 
    adorn_percentages(denominator = "row") %>% 
    adorn_pct_formatting(digits = 1) %>% 
    adorn_ns(position = "front") %>% 
    adorn_title(placement = "combined")
  
  vroom_write(
    tib_confusion,
    path = paste("./4_results",
                 "2_csv",
                 "table_confusion_beh_minutes.csv",
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
}
compute_img_irr_v1 <- function() {
  
  coder_names_tc <- 
    colnames(tib_irr_act) %>% 
    str_subset(pattern = "_TC",
               negate = FALSE)
  # coder_names <- 
  #   colnames(tib_irr_act) %>% 
  #   str_subset(pattern = "_TC|start_time",
  #              negate = TRUE)
  
  len_coder_names <- 
    length(coder_names_tc)
  lst_coder_pairs_tc <- 
    list()
  
  for (ii in seq_along(coder_names_tc)) {
    
    message(ii,
            appendLF = TRUE)
    
    coder_name_tc <- 
      coder_names_tc[ii]
    
    if (ii < len_coder_names) {
      
      coder_pair_tc <- 
        paste(coder_name_tc,
              coder_names_tc[-seq_len(ii)])
      
    } else if (ii == len_coder_names) {
      
      next()
      
    }
    
    lst_coder_pairs_tc[[ii]] <- 
      coder_pair_tc
    
  }
  
  lst_coder_pairs_tc_all <- 
    list()
  
  for (ii in seq_along(coder_names_tc)) {
    
    message(ii,
            appendLF = TRUE)
    
    coder_name_tc <- 
      coder_names_tc[ii]
    
    coder_pair_tc <- 
      paste(coder_name_tc,
            coder_names_tc[-ii])
    
    lst_coder_pairs_tc_all[[ii]] <- 
      coder_pair_tc
    
  }
  
  coder_pairs_tc <- 
    lst_coder_pairs_tc %>% 
    unlist()
  coder_pairs_tc_all <- 
    lst_coder_pairs_tc_all %>% 
    unlist()
  
  coder_pairs <- 
    coder_pairs_tc %>% 
    str_remove_all(pattern = "_TC")
  coder_pairs_all <- 
    coder_pairs_tc_all %>% 
    str_remove_all(pattern = "_TC")
  
  lst_results <- 
    list()
  
  for (ii in seq_along(coder_pairs_tc)) {
    
    coder_pair_tc <- 
      coder_pairs_tc[ii]
    coder_pair <- 
      coder_pairs[ii]
    
    coder_1_tc <- (
      coder_pair_tc %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[1]
    coder_2_tc <- (
      coder_pair_tc %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[2]
    coder_1 <- (
      coder_pair %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[1]
    coder_2 <- (
      coder_pair %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[2]
    
    message("Calculating IRR statistics for ", coder_1, " and ", coder_2, "...",
            appendLF = FALSE)
    
    irr_act_agr <- 
      (tib_irr_act[, coder_1_tc] == tib_irr_act[ , coder_2_tc]) %>% 
      sum() / 
      nrow(tib_irr_act) *
      100
    irr_act_krp <- 
      irr::kripp.alpha(
        t(tib_irr_act[, c(coder_1_tc, coder_2_tc)]), # raters X subjects
        method = "nominal"
      )$value
    irr_act_kpa <- 
      irr::kappa2(
        tib_irr_act[, c(coder_1_tc, coder_2_tc)], # subjects x 2 raters
        weight = "unweighted"
      )$value
    irr_act_icc <- 
      irr::icc(
        tib_irr_act[, c(coder_1_tc, coder_2_tc)], # subjects x raters
        model = "oneway",
        type = "consistency",
        unit = "single"
      )$value
    
    irr_pos_agr <- 
      (tib_irr_pos[, coder_1_tc] == tib_irr_pos[ , coder_2_tc]) %>% 
      sum() / 
      nrow(tib_irr_pos) *
      100
    irr_pos_krp <- 
      irr::kripp.alpha(
        t(tib_irr_pos[, c(coder_1_tc, coder_2_tc)]), # raters X subjects
        method = "nominal"
      )$value
    irr_pos_kpa <- 
      irr::kappa2(
        tib_irr_pos[, c(coder_1_tc, coder_2_tc)], # subjects x 2 raters
        weight = "unweighted"
      )$value
    irr_pos_icc <- 
      irr::icc(
        tib_irr_pos[, c(coder_1_tc, coder_2_tc)], # subjects x raters
        model = "oneway",
        type = "consistency",
        unit = "single"
      )$value
    
    
    tbl_irr_act <- 
      tibble(
        coder_1 = coder_1,
        coder_2 = coder_2,
        schema  = "ACTIVITY",
        percent_agreement   = irr_act_agr,
        unweighted_kappa    = irr_act_kpa,
        krippendorffs_alpha = irr_act_krp,
        icc                 = irr_act_icc,
        .rows = 1
      )
    tbl_irr_pos <- 
      tibble(
        coder_1 = coder_1,
        coder_2 = coder_2,
        schema  = "POSTURE",
        percent_agreement   = irr_pos_agr,
        unweighted_kappa    = irr_pos_kpa,
        krippendorffs_alpha = irr_pos_krp,
        icc                 = irr_pos_icc,
        .rows = 1
      )
    
    lst_results[[ii]] <- 
      bind_rows(tbl_irr_act,
                tbl_irr_pos)
    
    # # Skip the first column since they are the criterion.
    # if (i == 1) {
    #   
    #   next()
    #   
    # }
    # 
    # coder_name_tc <- coder_names_tc[i]
    # coder_name <- coder_names[i]
    # message("Calculating IRR statistics for ", coder_name, "...",
    #         appendLF = FALSE)
    # 
    # irr_act_agr <- 
    #   (tib_irr_act[, "MARTINEZ_TC"] == tib_irr_act[ , coder_name_tc]) %>% 
    #   sum() / 
    #   nrow(tib_irr_act) *
    #   100
    # irr_act_krp <- 
    #   irr::kripp.alpha(
    #     t(tib_irr_act[, c("MARTINEZ_TC", coder_name_tc)]), # raters X subjects
    #     method = "nominal"
    #   )$value
    # irr_act_kpa <- 
    #   irr::kappa2(
    #     tib_irr_act[, c("MARTINEZ_TC", coder_name_tc)], # subjects x 2 raters
    #     weight = "unweighted"
    #   )$value
    # irr_act_icc <- 
    #   irr::icc(
    #     tib_irr_act[, c("MARTINEZ_TC", coder_name_tc)], # subjects x raters
    #     model = "oneway",
    #     type = "consistency",
    #     unit = "single"
    #   )$value
    # 
    # irr_pos_agr <- 
    #   (tib_irr_pos[, "MARTINEZ_TC"] == tib_irr_pos[ , coder_name_tc]) %>% 
    #   sum() / 
    #   nrow(tib_irr_pos) *
    #   100
    # irr_pos_krp <- 
    #   irr::kripp.alpha(
    #     t(tib_irr_pos[, c("MARTINEZ_TC", coder_name_tc)]), # raters X subjects
    #     method = "nominal"
    #   )$value
    # irr_pos_kpa <- 
    #   irr::kappa2(
    #     tib_irr_pos[, c("MARTINEZ_TC", coder_name_tc)], # subjects x 2 raters
    #     weight = "unweighted"
    #   )$value
    # irr_pos_icc <- 
    #   irr::icc(
    #     tib_irr_pos[, c("MARTINEZ_TC", coder_name_tc)], # subjects x raters
    #     model = "oneway",
    #     type = "consistency",
    #     unit = "single"
    #   )$value
    # tbl_irr_act <- 
    #   tibble(
    #     coder  = coder_name,
    #     schema = "ACTIVITY",
    #     percent_agreement   = irr_act_agr,
    #     unweighted_kappa    = irr_act_kpa,
    #     krippendorffs_alpha = irr_act_krp,
    #     icc                 = irr_act_icc,
    #     .rows = 1
    #   )
    # tbl_irr_pos <- 
    #   tibble(
    #     coder  = coder_name,
    #     schema = "POSTURE",
    #     percent_agreement   = irr_pos_agr,
    #     unweighted_kappa    = irr_pos_kpa,
    #     krippendorffs_alpha = irr_pos_krp,
    #     icc                 = irr_pos_icc,
    #     .rows = 1
    #   )
    # if (i == 2) {
    #   
    #   lst_results_act <- list(tbl_irr_act)
    #   lst_results_pos <- list(tbl_irr_pos)
    #   
    # } else if (i > 2) {
    #   
    #   lst_results_act[[i]] <- tbl_irr_act
    #   lst_results_pos[[i]] <- tbl_irr_pos
    #   
    # }
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  tbl_irr_results <- 
    lst_results %>% 
    bind_rows()
  tbl_irr_results <- 
    tbl_irr_results[order(tbl_irr_results$schema), ]
  
  # tbl_irr_results <- 
  #   bind_rows(lst_results_act,
  #             lst_results_pos)
  
  lst_results_all <- 
    list()
  
  for (ii in seq_along(coder_pairs_tc_all)) {
    
    coder_pair_tc <- 
      coder_pairs_tc_all[ii]
    coder_pair <- 
      coder_pairs_all[ii]
    
    coder_1_tc <- (
      coder_pair_tc %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[1]
    coder_2_tc <- (
      coder_pair_tc %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[2]
    coder_1 <- (
      coder_pair %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[1]
    coder_2 <- (
      coder_pair %>% 
        str_split(pattern = " ") %>% 
        unlist()
    )[2]
    
    message("Calculating IRR statistics for ", coder_1, " and ", coder_2, "...",
            appendLF = FALSE)
    
    irr_act_agr <- 
      (tib_irr_act[, coder_1_tc] == tib_irr_act[ , coder_2_tc]) %>% 
      sum() / 
      nrow(tib_irr_act) *
      100
    irr_act_krp <- 
      irr::kripp.alpha(
        t(tib_irr_act[, c(coder_1_tc, coder_2_tc)]), # raters X subjects
        method = "nominal"
      )$value
    irr_act_kpa <- 
      irr::kappa2(
        tib_irr_act[, c(coder_1_tc, coder_2_tc)], # subjects x 2 raters
        weight = "unweighted"
      )$value
    irr_act_icc <- 
      irr::icc(
        tib_irr_act[, c(coder_1_tc, coder_2_tc)], # subjects x raters
        model = "oneway",
        type = "consistency",
        unit = "single"
      )$value
    
    irr_pos_agr <- 
      (tib_irr_pos[, coder_1_tc] == tib_irr_pos[ , coder_2_tc]) %>% 
      sum() / 
      nrow(tib_irr_pos) *
      100
    irr_pos_krp <- 
      irr::kripp.alpha(
        t(tib_irr_pos[, c(coder_1_tc, coder_2_tc)]), # raters X subjects
        method = "nominal"
      )$value
    irr_pos_kpa <- 
      irr::kappa2(
        tib_irr_pos[, c(coder_1_tc, coder_2_tc)], # subjects x 2 raters
        weight = "unweighted"
      )$value
    irr_pos_icc <- 
      irr::icc(
        tib_irr_pos[, c(coder_1_tc, coder_2_tc)], # subjects x raters
        model = "oneway",
        type = "consistency",
        unit = "single"
      )$value
    
    
    tbl_irr_act <- 
      tibble(
        coder_1 = coder_1,
        coder_2 = coder_2,
        schema  = "ACTIVITY",
        percent_agreement   = irr_act_agr,
        unweighted_kappa    = irr_act_kpa,
        krippendorffs_alpha = irr_act_krp,
        icc                 = irr_act_icc,
        .rows = 1
      )
    tbl_irr_pos <- 
      tibble(
        coder_1 = coder_1,
        coder_2 = coder_2,
        schema  = "POSTURE",
        percent_agreement   = irr_pos_agr,
        unweighted_kappa    = irr_pos_kpa,
        krippendorffs_alpha = irr_pos_krp,
        icc                 = irr_pos_icc,
        .rows = 1
      )
    
    lst_results_all[[ii]] <- 
      bind_rows(tbl_irr_act,
                tbl_irr_pos)
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  tbl_irr_results_all <- 
    bind_rows(lst_results_all)
  tbl_irr_results_all <- 
    tbl_irr_results_all[order(tbl_irr_results_all$schema), ]
  
  # Export irr results.
  fnm_results <- 
    paste0("irr_results_",
           str_sub(dte_quality_check,
                   start = 9,
                   end = 18),
           ".csv")
  
  vroom_write(
    tbl_irr_results,
    path = paste(fdr_results,
                 fnm_results,
                 sep = "/"),
    delim = ",",
    progress = FALSE
  )
  
}
compute_img_irr_v2 <- function(tib_mer_schema) {
  
  # # CHANGES:
  
  # -A LOT.
  # -Dependent on tibble from shape_img_irr_v2 function.
  
  # # FUNCTIONS & ARGUMENTS:
  
  # FUNCTION: [NONE]
  # ARG: tib_mer_schema
  #      One of either of Activity or Posture and has "_TC" columns from the 
  #      annotations.
  # ARG: qc_date
  #      Date of image set.
  
  # # TESTING
  # tib_mer_schema <- 
  #   tib_mer_tc_pos
  # qc_date <- 
  #   fld_irr
  
  # # # SEE IF I CAN USE THIS LATER FOR DISQUOTING A SYMBOL.
  # expression(tib_irr_pos)
  # quo(tib_irr_pos)
  # rlang::expr(tib_irr_pos)
  
  vct_names_tc <- 
    colnames(tib_mer_schema) %>% 
    str_subset(pattern = "_TC",
               negate = FALSE)
  tib_pairs_tc <- 
    utils::combn(vct_names_tc,
                 m = 2,
                 simplify = TRUE) %>% 
    t() %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    transmute(coder_1 = V1,
              coder_2 = V2)
  
  # # # Perhaps include an if statement for doing all combinations, not just
  # # # unique ones.
  # tib_pairs_tc <- 
  #   expand_grid(coder_1 = vct_names_tc,
  #               coder_2 = vct_names_tc) %>% 
  #   filter(coder_1 != coder_2)
  
  tib_irr_schema <- 
    tibble(
      coder_1             = character(),
      coder_2             = character(),
      schema              = character(),
      percent_agreement   = double(),
      unweighted_kappa    = double(),
      krippendorffs_alpha = double(),
      icc                 = double()
    )
  
  for (i in vec_seq_along(tib_pairs_tc)) {
    
    coder_1_tc <- 
      tib_pairs_tc$coder_1[i]
    coder_2_tc <- 
      tib_pairs_tc$coder_2[i]
    
    coder_1 <- 
      coder_1_tc %>% 
      str_remove(pattern = "_TC")
    coder_2 <- 
      coder_2_tc %>% 
      str_remove(pattern = "_TC")
    
    message("Calculating IRR statistics for ", coder_1, " and ", coder_2, "...",
            appendLF = FALSE)
    
    irr_schema_agr <- 
      (tib_mer_schema[, coder_1_tc] == tib_mer_schema[ , coder_2_tc]) %>% 
      sum() / 
      nrow(tib_mer_schema) *
      100
    irr_schema_krp <- 
      irr::kripp.alpha(
        t(tib_mer_schema[, c(coder_1_tc, coder_2_tc)]), # raters X subjects
        method = "nominal"
      )$value
    irr_schema_kpa <- 
      irr::kappa2(
        tib_mer_schema[, c(coder_1_tc, coder_2_tc)], # subjects x 2 raters
        weight = "unweighted"
      )$value
    irr_schema_icc <- 
      irr::icc(
        tib_mer_schema[, c(coder_1_tc, coder_2_tc)], # subjects x raters
        model = "oneway",
        type = "consistency",
        unit = "single"
      )$value
    
    tib_irr_schema <- 
      tib_irr_schema %>% 
      bind_rows(
        tibble(
          coder_1             = coder_1,
          coder_2             = coder_2,
          schema              = tib_mer_schema$schema[1],
          percent_agreement   = irr_schema_agr,
          unweighted_kappa    = irr_schema_kpa,
          krippendorffs_alpha = irr_schema_krp,
          icc                 = irr_schema_icc,
          .rows = 1
        )
      )
    
    message("DONE",
            appendLF = TRUE)
    
  }
  
  return(tib_irr_schema)
  
}
# Export irr results.
fnm_results <- 
  paste0("irr_results_",
         str_sub(fld_irr,
                 start = 9,
                 end = 18),
         ".csv")

bind_rows(
  compute_img_irr_v2(tib_mer_schema = tib_mer_tc_act),
  compute_img_irr_v2(tib_mer_schema = tib_mer_tc_pos)
) %>% 
  vroom_write(
    path = paste(fdr_results,
                 fnm_results,
                 sep = "/"),
    delim = ",",
    progress = FALSE
    
  )
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                            GRAPHIC FUNCTIONS                            ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot_irr_v1 <- function() {
  
  fdr_mer <- 
    "./OxfordImageBrowser-win32-x64/6_Training/Quality Check/merged files"
  qc_date <- 
    "2021-10-08"
  fnm_mer <- 
    list.files(path = fdr_mer,
               pattern = qc_date)
  
  tib_mer <- 
    suppressMessages(
      paste(fdr_mer,
            fnm_mer,
            sep = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    ) %>% 
    group_by(schema) %>% 
    mutate(img_number = 
             schema %>% 
             vec_size() %>% 
             seq_len(),
           .after = start_time) %>% 
    ungroup()
  
  
  vct_coder_names <- 
    tib_mer %>% 
    select(!schema:img_number) %>% 
    colnames()
  
  tib_gpt <- 
    tibble(
      schema    = character(),
      coder     = character(),
      code      = character(),
      img_start = integer(),
      img_stop  = integer(),
      img_middle = double(),
      duration  = integer()
    )
  
  for (i in seq_along(vct_coder_names)) {
    
    .coder_name <- 
      vct_coder_names[i]
    
    tib_gpt_coder <- 
      tib_mer %>% 
      select(schema,
             start_time,
             img_number,
             .data[[.coder_name]]) %>% 
      group_by(schema) %>% 
      mutate(
        events = vec_identify_runs(.data[[.coder_name]]),
        duration = seq_duration(.data[[.coder_name]])
      ) %>% 
      group_by(schema,
               events) %>% 
      summarise(
        schema    = schema[1],
        coder     = .coder_name,
        code      = .data[[.coder_name]][1],
        img_start = min(img_number),
        img_stop  = max(img_number) + 1,
        img_middle = (img_stop - img_start) / 2 + img_start,
        duration  = duration[1],
        .groups   = "drop"
      ) %>% 
      mutate(img_stop = as.integer(img_stop))
    
    tib_gpt <- 
      bind_rows(tib_gpt,
                tib_gpt_coder)
    
  }
  
  tib_gpt <- 
    tib_gpt %>% 
    mutate(coder = factor(coder,
                          levels = c("MARTINEZ",
                                     str_subset(unique(coder),
                                                pattern = "MARTINEZ",
                                                negate = TRUE))))
  
  # tib_gpt <- 
  #   tib_mer %>% 
  #   mutate(start_time = 
  #            start_time %>% 
  #            as.character() %>% 
  #            paste("2020-01-01", .) %>% 
  #            ymd_hms()) %>%
  #   group_by(schema) %>% 
  #   mutate(across(.cols = MARTINEZ:last_col(),
  #                 .fns = ~ vec_identify_runs(.x),
  #                 .names = "{.col}_events"),
  #          .before = MARTINEZ) %>% 
  #   mutate(across(.cols = MARTINEZ:last_col(),
  #                 .fns = ~ seq_duration(.x),
  #                 .names = "{.col}_duration"),
  #          .before = MARTINEZ) %>% 
  #   mutate(
  #     # end_time = 
  #     #   start_time %>% 
  #     #   lead(default = start_time[vec_size(start_time)] + 1),
  #     img_number = 
  #       schema %>% 
  #       vec_size() %>% 
  #       seq_len(),
  #     # img_number_end = 
  #     #   img_number %>% 
  #     #   lead(default = vec_size(schema)),
  #     .after = start_time
  #     ) %>% 
  #   ungroup() %>% 
  #   pivot_longer(cols = !(schema:img_number_end),
  #                names_to = "coder",
  #                values_to = "code") %>% 
  #   mutate(coder = factor(coder,
  #                         levels = c("MARTINEZ",
  #                                    str_subset(unique(coder),
  #                                               pattern = "MARTINEZ",
  #                                               negate = TRUE))))
  
  # 1024 x 500
  ggplot(tib_gpt %>%
           filter(schema == "POSTURE")) +
    geom_tile(mapping = aes(x = img_middle,
                            y = coder,
                            width = duration,
                            fill = code),
              height = 0.75,
              # width = 1,
              color = "black"
    ) +
    theme_light() +
    theme(
      # panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y.left = element_text(
        hjust = 1,
        # color = "black",
        # margin = margin(r = 2, unit = "mm"),
        size = 10
      ),
      # axis.ticks.length.y.left = element_blank()
    ) +
    ggplot2::labs(x = "Image Number") +
    scale_x_continuous(
      expand = expansion(mult = c(0.00, 0.05)) # DEFUAULT: c(0.05, 0.05)
    ) +
    scale_y_discrete(
      expand = expansion(add = c(0.45, 0.00)) # DEFAULT: C(0.6, 0.6)
    )
  ggplot(tib_gpt %>%
           filter(schema == "ACTIVITY")) +
    geom_tile(mapping = aes(x = img_middle,
                            y = coder,
                            width = duration,
                            fill = code),
              height = 0.75,
              # width = 1,
              color = "black"
    ) +
    theme_light() +
    theme(
      # panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y.left = element_text(
        hjust = 1,
        # color = "black",
        # margin = margin(r = 2, unit = "mm"),
        size = 10
      ),
      # axis.ticks.length.y.left = element_blank()
    ) +
    ggplot2::labs(x = "Image Number") +
    scale_x_continuous(
      expand = expansion(mult = c(0.00, 0.05)) # DEFUAULT: c(0.05, 0.05)
    ) +
    scale_y_discrete(
      expand = expansion(add = c(0.45, 0.00)) # DEFAULT: C(0.6, 0.6)
    ) +
    ggplot2::scale_fill_brewer(
      type = "seq",
      palette = "Paired"
      # palette = "Set3"
    )
  
  # Can only geom_rect with non0characters
  # geom_rect(mapping = aes(xmin = img_number,
  #                         xmax = img_number_end,
  #                         ymin = as.integer(coder) - 1,
  #                         ymax = as.integer(coder),
  #                         fill = code),
  #           width = 1)
  geom_linerange(mapping = aes(xmin = img_number,
                               xmax = img_number_end,
                               y = coder,
                               color = code),
                 width = 1,
                 size = 10)
  # geom_line(mapping = aes(x = img_number,
  #                         y = coder,
  #                         color = code),
  #           width = 1,
  #           size = 10)
  # geom_segment(mapping = aes(x = img_number,
  #                            xend = img_number_end,
  #                            y = as.integer(coder) - 1,
  #                            yend = as.integer(coder),
  #                            color = code))
  # # DOES NOT WORK.
  # geom_col(mapping = aes(x = img_number,
  #                        y = coder,
  #                        fill = code))
  geom_point(mapping = aes(x = img_number,
                           y = coder,
                           color = code))
  
}

plot_docomp_v1 <- function() {
  
  fdr_vid_merged <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
  fdr_vid_processed <- 
    "./3_data/2_processed"
  
  
  # fdr_vid_merged <- 
  #   "./3_data/1_cleaned/merged"
  # fdr_vid_processed <- 
  #   "./3_data/2_processed"
  
  fls_vid_merged <- 
    list.files(path = fdr_vid_merged,
               pattern = ".csv")
  
  for (i in seq_along(fls_vid_merged)) {
    
    fnm_vid_merged <- fls_vid_merged[1]
    
    tib_vid_merged <- 
      suppressMessages(
        paste(fdr_vid_merged,
              fnm_vid_merged,
              sep = "/") %>% 
          vroom(delim = ",",
                progress = FALSE)
      )
    
  }
  
  tib_vid_merged[, !colnames(tib_vid_merged) %in% c("subject",
                                                    "time")] <- 
    tib_vid_merged[, colnames(tib_vid_merged)[!colnames(tib_vid_merged) %in% c("subject",
                                                                               "time")] %>% 
                     sort()]
  colnames(tib_vid_merged)[!colnames(tib_vid_merged) %in% c("subject",
                                                            "time")] %>% 
    sort()
  order()
  sort()
  
  
  
  # # create a dataset
  # specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
  # condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
  # value <- abs(rnorm(12 , 0 , 15))
  # data <- 
  #   data.frame(specie,
  #              condition,
  #              value)
  # 
  # # Stacked + percent
  # ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  #   geom_bar(position="stack", stat="identity")
  
  gpt_merged <- 
    tib_vid_merged[, str_detect(colnames(tib_vid_merged),
                                pattern = "events",
                                negate = TRUE)]
  
  
  tib_beh_when_pos_miss <- 
    tibble(subject = gpt_merged$subject,
           time = gpt_merged$time,
           sec01_beh_sec05_pos = as.character(NA),
           sec01_beh_sec10_pos = as.character(NA),
           .rows = nrow(gpt_merged))
  
  tib_beh_when_pos_miss$sec01_beh_sec05_pos <- 
    if_else(condition = gpt_merged$sec01_posture != gpt_merged$sec05_posture,
            true = gpt_merged$sec01_behavior,
            false = as.character(NA),
            missing = NULL)
  tib_beh_when_pos_miss$sec01_beh_sec10_pos <- 
    if_else(condition = gpt_merged$sec01_posture != gpt_merged$sec10_posture,
            true = gpt_merged$sec01_behavior,
            false = as.character(NA),
            missing = NULL)
  
  tib_beh_when_pos_miss %>% 
    group_by(sec01_beh_sec05_pos) %>% 
    summarise(n())
  tib_beh_when_pos_miss %>%
    count(sec01_beh_sec05_pos)
  
  df %>% add_count(gender, wt = runs)
  df %>% add_tally(wt = runs)
  
  tall
  is.na(tib_beh_when_pos_miss)
  
  gpt_merged <- 
    gpt_merged %>% 
    reshape2::melt(id.vars = "time",
                   variable.name = "code_source",
                   value.name = "code")
  gpt_merged <- 
    gpt_merged[order(gpt_merged$time), ]
  
  # works
  ggplot(data = gpt_merged) +
    geom_point(mapping = aes(x = time,
                             y = code_source,
                             color = code),
               # show.legend = FALSE
    )
  
  ggplot(data = gpt_merged) +
    geom_col(mapping = aes(x = time,
                           y = code_source,
                           fill = code),
             show.legend = FALSE)
  
  # testing
  ggplot(data = gpt_merged) +
    geom_col(mapping = aes(x = time,
                           y = code_source,
                           fill = code),
             show.legend = FALSE,
             stat = "position")
  ggplot(data = gpt_merged) +
    geom_segment(mapping = aes(x = time,
                               y = code_source,
                               color = code))
  
  test <- 
    tib_vid_merged[, str_detect(colnames(tib_vid_merged),
                                pattern = "events",
                                negate = TRUE)]
  test$time <-
    seq_len(nrow(tib_vid_merged)) %>%
    forcats::as_factor()
  test <- 
    test %>% 
    reshape2::melt(id.vars = "time",
                   variable.name = "code_source",
                   value.name = "code")
  test <- 
    test[order(test$time), ]
  
  ggplot(data = test) +
    geom_col(mapping = aes(x = time,
                           y = code_source,
                           fill = code),
             show.legend = FALSE,
             width = 1)
  # theme(panel.grid.major.x = NULL)
  
  ggplot(data = tib_vid_merged[, str_detect(colnames(tib_vid_merged),
                                            pattern = "behavior|time")]) +
    geom_col(mapping = aes(x = time,
                           y = "sec01_behavior",
                           fill = sec01_behavior),
             position = "stack")
  geom_bar(mapping = aes(x = time,
                         y = sec01_behavior,
                         fill = sec01_behavior),
           stat = "identity")
  geom_ba
  
  tib_vid_merged$sec01_behavior
  
  test
  table(tib_vid_merged$sec01_behavior,
        tib_vid_merged$sec05_behavior) %>% 
    addmargins()
  
  test <- 
    table(tib_vid_merged$sec01_behavior,
          tib_vid_merged$sec05_behavior) %>% 
    prop.table(margin = 1) %>% 
    as.data.frame
  test[order(test$Var1), ]
  
  (tib_vid_merged$sec01_behavior == "[G] Electronics") %>% 
    sum()
  2393/2446
  
  tabl
  prop.table(tib_vid_merged$sec01_behavior,
             tib_vid_merged$sec05_behavior,
             margin = 2)
  rows
  
}

plot_flac_v1 <- function() {
  
  fdr_vid_merged <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
  fdr_vid_processed <- 
    "./3_data/2_processed"
  
  
  # fdr_vid_merged <- 
  #   "./3_data/1_cleaned/merged"
  # fdr_vid_processed <- 
  #   "./3_data/2_processed"
  
  fls_vid_merged <- 
    list.files(path = fdr_vid_merged,
               pattern = ".csv")
  
  # Break down merged files by posture, behavior, sub-behavior, environment and intensity.
  # Merge all into one column to see project wide the distribution of codes.
  tib_posture_each <- 
    tibble(
      second = integer()
    )
  tib_behavior_each <- 
    tibble(
      second = integer()
    )
  tib_intensity_each <- 
    tibble(
      second = integer()
    )
  tib_environment_each <- 
    tibble(
      second = integer()
    )
  
  for (i in seq_along(fls_vid_merged)) {
    
    fnm_vid_merged <- 
      fls_vid_merged[i]
    
    tib_vid_merged <- 
      suppressMessages(
        paste(fdr_vid_merged,
              fnm_vid_merged,
              sep = "/") %>% 
          vroom(delim = ",",
                progress = FALSE)
      )
    
    tib_posture <- 
      tibble(
        second = seq_len(nrow(tib_vid_merged)),
        column = tib_vid_merged$posture
      )
    tib_behavior <- 
      tibble(
        second = seq_len(nrow(tib_vid_merged)),
        column = tib_vid_merged$behavior
      )
    tib_intensity <- 
      tibble(
        second = seq_len(nrow(tib_vid_merged)),
        column = tib_vid_merged$intensity
      )
    tib_environment <- 
      tibble(
        second = seq_len(nrow(tib_vid_merged)),
        column = tib_vid_merged$environment
      )
    
    subject_visit <- 
      fnm_vid_merged %>% 
      str_extract(pattern = "[0-9][0-9][0-9][0-9]V[0-9]")
    
    colnames(tib_posture)[colnames(tib_posture) == "column"] <- 
      subject_visit
    colnames(tib_behavior)[colnames(tib_behavior) == "column"] <- 
      subject_visit
    colnames(tib_intensity)[colnames(tib_intensity) == "column"] <- 
      subject_visit
    colnames(tib_environment)[colnames(tib_environment) == "column"] <- 
      subject_visit
    
    tib_posture_each <- 
      full_join(tib_posture_each,
                tib_posture,
                by = "second")
    tib_behavior_each <- 
      full_join(tib_behavior_each,
                tib_behavior,
                by = "second")
    tib_intensity_each <- 
      full_join(tib_intensity_each,
                tib_intensity,
                by = "second")
    tib_environment_each <- 
      full_join(tib_environment_each,
                tib_environment,
                by = "second")
    
    if (i == 1) {
      
      lst_codes_all <- 
        tibble(posture = tib_vid_merged$posture,
               behavior = tib_vid_merged$behavior,
               intensity = tib_vid_merged$intensity,
               tib_vid_merged$environment) %>% 
        list()
      
      # lst_posture <- 
      #   tib_vid_merged$posture %>% 
      #   tibble(posture = .) %>% 
      #   list()
      # lst_behavior <- 
      #   tib_vid_merged$behavior %>% 
      #   tibble(behavior = .) %>% 
      #   list()
      # lst_intensity <- 
      #   tib_vid_merged$intensity %>% 
      #   tibble(intensity = .) %>% 
      #   list()
      # lst_environment <- 
      #   tib_vid_merged$environment %>% 
      #   tibble(environment = .) %>% 
      #   list()
      
    } else if (i > 1) {
      
      lst_codes_all[[i]] <- 
        tibble(posture = tib_vid_merged$posture,
               behavior = tib_vid_merged$behavior,
               intensity = tib_vid_merged$intensity,
               tib_vid_merged$environment)
      
      # lst_posture[[i]] <- 
      #   tib_vid_merged$posture %>% 
      #   tibble(posture = .)
      # lst_behavior[[i]] <- 
      #   tib_vid_merged$behavior %>% 
      #   tibble(behavior = .)
      # lst_intensity[[i]] <- 
      #   tib_vid_merged$intensity %>% 
      #   tibble(intensity = .)
      # lst_environment[[i]] <- 
      #   tib_vid_merged$environment %>% 
      #   tibble(environment = .)
      
    }
  }
  
  tib_codes_all <- 
    bind_rows(lst_codes_all)
  
  # tib_posture_all <- 
  #   bind_rows(lst_posture)
  # tib_behavior_all <- 
  #   bind_rows(lst_behavior)
  # tib_intensity_all <- 
  #   bind_rows(lst_intensity)
  # tib_environment_all <- 
  #   bind_rows(lst_environment)
  
  gpt_posture_each <- 
    tib_posture_each %>% 
    reshape2::melt(id.vars = "second",
                   variable.name = "code_source",
                   value.name = "code")
  gpt_behavior_each <- 
    tib_behavior_each %>% 
    reshape2::melt(id.vars = "second",
                   variable.name = "code_source",
                   value.name = "code")
  gpt_intensity_each <- 
    tib_intensity_each %>% 
    reshape2::melt(id.vars = "second",
                   variable.name = "code_source",
                   value.name = "code")
  gpt_environment_each <- 
    tib_environment_each %>% 
    reshape2::melt(id.vars = "second",
                   variable.name = "code_source",
                   value.name = "code")
  
  tib_posture_each %>% 
    table()
  
  ggplot(data = gpt_posture_each) +
    geom_col(mapping = aes(x = code_source,
                           y = code,
                           fill = code
    ),
    # position = position_fill()
    )
  ggplot(data = gpt_behavior_each) +
    geom_col(mapping = aes(x = code_source,
                           y = code,
                           fill = code),
             # position = position_fill()
    )
  ggplot(data = gpt_intensity_each) +
    geom_col(mapping = aes(x = code_source,
                           y = code,
                           fill = code),
             # position = position_fill()
    )
  ggplot(data = gpt_environment_each) +
    geom_col(mapping = aes(x = code_source,
                           y = code,
                           fill = code),
             # position = position_fill()
    )
  
  ggplot(data = tib_codes_all) +
    geom_bar(mapping = aes(x = posture,
                           fill = posture))
  ggplot(data = tib_codes_all) +
    geom_bar(mapping = aes(x = behavior,
                           fill = behavior))
  ggplot(data = tib_codes_all) +
    geom_bar(mapping = aes(x = intensity,
                           fill = intensity))
  ggplot(data = tib_codes_all) +
    geom_bar(mapping = aes(x = environment,
                           fill = environment))
  
  # testing
  ggplot(data = gpt_posture_each) +
    geom_col(mapping = aes(x = second,
                           y = code_source,
                           fill = code
    ),
    # position = position_fill()
    )
  
  tib_vid_merged[, !colnames(tib_vid_merged) %in% c("subject",
                                                    "time")] <- 
    tib_vid_merged[, colnames(tib_vid_merged)[!colnames(tib_vid_merged) %in% c("subject",
                                                                               "time")] %>% 
                     sort()]
  colnames(tib_vid_merged)[!colnames(tib_vid_merged) %in% c("subject",
                                                            "time")] %>% 
    sort()
  order()
  sort()
  
  
  
  # # create a dataset
  # specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
  # condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
  # value <- abs(rnorm(12 , 0 , 15))
  # data <- 
  #   data.frame(specie,
  #              condition,
  #              value)
  # 
  # # Stacked + percent
  # ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  #   geom_bar(position="stack", stat="identity")
  
  gpt_merged <- 
    tib_vid_merged[, str_detect(colnames(tib_vid_merged),
                                pattern = "events",
                                negate = TRUE)]
  
  
  tib_beh_when_pos_miss <- 
    tibble(subject = gpt_merged$subject,
           time = gpt_merged$time,
           sec01_beh_sec05_pos = as.character(NA),
           sec01_beh_sec10_pos = as.character(NA),
           .rows = nrow(gpt_merged))
  
  tib_beh_when_pos_miss$sec01_beh_sec05_pos <- 
    if_else(condition = gpt_merged$sec01_posture != gpt_merged$sec05_posture,
            true = gpt_merged$sec01_behavior,
            false = as.character(NA),
            missing = NULL)
  tib_beh_when_pos_miss$sec01_beh_sec10_pos <- 
    if_else(condition = gpt_merged$sec01_posture != gpt_merged$sec10_posture,
            true = gpt_merged$sec01_behavior,
            false = as.character(NA),
            missing = NULL)
  
  tib_beh_when_pos_miss %>% 
    group_by(sec01_beh_sec05_pos) %>% 
    summarise(n())
  tib_beh_when_pos_miss %>%
    count(sec01_beh_sec05_pos)
  
  df %>% add_count(gender, wt = runs)
  df %>% add_tally(wt = runs)
  
  tall
  is.na(tib_beh_when_pos_miss)
  
  gpt_merged <- 
    gpt_merged %>% 
    reshape2::melt(id.vars = "time",
                   variable.name = "code_source",
                   value.name = "code")
  gpt_merged <- 
    gpt_merged[order(gpt_merged$time), ]
  
  # works
  ggplot(data = gpt_merged) +
    geom_point(mapping = aes(x = time,
                             y = code_source,
                             color = code),
               # show.legend = FALSE
    )
  
  ggplot(data = gpt_merged) +
    geom_col(mapping = aes(x = time,
                           y = code_source,
                           fill = code),
             show.legend = FALSE)
  
  # testing
  ggplot(data = gpt_merged) +
    geom_col(mapping = aes(x = time,
                           y = code_source,
                           fill = code),
             show.legend = FALSE,
             stat = "position")
  ggplot(data = gpt_merged) +
    geom_segment(mapping = aes(x = time,
                               y = code_source,
                               color = code))
  
  test <- 
    tib_vid_merged[, str_detect(colnames(tib_vid_merged),
                                pattern = "events",
                                negate = TRUE)]
  test$time <-
    seq_len(nrow(tib_vid_merged)) %>%
    forcats::as_factor()
  test <- 
    test %>% 
    reshape2::melt(id.vars = "time",
                   variable.name = "code_source",
                   value.name = "code")
  test <- 
    test[order(test$time), ]
  
  ggplot(data = test) +
    geom_col(mapping = aes(x = time,
                           y = code_source,
                           fill = code),
             show.legend = FALSE,
             width = 1)
  # theme(panel.grid.major.x = NULL)
  
  ggplot(data = tib_vid_merged[, str_detect(colnames(tib_vid_merged),
                                            pattern = "behavior|time")]) +
    geom_col(mapping = aes(x = time,
                           y = "sec01_behavior",
                           fill = sec01_behavior),
             position = "stack")
  geom_bar(mapping = aes(x = time,
                         y = sec01_behavior,
                         fill = sec01_behavior),
           stat = "identity")
  geom_ba
  
  tib_vid_merged$sec01_behavior
  
  test
  table(tib_vid_merged$sec01_behavior,
        tib_vid_merged$sec05_behavior) %>% 
    addmargins()
  
  test <- 
    table(tib_vid_merged$sec01_behavior,
          tib_vid_merged$sec05_behavior) %>% 
    prop.table(margin = 1) %>% 
    as.data.frame
  test[order(test$Var1), ]
  
  (tib_vid_merged$sec01_behavior == "[G] Electronics") %>% 
    sum()
  2393/2446
  
  tabl
  prop.table(tib_vid_merged$sec01_behavior,
             tib_vid_merged$sec05_behavior,
             margin = 2)
  rows
  
}

plot_chamber_v1 <- function() {
  
  fdr_vid_merged <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
  fdr_vid_processed <- 
    "./3_data/2_processed"
  
  
  # fdr_vid_merged <- 
  #   "./3_data/1_cleaned/merged"
  # fdr_vid_processed <- 
  #   "./3_data/2_processed"
  
  fls_vid_merged <- 
    list.files(path = fdr_vid_merged,
               pattern = ".csv")
  
  
  fnm_vid_merged <- 
    fls_vid_merged[1]
  
  tib_vid_merged <- 
    suppressMessages(
      paste(fdr_vid_merged,
            fnm_vid_merged,
            sep = "/") %>% 
        vroom(delim = ",",
              progress = FALSE)
    )
  
  tib_vid_merged$datetime <- 
    tib_vid_merged$datetime %>% 
    with_tz(tzone = "America/Denver")
  tib_vid_merged$time %>% 
    attributes()
  
  gpt_merged <- 
    tib_vid_merged %>% 
    select(time, posture, num_events_pos, behavior, num_events_act, intensity, O21:kcal_min) %>% 
    dplyr::filter(!(is.na(posture) | is.na(O21)))
  gpt_merged <- 
    tib_vid_merged %>% 
    select(time, posture, behavior, intensity, O21:kcal_min) %>% 
    dplyr::filter(!(is.na(posture) | is.na(O21))) %>% 
    filter(kcal_min > 3)
  
  gpt_merged$time %>% attributes()
  
  sub_interval <-   
    interval(start = ymd_hms("2018-03-16 08:40:00",
                             tz = "America/Denver"),
             end   = ymd_hms("2018-03-16 09:30:00",
                             tz = "America/Denver"))
  gpt_merged <- 
    tib_vid_merged %>% 
    filter(datetime %within% sub_interval)
  tib_start_stop_posture <- 
    gpt_merged %>% 
    transmute(time = time,
              posture = posture,
              start = c(1L, diff(num_events_pos)),
              stop  = c(diff(num_events_pos), 1)) %>% 
    group_by(posture) %>% 
    summarise(start_time = time[start == 1],
              stop_time = time[stop == 1],
              .groups = "keep")
  tib_start_stop_intensity <- 
    gpt_merged %>% 
    transmute(time = time,
              intensity = intensity,
              start = c(1L, diff(num_events_pos)),
              stop  = c(diff(num_events_pos), 1)) %>% 
    group_by(intensity) %>% 
    summarise(start_time = time[start == 1],
              stop_time = time[stop == 1],
              .groups = "keep")
  tib_start_stop_behavior <- 
    gpt_merged %>% 
    transmute(time = time,
              behavior = behavior,
              start = c(1L, diff(num_events_act)),
              stop  = c(diff(num_events_act), 1)) %>% 
    group_by(behavior) %>% 
    summarise(start_time = time[start == 1],
              stop_time = time[stop == 1],
              .groups = "keep")
  tib_label <- 
    tibble(
      time = seconds(gpt_merged$time[1]) - 180,
      y    = c(0, 0.05, 0.1),
      label = c("posture",
                "behavior",
                "intensity")
    )
  
  code_colors <-
    c(
      "[G] Sports/Exercise"              = "red",
      "[G] Eating/Drinking"              = "blue",
      # "[G] Transportation"               = 3L,
      "[G] Electronics"                  = "#00FDEE", # teal
      "[G] Other - Manipulating Objects" = "#00AD1F", # green
      "[G] Other - Carrying Load w/ UE"  = "yellow",
      # "[G] Other - Pushing Cart"         = 4L,
      # "[G] Talking - Person"             = 2L,
      # "[G] Talking - Phone"              = 2L,
      # "[Q] Caring/Grooming - Adult"      = 5L,
      # "[Q] Caring/Grooming - Animal/Pet" = 5L,
      # "[Q] Caring/Grooming - Child"      = 5L,
      "[LQ] Caring Grooming - Self"       = "purple",
      "[LQ] Cleaning"                     = "#FD00F9", # pink
      # "[LQ] C/F/R/M"                      = 6L,
      "[LQ] Cooking/Meal Preperation"     = "orange",
      # "[LQ] Laundry"                      = 6L,
      # "[LQ] Lawn&Garden"                  = 6L,
      # "[LQ] Leisure Based"                = 2L,
      "[T] Only [P/M] Code"              = "#808080", # grey
      # "[T] Talking - Researchers"        = 7L,
      # "[T] Intermittent Activity"        = 7L,
      # "[U] Dark/Obscured/OoF"            = "black",
      # "[P} Lying"                            = 1L,
      "[P] Sitting"                          = "blue",
      # "[P] Crouching / Kneeling / Squating"  = 2L,
      "[P] Standing"                         = "#00AD1F", # green
      # "[P] Other"                            = 4L,
      # "[P] Intermittent Posture"             = 4L,
      "[M] Walking"                          = "red",
      # "[M] Stepping"                         = 5L,
      # "[M] Running"                          = 5L,
      # "[M] Ascending Stairs"                 = 5L,
      # "[M] Descending Stairs"                = 5L,
      # "[M] Crouching / Squating"             = 6L,
      # "[M] Cycling"                          = 6L,
      # "[M] Other"                            = 7L,
      # "[M] Intermittent Movement"            = 7L,
      # "[T] Intermittent P/M"                 = 8L,
      "[U] Dark/Obscured/OoF"                = "black",
      "Sedentary" = "blue",
      "Light"     = "#00AD1F", # green
      "Mod-Vig"   = "red"
    )
  
  ggplot(data = gpt_merged) +
    geom_line(mapping = aes(x = time,
                            y = VO2),
              size = 1) +
    geom_segment(data = tib_start_stop_posture,
                 mapping = aes(x = start_time,
                               y = 0,
                               xend = stop_time,
                               yend = 0,
                               color = posture),
                 size = 5) +
    geom_segment(data = tib_start_stop_behavior,
                 mapping = aes(x = start_time,
                               y = 0.05,
                               xend = stop_time,
                               yend = 0.05,
                               color = behavior),
                 size = 5) +
    geom_segment(data = tib_start_stop_intensity,
                 mapping = aes(x = start_time,
                               y = 0.1,
                               xend = stop_time,
                               yend = 0.1,
                               color = intensity),
                 size = 5) +
    # geom_text(mapping = aes(x = seconds(time[1]) - 120,
    #                         y = 0,
    #                         label = "posture"))
    geom_text(data = tib_label,
              mapping = aes(x = time,
                            y = y,
                            label = label)) +
    scale_color_manual(values = code_colors)
  
}

plot_chamber_v2 <- function() {
  
  fdr_vid_merged <-
    "Colorado/Noldus Observer XT 14/2_Event Logs/2_merge"
  fdr_vid_processed <- 
    "./3_data/2_processed"
  fnm_merged_all <-
    "merged_all.rds"
  fnm_duration_all <- 
    "duration_all.rds"
  
  tib_vid_mer_all <-
    readr::read_rds(
      paste(fdr_vid_merged,
            fnm_merged_all,
            sep = "/")
    )
  # tib_duration_all <- 
  #   readr::read_rds(
  #     paste(fdr_vid_processed,
  #           fnm_duration_all,
  #           sep = "/")
  #   ) %>% 
  #   ungroup()
  
  # tib_duration_all$code_type %>% unique()
  # test <- 
  # tib_duration_all %>% 
  #   filter(subject == 1002,
  #          code_type == "posture")
  # hist(test$duration)
  # tib_duration_all %>% class()
  # ggplot(data = test) +
  #   geom_bar(mapping = aes(x = duration))
  # geom
  # tib_duration_all %>% 
  #   count(subject, sample_length, code_type, code, duration)
  # test <- 
  #   test %>% 
  #   ungroup()
  # 
  # tib_vid_merged %>% 
  #   dplyr::relocate(wher)
  
  # this poo2 works. see if there is a way to make distance between bars smaller.
  # also implement docomp_duration into a way were subsets of time can be picked out.
  # Also see how overlaying works: look at FLAC colorado_graphs code.
  
  get_duration_v2 <- function(tib,
                              begin_time = NULL,
                              end_time = NULL,
                              .code_type = NULL,
                              .subject = NULL) {
    
    tib <- 
      tib_vid_mer_all
    
    tib <- 
      tib %>% 
      rename_with(.fn = ~ paste0(.x, "_vid"),
                  .cols = c(contains("pos"),
                            contains("beh"),
                            contains("activity"),
                            contains("intensity"),
                            contains("environment")))
    
    # begin_time <- 
    #   "11:00:00"
    # end_time <- 
    #   "11:50:00"
    # begin_time <-
    #   "08:40:00"
    # end_time <-
    #   "09:30:00"
    .subject <- 
      1001L
    var_plot <- 
      "VO2"
    
    code_sources <- 
      colnames(tib) %>% 
      str_subset(pattern = "posture") %>% 
      str_extract(pattern = "(?!.*[posture_]).*") %>% 
      unique()
    
    tib_subject <- 
      tib %>% 
      filter(subject == .subject)
    
    tib_long <- 
      full_join(
        tib_subject %>%
          select(1:time,
                 .data[[paste0("posture_", code_sources[1])]]:
                   .data[[paste0("environment_", code_sources[length(code_sources)])]]) %>%
          select(!contains("activity")) %>% 
          pivot_longer(cols = !c((1:time)),
                       names_to = c("code_type",
                                    "code_source"),
                       names_sep = "_(?!.*_)",
                       values_to = "code"),
        tib_subject %>%
          select(1:time,
                 contains("events"))  %>% 
          select(!contains("raw")) %>% 
          rename_with(.fn = ~ str_remove(.x, pattern = "events_"),
                      .cols = !(1:time)) %>%
          pivot_longer(cols = !c((1:time)),
                       names_to = c("code_type",
                                    "code_source"),
                       names_sep = "_(?!.*_)",
                       values_to = "event"),
        by = c("study", "subject", "visit", "datetime", "date", "time",
               "code_type", "code_source")
      ) %>% 
      full_join(
        tib_subject %>% 
          select(1:time,
                 contains(var_plot)),
        by = c("study", "subject", "visit", "datetime", "date", "time")
      ) %>% 
      mutate(time_character = as.character(time))
    
    # tib_mer_long_by_event <- 
    #   full_join(
    #     tib_vid_mer_all %>%
    #       select(!c(contains("raw"),
    #                 contains("bucket"),
    #                 contains("comment"),
    #                 contains("events")
    #       )) %>% 
    #       pivot_longer(cols = !c((1:time)),
    #                    names_to = c("code_type",
    #                                 ".value"),
    #                    names_sep = "_(?!.*_)",
    #                    values_to = "value") %>% 
    #       rename_with(.fn = ~ paste0("code_", .x),
    #                   .cols = !(1:code_type)),
    #     tib_vid_mer_all %>%
    #       select(1:time,
    #              contains("events"))  %>% 
    #       select(!contains("raw")) %>% 
    #       rename_with(.fn = ~ str_remove(.x, pattern = "events_"),
    #                   .cols = !(1:time)) %>% 
    #       pivot_longer(cols = !c((1:time)),
    #                    names_to = c("code_type",
    #                                 ".value"),
    #                    names_sep = "_(?!.*_)",
    #                    values_to = "value") %>% 
    #       rename_with(.fn = ~ paste0("event_", .x),
    #                   .cols = !(1:code_type)),
    #     by = c("study", "subject", "visit", "datetime", "date", "time", "code_type")
    #   )
    
    # tib_long_subject <-  
    #   tib_long %>% 
    #   mutate(time_character = as.character(time),
    #          subject == .subject)
    
    begin_dttm <- (
      tib_long %>% 
        filter(time_character == begin_time) %>% 
        pull(datetime))[1]
    end_dttm <- (
      tib_long %>% 
        filter(time_character == end_time) %>% 
        pull(datetime))[1]
    subset_interval <- 
      lubridate::interval(start = begin_dttm,
                          end =   end_dttm)
    
    statistical_mode <- function(x) {
      ux <- unique(x)
      tab <- tabulate(match(x, ux))
      ux[tab == max(tab)]
    }
    
    tib_duration <- 
      tib_long %>% 
      filter(datetime %within% subset_interval) %>%
      # !(code_type == "activity"), I do this earlier. 
      group_by(code_type, code_source) %>% 
      transmute(
        subject,
        datetime,
        code_source,
        code_type,
        code,
        .data[[var_plot]],
        start = c(1L, diff(event)),
        stop  = c(diff(event), 1L),
        event = event
      ) %>% 
      group_by(subject, code_source, code_type, code, event) %>%
      summarise(
        {{var_plot}} := statistical_mode(.data[[var_plot]]),
        start_dttm  = datetime[start == 1],
        stop_dttm   = datetime[stop == 1] + 1,
        duration      = n(),
        .groups = "drop"
      ) %>% 
      arrange(code_source, code_type, start_dttm) %>% 
      relocate(event,
               .after = last_col())
    
  }
  
  # tib_duration <- 
  #   tib_duration %>% 
  #   mutate(code_factor = factor(code,
  #                               levels = c("Sports/Exercise",
  #                                          "Eating/Drinking",
  #                                          "Transportation",
  #                                          "Electronics",
  #                                          "Other - Manipulating Objects",
  #                                          "Other - Carrying Load w/ UE",
  #                                          "Other - Pushing Cart",
  #                                          "Talking - Person",
  #                                          "Talking - Phone",
  #                                          "Caring/Grooming - Adult",
  #                                          "Caring/Grooming - Animal/Pet",
  #                                          "Caring/Grooming - Child",
  #                                          "Caring/Grooming - Self",
  #                                          "Cleaning",
  #                                          "C/F/R/M",
  #                                          "Cooking/Meal Preperation",
  #                                          "Laundry" ,
  #                                          "Lawn&Garden",
  #                                          "Leisure Based",
  #                                          "Only [P/M] Code",
  #                                          "Talking - Researchers",
  #                                          "Intermittent Activity",
  #                                          "Lying",
  #                                          "Sitting",
  #                                          "Crouching / Kneeling / Squating",
  #                                          "Standing",
  #                                          "Other - Posture",
  #                                          "Intermittent Posture",
  #                                          "Walking",
  #                                          "Stepping",
  #                                          "Running",
  #                                          "Ascending Stairs",
  #                                          "Descending Stairs",
  #                                          "Crouching / Squating",
  #                                          "Cycling",
  #                                          "Other - Movement",
  #                                          "Intermittent Movement",
  #                                          "Intermittent P/M",
  #                                          "Sedentary",
  #                                          "Light",
  #                                          "Mod-Vig",
  #                                          "Dark/Obscured/OoF")),
  #          .after = code)
  # lvls_environment <-
  #   c("Domestic",
  #     "Non-Domestic",
  #     "Errands/Shopping",
  #     "Occupation",
  #     "Organizational/Civic/Religiious",
  #     "Dark/Obscured/OoF")
  
  tib_duration <- 
    tib_duration %>% 
    mutate(
      code_label = recode(code,
                          "Sport&Exercise"                   = "[2] Sport&Exercise",
                          "Leisure"                         = "[2] Leisure",
                          "Transportation"                  = "[2] Transportation",
                          "Other"                           = "[2] Other",
                          "Caring&Grooming"                 = "[2] Caring&Grooming",
                          "Household"                       = "[2] Household",
                          "Transition"                      = "[2] Transition",
                          "Uncoded"                         = "[2] Uncoded",
                          "Sedentary"                        = "[1] Sedentary",
                          "Light"                            = "[1] Light",
                          "Mod-Vig"                          = "[1] Mod-Vig",
                          "Crouching"                        = "[3] Crouching",
                          "Movement"                         = "[3] Movement",
                          "Posture Other"                    = "[3] Posture Other",
                          "Sit"                              = "[3] Sit",
                          "Stand"                            = "[3] Stand",
                          "Lying"                            = "[3] Lying",
                          "Sitting"                          = "[P] Sitting",
                          "Crouching / Kneeling / Squating"  = "[P] Crouching / Kneeling / Squating",
                          "Standing"                         = "[P] Standing",
                          "Other - Posture"                  = "[P] Other - Posture",
                          "Intermittent Posture"             = "[P] Intermittent Posture",
                          "Walking"                          = "[P] Walking",
                          "Stepping"                         = "[P] Stepping",
                          "Running"                          = "[P] Running",
                          "Ascending Stairs"                 = "[P] Ascending Stairs",
                          "Descending Stairs"                = "[P] Descending Stairs",
                          "Crouching / Squating"             = "[P] Crouching / Squating",
                          "Cycling"                          = "[P] Cycling",
                          "Other - Movement"                 = "[P] Other - Movement",
                          "Intermittent Movement"            = "[P] Intermittent Movement",
                          "Intermittent P/M"                 = "[P] Intermittent P/M",
                          "Dark/Obscured/OoF"                = "[U] Dark/Obscured/OoF")
    )
  
  tib_var <- 
    tib_subject %>% 
    select(datetime,
           .data[[var_plot]]) %>% 
    filter(datetime %within% subset_interval)
  
  tib_position <- 
    tib_var %>% 
    summarise(
      position = min(.data[[var_plot]])*0.90,
      position*0.85,
      position*0.70
    ) %>%
    unlist() %>% 
    tibble(
      x = min(tib_var$datetime) - 120,
      y = .,
      label = c("Intensity",
                "Beh Domain",
                "Pos Domain")
      
    )
  
  # .code_type <- 
  #   "posture"
  
  tib_dur_pos <- 
    tib_duration %>% 
    filter(code_type == "pos_domain")
  tib_dur_beh_dom <- 
    tib_duration %>% 
    filter(code_type == "beh_domain")
  tib_dur_int <- 
    tib_duration %>% 
    filter(code_type == "intensity")
  
  chr_legend_order <- 
    tib_duration %>%
    filter(str_detect(code_type,
                      pattern = "pos_domain|beh_domain|intensity")) %>%
    arrange(code_label) %>%
    pull(code) %>%
    unique()
  # tib_duration %>% 
  #   filter(str_detect(code_type,
  #                     pattern = "posture|beh_domain|intensity")) %>%
  #   # arrange(code_label) %>% 
  #   pull(code) %>% 
  #   unique()    
  
  # ggplot(data = tib_var) +
  #   geom_line(mapping = aes(x = datetime,
  #                           y = .data[[var_plot]]),
  #             size = 1) +
  #   coord_flip() +
  #   geom_segment(data = tib_dur_beh_dom,
  #                mapping = aes(y     = start_dttm,
  #                              x     = tib_position$y[1],
  #                              yend  = end_dttm,
  #                              xend  = tib_position$y[1],
  #                              color = code),
  #                size = 5) +
  #   geom_segment(data = tib_dur_int,
  #                mapping = aes(y = start_dttm,
  #                              x = tib_position$y[2],
  #                              yend = end_dttm,
  #                              xend = tib_position$y[2],
  #                              color = code),
  #                size = 5) +
  #   geom_segment(data = tib_dur_pos,
  #                mapping = aes(y = start_dttm,
  #                              x = tib_position$y[3],
  #                              yend = end_dttm,
  #                              xend = tib_position$y[3],
  #                              color = code),
  #                size = 5) +
  #   geom_text(data = tib_position,
  #             mapping = aes(y = x,
  #                           x = y,
  #                           label = label)) 
  #   geom_line(data = tib_var,
  #             mapping = aes(y = datetime,
  #                           x = .data[[var_plot]]),
  #             size = 1)
  
  ggplot(data = tib_var) +
    geom_line(mapping = aes(x = datetime,
                            y = .data[[var_plot]]),
              size = 1) +
    # geom_linerange(data = tib_dur_beh_dom,
    #                mapping = aes(xmin     = start_dttm,
    #                              y     = tib_position$y[1],
    #                              xmax  = end_dttm,
    #                              # yend  = tib_position$y[1],
    #                              color = code),
    #                # position = position_nudge(y = 1),
    #                size = 5)
    geom_segment(data = tib_dur_int,
                 mapping = aes(x     = start_dttm,
                               y     = tib_position$y[1],
                               xend  = end_dttm,
                               yend  = tib_position$y[1],
                               color = code_label),
                 size = 5) +
    geom_segment(data = tib_dur_beh_dom,
                 mapping = aes(x = start_dttm,
                               y = tib_position$y[2],
                               xend = end_dttm,
                               yend = tib_position$y[2],
                               color = code_label),
                 size = 5) +
    geom_segment(data = tib_dur_pos,
                 mapping = aes(x = start_dttm,
                               y = tib_position$y[3],
                               xend = end_dttm,
                               yend = tib_position$y[3],
                               color = code_label),
                 size = 5) +
    geom_text(data = tib_position,
              mapping = aes(x = x,
                            y = y,
                            label = label)) +
    scale_x_datetime(
      date_labels = "%H:%M:%S",
      breaks = "2 min"
    ) +
    labs(
      title = paste("Subset of Subject:", .subject),
      subtitle = paste0("Interval: ", begin_time, " - ", end_time),
      x = "Time",
      y = var_plot
    ) + 
    theme(
      axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)
    ) +
    # scale_color_ordinal(
    #   name = "code",
    #   labels = chr_legend_order
    # )
    # scale_color_discrete(
    #   name = "Code",
    #   labels = chr_legend_order
    # )
    scale_color_manual(
      name = "Code",
      labels = chr_legend_order,
      values = c(
        "#3288bd", # intensity Div - Spectral - 9 class
        "#d53e4f",
        "#66c2a5",
        "#016c59", # beh Seq - PuBuGn  - 5 class
        "#1c9099",
        "#67a9cf",
        "#bdc9e1",
        "#f6eff7",
        "#000000", # "#e31a1c",
        "#1b9e77", # pos Qual - Dark2 - 8 class
        "#d95f02",
        "#7570b3",
        "#e7298a",
        "#66a61e", 
        "#e6ab02",
        "#a6761d")
    )
  # 11:00:00 - 11:50:00
  c(
    "#3288bd", # intensity Div - Spectral - 9 class
    "#d53e4f",
    "#66c2a5",
    "#016c59", # beh Seq - PuBuGn  - 5 class
    "#1c9099",
    "#67a9cf",
    "#bdc9e1",
    "#f6eff7",
    "#000000", # "#e31a1c",
    "#1b9e77", # pos Qual - Dark2 - 8 class
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#66a61e", 
    "#e6ab02",
    "#a6761d")
  
  
  c("#d7191c", # beh Div - Spectral  - 5 class
    "#fdae61",
    "#abdda4",
    "#2b83ba",
    "#ffffbf",
    "#000000", # "#e31a1c",
    "#3288bd", # intensity Div - Spectral - 9 class
    "#66c2a5",
    "#d53e4f",
    "#1b9e77", # pos Qual - Dark2 - 8 class
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#66a61e", 
    "#e6ab02",
    "#a6761d")
  c("#a6cee3", # beh Qual - Paired  - 9 class
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#000000", # "#e31a1c",
    "#fdbf6f",
    "#ff7f00",
    "#cab2d6",
    "#3288bd", # intensity Div - Spectral - 9 class
    "#66c2a5",
    "#d53e4f",
    "#000000", #black - Dark/Obscured/OoF
    "#1b9e77", # pos Qual - Dark2 - 8 class
    "#d95f02",
    "#7570b3")
  c("#ffffd9",
    "#edf8b1",
    "#c7e9b4",
    "#7fcdbb",
    "#41b6c4",
    "#1d91c0",
    "#225ea8",
    "#253494",
    "#081d58",
    "#000000",
    "#ece2f0",# int
    "#a6bddb",
    "#1c9099",
    "#e0ecf4", # pos
    "#9ebcda",
    "#8856a7")
  
  # geom_text(mapping = aes(x = seconds(time[1]) - 120,
  #                         y = 0,
  #                         label = "posture"))
  geom_text(data = tib_label,
            mapping = aes(x = time,
                          y = y,
                          label = label)) +
    scale_color_manual(values = code_colors)
  
  Labels <- c("G", "G", "A", "C", "M", "B", "M", "G", "A","M")
  Percent <- c("-0.241","-0.046", "-0.037", "-0.024", "-0.003","0.007","0.01","0.059","0.121", "0.152")
  df <- data.frame(Labels, Percent)
  df$Percent <- as.numeric(as.character(df$Percent))
  legend_ord <- levels(with(df, reorder(Labels, Percent)))
  reorder(tib_duration$code_type, tib_duration$code)
  
  library(ggplot2)
  p <- ggplot(data=df, aes(x=reorder(Labels, Percent), y = Percent, fill=Labels)) 
  p <- p + geom_bar(stat="identity")
  p + scale_fill_discrete(breaks=legend_ord)
  
  poo <- 
    tib_duration_all %>% 
    filter(subject == 1002) %>% 
    group_by(sample_length) %>% 
    mutate(y = cur_group_id()) %>% 
    ungroup() %>% 
    mutate(sample_length = factor(sample_length))
  poo2 <- 
    poo %>% 
    mutate(middle_dttm = start_dttm + (stop_dttm - start_dttm)/2,
           .after = start_dttm)
  poo2 %>% 
    filter(code_type == "posture") %>% 
    ggplot() +
    geom_tile(mapping = aes(x = middle_dttm,
                            y = sample_length,
                            width = duration,
                            fill = code)
              ,height = 0.75 # OG = 1
    )
  
  poo %>% 
    filter(code_type == "posture") %>% 
    # mutate(code = factor(code,
    #                      levels = lvl_posture)) %>% 
    ggplot() +
    geom_linerange(mapping = aes(
      # x = start_time,
      x = sample_length,
      ymin = start_dttm,
      # xend = stop_time,
      # xend = sample_length,
      ymax = stop_dttm,
      color = code),
      position = position_dodge(width = 0.5),
      # position = position_nudge(x = 1),
      size = 5
    ) +
    coord_flip()
  
  return(tib)
  
  
  poo %>% 
    filter(code_type == "posture") %>% 
    # mutate(code = factor(code,
    #                      levels = lvl_posture)) %>% 
    ggplot() +
    geom_linerange(mapping = aes(
      # x = start_time,
      x = sample_length,
      ymin = start_dttm,
      # xend = stop_time,
      # xend = sample_length,
      ymax = stop_dttm,
      color = code),
      position = position_dodge(width = 0.5),
      # position = position_nudge(x = 1),
      size = 5
    ) +
    coord_flip()
  ggplot() +
    geom_segment(
      mapping = aes(
        # x = start_time,
        x = start_dttm,
        y = sample_length,
        # xend = stop_time,
        xend = stop_dttm,
        yend = sample_length,
        color = code),
      # position = position_dodge(height = 1),
      position = position_nudge(x = 1),
      size = 5
    )
  
  # No gap between bars at all!
  pee %>% 
    filter(code_type == "posture") %>% 
    ggplot() +
    geom_rect(mapping = aes(xmin = start_dttm,
                            xmax = stop_dttm,
                            ymin = y - 1,
                            ymax = y,
                            fill = code))
  
  poo %>% 
    filter(code_type == "posture") %>% 
    ggplot() +
    geom_rect(mapping = aes(xmin = start_dttm,
                            xmax = stop_dttm,
                            ymin = as.integer(sample_length) - 1,
                            ymax = as.integer(sample_length),
                            fill = code))
  pie %>% 
    filter(code_type == "posture") %>% 
    ggplot() +
    geom_rect(mapping = aes(xmin = start_dttm,
                            xmax = stop_dttm,
                            ymin = as.integer(sample_length) - 2,
                            ymax = as.integer(sample_length) - 0.5,
                            fill = code))
  
  poo %>% 
    filter(code_type == "posture") %>% 
    ggplot() +
    geom_tile(mapping = aes(x = start_dttm,
                            y = sample_length,
                            # width = duration,
                            fill = code))
  poo2 %>% 
    filter(code_type == "posture") %>% 
    ggplot() +
    geom_tile(mapping = aes(x = middle_dttm,
                            y = sample_length,
                            width = duration,
                            fill = code)
              ,height = 0.75 # OG = 1
    )
  
  
  # among whole visit -------------------------------------------------------
  
  tib_duration_subject <- 
    tib_long %>% 
    filter(!is.na(code)) %>% 
    filter(!is.na(.data[[var_plot]])) %>% 
    # filter(datetime %within% subset_interval) %>%
    # !(code_type == "activity"), I do this earlier. 
    group_by(code_type, code_source) %>% 
    transmute(
      subject,
      datetime,
      code_source,
      code_type,
      code,
      .data[[var_plot]],
      start = c(1L, diff(event)),
      stop  = c(diff(event), 1L),
      event = event
    ) %>% 
    group_by(subject, code_source, code_type, code, event) %>%
    summarise(
      {{var_plot}} := statistical_mode(.data[[var_plot]]),
      start_dttm  = datetime[start == 1],
      stop_dttm   = datetime[stop == 1] + 1,
      duration      = n(),
      .groups = "drop"
    ) %>% 
    arrange(code_source, code_type, start_dttm) %>% 
    relocate(event,
             .after = last_col())
  tib_var_subject <- 
    tib %>% 
    filter(!is.na(.data[[paste0("posture_", code_sources[1])]])) %>% 
    filter(!is.na(.data[[var_plot]])) %>% 
    filter(subject == .subject) %>% 
    select(datetime,
           .data[[var_plot]])
  
  
  ggplot(data = tib_var_subject) +
    geom_line(mapping = aes(x = datetime,
                            y = .data[[var_plot]])) +
    scale_x_datetime(
      name = "Time",
      # labels = "%M",
      date_labels = "%H:%M:%S",
      breaks = "10 min"
    ) +
    theme(
      axis.text.x = element_text(angle = 40, vjust = 1.0, hjust = 1.0)
    )
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                                   OLD                                   ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##              # Test1 - clean_noldus_event_logs              ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# first made when making test data for com sci lab.
x <- read.csv("R:/PAHRL/Student Access/0_Students/MARTINEZ/3_Projects/FLAC/Noldus/Computer Science/Activity_Master_Project - FLAC_Example1 - Event Logs.csv",
              stringsAsFactors = F)
tail(x$Event_Type, n=1)
l <- length(x$Event_Type)
x$Event_Type[l] <- "End"
x$Event_Type[1] <- "Beginning"
x <- x[x$Event_Type!="State stop", ]
x <- x[ ,-c(1:4,11,17)]
x$High_Quality_Activity <- paste(x$Modifier_3, x$Modifier_4)
x$Domain <- x$Modifier_1
x$Environment <- x$Modifier_2
x <- x[ , -which(names(x) %in% c("Modifier_1", "Modifier_2", "Modifier_3","Modifier_4"))]

write.table(x, 
            file = paste0("R:/PAHRL/Student Access/0_Students/MARTINEZ/FLAC/Noldus/Computer Science/", "FLAC_Example1_Activity.csv"),
            sep = ",",
            row.names = F,
            col.names = T)

y <- read.csv("R:/PAHRL/Student Access/0_Students/MARTINEZ/FLAC/Noldus/Computer Science/Posture_Master_Project - FLAC_Example1 - Event Logs.csv",
              stringsAsFactors = F)
tail(y$Event_Type, n=1)
l <- length(y$Event_Type)
y$Event_Type[l] <- "End"
y$Event_Type[1] <- "Beginning"
y <- y[y$Event_Type!="State stop", ]
y <- y[ ,-c(1:4,11,15)]
y$Intensity <- y$Modifier_1
y <- y[ , -which(names(y) %in% c("Modifier_1", "Modifier_2", "Modifier_3","Modifier_4"))]

write.table(y, 
            file = paste0("R:/PAHRL/Student Access/0_Students/MARTINEZ/FLAC/Noldus/Computer Science/", "FLAC_Example1_Posture.csv"),
            sep = ",",
            row.names = F,
            col.names = T)

12345678901234567890123456789012345678901234567890123456789012345678901234567890

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##              # Test2 - clean_noldus_event_logs              ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# see if there is any difference between txt output and excel output

fpa_pos_raw <- "./Noldus Observer XT 14/2_Event Logs/0_raw/posture/"
fpa_act_raw <- "./Noldus Observer XT 14/2_Event Logs/0_raw/activity/"
# fnm_pos_raw <- "Posture_Master_Project - FLAC_1013v1_AS - Event Logs.txt"
fnm_pos_raw <- "Posture_Master_Project - FLAC_1013v1_AS - Event Logs.xlsx"

fpa_clean <- "./Noldus Observer XT 14/2_Event Logs/1_clean/"

fls_act <- list.files(path = fpa_act_raw,
                      pattern = ".xlsx")

study <- str_sub(fnm_pos_raw,
                 start = 26,
                 end = 29)
subject <- str_sub(fnm_pos_raw,
                   start = 31,
                   end = 34) %>% 
  as.integer()
visit <- str_sub(fnm_pos_raw,
                 start = 36,
                 end = 36) %>% 
  as.integer()


lgl_act_raw <- str_detect(
  fls_act,
  pattern = paste(subject,
                  visit,
                  sep = "v")
)
fnm_act_raw <- fls_act[lgl_act_raw]

# raw and clean
tib_pos_raw <- read_xlsx(path = paste0(fpa_pos_raw,
                                       fnm_pos_raw))
# vroom
# tib_pos_raw <- vroom(
#   file = paste0(fpa_pos_raw,
#                 fnm_pos_raw),
#   delim = "\t",
#   col_types = cols_only(Comment = col_character())
#   )

# options(digits.secs = 3)
# 
# strptime(tib_pos_raw$time[1],
#          format = "%m-%d-%Y %H:%M:%OS")

tib_pos_raw <- tib_pos_raw[tib_pos_raw$Event_Type!="State stop", ]
tib_pos_raw$study <- study
tib_pos_raw$subject <- subject
tib_pos_raw$visit <- visit
tib_pos_raw <- tib_pos_raw[ ,c("study",
                               "subject",
                               "visit",
                               "Time_Relative_hms",
                               "Duration_sf",
                               "Behavior",
                               "Modifier_1",
                               "Event_Type",
                               "Comment")]

ind_pos_start <- which(
  tib_pos_raw$Behavior == "Start Time"
)
ind_pos_stop <- which(
  tib_pos_raw$Behavior == "Stop Time"
)
nrw_pos_raw <- nrow(tib_pos_raw)

# for later
# strptime(tib_pos_raw$Comment[ind_pos_start],
#          format = "%Y%m%d_%H%M%OS")
dtm_pos_start <- ymd_hms(tib_pos_raw$Comment[ind_pos_start],
                         tz = "America/Chicago")
dtm_pos_stop <- ymd_hms(tib_pos_raw$Comment[ind_pos_stop],
                        tz = "America/Chicago")

# continue raw clean
tib_pos_raw <- tib_pos_raw[-c(1:ind_pos_start, ind_pos_stop:nrw_pos_raw), ]

# sbs
nrw_pos_raw2 <- nrow(tib_pos_raw)
int_duration <- as.vector(
  difftime(
    tib_pos_raw$Time_Relative_hms[seq_len(nrw_pos_raw2 - 1) + 1],
    tib_pos_raw$Time_Relative_hms[seq_len(nrw_pos_raw2 - 1)],
    units = "secs"
  )
)
int_duration <- c(round(int_duration,
                        digits = 0),
                  round(tib_pos_raw$Duration_sf[nrow(tib_pos_raw)],
                        digits = 0))
int_duration
int_duration[is.na(int_duration)] <- 1
int_duration <- as.integer(int_duration)

sbs_int_events <- rep(1:length(int_duration),
                      times = int_duration)
sbs_str_posture <- rep(tib_pos_raw$Behavior,
                       times = int_duration)
sbs_str_intensity  <- rep(tib_pos_raw$Modifier_1,
                          times = int_duration)
sbs_str_comment <- rep(tib_pos_raw$Comment,
                       times = int_duration)

# dttm_start <- strptime(data_ap_raw$time[1],
#                        format = "%Y-%m-%d %H:%M:%S") %>% 
#   as.POSIXct(tz = "America/Chicago")
nrw_pos_sbs <- length(sbs_int_events)
sbs_dtm_times <- dtm_pos_start + (0:(nrw_pos_sbs - 1))
sbs_dte_dates <- date(sbs_dtm_times)

tib_pos_sbs <- tibble(
  study = study,
  subject = subject,
  visit = visit,
  time       = sbs_dtm_times,
  date       = sbs_dte_dates,
  posture = sbs_str_posture,
  intensity = sbs_str_intensity,
  pos_comment = sbs_str_comment,
  num_pos_events = sbs_int_events,
  .rows = nrw_pos_sbs
)

# ACTIVITY

tib_act_raw <- read_xlsx(path = paste0(fpa_act_raw,
                                       fnm_act_raw))
# vroom
# tib_act_raw <- vroom(
#   file = paste0(fpa_act_raw,
#                 fnm_act_raw),
#   delim = "\t",
#   col_types = cols_only(Comment = col_character())
#   )

# options(digits.secs = 3)
# 
# strptime(tib_act_raw$time[1],
#          format = "%m-%d-%Y %H:%M:%OS")

tib_act_raw <- tib_act_raw[tib_act_raw$Event_Type!="State stop", ]
tib_act_raw$study <- study
tib_act_raw$subject <- subject
tib_act_raw$visit <- visit

tib_act_raw <- unite(tib_act_raw, 
                     col = "Activity",
                     Modifier_2, Modifier_3, Modifier_4,
                     na.rm = TRUE)

# tib_act_raw$Activity <- paste(tib_act_raw$Modifier_3, tib_act_raw$Modifier_4)
tib_act_raw$Domain <- tib_act_raw$Modifier_1

tib_act_raw <- tib_act_raw[ ,c("study",
                               "subject",
                               "visit",
                               "Time_Relative_hms",
                               "Duration_sf",
                               "Behavior",
                               "Activity",
                               "Domain",
                               "Event_Type",
                               "Comment")]

ind_act_start <- which(
  tib_act_raw$Behavior == "[U] Start Time"
)
ind_act_stop <- which(
  tib_act_raw$Behavior == "[U] Stop Time"
)
nrw_act_raw <- nrow(tib_act_raw)

# for later
# strptime(tib_act_raw$Comment[ind_act_start],
#          format = "%Y%m%d_%H%M%OS")
dtm_act_start <- ymd_hms(tib_act_raw$Comment[ind_act_start],
                         tz = "America/Chicago")
dtm_act_stop <- ymd_hms(tib_act_raw$Comment[ind_act_stop],
                        tz = "America/Chicago")

# continue raw clean
tib_act_raw <- tib_act_raw[-c(1:ind_act_start, ind_act_stop:nrw_act_raw), ]

# sbs
nrw_act_raw2 <- nrow(tib_act_raw)
int_duration <- as.vector(
  difftime(
    tib_act_raw$Time_Relative_hms[seq_len(nrw_act_raw2 - 1) + 1],
    tib_act_raw$Time_Relative_hms[seq_len(nrw_act_raw2 - 1)],
    units = "secs"
  )
)
int_duration <- c(round(int_duration,
                        digits = 0),
                  round(tib_act_raw$Duration_sf[nrow(tib_act_raw)],
                        digits = 0))
int_duration
int_duration[is.na(int_duration)] <- 1
int_duration <- as.integer(int_duration)

sbs_int_events <- rep(1:length(int_duration),
                      times = int_duration)
sbs_str_behavior <- rep(tib_act_raw$Behavior,
                        times = int_duration)
sbs_str_activity <- rep(tib_act_raw$Activity,
                        times = int_duration)
sbs_str_domain <- rep(tib_act_raw$Domain,
                      times = int_duration)
sbs_str_comment <- rep(tib_act_raw$Comment,
                       times = int_duration)

# dttm_start <- strptime(data_ap_raw$time[1],
#                        format = "%Y-%m-%d %H:%M:%S") %>% 
#   as.actIXct(tz = "America/Chicago")
nrw_act_sbs <- length(sbs_int_events)
sbs_dtm_times <- dtm_act_start + (0:(nrw_act_sbs - 1))
sbs_dte_dates <- date(sbs_dtm_times)

tib_act_sbs <- tibble(
  study = study,
  subject = subject,
  visit = visit,
  time       = sbs_dtm_times,
  date       = sbs_dte_dates,
  behavior = sbs_str_behavior,
  activity = sbs_str_activity,
  domain = sbs_str_domain,
  act_comment = sbs_str_comment,
  num_act_events = sbs_int_events,
  .rows = nrw_act_sbs
)


tib_mer_sbs <- left_join(tib_pos_sbs,
                         tib_act_sbs,
                         by = c("study", "subject", "visit", "time", "date"))

tib_mer_sbs <- tib_mer_sbs[, c("study", 
                               "subject", 
                               "visit", 
                               "time", 
                               "date",
                               "posture",
                               "behavior",
                               "activity",
                               "domain",
                               "intensity",
                               "pos_comment",
                               "act_comment",
                               "num_pos_events",
                               "num_act_events")]

fnm_clean <- str_sub(fnm_pos_raw,
                     start = 26,
                     end = 36)
fnm_clean <- paste0(study, "_", subject, "v", visit, ".csv")

vroom_write(tib_mer_sbs,
            path = paste0(fpa_clean,
                          fnm_clean),
            delim = ",")


strp
test <- strptime(dtm_pos_start,
                 format = "%Y-%m-%d %H:%M:%OS")
test + tib_pos_raw$Duration_sf

difft

intg_event_time <- as.vector(
  difftime(
    data_ap_raw$time[seq_len(nrow_ap_raw2 - 1) + 1],
    data_ap_raw$time[seq_len(nrow_ap_raw2 - 1)],
    units = "secs"
  )
)
intg_event_time <- c(round(intg_event_time,
                           digits = 0),
                     round(data_ap_raw$interval[nrow_ap_raw2],
                           digits = 0))

intg_event_time[is.na(intg_event_time)] <- 1
intg_event_time <- as.integer(intg_event_time)

sbs_intg_events <- rep(1:length(intg_event_time),
                       times = intg_event_time)
sbs_intg_acts   <- as.integer(rep(data_ap_raw$activity,
                                  times = intg_event_time))
sbs_dble_steps  <- rep(data_ap_raw$cumulativesteps,
                       times = intg_event_time)
diff(tib_pos_raw$Duration_sf,
     lag = 1,
     differences = 1)


dttm_start <- strptime(data_ap_raw$time[1],
                       format = "%Y-%m-%d %H:%M:%S") %>% 
  as.POSIXct(tz = "America/Chicago")
nrow_ap_sbs <- length(sbs_intg_events)
sbs_dttm_times <- dttm_start + (0:(nrow_ap_sbs - 1))
sbs_date_dates <- date(sbs_dttm_times)

# make sbs variables:
sbs_intg_events <- rep(1:length(intg_event_time),
                       times = intg_event_time)
sbs_intg_acts   <- as.integer(rep(data_ap_raw$activity,
                                  times = intg_event_time))
sbs_dble_steps  <- rep(data_ap_raw$cumulativesteps,
                       times = intg_event_time)
diff(tib_pos_raw$Duration_sf,
     lag = 1,
     differences = 1)


dttm_start <- strptime(data_ap_raw$time[1],
                       format = "%Y-%m-%d %H:%M:%S") %>% 
  as.POSIXct(tz = "America/Chicago")
nrow_ap_sbs <- length(sbs_intg_events)
sbs_dttm_times <- dttm_start + (0:(nrow_ap_sbs - 1))
sbs_date_dates <- date(sbs_dttm_times)



readLines(con = paste0(fpa_pos_raw,
                       fnm_pos_raw),
          encoding = "UTF16LE")
read.delim(file = paste0(fpa_pos_raw,
                         fnm_pos_raw),
           fileEncoding = "UTF16LE",
           skipNul = TRUE)

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##              # Test3 - clean_noldus_event_logs              ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Test with colorado data
# see if there is any difference between txt output and excel output

fdr_vid_raw <- "./Colorado/Noldus Observer XT 14/2_Event Logs/0_raw"
# fpa_act_raw <- "./Colorado/Noldus Observer XT 14/2_Event Logs/0_raw"
# fnm_pos_raw <- "Posture_Master_Project - FLAC_1013v1_AS - Event Logs.txt"

# fnm_vid_raw_act <- "Activity_Master_Project - CO_1001_JM - Event Logs.xlsx"
# fnm_vid_raw_pos <- "Posture_Master_Project - CO_1001_JM - Event Logs.xlsx"

fdr_vid_clean <- "./Colorado/Noldus Observer XT 14/2_Event Logs/1_clean"

# fls_vid_raw <- 
#   list.files(path = fdr_vid_raw,
#              pattern = ".xlsx")
fls_vid_raw_act <- 
  list.files(path = fdr_vid_raw,
             pattern = ".xlsx") %>% 
  str_subset(pattern = "Activity")
fls_vid_raw_pos <- 
  list.files(path = fdr_vid_raw,
             pattern = ".xlsx") %>% 
  str_subset(pattern = "Posture")

# Start "cleaning" function: Posture
fls_vid_raw <- fls_vid_raw_pos

fnm_vid_raw <- fls_vid_raw[1]

str_extract(pattern = "(?:(?!_SPLIT).)*")# Captures everything before first "_SPLIT".
str_extract(pattern = "([^_]*)$") # Captures everything after last "_".

schema <- 
  fnm_vid_raw %>% 
  str_extract(pattern = "Activity|Posture")
study <- 
  fnm_vid_raw %>% 
  str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
  str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
  str_extract(pattern = "(?:(?!_).)*")
subject <- 
  fnm_vid_raw %>% 
  str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
  str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
  str_extract(pattern = "[0-9][0-9][0-9][0-9]") %>% 
  as.integer()

if (study == "FLAC") {
  
  visit <- 
    fnm_vid_raw %>% 
    str_sub(start = 36,
            end = 36) %>% 
    as.integer()
  
} else if (study == "CO") {
  
  visit <- 1L
  
}


# POSTURE raw and clean
tib_vid_raw <- 
  read_xlsx(
    path = paste(fdr_vid_raw,
                 fnm_vid_raw,
                 sep = "/"),
    progress = FALSE
  )
tib_vid_raw <- 
  tib_vid_raw[tib_vid_raw$Event_Type != "State stop", ]
# tib_vid_raw$study <- study
# tib_vid_raw$subject <- subject
# tib_vid_raw$visit <- visit
# tib_vid_raw <- 
#   tib_vid_raw[ ,c("Time_Relative_hms",
#                   "Duration_sf",
#                   "Behavior",
#                   "Modifier_1",
#                   "Event_Type",
#                   "Comment")]

# Make Posture Uncoded codes the same as Activity.
if (schema == "Posture") {
  
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded - Dark/Obscured/OoF"] <- 
    "[U] Dark/Obscured/OoF"
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded; Start/Stop"] <- 
    "[U] Start/Stop"
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Start Time"] <- 
    "[U] Start Time"
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Stop Time"] <- 
    "[U] Stop Time"
  
}


# Double check a behavior code has same relative time as the start and
# stop times.
dtm_relative_hms_start <- 
  tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Start Time"]
dtm_relative_hms_stop <- 
  tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Stop Time"]

# Remove "[U] Start/Stop" codes and State Points.
tib_vid_raw <- 
  tib_vid_raw[(tib_vid_raw$Behavior == "[U] Start/Stop" |
                 tib_vid_raw$Behavior == "[U] Start Time" |
                 tib_vid_raw$Behavior == "[U] Stop Time") == FALSE, ]

if (any(tib_vid_raw$Time_Relative_hms == dtm_relative_hms_start) == FALSE |
    any(tib_vid_raw$Time_Relative_hms == dtm_relative_hms_stop) == TRUE) {
  
  stop("STAAAAAAAAAAAAAAAAAHP")
  
}



nrw_pos_raw <- 
  nrow(tib_vid_raw)
dtm_vid_start <- 
  ymd_hms(tib_vid_raw$Comment[ind_start],
          tz = "America/Chicago")
dtm_vid_stop <- 
  ymd_hms(tib_vid_raw$Comment[ind_stop],
          tz = "America/Chicago")

# continue raw clean
tib_vid_raw <- tib_vid_raw[-c(1, ind_pos_start, nrw_pos_raw), ]


# AVSA
# Create dtm vector where last entry is time[nrow] + interval[nrow] for diff
# function. Do it this way as previous method of adding in the rounded
# interval[nrow] to stripped time does not take into account fractional
# seconds.
raw_dtm <- 
  c(tib_vid_raw$time,
    tib_vid_raw$time[nrw_ap_raw2] + tib_vid_raw$interval[nrw_ap_raw2])

# To make sec-by-sec, take the difference between each time and the previous
# time. Do this with the "strptimed" date-time's as this will account for 
# added fractional seconds. Not doing so will result in a sbs file less 
# than 2 hours for visits. This will also make time entries exactly equal to
# on and off times.
raw_dtm_strpd <-
  raw_dtm %>% 
  strptime(format = "%Y-%m-%d %H:%M:%OS") %>%
  as.POSIXct(tz = "America/Chicago")
int_event_time <- 
  raw_dtm_strpd %>% 
  diff.POSIXt(lag = 1,
              differences = 1) %>%
  as.vector()
int_event_time[is.na(int_event_time)] <- 1
int_event_time <- as.integer(int_event_time)

# Make sbs variables.
sbs_int_events <- 
  int_event_time %>% 
  seq_along() %>% 
  rep(times = int_event_time)
sbs_int_acts <- 
  tib_vid_raw$activity %>% 
  rep(times = int_event_time) %>% 
  as.integer()
sbs_dbl_steps <-
  tib_vid_raw$cumulativesteps %>% 
  rep(times = int_event_time) %>% 
  as.double()
nrw_ap_sbs <- length(sbs_int_events)
sbs_dtm_times <- 
  seq.POSIXt(
    from = raw_dtm_strpd[1],
    by = "secs",
    length.out = nrw_ap_sbs
  )
sbs_dte_dates <- date(sbs_dtm_times)


# for later
# strptime(tib_vid_raw$Comment[ind_pos_start],
#          format = "%Y%m%d_%H%M%OS")

# sbs
nrw_pos_raw2 <- nrow(tib_vid_raw)
int_duration <- as.vector(
  difftime(
    tib_vid_raw$Time_Relative_hms[seq_len(nrw_pos_raw2 - 1) + 1],
    tib_vid_raw$Time_Relative_hms[seq_len(nrw_pos_raw2 - 1)],
    units = "secs"
  )
)
int_duration <- c(round(int_duration,
                        digits = 0),
                  round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
                        digits = 0))
int_duration
int_duration[is.na(int_duration)] <- 1
int_duration <- as.integer(int_duration)

sbs_int_events <- rep(1:length(int_duration),
                      times = int_duration)
sbs_str_posture <- rep(tib_vid_raw$Behavior,
                       times = int_duration)
sbs_str_intensity  <- rep(tib_vid_raw$Modifier_1,
                          times = int_duration)
sbs_str_comment <- rep(tib_vid_raw$Comment,
                       times = int_duration)

# dttm_start <- strptime(data_ap_raw$time[1],
#                        format = "%Y-%m-%d %H:%M:%S") %>% 
#   as.POSIXct(tz = "America/Chicago")
nrw_pos_sbs <- length(sbs_int_events)
sbs_dtm_times <- dtm_pos_start + (0:(nrw_pos_sbs - 1))
sbs_dte_dates <- date(sbs_dtm_times)

tib_pos_sbs <- tibble(
  study   = study,
  subject = subject,
  # visit = visit,
  time       = sbs_dtm_times,
  date       = sbs_dte_dates,
  posture    = sbs_str_posture,
  intensity  = sbs_str_intensity,
  pos_comment = sbs_str_comment,
  num_pos_events = sbs_int_events,
  .rows = nrw_pos_sbs
)

# ACTIVITY --
fls_vid_raw <- fls_vid_raw_act

fnm_vid_raw <- fls_vid_raw[1]

str_extract(pattern = "(?:(?!_SPLIT).)*")# Captures everything before first "_SPLIT".
str_extract(pattern = "([^_]*)$") # Captures everything after last "_".

schema <- 
  fnm_vid_raw %>% 
  str_extract(pattern = "Activity|Posture")
study <- 
  fnm_vid_raw %>% 
  str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
  str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
  str_extract(pattern = "(?:(?!_).)*")
subject <- 
  fnm_vid_raw %>% 
  str_extract(pattern = "(?:(?! - Event Logs.xlsx).)*") %>%  # Capture everything before first.
  str_extract(pattern = "([^ - ]*)$") %>% # Capture everything after.
  str_extract(pattern = "[0-9][0-9][0-9][0-9]") %>% 
  as.integer()

if (study == "FLAC") {
  
  visit <- 
    fnm_vid_raw %>% 
    str_sub(start = 36,
            end = 36) %>% 
    as.integer()
  
} else if (study == "CO") {
  
  visit <- 1L
  
}

tib_vid_raw <- 
  read_xlsx(
    path = paste(fdr_vid_raw,
                 fnm_vid_raw,
                 sep = "/"),
    progress = FALSE
  )

# Get datetime start, stop after removing any NA's but before removing any 
# other rows. This is because if a boolean has NA when subsetting it will
# include the NA within the subset.
dtm_comment <- 
  read_xlsx(
    path = paste(fdr_vid_raw,
                 fnm_vid_raw,
                 sep = "/"),
    range = cell_cols(12:24),
    col_types = "date",
    progress = FALSE,
    .name_repair = "minimal"
  ) %>% 
  dplyr::pull("Comment")
dtm_comment <- 
  dtm_comment[is.na(tib_vid_raw$Behavior) == FALSE]
tib_vid_raw <- 
  tib_vid_raw[is.na(tib_vid_raw$Behavior) == FALSE, ]

dtm_vid_start <- 
  dtm_comment[tib_vid_raw$Behavior == "[U] Start Time"] #still UTC
dtm_vid_stop <- 
  dtm_comment[tib_vid_raw$Behavior == "[U] Stop Time"] #still UTC

# test <- 
#   read_xlsx(
#     path = paste(fdr_vid_raw,
#                  fnm_vid_raw,
#                  sep = "/"),
#     range = cell_cols(12:24),
#     col_types = "text",
#     progress = FALSE,
#     .name_repair = "minimal"
#   ) %>% 
#   dplyr::pull("Behavior")

tib_vid_raw <- 
  tib_vid_raw[tib_vid_raw$Event_Type != "State stop", ]
# tib_vid_raw <- 
#   tib_vid_raw[is.na(tib_vid_raw$Behavior) == FALSE, ]
tib_vid_raw <- 
  tib_vid_raw[tib_vid_raw$Behavior != "*General Placeholder*", ]

if ((tib_vid_raw$Behavior == "[U] Start/Stop") %>% 
    sum() != 2) {
  
  # IDK man.
  stop("STAAAAAAAAAAAAAAAAAHP")
  
}

# tib_vid_raw$study <- study
# tib_vid_raw$subject <- subject
# tib_vid_raw$visit <- visit
# tib_vid_raw <- 
#   tib_vid_raw[ ,c("Time_Relative_hms",
#                   "Duration_sf",
#                   "Behavior",
#                   "Modifier_1",
#                   "Event_Type",
#                   "Comment")]

# Make Posture Uncoded codes the same as Activity.
if (schema == "Posture") {
  
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded - Dark/Obscured/OoF"] <- 
    "[U] Dark/Obscured/OoF"
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Uncoded; Start/Stop"] <- 
    "[U] Start/Stop"
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Start Time"] <- 
    "[U] Start Time"
  tib_vid_raw$Behavior[tib_vid_raw$Behavior == "Stop Time"] <- 
    "[U] Stop Time"
  
}

# Double check a behavior code has same relative time as the start and
# stop times.

# The comments column is parsed as strings. Date-times in Excel that are in the
# mdy_hms format are contained as # of days since the date 1899-12-30. Therefore,
# need to change the date-time string from the start and stop placeholders to 
# double class then to date class and then finally into POSIXct class (numbers
# after the decimal point is the hms time). Don't change the tz to Chicago or 
# Denver until the end...

# I was using the below code which I adapted from process_ap function from 
# activpalprocessing package but for some reason the start time was always 1 sec
# behind the time I placed (using CO_1001). This happened with my adapted code
# AND the OG code.
# tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Start Time"] %>% 
#   as.double() %>% 
#   lubridate::as_date(origin = "1899-12-30") %>% 
#   lubridate::as_datetime(tz = "UTC")
# as.POSIXlt(
#   as.POSIXct(
#     as.Date(
#       as.numeric(
#         sub(
#           "#", "", tib_vid_raw$Comment[tib_vid_raw$Behavior == "[U] Start Time"]
#         )
#       ),
#       origin = "1899-12-30"
#     )
#   ),
#   tz = "UTC"
# )

dtm_relative_hms_start <- 
  tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Start Time"]
dtm_relative_hms_stop <- 
  tib_vid_raw$Time_Relative_hms[tib_vid_raw$Behavior == "[U] Stop Time"]

# find a way to make sure these are always within 5 seconds of each other.
difftime(
  time1 = dtm_relative_hms_stop,
  time2 = dtm_relative_hms_start,
  units = "secs"
)
difftime(
  time1 = dtm_vid_stop,
  time2 = dtm_vid_start,
  units = "secs"
)

# Remove "[U] Start/Stop" codes and State Points.
tib_vid_raw <- 
  tib_vid_raw[(tib_vid_raw$Behavior == "[U] Start/Stop" |
                 tib_vid_raw$Behavior == "[U] Start Time" |
                 tib_vid_raw$Behavior == "[U] Stop Time") == FALSE, ]

# if (any(tib_vid_raw$Time_Relative_hms == dtm_relative_hms_start) == FALSE |
#     any(tib_vid_raw$Time_Relative_hms == dtm_relative_hms_stop) == TRUE) {
#   
#   stop("STAAAAAAAAAAAAAAAAAHP")
#   
# }

# Check to see if the timestamp of [U] Stop Time was placed at the same time as
# the last [U] Start/Stop.

if (tib_vid_raw$Time_Relative_hms[1] != dtm_relative_hms_start) {
  
  # The timestmap of [U] Start Time was NOT placed at the same time as the first
  # annotation code.
  stop("First code does not align with start time.")
  
}

if ((tib_vid_raw$Time_Relative_hms[nrow(tib_vid_raw)] +
     tib_vid_raw$Duration_sf[nrow(tib_vid_raw)]) %>% 
    strptime(format = "%Y-%m-%d %H:%M:%S") %>% 
    as.POSIXct(tz = "UTC") != 
    dtm_relative_hms_stop) {
  
  # The timestamp of [U] Stop Time was NOT placed at the same time as the last
  # [U] Start/Stop.
  stop("Stop time does not match last code timestamp + its duration.")
  
}

# tib_vid_raw$study <- study
# tib_vid_raw$subject <- subject
# tib_vid_raw$visit <- visit

# Try to do all the posture/actvity differences at the end. Give posture a
# "Modifier_2" column that is NA for now. Make Activity's Modifier_2 column
# a collapse of all the other modifiers EXCEPT modifier_1 which should ALWAYS 
# be domain (environment after cleaning).
# if (schema == "Activity") {
#   
#   colnames(tib_vid_raw)[colnames(tib_vid_raw) == "Modifier_1"] <- "Environment"
#   
# } else if (schema == "Posture") {
#   
#   colnames(tib_vid_raw)[colnames(tib_vid_raw) == "Modifier_1"] <- "Intensity"
#   
# }

colnames(tib_vid_raw) %>% 
  str_detect(pattern = "Modifier")

# So the unite function does not combine the environment column when cleaning
# an activity file.
colnames(tib_vid_raw)[colnames(tib_vid_raw) == "Modifier_1"] <- "Mod_1"

tib_vid_raw
tib_vid_raw <- 
  tib_vid_raw %>% 
  unite(col = "Activity",
        contains("Modifier",
                 ignore.case = FALSE,
                 vars = NULL),
        remove = TRUE,
        na.rm = TRUE)

# COLORADO SPECIFIC
tib_vid_raw$Domain <- "Non-Domestic"

tib_vid_raw <- tib_vid_raw[ ,c("study",
                               "subject",
                               # "visit",
                               "Time_Relative_hms",
                               "Duration_sf",
                               "Behavior",
                               "Activity",
                               "Domain",
                               "Event_Type",
                               "Comment")]

ind_act_start <- which(
  tib_vid_raw$Behavior == "[U] Start Time"
)
ind_act_stop <- which(
  tib_vid_raw$Behavior == "[U] Stop Time"
)
nrw_act_raw <- nrow(tib_vid_raw)

# for later
# strptime(tib_vid_raw$Comment[ind_act_start],
#          format = "%Y%m%d_%H%M%OS")
dtm_vid_start <- ymd_hms(tib_vid_raw$Comment[ind_act_start],
                         tz = "America/Chicago")
dtm_vid_stop <- ymd_hms(tib_vid_raw$Comment[ind_act_stop],
                        tz = "America/Chicago")

# continue raw clean
tib_vid_raw <- tib_vid_raw[-c(1, ind_act_start, nrw_act_raw), ]

# sbs
nrw_act_raw2 <- nrow(tib_vid_raw)
int_duration <- as.vector(
  difftime(
    tib_vid_raw$Time_Relative_hms[seq_len(nrw_act_raw2 - 1) + 1],
    tib_vid_raw$Time_Relative_hms[seq_len(nrw_act_raw2 - 1)],
    units = "secs"
  )
)
int_duration <- c(round(int_duration,
                        digits = 0),
                  round(tib_vid_raw$Duration_sf[nrow(tib_vid_raw)],
                        digits = 0))
int_duration
int_duration[is.na(int_duration)] <- 1
int_duration <- as.integer(int_duration)

sbs_int_events <- rep(1:length(int_duration),
                      times = int_duration)
sbs_str_behavior <- rep(tib_vid_raw$Behavior,
                        times = int_duration)
sbs_str_activity <- rep(tib_vid_raw$Activity,
                        times = int_duration)
sbs_str_domain <- rep(tib_vid_raw$Domain,
                      times = int_duration)
sbs_str_comment <- rep(tib_vid_raw$Comment,
                       times = int_duration)

# dttm_start <- strptime(data_ap_raw$time[1],
#                        format = "%Y-%m-%d %H:%M:%S") %>% 
#   as.actIXct(tz = "America/Chicago")
nrw_act_sbs <- length(sbs_int_events)
sbs_dtm_times <- dtm_vid_start + (0:(nrw_act_sbs - 1))
sbs_dte_dates <- date(sbs_dtm_times)

tib_act_sbs <- tibble(
  study   = study,
  subject = subject,
  # visit = visit,
  time    = sbs_dtm_times,
  date    = sbs_dte_dates,
  behavior       = sbs_str_behavior,
  activity       = sbs_str_activity,
  domain         = sbs_str_domain,
  act_comment    = sbs_str_comment,
  num_act_events = sbs_int_events,
  .rows = nrw_act_sbs
)


tib_mer_sbs <- left_join(tib_pos_sbs,
                         tib_act_sbs,
                         by = c("study",
                                "subject",
                                # "visit",
                                "time",
                                "date"))

tib_mer_sbs <- tib_mer_sbs[, c("study", 
                               "subject", 
                               # "visit", 
                               "time", 
                               "date",
                               "posture",
                               "behavior",
                               "activity",
                               "domain",
                               "intensity",
                               "pos_comment",
                               "act_comment",
                               "num_pos_events",
                               "num_act_events")]

# fnm_clean <- 
#   str_sub(fnm_vid_raw_pos,
#           start = 26,
#           end = 36)
fnm_clean <- 
  paste0(study, 
         "_", 
         subject, 
         # "v", 
         # visit, 
         ".csv")

vroom_write(
  tib_mer_sbs,
  path = paste(fdr_clean,
               fnm_clean,
               sep = "/"),
  delim = ","
)


##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                Test1 - change_noldus_encoding                ::
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
?readlines()
rea

writeLines(iconv(readLines("tmp.html"), from = "ANSI_X3.4-1986", to = "UTF8"), 
           file("tmp2.html", encoding="UTF-8"))


##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    activPAL_correction_v1                    ::
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# AP files after 11/01/2018 have incorrect date due to outdated
# PALStudio WHICH. IS. STILL. BEING. USED. However, applying correction
# factor will account for dst for an AP that croses from no-yes/yes-no.
if (dtm_visit > as.Date("2018-11-01")) {
  
  ### CORRECTION FACTOR ###
  tib_ap_raw$time <- tib_ap_raw$time + 3106.8918*24*60*60
  
  # after testing all files were at least 6 sec off
  tib_ap_raw$time <- tib_ap_raw$time + 6 
  
  # daylight savings
  if (dst(dtm_file) == FALSE &
      dst(dtm_visit) == TRUE) {
    
    # n-y: substract 1 hour because it is ahead
    tib_ap_raw$time <- tib_ap_raw$time - 60*60 
    
  } else if (dst(dtm_file) == TRUE &
             dst(dtm_visit) == FALSE) {
    
    # y-n: add 1 hour because it is behind
    tib_ap_raw$time <- tib_ap_raw$time + 60*60
    
  }
  
} else if (length(lgl_dst) == 2) {
  
  # For AP files that switch from y-n or n-yes, must add/remove an hour.
  ind_dst <- which(duplicated(tib_ap_raw$is_dst) == FALSE)[2]
  
  if (lgl_dst[1] == FALSE) {
    
    # n-y: add one hour when dst is TRUE
    tib_ap_raw$time[ind_dst:nrw_ap_raw2] <- 
      tib_ap_raw$time[ind_dst:nrw_ap_raw2] + 60*60
    
  } else if (lgl_dst[1] == TRUE) {
    
    # y-n: subtract one hour when dst is FALSE
    tib_ap_raw$time[ind_dst:nrw_ap_raw2] <- 
      tib_ap_raw$time[ind_dst:nrw_ap_raw2] - 60*60
    
  }
  
}
