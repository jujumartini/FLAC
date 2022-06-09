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
create_videos_list <- function(path_flv,
                               path_mp4,
                               path_part) {
  # path_flv <- "./Noldus Observer XT 14/Surface Videos/flv/"
  # path_mp4 <- "./Noldus Observer XT 14/Surface Videos/mp4/"
  # path_part <- "./Noldus Observer XT 14/7_Merging Part Videos/"
  
  # flv
  list_flv <- 
    list.files(path = path_flv,
               pattern = ".flv",
               recursive = T)
  list_flv <- 
    sub(".*/",
        "",
        list_flv) %>% 
    sub("\\.[^\\.]*$",
        "",
        .)
  order_flv <- 
    as.numeric(str_sub(list_flv, start = 7, end = 9))
  visit_flv <- 
    as.numeric(str_sub(list_flv, start = 11, end = 11))
  df_flv <- tibble(
    visit     = visit_flv,
    ID        = order_flv,
    flv_file  = list_flv
  )
  
  # mp4
  list_mp4 <- 
    list.files(path = path_mp4,
               pattern = ".mp4",
               recursive = T)
  list_mp4 <- 
    sub(".*/",
        "",
        list_mp4) %>% 
    sub("\\.[^\\.]*$",
        "",
        .)
  order_mp4 <- 
    as.numeric(str_sub(list_mp4, start = 7, end = 9))
  visit_mp4 <- 
    as.numeric(str_sub(list_mp4, start = 11, end = 11))
  df_mp4 <-
    tibble(
      visit     = visit_mp4,
      ID        = order_mp4,
      mp4_file  = list_mp4
    )
  
  # 1st merge & converted column
  df_videos <- 
    full_join(df_flv,
              df_mp4,
              by = c("visit", "ID"))
  
  df_videos$converted <- 
    "YES"
  df_videos$converted[is.na(df_videos$flv_file)] <- 
    "YES (No flv found)"
  df_videos$converted[is.na(df_videos$mp4_file)] <- 
    "NO"
  
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
  
  order_part <- 
    as.numeric(str_sub(list_part_flv, start = 7, end = 9))
  visit_part <- 
    as.numeric(str_sub(list_part_flv, start = 11, end = 11))
  
  df_part <- 
    tibble(
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
  df_videos_2 <-
    rbind(df_videos,
          df_part)
  
  df_videos_2 <- 
    df_videos_2[order(df_videos_2$visit, df_videos_2$ID), ]
  
  # remove duplicate entries for videos that needed to be fixed
  paste0(df_videos_2$visit,
         df_videos_2$ID)
  lgl_dup <- 
    duplicated(paste0(df_videos_2$visit,
                      df_videos_2$ID),
               fromLast = TRUE)
  df_videos_2 <- 
    df_videos_2[!(lgl_dup), ]
  
  vroom::vroom_write(df_videos_2,
                     path = "./video_conversion.csv",
                     delim = ",")
  
  # test <- df_videos_2[is.na(df_videos_2$flv_file) | is.na(df_videos_2$mp4_file), ]
  # df_videos <- df_videos_2[is.na(df_videos_2$flv_file) | is.na(df_videos_2$mp4_file), ]
  # 
  # View(df_videos_2)  
}
copy_and_move_odx <- function(project) {
  switch(
    project,
    "FLAC - Aim 1" = {
      fdr_from <- 
        "S:/_V_PAHRL/FLAC/Colorado/Noldus Observer XT 14"
      fdr_to <- 
        "S:/_V_PAHRL/FLAC/Colorado/Noldus Observer XT 14/1_ODX"
      sub_vis_pattern <- 
        "CO_\\d{4}_\\w{2}"
    },
    "FLAC - Aim 2" = {
      fdr_from <- 
        "S:/_V_PAHRL/FLAC/Noldus Observer XT 14"
      fdr_to <- 
        "S:/_V_PAHRL/FLAC/Noldus Observer XT 14/1_ODX"
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
    stringr::str_subset(pattern = regex(sub_vis_pattern,
                                        ignore_case = TRUE),
                        negate = FALSE) %>% 
    # Keep odx files from irrelevant folders.
    stringr::str_subset(pattern = "1_ODX|Master|z_ARCHIVE|6_Training",
                        negate = TRUE) %>% 
    stringr::str_subset(pattern = "3_Posture|4_Activity",
                        negate = FALSE) %>% 
    # Remove ODX files that contain multiple observations if the coder forgot to
    # only export one observation
    stringr::str_subset(pattern = "_\\w{2} - ",
                        negate = TRUE)
  vec_copied <- 
    vector(mode = "logical",
           length = length(fls_odx))
  
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
unflip_oxford_imgs <- function(folder_day) {
  
  id <- str_sub(folder_day,
                start = 1,
                end = 7)
  
  tri <- str_sub(folder_day,
                 start = 9,
                 end = 9)
  
  # imgs in medium and thumbnail will always be same # and name
  list_img_names <- list.files(path = paste0("./OxfordImageBrowser/2_Browser Images to Annotate/",
                                             id,
                                             "/Tri ",
                                             tri,
                                             "/",
                                             folder_day,
                                             "/medium/"),
                               pattern = ".jpg")
  
  message("Correcting IMG...")
  
  # loop
  for (i in seq_along(list_img_names)) {
    
    cat(i, " ", sep = "")
    img_name <- list_img_names[i]
    
    img_medium <- load.image(file = paste0("./OxfordImageBrowser/2_Browser Images to Annotate/",
                                           id,
                                           "/Tri ",
                                           tri,
                                           "/",
                                           folder_day,
                                           "/medium/",
                                           img_name)) %>% 
      imrotate(angle = 180)
    
    img_thumb <- load.image(file = paste0("./OxfordImageBrowser/2_Browser Images to Annotate/",
                                          id,
                                          "/Tri ",
                                          tri,
                                          "/",
                                          folder_day,
                                          "/thumbnail/",
                                          img_name)) %>% 
      imrotate(angle = 180)
    
    save.image(img_medium,
               file = paste0("./OxfordImageBrowser/2_Browser Images to Annotate/",
                             id,
                             "/Tri ",
                             tri,
                             "/",
                             folder_day,
                             "/medium/",
                             img_name))
    
    save.image(img_thumb,
               file = paste0("./OxfordImageBrowser/2_Browser Images to Annotate/",
                             id,
                             "/Tri ",
                             tri,
                             "/",
                             folder_day,
                             "/thumbnail/",
                             img_name))
    
  }
  
  message("\n",
          "---------------------------------------Done---------------------------------------\n",
          "                   ", folder_day, " Oxford IMGs unflipped")
  
}
view_unconverted_videos <- function(path_flv,
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
create_oxford_images <- function(fdr_load,
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
  # fdr_load <-
  #   "./FLAC_AIM3_DATA/images"
  # fdr_fake_images <- 
  #   "./OxfordImageBrowser-win32-x64/6_Fake Images"
  # fdr_img <- 
  #   "DLW_Pilot_BAY01_2022_03_10"
  
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
extract_autographer_images <- function(fdr_load,
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
extract_frames <- function(fdr_video,
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
  
  # fdr_video <-
  #   "FLAC_AIM3_DATA/PILOT/video"
  # fdr_frame <-
  #   "FLAC_AIM3_DATA/PILOT/images"
  # id_date <- 
  #   "DLW_Pilot_STE01_2022_03_05"
  # ext_frame <-
  #   "jpg"
  
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
  
  cli_progress_done()
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
read_chamber <- function(fdr_raw,
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
    
    if (chk_dtm_split) {
      
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
      
    } else if (chk_delimited) {
      
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
    colnames(df_csv)
    if (chk_final) {
      cli_abort("{fnm_tsv} STAAAAAAAAAAAAAAAAAAAHHHHHHHHHHHPPPPPPPPPPPP")
    }
    
    data.table::fwrite(
      df_csv,
      file = fpa_csv,
      sep = ","
    )
    
  }
}
read_img_irr <- function(fls_pos,
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
read_img_timestamps <- function(fdr_timestamps,
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
clean_noldus <- function(fdr_read,
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
  # fdr_read <- 
  #   fs::path("FLAC_AIM1_DATA",
  #            "1_AIM1_RAW_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "2_AIM1_CLEANED_DATA")
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
    
    cnt <- 
      cnt + 1
    
  }
  
  cli_progress_done()
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
clean_oxford <- function(fdr_raw,
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
  cnt <- 0
  
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
      "Cleaning file #{cnt + 1} out of {length(vec_fpa_raw)} files."
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
    cnt <- 
      cnt + 1
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
shape_ag_raw_flac <- function(fdr_read,
                              fdr_write,
                              fdr_project  = NULL,
                              folder       = "NOLDUS_ACTIVITY",
                              freq         = 100,
                              filter_sub   = NULL,
                              filter_loc   = NULL,
                              project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Add df_start_stop_raw.
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
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "2_AIM1_CLEANED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_project <- 
  #   NULL
  # folder <- 
  #   "GT3X_LW_CSV_RAW"
  # freq <- 100
  # filter_sub <- 
  #   NULL
  # filter_loc <- 
  #   NULL
  # project_only <- 
  #   FALSE
  
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
  df_start_stop_raw <- 
    path("FLAC_AIM1_DATA",
         "1_AIM1_RAW_DATA",
         "df_start_stop.csv") %>% 
    fread(sep = ",")
  df_start_stop_raw[, `:=`(
    start = lubridate::force_tz(
      start,
      tzone = fcase(info_flac_aim == "AIM1", "America/Denver",
                    info_flac_aim == "AIM2", "America/Chicago")
    ),
    stop = lubridate::force_tz(
      stop,
      tzone = fcase(info_flac_aim == "AIM1", "America/Denver",
                    info_flac_aim == "AIM2", "America/Chicago")
    )
  )]
  
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
      df_start_stop[
        study == df_info$study &
          subject == df_info$subject &
          visit == df_info$visit
      ][1]
    
    if (anyNA.data.frame(df_on)) {
      
      df_on <- 
        df_start_stop_raw[
          study == df_info$study &
            subject == df_info$subject &
            visit == df_info$visit
        ]
      
    }
    
    if (nrow(df_on) == 0L) {
      
      cli_warn(c(
        "{paste(df_info[, .(study, subject, visit)], 
                collapse = '_')}",
        "!" = "No entry in df_start_stop from {fdr_write}.",
        "!" = "No entry in df_start_stop from {path('FLAC_AIM1_DATA/1_AIM1_RAW_DATA')}.",
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
shape_ag_sec_flac <- function(fdr_read,
                              fdr_write,
                              fdr_project = NULL,
                              folder = "GT3X_RH_CSV_1SEC",
                              epoch = 1,
                              filter_sub = NULL,
                              filter_loc = NULL,
                              project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Add df_start_stop_raw.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of cleaned ag_second files.
  # ARG: fdr_write
  #      File directory of shaped ag_second files.
  # ARG: folder
  #      Folder name of cleaned ag_second files. Shaped ag_second files will be
  #      written to a folder with same folder name under fdr_write.
  # ARG: fdr_project
  #      File directory to where all the shaped data resides in a project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: filter_loc
  #      Integer vector to subset from vct_fpa_read.
  # ARG: project_only
  #      Should merged files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "2_AIM1_CLEANED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_project <- 
  #   NULL
  # folder <- 
  #   "GT3X_RH_CSV_1SEC"
  # epoch <- 
  #   1
  # filter_sub <- 
  #   NULL
  # filter_loc <- 
  #   NULL
  # project_only <- 
  #   FALSE
  
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
  epoch <- 
    as.integer(epoch)
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
  df_start_stop_raw <- 
    path("FLAC_AIM1_DATA",
         "1_AIM1_RAW_DATA",
         "df_start_stop.csv") %>% 
    fread(sep = ",")
  df_start_stop_raw[, `:=`(
    start = lubridate::force_tz(
      start,
      tzone = fcase(info_flac_aim == "AIM1", "America/Denver",
                    info_flac_aim == "AIM2", "America/Chicago")
    ),
    stop = lubridate::force_tz(
      stop,
      tzone = fcase(info_flac_aim == "AIM1", "America/Denver",
                    info_flac_aim == "AIM2", "America/Chicago")
    )
  )]
  
  for (i in cli_progress_along(vct_fpa_read,
                               format = progress_format,
                               clear = FALSE)) {
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             INFO                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    fpa_read <- 
      vct_fpa_read[i]
    fnm_read <- 
      fs::path_file(fpa_read)
    
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
        str_extract(pattern = "\\d{4}") %>%
        as.integer(),
      epoch_fnm = 
        sbj_epoch %>% 
        str_remove(pattern = "\\d{4}"),
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
      "filter",
      "dte_start",
      "tim_start",
      "epoch_df") := 
        readLines(fpa_read,
                  n = 10) %>%
        str_split(pattern = " ") %>%
        vec_unchop() %>%
        # For weird CO_1001_RH file that has extra commas.
        stringi::stri_replace_all(regex = ",",
                                  replacement = "") %>% 
        vec_chop(indices = list(8, 10, 12, 15, 17, 27, 24, 30))]
    df_info[, `:=`(dtm_start =
                     paste(dte_start, tim_start, sep = " ") %>%
                     lubridate::mdy_hms(tz = df_info$time_zone),
                   dte_start = NULL,
                   tim_start = NULL)]
    setcolorder(df_info, c("study", "subject", "visit"))
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    df_shp <- 
      data.table::fread(
        fpa_read,
        skip = 10,
        sep = ",",
        header = TRUE,
        showProgress = FALSE,
      )
    # Consistency: lowercase and separate with underscore.
    setnames(
      df_shp, 
      function(.x) 
        stri_trans_tolower(.x) %>% 
        str_replace(" ",
                    "_")
    )
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            SHAPE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Axis names.
    setnames(
      df_shp,
      old = c("axis1", "axis2", "axis3"),
      new = c("axis_1", "axis_2", "axis_3")
    )
    
    # On & off
    df_shp[, `:=`(
      datetime = 
        stri_c(date, time, sep = " ") %>% 
        lubridate::mdy_hms(tz = ..df_info$time_zone)
    )]
    df_on <- 
      df_start_stop[study == df_info$study &
                      subject == df_info$subject &
                      visit == df_info$visit][1]
    
    if (anyNA.data.frame(df_on)) {
      
      df_on <- 
        df_start_stop_raw[
          study == df_info$study &
            subject == df_info$subject &
            visit == df_info$visit
        ]
      
    }
    
    if (nrow(df_on) == 0L) {
      
      cli_warn(c(
        "{paste(df_info[, .(study, subject, visit)], 
                collapse = '_')}",
        "!" = "No entry in df_start_stop from {fdr_write}.",
        "!" = "No entry in df_start_stop from {path('FLAC_AIM1_DATA/1_AIM1_RAW_DATA')}.",
        "i" = "No on/off filtering done."
      ))
      
    } else {
      
      df_shp <- 
        df_shp[between(datetime,
                       lower = df_on$start,
                       upper = df_on$stop), ]
      
    }
    
    # Add/remove variables
    df_shp[, `:=`(
      study    = ..df_info$study,
      subject  = ..df_info$subject,
      visit    = ..df_info$visit,
      date     = lubridate::date(datetime),
      time     = format(datetime, "%H:%M:%S"),
      datetime = lubridate::with_tz(datetime,
                                    tzone = "UTC")
    )]
    df_shp[, c(
      "steps", "inclinometer_off", "inclinometer_standing", 
      "inclinometer_sitting", "inclinometer_lying"
    ) := NULL]
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
  
  cli_progress_done()
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
shape_chamber <- function(fdr_read,
                          fdr_write,
                          fdr_project = NULL,
                          folder = "CHAMBER",
                          filter_sub = NULL,
                          project_only = FALSE) {
  
  # Move getting intensity and second by second into DOINT specific stuff.
  # Match format to how Dr. Strath would like it.
  
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
  # ARG: project_only
  #      Should merged files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "2_AIM1_CLEANED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_project <-
  #   NULL
  # folder <-
  #   "CHAMBER"
  # filter_sub <-
  #   NULL
  # project_only <- FALSE
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   project_only = project_only,
                   type         = "Shap",
                   file_source  = "CHAMBER")
  vct_fpa_read <- 
    get_fpa_read(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_source_1 = folder,
      filter_sub    = filter_sub
    )
  
  # for(i in seq_along(vct_fpa_read)) {
  for (i in cli_progress_along(vct_fpa_read,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_read <- 
      vct_fpa_read[i]
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
      data.table::fread(
        input = fpa_read,
        sep = ",",
        header = TRUE
      ) %>% 
      rename_with(.cols = everything(),
                  .fn = str_to_lower) %>% 
      as.data.table()
    
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                            SHAPE                          ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    df_shp <- 
      bind_cols(df_info[, .(study, subject, visit)],
                df_cln) %>% 
      transmute(
        study, subject, visit,
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
                  .fn = ~paste(str_to_lower(df_info$file_source),
                               .x,
                               sep = "_")) %>% 
      as.data.table()
    
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
        sep = ","
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
      sep = ","
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
        sep = ","
      )
      arrow::write_feather(
        df_shp,
        sink = fs::path_ext_set(path = fpa_project,
                                ext = "feather")
      )
    }
    
    # message(i, fnm_read)
    cnt <- 
      cnt + 1
    
  }
  
  cli_progress_done()
  cli_alert_success("SUCCESS. {cnt} File{?/s} {info_function}ed")
  
}
shape_img_irr <- function(fdr_irr,
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
shape_noldus <- function(fdr_read,
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
shape_oxford <- function(type,
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
  # fdr_cln <- 
  #   "./3_data/1_cleaned"
  # fdr_shp <- 
  #   "./3_data/2_shaped"
  # df_timestamps <-
  #   NULL
  # # df_timestamps <-
  # #   tib_img_stamps
  # key_coder_initials <- 
  #   c("bayer"        = "AB",
  #     "smith"        = "AS",
  #     "peraltawerns" = "AP",
  #     "miller"       = "EM",
  #     "martinez"     = "JM",
  #     "chang"        = "LC",
  #     "almanza"      = "MA")
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
shape_rmr <- function(fdr_read,
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
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "2_AIM1_CLEANED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_project <-
  #   NULL
  # folder <-
  #   "RMR"
  # filter_sub <-
  #   NULL
  # project_only <- FALSE
  
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
      vct_fpa_read[i]
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
    #   !near(df_cln$vo2_l_min[2],
    #         ((df_cln$vo2_l_min[1] * 1000) / df_cln$wt_kg[1]),
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
merge_chamber_rmr <- function(fdr_read,
                              fdr_write,
                              fdr_project = NULL,
                              fld_chm = "CHAMBER",
                              fld_rmr = "RMR",
                              fld_merge = paste(fld_chm,
                                                fld_rmr,
                                                sep = "_"),
                              filter_sub = NULL,
                              project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   Go back to naming function merge_noldus instead of merge_data.
  # -   Incorporate filter_sub code.
  # -   Incorporate project code.
  # -   Incorporate initiate_wrangle as it is the same across all wrangle functions.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read_noldus
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of shaped noldus files.
  # ARG: fdr_write
  #      File directory of merged noldus files.
  # ARG: fld_chm
  #      Folder name of shaped chamber activity files.
  # ARG: fld_rmr
  #      Folder name of shaped rmr posture files.
  # ARG: fld_merge
  #      Folder name to save merged chamber/rmr files to.
  # ARG: fdr_project
  #      File directory to where all the merged data resides in the project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: project_only
  #      Should merged files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "4_AIM1_MERGED_DATA")
  # fdr_project <-
  #   NULL
  # fld_chm <-
  #   "CHAMBER"
  # fld_rmr <-
  #   "RMR"
  # fld_merge <-
  #   paste(fld_chm,
  #         fld_rmr,
  #         sep = "_")
  # filter_sub <-
  #   NULL
  # project_only <- FALSE
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   project_only = project_only,
                   type         = "Merg",
                   file_source  = "CHAMBER & RMR")
  lst_all <- 
    list()
  vct_fpa_read <- 
    get_fpa_read(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_source_1 = fld_chm,
      name_source_2 = fld_rmr,
      name_merged   = fld_merge,
      filter_sub    = filter_sub
    )
  
  vct_fpa_chm <- 
    vct_fpa_read %>% 
    fs::path_filter(glob = paste0("*/*", fld_chm, "/*"))
  vct_fpa_rmr <- 
    vct_fpa_read %>% 
    fs::path_filter(glob = paste0("*/*", fld_rmr, "/*"))
  info_study <- 
    vct_fpa_read %>% 
    fs::path_file() %>% 
    str_split(pattern = "_") %>% 
    pluck(1, 1)
  
  for (i in cli_progress_along(vct_fpa_chm,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_read <- 
      vct_fpa_chm[i]
    fnm_read <- 
      fs::path_file(fpa_read)
    fpa_rmr <- 
      fnm_read %>% 
      fs::path_ext_remove() %>% 
      str_replace(pattern = fld_chm,
                  replacement = fld_rmr) %>% 
      str_subset(vct_fpa_rmr,
                 pattern = .)
    
    if(is_empty(fpa_rmr)) {
      cli_warn(c(
        "File #{i}: {fnm_read}",
        "!" = "No accompanying posture file found.",
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
        arrow::read_feather(fpa_read,
                            col_select = c(study:time,
                                           chamber_vo2_ml_min)),
        arrow::read_feather(fpa_rmr,
                            col_select = c(study:rmr_mass_kg, 
                                           rmr_vo2_ml_kg_min)),
        by = c("study", "subject", "visit")
      ) %>% 
      mutate(chamber_vo2_ml_kg_min = chamber_vo2_ml_min / rmr_mass_kg,
             mets_rmr = chamber_vo2_ml_kg_min / rmr_vo2_ml_kg_min,
             mets_standard = chamber_vo2_ml_kg_min / 3.5) %>% 
      select(study:time,
             chamber_vo2_ml_kg_min,
             rmr_vo2_ml_kg_min:last_col()) %>% 
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
                 regexp = fld_merge)[
                   which.min(
                     dir_ls(fdr_project,
                            type = "directory",
                            regexp = fld_merge) %>% 
                       str_length()
                   )
                 ],
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
               regexp = fld_merge)[
                 which.min(
                   dir_ls(fdr_write,
                          type = "directory",
                          regexp = fld_merge) %>% 
                     str_length()
                 )
               ],
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
                 regexp = fld_merge)[
                   which.min(
                     dir_ls(fdr_project,
                            type = "directory",
                            regexp = fld_merge) %>% 
                       str_length()
                   )
                 ],
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
               regexp = fld_merge)[
                 which.min(
                   dir_ls(fdr_project,
                          type = "directory",
                          regexp = fld_merge) %>% 
                     str_length()
                 )
               ],
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
             regexp = fld_merge)[
               which.min(
                 dir_ls(fdr_write,
                        type = "directory",
                        regexp = fld_merge) %>% 
                   str_length()
               )
             ],
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
               regexp = fld_merge)[
                 which.min(
                   dir_ls(fdr_project,
                          type = "directory",
                          regexp = fld_merge) %>% 
                     str_length()
                 )
               ],
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
merge_chamber_ag_model_estimates <- function(fdr_read,
                                             fdr_write,
                                             fdr_project  = NULL,
                                             fnm_acc      = "AG_MODEL_ESTIMATES",
                                             fnm_chm_rmr  = "CO_ALL_CHAMBER_RMR.feather",
                                             fnm_merge    = "CHAMBER_RMR_AG_MODEL",
                                             filter_sub   = NULL,
                                             project_only = FALSE) {
  
  ###  VERSION 2  :::::::::::::::::::::::::::::::::::::::::::::::::
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # - Make sure it works by keeping {study}_ALL_MODEL_ESTIMATES in the shaped
  #   folder.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # - initiate_wrangle
  # - get_fpa_read
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # fdr_read
  #   File directory of {study}_ALL_MODEL_ESTIMATES files.
  # fdr_write
  #   File directory of merged chamber_rmr files and where merged_chamber_rmr
  #   file will reside
  # fdr_project
  #   File directory to where all the merged data resides in the project. If
  #   this is supplied then files are written to both fdr_write and fdr_project.
  # fnm_acc
  #   Folder name of shaped noldus activity files.
  # fnm_chm_rmr
  #   Folder name of shaped noldus posture files.
  # fnm_merge
  #   Folder name to save merged activity/posture files to.
  # filter_sub
  #   Vector of subjects to filter the vct_fpa_read base.
  # project_only
  #   Should merged files only be written to fdr_project?
  ###  TODO  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # - Remove code after compute_acc_model_estimates is updated.
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "4_AIM1_MERGED_DATA")
  # fdr_project <-
  #   NULL
  # fnm_acc <-
  #   "AG_MODEL_ESTIMATES"
  # fnm_chm_rmr <-
  #   "CO_ALL_CHAMBER_RMR.feather"
  # fnm_merge <-
  #   "CHAMBER_RMR_AG_MODEL"
  # filter_sub <-
  #   NULL
  # project_only <-
  #   FALSE
  
  fpa_chm <- 
    dir_ls(path    = fdr_write,
           recurse = TRUE,
           regexp  = fnm_chm_rmr)
  # Use the most recent AG_MODEL_ESTIMATES file if there are multiple.
  fpa_acc <- 
    chuck(
      .x = dir_ls(path   = fdr_read,
                  regexp = fnm_acc),
      dir_ls(path   = fdr_read,
             regexp = fnm_acc) |> 
        path_file() |> 
        stri_extract_first_regex(pattern = "\\d{4}-\\d{2}-\\d{2}") |> 
        lubridate::as_date() |> 
        lubridate::seconds() |> 
        which.max()
    )
  
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                            MERGE                          ----
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  df_mer <- 
    left_join(
      arrow::read_feather(fpa_acc),
      arrow::read_feather(fpa_chm),
      by = c("study", "subject", "visit", "datetime")
    ) %>% 
    mutate(
      date = as_date(datetime),
      time = 
        datetime |> 
        with_tz(tzone = "America/Chicago") |> 
        format("%H:%M:%S"),
      intensity_rmr = 
        fcase(
          mets_rmr < 1.5, "sedentary",
          mets_rmr >= 1.5 & mets_rmr < 3.0, "light",
          mets_rmr >= 3.0, "mvpa"
        ) |> 
        factor(levels = c("sedentary", "light", "mvpa")),
      intensity_standard =
        fcase(
          mets_standard < 1.5, "sedentary",
          mets_standard >= 1.5 & mets_standard < 3.0, "light",
          mets_standard >= 3.0, "mvpa"
        ) |> 
        factor(levels = c("sedentary", "light", "mvpa")),
      # TODO: Change this in compute_acc_model_estimates when you can.
      marcotte = 
        marcotte |> 
        forcats::fct_relabel(stri_trans_tolower) |> 
        fct_collapse(mvpa = c("moderate", "vigorous"))
      # TODO: END
    ) %>% 
    as_tibble() |> 
    fill(chamber_vo2_ml_kg_min:intensity_standard,
         .direction = "up") |> 
    select(study:datetime, date, time,
           chamber_vo2_ml_kg_min, rmr_vo2_ml_kg_min,
           mets_rmr:intensity_standard,
           sojourn3x = sojourn_3x,
           everything()) |> 
    rename_with(.cols = !study:intensity_standard,
                .fn = ~stri_c("intensity_", .x)) |> 
    # Only complete cases; when there is chamber data.
    filter(!is.na(chamber_vo2_ml_kg_min)) |>
    as.data.table()
  
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                            WRITE                          ----
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  fnm_write <- 
    stri_c(
      fnm_chm_rmr %>% 
        str_split(pattern = "_") %>% 
        vec_unchop() %>% 
        vec_slice(c(1, 2)) |> 
        stri_c(collapse = "_"),
      fnm_merge,
      fpa_acc |> 
        path_file() |> 
        stri_extract_first_regex(pattern = "\\d{4}-\\d{2}-\\d{2}"),
      sep = "_"
    )
  
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
    return()
  }
  
  fpa_write <- 
    fs::path(
      fdr_write,
      fnm_write
    )
  data.table::fwrite(
    df_mer,
    file = fs::path_ext_set(path = fpa_write,
                            ext = "csv"),
    sep = ",",
    showProgress = FALSE
  )
  arrow::write_feather(
    df_mer,
    sink = fs::path_ext_set(path = fpa_write,
                            ext = "feather")
  )
  
  if (!is_empty(fdr_project)) {
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
  
  cli_alert_success("SUCCESS.")
  
}
merge_noldus <- function(fdr_read,
                         fdr_write,
                         fdr_project = NULL,
                         fld_act = "NOLDUS_ACTIVITY",
                         fld_pos = "NOLDUS_POSTURE",
                         fld_merge = "NOLDUS_ACTIVITY_POSTURE",
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
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "4_AIM1_MERGED_DATA")
  # fdr_project <-
  #   NULL
  # fld_act <-
  #   "NOLDUS_ACTIVITY"
  # fld_pos <-
  #   "NOLDUS_POSTURE"
  # fld_merge <-
  #   "NOLDUS_ACTIVITY_POSTURE"
  # filter_sub <-
  #   NULL
  # project_only <- FALSE
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   project_only = project_only,
                   type         = "Merg",
                   file_source  = "NOLDUS")
  lst_all <- 
    list()
  vct_fpa_read <- 
    get_fpa_read_noldus(
      fdr_read      = fdr_read,
      fdr_write     = fdr_write,
      name_activity = fld_act,
      name_posture  = fld_pos,
      name_merged   = fld_merge,
      filter_sub    = filter_sub
    )
  vct_fpa_act <- 
    vct_fpa_read %>% 
    fs::path_filter(glob = paste0("*/*", fld_act, "/*"))
  vct_fpa_pos <- 
    vct_fpa_read %>% 
    fs::path_filter(glob = paste0("*/*", fld_pos, "/*"))
  info_study <- 
    vct_fpa_read %>% 
    fs::path_file() %>% 
    str_split(pattern = "_") %>% 
    pluck(1, 1)
  
  for (i in cli_progress_along(vct_fpa_act,
                               format = progress_format,
                               clear = FALSE)) {
    
    fpa_read <- 
      vct_fpa_act[i]
    fnm_read <- 
      fs::path_file(fpa_read)
    fpa_pos <- 
      fnm_read %>% 
      str_remove(pattern = "_(?!.*_).*") %>% 
      str_replace(pattern = "ACTIVITY",
                  replacement = "POSTURE") %>% 
      str_subset(vct_fpa_pos,
                 pattern = .)
    
    if(is_empty(fpa_pos)) {
      cli_warn(c(
        "File #{i}: {fnm_read}",
        "!" = "No accompanying posture file found.",
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
        arrow::read_feather(fpa_pos),
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
      # Fix driving whoopsie-daisy of calling it light.
      mutate(
        intensity_noldus = 
          fifelse(
            test = 
              behavior_noldus == "transportation" &
              activity_noldus == "driving automobile",
            yes  = "sedentary",
            no   = intensity_noldus,
            na   = NA_character_
          )
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
merge_noldus_chamber_rmr <- function(fdr_read,
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
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "4_AIM1_MERGED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "4_AIM1_MERGED_DATA")
  # fdr_project <-
  #   NULL
  # fld_nld <- 
  #   "NOLDUS_ACTIVITY_POSTURE"
  # fld_chm <- 
  #   "CHAMBER_RMR"
  # fld_merge <- 
  #   "NOLDUS_CHAMBER_RMR"
  # filter_sub <-
  #   NULL
  # project_only <- FALSE
  
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
      vct_fpa_nld[i]
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
process_duration_files <- function(vct_variable,
                                   vct_source,
                                   fdr_read,
                                   fdr_write,
                                   fld_mer,
                                   fnm_mer,
                                   ge_than = NULL) {
  
  ###  VERSION 6  :::::::::::::::::::::::::::::::::::::::::::::::::
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # - Make sure it works if the merged file is not in a sub-directory.
  # - If ge_than is not null, export ge_than merged file for analysis.
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # FUNCTION: seq_duration
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # vct_variable
  #   Self-explanatory
  # vct_source
  #   Self-explanatory
  # fdr_read
  #   File directory of merged file.
  # fdr_write
  #   File directory of processed file.
  # fld_mer
  #   Folder name of containing fnm_mer.
  # fnm_mer
  #   File name of merged file with all subject, visit entries in feather
  #   format.
  # ge_than
  #   A list with the first element containing a variable, the second element
  #   containing the source for the variable and the third element containing
  #   the value the variable_source has to be >= than.
  ###  TODO  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # - Remove code after shape_noldus is updated.
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # vct_variable <-
  #   c("posture",
  #     "behavior",
  #     "intensity")
  # vct_source <-
  #   c("noldus",
  #     "rmr",
  #     "standard")
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "4_AIM1_MERGED_DATA")
  # fdr_write <-
  #   fs::path("S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
  #            "MARTINEZ", "2_Conferences", "2022_ICAMPAM",
  #            "3_data", "4_processed")
  # fld_mer <-
  #   "NOLDUS_CHAMBER_RMR"
  # fnm_mer <-
  #   "CO_ALL_NOLDUS_CHAMBER_RMR.feather"
  # ge_than <-
  #   NULL
  # list("behavior", "noldus", 60)
  # fld_mer <-
  #   NULL
  # fnm_mer <-
  #   "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-05-08.feather"
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "5_AIM1_PROJECTS",
  #            "AIM1_WRIST_ACC_CHAMBER_COMPARISON_HLTHY",
  #            "1_data", "4_processed")
  
  if (is_null(fld_mer)) {
    
    fpa_mer <- 
      path(fdr_read,
           fnm_mer)
    
  } else {
    
    fpa_mer <- 
      path(fdr_read,
           list.files(path    = fdr_read,
                      pattern = fld_mer),
           fnm_mer)
    
  }
  
  df_mer <- 
    fpa_mer |> 
    arrow::read_feather() |>
    # TODO: Whenver dark/obscured/oof is coded for posture, intensity is ""
    #       Explicitly state that it is dark/obscured/oof.
    #       Also when looking at noldus_chamber_rmr merged files, the chamber
    #       data is not filled up.
    mutate(across(.cols = starts_with("intensity"),
                  .fns = 
                    ~factor(.x,
                            levels = c("sedentary",
                                       "light",
                                       "mvpa")) |> 
                    fct_explicit_na(na_level = "dark/obscured/oof")
    )) |> 
    # TODO: END
    as.data.table()
  
  if (!is_null(ge_than)) {
    
    chk_variable <- 
      !ge_than[[1]] %in% vct_variable
    chk_source <- 
      !ge_than[[2]] %in% vct_source
    chk_cutoff <- 
      !is.numeric(ge_than[[3]])
    
    if (any(chk_variable, chk_source, chk_cutoff)) {
      
      loc_error <- 
        which(
          c(chk_variable, chk_source, chk_cutoff)
        ) |> 
        as.character()
      
      error_1 <- 
        "First value in {.arg ge_than} is not in vct_variable."
      error_2 <- 
        "Second value in {.arg ge_than} is not in vct_source."
      error_3 <- 
        "Third value in {.arg ge_than} is not a numeric."
      
      switch(
        EXPR = loc_error,
        "123" = cli_abort(
          message = c("!" = error_1, "!" = error_2, "!" = error_3)
        ),
        "12"  = cli_abort(
          message = c("!" = error_1, "!" = error_2)
        ),
        "13"  = cli_abort(
          message = c("!" = error_1,"!" = error_3)
        ),
        "23"  = cli_abort(
          message = c("!" = error_2, "!" = error_3)
        ),
        "1"   = cli_abort(
          message = c("!" = error_1)
        ),
        "2"   = cli_abort(
          message = c("!" = error_2)
        ),
        "3"   = cli_abort(
          message = c("!" = error_3)
        )
      )
      
    }
    
    variable_source <- 
      stri_c(ge_than[[1]], "_", ge_than[[2]])
    df_mer <- 
      df_mer |> 
      group_by(study, subject, visit) |> 
      as_tibble() |> 
      # TODO: Remove duration_noldus columns from shape_noldus
      mutate(duration_behavior_noldus = NULL,
             duration_posture_noldus = NULL) |> 
      # TODO: END
      mutate(
        "duration_{variable_source}" :=
          seq_duration(vct_datetime = datetime,
                       vct_value = .data[[variable_source]]),
        "less_than_{ge_than[[3]]}" := 
          .data[[glue::glue("duration_{variable_source}")]] < ge_than[[3]],
        .after = time
      ) |> 
      mutate(across(
        .cols = !c(study:glue::glue("less_than_{ge_than[[3]]}")),
        function(.x) {
          if (is.factor(.x)) {
            fifelse(.data[[glue::glue("less_than_{ge_than[[3]]}")]],
                    yes = 
                      factor(NA,
                             levels = levels(.x)),
                    no  = .x)
          } else {
            base::ifelse(test = .data[[glue::glue("less_than_{ge_than[[3]]}")]],
                         yes  = NA,
                         no   = .x)
          }
        }
      )) |> 
      as.data.table()
    
    # Write for confusion matrix and agreement functions.
    fnm_ge_than <- 
      stri_c(
        path_ext_remove(fnm_mer),
        "DUR",
        ge_than[[1]] |> stri_sub(to = 3) |> stri_trans_toupper(),
        ge_than[[2]] |> stri_sub(to = 3) |> stri_trans_toupper(),
        "GE",
        ge_than[[3]],
        sep = "_"
      )
    
    if (is_null(fld_mer)) {
      
      arrow::write_feather(
        df_mer,
        sink = path(fdr_read,
                    path_ext_set(fnm_ge_than,
                                 ext = "feather"))
      )
      fwrite(
        df_mer,
        file = path(fdr_read,
                    path_ext_set(fnm_ge_than,
                                 ext = "csv")),
        sep = ","
      )
      
    } else {
      
      arrow::write_feather(
        df_mer,
        sink = path(fdr_read,
                    list.files(path    = fdr_read,
                               pattern = fld_mer),
                    path_ext_set(fnm_ge_than,
                                 ext = "feather"))
      )
      fwrite(
        df_mer,
        file = path(fdr_read,
                    list.files(path    = fdr_read,
                               pattern = fld_mer),
                    path_ext_set(fnm_ge_than,
                                 ext = "csv")),
        sep = ","
      )
      
    }
    
    
    # testt <- 
    # df_mer |>
    #   count(study, subject, visit, intensity_rmr) |>
    #   count(study, subject, visit, wt = n)
    # df_mer |>
    #   count(study, subject, visit, intensity_noldus) |>
    #   count(study, subject, visit, wt = n)
    # df_mer |>
    #   count(study, subject, visit, posture_noldus) |>
    #   count(study, subject, visit, wt = n)
    
  }
  
  vct_var_src <- 
    # Get all combinations between variables and sources.
    expand_grid(variable = vct_variable,
                source = vct_source) %>% 
    unite(col = "unite",
          variable, source,
          sep = "_") |> 
    pull() |> 
    # Only keep the combinations that are in the data.frame.
    stri_subset_regex(pattern = stri_c(names(df_mer), collapse = "|"))
  
  lst_duration <- 
    list()
  variable_source <- 
    ""
  cnt <- 
    0
  progress_format <- 
    stri_c(
      "Getting duration for variable_source {variable_source} | ",
      "{cli::pb_current}/{cli::pb_total} ({cli::pb_percent}) | ",
      "[{cli::pb_elapsed}] | {cli::pb_eta_str}"
    )
  
  cli_alert_info("Processing duration file from {fnm_mer} for variable_sources:")
  cli_alert_info(stri_c(vct_var_src, collapse = " "))
  
  if (!is_null(ge_than)) {
    
    cli_alert_info(
      stri_c(
        "Merged data filtered by {ge_than[[1]]}_{ge_than[[2]]} values",
        ">= {ge_than[[3]]} seconds",
        sep = " "
      )
    )
    
  } else {
    
    cli_alert_info(
        "Merged data not filtered by any variable_source."
    )
    
  }
  
  for (i in cli_progress_along(vct_var_src,
                               format = progress_format,
                               clear = FALSE)) {
    
    variable_source <- 
      vct_var_src[i]
    c(.variable, .source) %<-% (
      variable_source |> 
        stri_split_regex(pattern = "_") |> 
        vec_unchop() |> 
        vec_chop()
    )
    
    df_dur_var_source <- 
      df_mer |> 
      select(1:datetime, 
             all_of(variable_source)) %>% 
      group_by(study, subject, visit) |> 
      transmute(
        datetime,
        .data[[variable_source]],
        event    = rleid(.data[[variable_source]]),
        duration = seq_duration(vct_datetime = datetime,
                                vct_value    = .data[[variable_source]])
      ) |> 
      as_tibble() |> 
      group_by(study, subject, visit, event) |> 
      # As merged df treats a second value as what occurred UP TO THAT 
      # SECOND, the start time is the minimum value within an event minus one.
      # Do this for every value EXCEPT for the very first event as that is the
      # start time to the visit which is always treated ANCHOR NOT AS A DATA
      # POINT.
      summarise(
        source     = .source,
        variable   = .variable,
        value      = .data[[variable_source]][1],
        start_dttm = min(datetime) - 1,
        stop_dttm  = max(datetime),
        duration   = duration[1],
        .groups = "drop"
      ) |> 
      relocate(event,
               .after = last_col()) |> 
      as.data.table()
    setkey(df_dur_var_source, NULL)
    df_dur_var_source[event == 1, start_dttm := start_dttm + 1]
    
    lst_duration[[i]] <-
      df_dur_var_source
    
    cli_progress_update()
    cnt <- 
      cnt + 1
    
  }
  
  cli_progress_done()
  
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                            WRITE                          ----
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(is_null(fld_mer)) {
    
    fld_mer <- 
      fnm_mer |> 
      path_ext_remove() |> 
      stri_replace_all_regex(pattern = ".*ALL_",
                             replacement = "")
    
  }
  
  df_duration <- 
    lst_duration %>% 
    rbindlist()
  fnm_write <- 
    stri_c(
      df_duration$study[1],
      "ALL",
      if (!is_null(ge_than)) {
        stri_c("DUR",
               ge_than[[1]] |> stri_sub(to = 3) |> stri_trans_toupper(),
               ge_than[[2]] |> stri_sub(to = 3) |> stri_trans_toupper(),
               "GE",
               ge_than[[3]],
               sep = "_")
      } else {
        "DURATION"
      },
      fld_mer,
      sep = "_"
    )
  arrow::write_feather(
    df_duration,
    sink = path(fdr_write,
                path_ext_set(fnm_write, "feather"))
  )
  fwrite(
    df_duration,
    file = path(fdr_write,
                path_ext_set(fnm_write, "csv")),
    sep = ","
  )
  
  cli_alert_success("SUCCESS. {cnt} File{?/s} processed")
  
}
}
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####                                                                         %%%%
####                           SUMMARIZE FUNCTIONS                           ----
####                                                                         %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
summarize_duration <- function(sample_lengths,
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

summarize_value_distribution <- function() {
  
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
compute_acc_model_estimates <- function(fdr_read,
                                        fdr_write,
                                        fdr_project = NULL,
                                        folder = NULL,
                                        filter_sub = NULL,
                                        filter_loc = NULL,
                                        project_only = FALSE) {
  
  ###  CHANGES  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # -   First version
  ###  FUNCTIONS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # -   initiate_wrangle
  # -   get_fpa_read
  ###  ARGUMENTS  :::::::::::::::::::::::::::::::::::::::::::::::::
  # ARG: fdr_read
  #      File directory of cleaned ag_second files.
  # ARG: fdr_write
  #      File directory of shaped ag_second files.
  # ARG: folder
  #      Folder name of cleaned ag_second files. Shaped ag_second files will be
  #      written to a folder with same folder name under fdr_write.
  # ARG: fdr_project
  #      File directory to where all the shaped data resides in a project. If
  #      this is supplied then files are written to both fdr_write and fdr_project.
  # ARG: filter_sub
  #      Vector of subjects to filter the vct_fpa_read base.
  # ARG: filter_loc
  #      Integer vector to subset from vct_fpa_read.
  # ARG: project_only
  #      Should merged files only be written to fdr_project?
  ###  TESTING  :::::::::::::::::::::::::::::::::::::::::::::::::::
  # fdr_read <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "3_AIM1_SHAPED_DATA")
  # fdr_write <-
  #   fs::path("FLAC_AIM1_DATA",
  #            "5_AIM1_PROJECTS",
  #            "AIM1_WRIST_ACC_CHAMBER_COMPARISON_HLTHY")
  # fdr_project <-
  #   NULL
  # folder <-
  #   c("GT3X_RH_CSV_1SEC",
  #     "GT3X_RW_CSV_1SEC",
  #     "GT3X_RW_CSV_RAW")
  # filter_sub <-
  #   NULL
  # filter_loc <-
  #   NULL
  # project_only <-
  #   FALSE
  
  source(file = "./R/acc_models/acc_models.R")
  load(file = "./R/acc_models/lyden_2014.RData")
  load(file = "./R/acc_models/staudenmayer_2015.RData")
  library(MOCAModelData)
  library(zoo)
  
  initiate_wrangle(fdr_read     = fdr_read,
                   fdr_project  = fdr_project,
                   filter_sub   = filter_sub,
                   filter_loc   = filter_loc,
                   project_only = project_only,
                   type         = "Comput",
                   file_source  = "GT3X")
  vct_fpa_read <- 
    fs::dir_ls(
      path        = fdr_read,
      recurse     = FALSE,
      all         = TRUE,
      type        = "directory",
      regexp      = paste(folder,
                          # name_source_1,
                          sep = "|",
                          collapse = "|"),
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
  # }
  
  if (!is_empty(filter_loc)) {
    
    vct_fpa_read <- 
      vec_slice(vct_fpa_read,
                filter_loc)
    
  }
  
  # Get vector of subjects.
  vct_subject <- 
    vct_fpa_read |> 
    path_file() |> 
    stri_extract(regex = "\\d{4}") |> 
    unique()
  lst_estimate <- 
    list()
  devtools::unload("dtplyr")
  
  if (file_exists(path(fdr_write,
                       "CO_ALL_model_estimates_IN_PROGRESS.rds"))) {
    
    # See which subjects are left to run function on.
    lst_estimate_wip <- 
      readRDS(file = path(fdr_write,
                          "CO_ALL_model_estimates_IN_PROGRESS.rds"))
    filter_sub <- 
      map_chr(1:length(lst_estimate_wip),
              .f = \(.x) pluck(lst_estimate_wip, .x, 2, 1))
    vct_subject <- 
      vct_subject[!vct_subject %in% filter_sub]
    
  }
  
  for (i in seq_along(vct_subject)) {
    
    subject <- 
      vct_subject[i]
    visit <- 
      vct_fpa_read[i] |> 
      path_file() |> 
      stri_extract(regex = "\\d{1}")
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                             READ                           ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    message("\n",
            subject, " ", visit,
            "\n",
            appendLF = TRUE)
    
    vct_fpa_read_sub <- 
      vct_fpa_read |> 
      path_filter(regexp = subject) |> 
      path_filter(regexp = visit)
    
    df_rh_1sec <- 
      arrow::read_feather(
        file = 
          vct_fpa_read_sub |> 
          stri_subset_regex(pattern = "gt3x_rh_csv_1sec",
                            case_insensitive = TRUE)
      ) |> 
      slice(-1) |> 
      as.data.table()
    df_rw_1sec <- 
      arrow::read_feather(
        file = 
          vct_fpa_read_sub |> 
          stri_subset_regex(pattern = "gt3x_rw_csv_1sec",
                            case_insensitive = TRUE)
      ) |> 
      slice(-1) |> 
      as.data.table()
    df_rw_raw <- 
      arrow::read_feather(
        file = 
          vct_fpa_read_sub |> 
          stri_subset_regex(pattern = "gt3x_rw_csv_raw",
                            case_insensitive = TRUE)
      ) |> 
      slice(-1) |> 
      as.data.table()
    df_rw_raw[, vector_magnitude := sqrt(axis_x ^ 2 + axis_y ^ 2 + axis_z ^ 2)]
    
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                         APPLY MODELS                       ----
    ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    message("Sojourn_3x...",
            appendLF = FALSE)
    est_soj3x <- 
      lyden_2014_soj3x(
        counts   = df_rh_1sec$axis_1,
        counts.2 = df_rh_1sec$axis_2,
        counts.3 = df_rh_1sec$axis_3,
        vect.mag = df_rh_1sec$vector_magnitude,
        short    = 30
      )$METs
    est_soj3x <- 
      case_when(
        est_soj3x <= 1.5                    ~ 0,
        (est_soj3x > 1.5) & (est_soj3x < 3) ~ 1,
        est_soj3x >= 3                      ~ 2
      )
    est_soj3x <- 
      factor(est_soj3x,
             levels = c(0, 1, 2),
             labels = c("sedentary", "light", "mvpa"))
    message("DONE\n",
            "Montoye 2020...",
            appendLF = FALSE)
    est_montoye <- 
      montoye_2020(vm = df_rw_1sec$vector_magnitude)
    message("DONE\n",
            "Rowland 2014...",
            appendLF = FALSE)
    est_rowland <- 
      rowlands_2014(
        raw_x           = df_rw_raw$axis_x,
        raw_y           = df_rw_raw$axis_y,
        raw_z           = df_rw_raw$axis_z,
        vmcorrg_mod_15s = 489,
        samp_freq       = 100, 
        epoch           = 15,
        expand_1sec     = TRUE
      )
    message("DONE\n",
            "Hildebrand 2014...",
            appendLF = FALSE)
    est_hildebrand <- 
      hildebrand_2014(
        raw_x     = df_rw_raw$axis_x,
        raw_y     = df_rw_raw$axis_y,
        raw_z     = df_rw_raw$axis_z,
        freq      = 100,
        win.width = 60
      )
    message("DONE\n",
            "Freedson 1998...",
            appendLF = FALSE)
    est_freedson <- 
      freedson_1998(ag_data_vaxis_hip_1sec = df_rh_1sec$axis_1)
    message("DONE\n",
            "Staudenmayer 2015...",
            appendLF = FALSE)
    est_staudenmayer <- 
      staudenmayer_2015(
        raw_x               = df_rw_raw$axis_x,
        raw_y               = df_rw_raw$axis_y,
        raw_z               = df_rw_raw$axis_z,
        vector_magnitude    = df_rw_raw$vector_magnitude,
        freq                = 100
      )
    message("DONE\n",
            "Marcotte 2021...",
            appendLF = FALSE)
    # est_marcotte <- 
    #   marcotte_2021_soj_g(
    #     data                      = 
    #       df_rw_raw |> 
    #       select(Timestamp = datetime,
    #              AxisX     = axis_x,
    #              AxisY     = axis_y,
    #              AxisZ     = axis_z,
    #              VM        = vector_magnitude) %>% 
    #       as.data.frame(),
    #     export_format             = "seconds",
    #     freq                      = 100,
    #     step1_sd_threshold        = 0.00375,
    #     step2_min_window_length   = 0,
    #     step2_nest_length         = 5,
    #     step3_nest_length         = 60,
    #     step3_orig_soj_length_min = 180
    #   )$step3_estimate_intensity
    # est_marcotte <- 
    #   case_when(
    #     est_marcotte == "Sedentary"                                 ~ 0,
    #     est_marcotte == "Light"                                     ~ 1,
    #     (est_marcotte == "Moderate") | (est_marcotte == "Vigorous") ~ 2
    #   )
    # est_marcotte <- 
    #   factor(est_marcotte,
    #          levels = c(0, 1, 2),
    #          labels = c("sedentary", "light", "mvpa"))
    est_marcotte_fudge = soj_g(data = 
                                 df_rw_raw |> 
                                 select(Timestamp = datetime,
                                        AxisX     = axis_x,
                                        AxisY     = axis_y,
                                        AxisZ     = axis_z,
                                        VM        = vector_magnitude),
                               export_format = "seconds",
                               freq = 100,
                               step1_sd_threshold = .00375,
                               step2_nest_length = 5,
                               step3_nest_length = 60,
                               step3_orig_soj_length_min = 180)$step3_estimate_intensity
    est_marcotte_fudge <-
      case_when(
        est_marcotte_fudge == "Sedentary"                                 ~ 0,
        est_marcotte_fudge == "Light"                                     ~ 1,
        (est_marcotte_fudge == "Moderate") | (est_marcotte_fudge == "Vigorous") ~ 2
      ) |> 
      factor(levels = c(0, 1, 2),
             labels = c("sedentary", "light", "mvpa"))
    
    message("DONE\n",
            appendLF = FALSE)
    
    df_estimate_subject_visit <- 
      data.table(
        study        = "CO",
        subject      = as.integer(subject),
        visit        = as.integer(visit),
        datetime     = df_rw_1sec$datetime,
        sojourn_3x   = est_soj3x,
        montoye      = est_montoye,
        rowland      = est_rowland,
        hildebrand   = est_hildebrand,
        freedson     = est_freedson,
        staudenmayer = est_staudenmayer,
        marcotte     = est_marcotte_fudge
      )
    
    lst_estimate[[i]] <- 
      df_estimate_subject_visit
    
    
  }
  
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                            WRITE                          ----
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if (FALSE) {
    
    # For interactive use and if memory runs out.
    ## TODO: Find a way to automate this, meaning if there is a function
    # that returns true when R is interactive and TRUE when memory has ran out,
    # probably a mixture of tryCatch and some function I dont know about yet.
    saveRDS(lst_estimate,
            file = path(fdr_write,
                        "CO_ALL_model_estimates_IN_PROGRESS.rds"))
    
  }
  
  fnm_estimate <- 
    stri_c(
      "CO_ALL_AG_MODEL_ESTIMATES_",
      Sys.Date(),
      ".feather"
    )
  arrow::write_feather(
    rbindlist(lst_estimate),
    sink = path(fdr_write,
                fnm_estimate)
  )
  
  cli_alert_success(
    "SUCCESS. {length(lst_estimate)} File{?/s} {info_function}ed"
  )
  library(dtplyr)
  
  # df_estimate <- 
  #   rbindlist(lst_estimate) |> 
  #   mutate(marcotte = 
  #            case_when(
  #              marcotte == "Sedentary"                             ~ 0,
  #              marcotte == "Light"                                 ~ 1,
  #              (marcotte == "Moderate") | (marcotte == "Vigorous") ~ 2
  #            ) |> 
  #            factor(levels = c(0, 1, 2),
  #                   labels = c("sedentary", "light", "mvpa")))
  # df_estimate <- 
  #   arrow::read_feather(
  #     path(fdr_write,
  #          "CO_ALL_AG_MODEL_ESTIMATES_2022-05-06.feather")
  #   )
  # df_estimate <- 
  #   df_estimate |> 
  #   rename(sojourn3x_present    = sojourn_3x,
  #          montoye_present      = montoye,
  #          rowland_present      = rowland,
  #          hildebrand_present   = hildebrand,
  #          freedson_present     = freedson,
  #          staudenmayer_present = staudenmayer,
  #          marcotte_present     = marcotte)
  # df_past <- 
  #   fread(file = path(fdr_write,
  #                     "summary_data_2022-03-01.csv"),
  #         sep = ",")
  # df_past <- 
  #   df_past |> 
  #   group_by(participant, time) |> 
  #   mutate(datetime = seq.POSIXt(from = time[1],
  #                                to = time[1] + 59,
  #                                by = 1),
  #          .after = time) |> 
  #   rename(sojourn3x_past = soj,
  #          montoye_past = mont,
  #          rowland_past = rowlands,
  #          hildebrand_past = hilde,
  #          freedson_past = free,
  #          staudenmayer_past = umass,
  #          marcotte_past = mar) |> 
  #   ungroup()
  # test <- 
  #   full_join(df_estimate,
  #             df_past,
  #             by = c("subject" = "participant",
  #                    "datetime")) |> 
  #   select(!contains("chamber")) |> 
  #   select(study:datetime,
  #          starts_with("soj"),
  #          starts_with("mont"),
  #          starts_with("row"),
  #          starts_with("hild"),
  #          starts_with("free"),
  #          starts_with("stau"),
  #          starts_with("mar"))
  # poo <- 
  # test |> 
  #   group_by(subject) |> 
  #   summarize(
  #     sed_mar_pres = sum(marcotte_present == "sedentary",
  #                        na.rm = TRUE),
  #     sed_mar_past = sum(marcotte_past == "sed",
  #                        na.rm = TRUE),
  #     lit_mar_pres = sum(marcotte_present == "light",
  #                        na.rm = TRUE),
  #     lit_mar_past = sum(marcotte_past == "light",
  #                        na.rm = TRUE),
  #     mvp_mar_pres = sum(marcotte_present == "mvpa",
  #                        na.rm = TRUE),
  #     mvp_mar_past = sum(marcotte_past == "mvpa",
  #                        na.rm = TRUE)
  #   )
  # poo
  # fwrite(poo,
  #        file = path(fdr_write,
  #                    "marcotte_intensity_by_visit_2022-05-06.csv"),
  #        sep = ","
  #        )
  # vct_map <-
  #   df_estimate |> 
  #   select(!study:datetime) |> 
  #   names()
  # df_overall_perc <-
  #   map_dfr(.x = vct_map,
  #           .f = function(.x) table(df_estimate[[.x]], useNA = "always") / nrow(df_estimate)) |>
  #   mutate(method = vct_map,
  #          .before = 1) |> 
  #   select(-5)
  # vct_map <-
  #   df_past |> 
  #   select(!c(participant:chamber.METs,
  #          contains("chamber"))) |>
  #   select(sojourn3x_past, montoye_past, rowland_past, hildebrand_past,
  #          freedson_past, staudenmayer_past, marcotte_past) |> 
  #   names()
  # df_overall_perc_past <-
  #   map_dfr(.x = vct_map,
  #           .f = function(.x) table(df_past[[.x]], useNA = "always") / nrow(df_past)) |>
  #   mutate(method = vct_map,
  #          .before = 1) |> 
  #   select(
  #     method, sed, light, mvpa
  #     # everything()
  #   )
  # df_perc_together <- 
  #   bind_cols(df_overall_perc,
  #             df_overall_perc_past |> 
  #               transmute(sed_past = sed,
  #                         light_past = light,
  #                         mvpa_past = mvpa)) %>% 
  #   mutate(sed_diff = (sedentary - sed_past) / sed_past,
  #          light_diff = (light - light_past) / light_past,
  #          mvpa_diff = (mvpa - mvpa_past) / mvpa_past) %>% 
  #   mutate(across(.cols = !method,
  #                 .fns = as.double)) |> 
  #   select(method, starts_with("sed"),
  #          starts_with("light"),
  #          starts_with("mvpa"))
  # 
  # fwrite(
  #   df_perc_together,
  #   file = path(fdr_write,
  #               stri_c("estimate_diff_", "2022-05-08", "_to_", "2022-03-01", ".csv")),
  #   sep = ","
  # )
  
}
compute_agreement <- function() {
  
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
compute_classification <- function() {
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


compute_confusion_matrix <- function() {
  
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
compute_img_irr <- function(tib_mer_schema) {
  
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