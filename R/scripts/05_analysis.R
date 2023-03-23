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
#                                     DOINT                                 ----
####                                                                        %%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Setup -----------------------------------------------------------------------
source("./R/Scripts/02_functions.R")
fdr_raw <- 
  fs::path("FLAC_AIM1_DATA",
           "1_AIM1_RAW_DATA")
fdr_merge <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOINT", "3_data", "3_merged"
  )
fdr_process <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOINT", "3_data", "4_processed"
  )
fdr_result <- 
  fs::path(
    "S:", "_R_CHS_Research", "PAHRL", "Student Access", "0_Students",
    "MARTINEZ", "1_Publication Work", "DOINT", "4_results"
  )
initiate_analysis(fdr_result = fdr_result)

## Summary ---------------------------------------------------------------------
compute_summary_subject_characteristics(
  fdr_read     = fdr_raw,
  fdr_write    = fdr_result,
  fnm_teleform = "COV1MERGED2022-03-09v2.csv",
  filter_sub   = 
    path(fdr_process,
         "CO_ALL_DUR_BEH_NOL_GE_60_NOLDUS_CHAMBER_RMR.feather") |> 
    arrow::read_feather() |> 
    pull(subject) |> 
    unique()
)
compute_summary_visit(
  fdr_read   = fdr_process,
  fdr_write  = fdr_result,
  time_units = "hours" # secs, mins, hours
)
## Bias ------------------------------------------------------------------------
###  INTENSITY  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
#### Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_60.feather",
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_600.feather",
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_1800.feather"
    ),
  .f = 
    ~compute_bias(
      fdr_read          = fdr_process,
      fdr_write         = fdr_result,
      fnm_visit_summary = .x,
      vct_criterion     = c("standard",
                            "rmr"),
      vct_estimate      = c("noldus"),
      output            =   "time" # time, percent
    )
)
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather",
  vct_criterion     = c("standard",
                        "rmr"),
  vct_estimate      = c("noldus"),
  output            =   "time" # time, percent
)
#### Percent ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_60.feather",
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_600.feather",
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_BEH_NOL_GE_1800.feather"
    ),
  .f = 
    ~compute_bias(
      fdr_read          = fdr_process,
      fdr_write         = fdr_result,
      fnm_visit_summary = .x,
      vct_criterion     = c("standard",
                            "rmr"),
      vct_estimate      = c("noldus"),
      output            =   "percent" # time, percent
    )
)
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather",
  vct_criterion     = c("standard",
                        "rmr"),
  vct_estimate      = c("noldus"),
  output            =   "percent" # time, percent
)
###  INACTIVE  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
#### Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DUR_BEH_NOL_GE_60.feather",
      "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DUR_BEH_NOL_GE_600.feather",
      "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DUR_BEH_NOL_GE_1800.feather"
    ),
  .f = 
    ~compute_bias(
      fdr_read          = fdr_process,
      fdr_write         = fdr_result,
      fnm_visit_summary = .x,
      vct_criterion     = c("standard",
                            "rmr"),
      vct_estimate      = c("noldus"),
      output            =   "time" # time, percent
    )
)
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DURATION.feather",
  vct_criterion     = c("standard",
                        "rmr"),
  vct_estimate      = c("noldus"),
  output            =   "time" # time, percent
)
#### Percent ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DUR_BEH_NOL_GE_60.feather",
      "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DUR_BEH_NOL_GE_600.feather",
      "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DUR_BEH_NOL_GE_1800.feather"
    ),
  .f = 
    ~compute_bias(
      fdr_read          = fdr_process,
      fdr_write         = fdr_result,
      fnm_visit_summary = .x,
      vct_criterion     = c("standard",
                            "rmr"),
      vct_estimate      = c("noldus"),
      output            =   "percent" # time, percent
    )
)
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INACTIVE_FROM_DURATION.feather",
  vct_criterion     = c("standard",
                        "rmr"),
  vct_estimate      = c("noldus"),
  output            =   "percent" # time, percent
)

###  PUT ALL RESULTS TOGETHER  ::::::::::::::::::::::::::::::::::::::
# Putting all the bias results together in one table.
combine_bias <- function(fdr_result,
                         variable = "intensity",
                         type     = "minutes") {
  
  # variable = "intensity"
  # type     = "minutes"
  # type     = "percent"
  
  bias_var_type <- 
    stri_c(
      "BIAS",
      stri_trans_toupper(variable),
      stri_trans_toupper(type),
      sep = "_"
    )
  vct_fpa_bias <- 
    dir_ls(
      path   = path(fdr_result,
                    "2_csv"),
      regexp = bias_var_type
    ) |> 
    stri_subset(regex  = "WIDE|LONG",
                negate = TRUE)
  ord_duration <- 
    vct_fpa_bias |> 
    path_file() |> 
    stri_extract_all_regex(pattern = "\\d*") |> 
    vec_unchop() |> 
    stri_remove_empty() |> 
    as.integer() |> 
    order()
  vct_duration <- 
    (vct_fpa_bias |> 
       path_file() |> 
       stri_extract_all_regex(pattern = "\\d*") |> 
       vec_unchop() |> 
       stri_remove_empty() |> 
       as.integer() /
       60) |> 
    as.character()
  
  chk_normal <- 
    vct_fpa_bias |> 
    path_file() |> 
    stri_detect(regex = "DURATION") |> 
    any()
  
  if (chk_normal) {
    
    # vct_fpa_bias includes bias from normal duration file.
    vct_duration <- 
      c(
        "Normal",
        vct_duration[ord_duration]
      )
    vct_fpa_bias[-1] <- 
      vct_fpa_bias[-1][ord_duration]
    
    
  } else {
    
    vct_duration <- 
      vct_duration[ord_duration]
    vct_fpa_bias <- 
      vct_fpa_bias[ord_duration]
    
  }
  
  for (i in seq_along(vct_fpa_bias)) {
    
    fpa_bais <- 
      vct_fpa_bias[i]
    duration <- 
      vct_duration[i]
    
    df_bias <- 
      fread(file = fpa_bais,
            sep = ",") |> 
      mutate(Criterion = 
               Criterion |> 
               stri_trans_totitle(),
             Estimate = 
               Estimate |> 
               stri_trans_totitle() |> 
               stri_c(" Bias"),
             Value    = 
               Value |> 
               stri_trans_totitle()) |> 
      as_tibble()
    df_criterion <- 
      df_bias |> 
      # filter(Estimate == "Freedson") |>
      mutate(Estimate = "Chamber Minutes ± SD") |> 
      rename(duration = `Mean ± SD`) |> 
      select(Criterion, Value, Estimate, duration)
    colnames(df_criterion)[colnames(df_criterion) == "duration"] <- duration
    
    if (stri_trans_toupper(type) == "PERCENT") {
      
      df_bias <- 
        df_bias |> 
        unite(col = !!duration,
              Bias:CI,
              sep = " ")
      
    } else {
      
      df_bias <- 
        df_bias |> 
        unite(col = !!duration,
              `Bias  (minutes)`:`CI  (minutes)`,
              sep = " ")
    }
    
    df_bias <- 
      df_bias |> 
      select(Criterion, Value, Estimate, last_col()) %>% 
      bind_rows(df_criterion,
                .) |> 
      mutate(across(.cols = Criterion:Estimate,
                    .fns = ~forcats::as_factor(.x))) |> 
      arrange(Criterion, Value) |>
      rename(Method    = Estimate,
             Intensity = Value)
    
    if (i == 1) {
      
      df_combined <- 
        df_bias
      
    } else {
      
      df_combined <- 
        left_join(df_combined,
                  df_bias,
                  by = c("Criterion", "Intensity", "Method"))
      
    }
  }
  
  df_combined |> 
    fwrite(
      file = 
        path(
          fdr_result,
          "2_csv",
          stri_c("CO", bias_var_type, "COMBINED_LONG.csv", sep = "_")
        ),
      sep  = ",",
      bom  = TRUE
    )
  df_combined |> 
    pivot_longer(cols      = !Criterion:Method,
                 names_to  = "Duration",
                 values_to = "Value") |> 
    pivot_wider(names_from  = "Method",
                values_from = "Value") |> 
    fwrite(
      file = 
        path(
          fdr_result,
          "2_csv",
          stri_c("CO", bias_var_type, "COMBINED_WIDE.csv", sep = "_")
        ),
      sep  = ",",
      bom  = TRUE
    )
  
  cli_inform(c(
    "v" = "SUCCESS"
  ))
  
}
combine_bias(fdr_result = fdr_result,
             variable   = "intensity",
             type       = "minutes")
combine_bias(fdr_result = fdr_result,
             variable   = "intensity",
             type       = "percent")
combine_bias(fdr_result = fdr_result,
             variable   = "inactive",
             type       = "minutes")
combine_bias(fdr_result = fdr_result,
             variable   = "inactive",
             type       = "percent")


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
  pivot_longer(cols = !Criterion:Method,
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
###  INTENSITY  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
walk(
  .x = 
    c(
      "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_60.feather",
      "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_600.feather",
      "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_1800.feather"
    ),
  .f = 
    ~compute_confusion_matrix(
      fdr_read      = fdr_merge,
      fdr_write     = fdr_result,
      fld_mer       = "NOLDUS_CHAMBER_RMR",
      fnm_mer       = .x,
      variable      = "intensity",
      vct_criterion = c("standard",
                        "rmr"),
      vct_estimate  = c("noldus"),
      output        = "percent" # minute, percent, both
    )
)
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
###  INACTIVE  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
walk(
  .x = 
    c(
      "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_60.feather",
      "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_600.feather",
      "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_1800.feather"
    ),
  .f = 
    ~compute_confusion_matrix(
      fdr_read      = fdr_merge,
      fdr_write     = fdr_result,
      fld_mer       = "NOLDUS_CHAMBER_RMR",
      fnm_mer       = .x,
      variable      = "inactive",
      vct_criterion = c("standard",
                        "rmr"),
      vct_estimate  = c("noldus"),
      output        = "percent" # minute, percent, both
    )
)
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = "NOLDUS_CHAMBER_RMR",
  fnm_mer       = "CO_ALL_NOLDUS_CHAMBER_RMR_DUR_BEH_NOL_GE_60.feather",
  variable      = "inactive",
  vct_criterion = c("standard",
                    "rmr"),
  vct_estimate  = c("noldus"),
  output        = "percent" # minute, percent, both
)

## Intensity Distribution ------------------------------------------------------
fdr_read  = fdr_process
fdr_write = fdr_result
fnm_read  = "CO_ALL_DURATION_NOLDUS_CHAMBER_RMR.feather"

walk(
  .x = c("CO_ALL_DURATION_NOLDUS_CHAMBER_RMR.feather",
         "CO_ALL_DUR_BEH_NOL_GE_60_NOLDUS_CHAMBER_RMR.feather",
         "CO_ALL_DUR_BEH_NOL_GE_600_NOLDUS_CHAMBER_RMR.feather",
         "CO_ALL_DUR_BEH_NOL_GE_1800_NOLDUS_CHAMBER_RMR.feather"),
  .f = function(.x) {
    
    fnm_read = .x
    dur_type <- 
      fnm_read |> 
      path_ext_remove() |> 
      stri_replace(regex = "CO_ALL_",
                   replacement = "") |> 
      stri_replace(regex = "_NOLDUS_CHAMBER_RMR",
                   replacement = "")
    
    fpa_read <- 
      dir_ls(path    = fdr_read,
             recurse = TRUE,
             regexp  = fnm_read)
    
    df_duration <- 
      fpa_read |> 
      arrow::read_feather()
    df_distrib <- 
      df_duration |> 
      filter(variable == "intensity") |> 
      filter(!is.na(value)) |> 
      filter(value != "dark/obscured/oof") |> 
      count(value,
            wt = duration) |> 
      mutate(total = 
               n |> 
               sum() |> 
               as.double(),
             percentage = as.double(n) / total) |> 
      as_tibble()
    df_distrib <- 
      df_distrib |> 
      mutate(`total (minutes)` = 
               (total / 60) |> 
               round(digits = 1),
             percentage = label_percent(accuracy = 0.1)(percentage),
             n          = NULL,
             total      = NULL) |> 
      pivot_wider(names_from = value,
                  values_from = percentage)
    
    fnm_write <- 
      stri_c(
        "CO",
        "DISTRIBUTION",
        "INTENSITY",
        dur_type,
        sep = "_"
      ) |> 
      path_ext_set(ext = "csv")
    
    fwrite(
      df_distrib,
      file = path(fdr_write,
                  "2_csv",
                  fnm_write),
      sep = ",",
      bom = TRUE
    )
    
    cli_inform(c(
      "v" ="SUCCESS"
    ))
  }
)
walk(
  .x = c("CO_ALL_DURATION_NOLDUS_CHAMBER_RMR.feather",
         "CO_ALL_DUR_BEH_NOL_GE_60_NOLDUS_CHAMBER_RMR.feather",
         "CO_ALL_DUR_BEH_NOL_GE_600_NOLDUS_CHAMBER_RMR.feather",
         "CO_ALL_DUR_BEH_NOL_GE_1800_NOLDUS_CHAMBER_RMR.feather"),
  .f = function(.x) {
    
    fnm_read = .x
    dur_type <- 
      fnm_read |> 
      path_ext_remove() |> 
      stri_replace(regex = "CO_ALL_",
                   replacement = "") |> 
      stri_replace(regex = "_NOLDUS_CHAMBER_RMR",
                   replacement = "")
    
    fpa_read <- 
      dir_ls(path    = fdr_read,
             recurse = TRUE,
             regexp  = fnm_read)
    
    df_duration <- 
      fpa_read |> 
      arrow::read_feather()
    df_distrib <- 
      df_duration |> 
      filter(variable == "inactive") |> 
      filter(!is.na(value)) |> 
      filter(value != "dark/obscured/oof") |> 
      count(value,
            wt = duration) |> 
      mutate(total = 
               n |> 
               sum() |> 
               as.double(),
             percentage = as.double(n) / total) |> 
      as_tibble()
    df_distrib <- 
      df_distrib |> 
      mutate(`total (minutes)` = 
               (total / 60) |> 
               round(digits = 1),
             percentage = label_percent(accuracy = 0.1)(percentage),
             n          = NULL,
             total      = NULL) |> 
      pivot_wider(names_from = value,
                  values_from = percentage)
    
    fnm_write <- 
      stri_c(
        "CO",
        "DISTRIBUTION",
        "INACTIVE",
        dur_type,
        sep = "_"
      ) |> 
      path_ext_set(ext = "csv")
    
    fwrite(
      df_distrib,
      file = path(fdr_write,
                  "2_csv",
                  fnm_write),
      sep = ",",
      bom = TRUE
    )
    
    cli_inform(c(
      "v" ="SUCCESS"
    ))
  }
)

compute_summary_chamber_distribution <- function() {
  
  # fdr_read <- 
  #   path("FLAC_AIM1_DATA",
  #        "4_AIM1_MERGED_DATA")
  # fdr_write <- 
  #   path("S:", "_R_CHS_Research", "PAHRL", "Student Access",
  #        "0_Students", "MARTINEZ", "2_Conferences", "2022_ICAMPAM",
  #        "4_results", "2_csv")
  # fld_mer <- 
  #   "NOLDUS_CHAMBER_RMR"
  # fnm_mer <- 
  #   "CO_ALL_NOLDUS_CHAMBER_RMR.feather"
  fdr_read  = fdr_merge
  fdr_write = fdr_result
  fld_mer   = "NOLDUS_CHAMBER_RMR"
  fnm_mer   =  "CO_ALL_NOLDUS_CHAMBER_RMR.feather"
  
  fpa_mer <- 
    dir_ls(
      path    = fdr_read,
      recurse = TRUE,
      # glob    = stri_c(fld_mer, "*/*", fnm_mer)
      regexp  = fnm_mer
    )
  df_mer[df_mer$mets_rmr < 0, ]
  df_mer <- 
    fpa_mer |> 
    arrow::read_feather() |> 
    # TODO: do this in merge noldus chamber rmr function.For know just filter it out.
    # as_tibble() |> 
    # mutate(across(
    #   .cols = !c(study:time,
    #              chamber_vo2_ml_kg_min:last_col()),
    #   .fns = ~base::ifelse((mets_rmr < 0 | mets_standard < 0),
    #                        yes = factor(NA),
    #                        no = .x)
    # )) |>
    # Remove the first row of each visit to have correct comparisons (start time
    # was always treated as an anchor, not as a data point.)
    # TODO END
    group_by(study, subject, visit) |> 
    slice(-1) |> 
    ungroup() |> 
    as.data.table() |> 
    mutate(across(
      .cols = starts_with("intensity"),
      .fns = ~fct_explicit_na(.x,
                              na_level = "dark/obscured/oof")
    )) |> 
    as.data.table()
  
  df_summary <- 
    df_mer |> 
    filter(!(mets_rmr < 0 | mets_standard < 0)) |> 
    select(study:time,
           behavior_noldus,intensity_noldus, mets_rmr, mets_standard) |> 
    # Change back to minute format.
    group_by(study, subject, visit,
             arrange = FALSE) |> 
    mutate(run           = rleid(mets_rmr),
           mets_rmr      = round(mets_rmr,
                                 digits = 1),
           mets_standard = round(mets_standard,
                                 digits = 1)) |> 
    group_by(study, subject, visit, run,
             arrange = FALSE) |> 
    slice_head(n = 1) |> 
    as.data.table()
  
  df1 <- 
    df_summary |>
    as_tibble() %>%
    tabyl(mets_rmr, behavior_noldus) 
  df2 <- 
    df_summary |> 
    as_tibble() |> 
    count(mets_rmr, behavior_noldus)
  
  df3 <- 
    df_summary |> 
    as_tibble() |> 
    mutate(
      cut_rmr =
        mets_rmr |> 
        cut(breaks = c(0,
                       seq(from = 1.0,
                           to = 3.0,
                           by = 0.2),
                       Inf),
            right = FALSE),
      cut_standard = 
        mets_standard |> 
        cut(breaks = c(0,
                       seq(from = 1.0,
                           to = 3.0,
                           by = 0.2),
                       Inf),
            right = FALSE),
      cut_rmr_int =
        mets_rmr |> 
        cut(breaks = c(0,
                       1.0, 1.5, 3.0,
                       Inf),
            right = FALSE),
      cut_standard_int = 
        mets_standard |> 
        cut(breaks = c(0,
                       1.0, 1.5, 3.0,
                       Inf),
            right = FALSE)
    )
  df3 |> 
    count(cut = cut_rmr, Behavior = behavior_noldus) |> 
    group_by(cut) |> 
    arrange(-n, .by_group = TRUE) |> 
    slice_head(n = 1) |> 
    ungroup() |> 
    mutate(`MET Range` = 
             cut |> 
             stri_replace_all_regex(pattern = "\\[|\\)",
                                    replacement = "") |> 
             stri_replace_all_regex(pattern = ",",
                                    replacement = " - ") |> 
             stri_replace_all_regex(pattern = "0 -",
                                    replacement = "<") |> 
             stri_replace_all_regex(pattern = "3 - Inf",
                                    replacement = "\u2265 3.0"),
           
           n = NULL,
           cut = NULL,
           .before = 1) |> 
    rename(RMR = cut)
  df3 |> 
    count(cut = cut_standard, Behavior = behavior_noldus) |> 
    group_by(cut) |> 
    arrange(-n, .by_group = TRUE) |> 
    slice_head(n = 1) |> 
    mutate(`MET Range` = 
             cut |> 
             stri_replace_all_regex(pattern = "\\[|\\)",
                                    replacement = "") |> 
             stri_replace_all_regex(pattern = ",",
                                    replacement = " - ") |> 
             stri_replace_all_regex(pattern = "0 -",
                                    replacement = "<") |> 
             stri_replace_all_regex(pattern = "3 - Inf",
                                    replacement = "\u2265 3.0"),
           n = NULL,
           .before = 1) |> 
    select(-cut)
  df_distr <- 
    left_join(
      df3 |> 
        tabyl(cut_standard) |> 
        adorn_pct_formatting(digits = 2) |> 
        mutate(`MET Range` = 
                 cut_standard |> 
                 stri_replace_all_regex(pattern = "\\[|\\)",
                                        replacement = "") |> 
                 stri_replace_all_regex(pattern = ",",
                                        replacement = " - ") |> 
                 stri_replace_all_regex(pattern = "0 -",
                                        replacement = "<") |> 
                 stri_replace_all_regex(pattern = "3 - Inf",
                                        replacement = "\u2265 3.0"),
               Standard = percent,
               cut_standard = NULL,
               n            = NULL,
               percent      = NULL),
      df3 |> 
        tabyl(cut_rmr) |> 
        adorn_pct_formatting(digits = 2) |> 
        mutate(`MET Range` = 
                 cut_rmr |> 
                 stri_replace_all_regex(pattern = "\\[|\\)",
                                        replacement = "") |> 
                 stri_replace_all_regex(pattern = ",",
                                        replacement = " - ") |> 
                 stri_replace_all_regex(pattern = "0 -",
                                        replacement = "<") |> 
                 stri_replace_all_regex(pattern = "3 - Inf",
                                        replacement = "\u2265 3.0"),
               RMR = percent,
               cut_rmr = NULL,
               n       = NULL,
               percent = NULL),
      by = "MET Range"
    )
  fwrite(
    df_distr,
    file = path(fdr_write,
                "CO_DISTRIBUTION_METS.csv"),
    sep = ",",
    bom = TRUE
  )
  fwrite(
    df1,
    file = path(fdr_write,
                "CO_METS_BEHAVIOR_MATRIX.csv"),
    sep = ",",
    bom = TRUE
  )
  
  df3 |> 
    tabyl(cut_rmr_int) |> 
    adorn_pct_formatting(digits = 2)
  df3 |> 
    tabyl(cut_standard_int) |> 
    adorn_pct_formatting(digits = 2)
  
  df_summary |> 
    count(mets_rmr)
  pull(mets_rmr) |> 
    quantile(probs = seq(from = 0,
                         to   = 1,
                         by = 0.25))
  mutate(
    distr_rmr = 
      mets_rmr |> 
      quantile(probs = seq(from = 0,
                           to   = 1,
                           by = 0.25))
  )
  
  # from 05_analysis
  df_mer %>% 
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
  
}

## Plot ------------------------------------------------------------------------
# # # # # # # # # # # # plot_bias
# - Outputs three types of graphs:
#   - "Default" which shows all of the variable values on one 
#     axis and facets by 1) combinations of vct_criterion & vct_estimate
#     & by 2) vct_variable.
#   - "Collapsed" which shows all combinations between vct_criterion
#     and vct_estimate for each variable in one graph then facets
#     by vct_variable.
#   - "Separate Values" which has all the values from each estimate
#     in one graph (faceted by values within vct_variable). CAN ONLY
#     ACCEPT ONE VARIABLE.
###  INACTIVE  :::::::::::::::::::::::::::::::::::::::::::::::::::
fig_bias_minute <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard", "rmr"),
    vct_estimate     = c("noldus"),
    vct_variable     = c("inactive"),
    lst_recode       = list(criterion = c("Rmr" = "RMR"),
                            estimate  = c("Noldus" = "Video"),
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = TRUE,
    separate_values  = FALSE,
    flip_axes        = FALSE,
    duration_type    = "BEH_NOL_GE_60",
    type             = "minute"
  )
fig_bias_percent <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard", "rmr"),
    vct_estimate     = c("noldus"),
    vct_variable     = c("inactive"),
    lst_recode       = list(criterion = c("Rmr" = "RMR"),
                            estimate  = c("Noldus" = "Video"),
                            variable  = NULL,
                            value     = c("Mvpa" = "MVPA")),
    collapse         = TRUE,
    separate_values  = FALSE,
    flip_axes        = FALSE,
    duration_type    = "BEH_NOL_GE_60",
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
###  INTENSITY  :::::::::::::::::::::::::::::::::::::::::::::::::::
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
    duration_type    = "BEH_NOL_GE_60",
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
    duration_type    = "BEH_NOL_GE_60",
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
# Order of how to lay out accelerometer algorithms.
ord_estimate <- 
  c(
    "hildebrand",
    "marcotte",
    "montoye",
    "rowlands",
    "staudenmayer",
    "freedson",
    "sojourn3x"
  )
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
###  INTENSITY  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
#### Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather", # normal
      # "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_300.feather", # 5 min
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_600.feather", # 10 min
      # "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_900.feather", # 15 min
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1200.feather" # 20 min
      # "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1500.feather" # 25 min
    ),
  .f = 
    ~compute_bias(
      fdr_read          = fdr_process,
      fdr_write         = fdr_result,
      fnm_visit_summary = .x,
      vct_criterion     = c("standard"),
      vct_estimate      = ord_estimate,
      output            =   "time" # time, percent
    )
)
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = ord_estimate,
  output            =   "time" # time, percent
)
#### Percent ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather", # normal
      # "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_300.feather", # 5 min
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_600.feather", # 10 min
      # "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_900.feather", # 15 min
      "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1200.feather" # 20 min
      # "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DUR_INT_STA_GE_1500.feather" # 25 min
    ),
  .f = 
    ~compute_bias(
      fdr_read          = fdr_process,
      fdr_write         = fdr_result,
      fnm_visit_summary = .x,
      vct_criterion     = c("standard"),
      vct_estimate      = ord_estimate,
      output            =   "percent" # time, percent
    )
)
compute_bias(
  fdr_read          = fdr_process,
  fdr_write         = fdr_result,
  fnm_visit_summary = "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.feather",
  vct_criterion     = c("standard"),
  vct_estimate      = ord_estimate,
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
###  INTENSITY  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
#### Both ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",  # normal
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_300.feather",  # 5 min
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_600.feather",  # 10 min
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_900.feather",  # 15 min
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1200.feather"  # 20 min
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1500.feather"  # 25 min
    ),
  .f = 
    ~compute_confusion_matrix(
      fdr_read      = fdr_merge,
      fdr_write     = fdr_result,
      fld_mer       = NULL,
      fnm_mer       = .x,
      variable      = "intensity",
      criterion     = "standard",
      vct_estimate  = ord_estimate,
      lst_recode    = list(criterion = c("Standard" = "Chamber"),
                           estimate  = NULL,
                           variable  = NULL,
                           value     = c("Mvpa" = "MVPA")),
      output        = "both" # minute, percent, both
    )
)
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  variable      = "intensity",
  criterion     = "standard",
  vct_estimate  = ord_estimate,
  lst_recode    = list(criterion = c("Standard" = "Chamber"),
                       estimate  = NULL,
                       variable  = NULL,
                       value     = c("Mvpa" = "MVPA")),
  output        = "both" # minute, percent, both
)
#### Time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",  # normal
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_300.feather",  # 5 min
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_600.feather",  # 10 min
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_900.feather",  # 15 min
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1200.feather"  # 20 min
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1500.feather"  # 25 min
    ),
  .f = 
    ~compute_confusion_matrix(
      fdr_read      = fdr_merge,
      fdr_write     = fdr_result,
      fld_mer       = NULL,
      fnm_mer       = .x,
      variable      = "intensity",
      vct_criterion = c("standard"),
      vct_estimate  = ord_estimate,
      output        = "minute" # minute, percent, both
    )
)
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = ord_estimate,
  output        = "minute" # minute, percent, both
)#### Percent ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = 
    c(
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",  # normal
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_300.feather",  # 5 min
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_600.feather",  # 10 min
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_900.feather",  # 15 min
      "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1200.feather"  # 20 min
      # "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15_DUR_INT_STA_GE_1500.feather"  # 25 min
    ),
  .f = 
    ~compute_confusion_matrix(
      fdr_read      = fdr_merge,
      fdr_write     = fdr_result,
      fld_mer       = NULL,
      fnm_mer       = .x,
      variable      = "intensity",
      vct_criterion = c("standard"),
      vct_estimate  = ord_estimate,
      output        = "percent" # minute, percent, both
    )
)
compute_confusion_matrix(
  fdr_read      = fdr_merge,
  fdr_write     = fdr_result,
  fld_mer       = NULL,
  fnm_mer       = "CO_ALL_CHAMBER_RMR_AG_MODEL_2022-06-15.feather",
  variable      = "intensity",
  vct_criterion = c("standard"),
  vct_estimate  = ord_estimate,
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
    vct_estimate     = ord_estimate,
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
fig_bias_minute <- 
  fig_bias_minute +
  labs(y = "\n")
ggsave(
  filename = path_ext_set("CHAAC_FIGURE_BIAS_INTENSITY_MINUTES_BY_ESTIMATE",
                          ext = "png"),
  plot     = fig_bias_minute,
  device   = "png",
  path     = dir_ls(path = fdr_result,
                    type = "directory",
                    regexp = "figure_table"),
  width    = 700 * 6,
  height   = 1200 * 6,
  units    = "px",
  dpi      = 100 * 6
)
fig_bias_percent <- 
  plot_bias(
    fdr_result       = fdr_result,
    vct_criterion    = c("standard"),
    vct_estimate     = ord_estimate,
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

df_time_vis_wide <- 
  fread(file = path(fdr_process,
                    "CO_VISIT_SUM_MINUTES_INTENSITY_FROM_DURATION.csv"),
        sep = ",")
# variable <- 
#   df_time_vis_wide$variable[1] |> 
#   stri_trans_totitle()
# time_unit <- 
#   df_time_vis_wide$time_unit[1] |> 
#   stri_c("s") |> 
#   stri_trans_totitle()
df_time_vis_long <- 
  df_time_vis_wide |> 
  as_tibble() |> 
  select(-contains("dark/obscured/oof")) |> 
  select(!c(study:visit, summary_statistic, total)) |> 
  pivot_longer(cols = !variable:time_unit,
               names_to = c("value", "source"),
               # names_to = c("intensity", "source"),
               names_sep = "_",
               values_to = "time") |> 
  mutate(
    value = 
      value |> 
      stri_trans_totitle() |> 
      forcats::as_factor() |> 
      forcats::fct_recode(MVPA = "Mvpa"),
    source = 
      source |> 
      stri_trans_totitle() |> 
      forcats::as_factor() |> 
      forcats::fct_relevel(c("Standard",
                             stri_trans_totitle(ord_estimate))) |> 
      forcats::fct_recode(Chamber = "Standard")
  )
  # select(Intensity = intensity,
  #        Source    = source,
  #        Time = value)
nm_mean <- 
  stri_c("Mean (", df_time_vis_long$time_unit[1], "s)")
df_mean <- 
  df_time_vis_long |> 
  group_by(value, source) |>
  summarize(
    "{nm_mean}" :=
    # `Mean (minutes)` =
      time |>
      mean() |>
      round(digits = 1) %>%
      # In case a source has a 0 time for a value, so that it still shows up on
      # the graph.
      if_else(. == 0,
              true = 1,
              false = .),
    sd   =
      time |>
      sd() |>
      round(digits = 1) %>%
      # In case a source has a 0 time for a value, so that it still shows up on
      # the graph.
      if_else(. == 0,
              true = 1,
              false = .),
    .groups = "drop"
  ) |> 
  mutate(
    upper_error = .data[[nm_mean]] + sd
  ) |> 
  left_join(
    # Get paired.test results.
    compare_means(formula = time ~ source,
                  data    = df_time_vis_long,
                  method = "t.test",
                  paired = TRUE,
                  group.by = "value",
                  ref.group = "Chamber",
                  p.adjust.method = "holm") |> 
      select(value,
             source = group2,
             # p,
             p_adj = p.adj) |> 
      mutate(
        # p_signif =
        #   fifelse(p <= 0.05,
        #           yes = "*",
        #           no  = ""),
        p_adj_signif = 
          fifelse(p_adj <= 0.05,
                  yes = "*",
                  no  = "")
        # p.signif = 
        #   case_when(
        #     p.adj <= 0.0001 ~ "****",
        #     p.adj <= 0.001 ~ "***",
        #     p.adj <= 0.01 ~ "**",
        #     p.adj <= 0.05 ~ "*",
        #     p.adj > 0.05 ~ ""
        #   )
      ) |> 
      # Add back in Chamber.
      bind_rows(
        tibble(
          value        = 
            factor(
              levels(df_time_vis_long$value),
              levels = levels(df_time_vis_long$value)
            ),
          source       = "Chamber",
          p_adj_signif = ""
        ),
        ... = _
      ) |> 
      mutate(source = forcats::as_factor(source)) |> 
      arrange(value),
    by = c("value", "source")
  )

df_mean |> 
  group_by(value) |> 
  mutate(
    loc_signif_test =
      case_when(
        upper_error == max(upper_error) ~ 
          .data[[nm_mean]] - max(upper_error) * 0.1,
        upper_error + max(upper_error) * 0.1 > max(upper_error) ~
          .data[[nm_mean]] - max(upper_error) * 0.1,
        TRUE ~ upper_error + max(upper_error) * 0.1
      ),
    loc_signif =
      case_when(
        # Since the max upper error sets the y scale, have the significance
        # symbol be inside the bar.
        upper_error == max(upper_error) ~ 
          .data[[nm_mean]] - max(upper_error) * 0.1,
        # If the upper error + 7/100th of the max upper error is larger than
        # the max upper error, then also have significance symbol go inside the
        # bar as it will mess up scale.
        upper_error + max(upper_error) * 0.07 > max(upper_error) ~
          .data[[nm_mean]] - max(upper_error) * 0.1,
        # Have the significance symbol be just above the upper error bar.
        TRUE ~ upper_error + max(upper_error) * 0.07
      )
  ) |> 
ggplot() +
# ggplot(data = df_mean) +
  geom_col(mapping = aes(x = source,
                         y = .data[[nm_mean]]),
           color = "black",
           fill = NA,
           position = position_dodge()) +
  geom_errorbar(mapping = aes(x = source,
                              ymin = .data[[nm_mean]],
                              ymax = upper_error),
                width = 0.25) +
  # # Testing significance symbol location.
  # geom_text(mapping = aes(x = source,
  #                         y = loc_signif_test),
  #           label = "I",
  #           color = "red") +
  geom_text(mapping = aes(x = source,
                          y = loc_signif,
                          label = p_adj_signif)) +
  facet_grid(rows = vars(value),
             scales = "free_y") +
  scale_x_discrete(name = "\n") +
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
      linewidth     = NULL,                              # DEFAULT
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
  filename = "CHAAC_FIGURE_MEANS_INTENSITY_MINUTES_NORMAL.png",
  plot = last_plot(),
  device = "png",
  path = path(fdr_result,
              "3_figure_table"),
  width  = 900 * 6,
  height = 700 * 6,
  units  = "px",
  dpi    = 100 * 6
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
  # df_plot,
  # x = "Intensity",
  # y = "Time",
  # fill = "Source",
  df_time_vis_long,
  x = "value",
  y = "time",
  fill = "source",
  # label = TRUE,
  add = "mean_se",
  error.plot = "upper_errorbar",
  position = position_dodge()
)
ggbarplot(
  # df_plot,
  # x = "Source",
  # y = "Time",
  # facet.by = "Intensity",
  # fill = "Source",
  # label = TRUE,
  df_time_vis_long,
  x = "source",
  y = "time",
  facet.by = "value",
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
