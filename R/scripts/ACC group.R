library(tidyverse)
library(data.table)
library(lubridate)
library(dtplyr)

path <- 
  "S:/_R_CHS_Research/PAHRL/Student Access/0_Students/MARTINEZ/3_Projects/FLAC - Aim 2/ACC Algo Development Group"

fls_raw <- 
  list.files(path = path,
             pattern = "RAW.csv$")

for (i in seq_along(fls_raw)) {
  fnm_raw <- 
    fls_raw[i]
  message(i, ": ", fnm_raw)
  df_raw <- 
    data.table::fread(
      file = file.path(path,
                       fnm_raw),
      sep = ",",
      skip = 10
    ) %>% 
    mutate(datetime = 
             Timestamp %>% 
             str_remove(pattern = "\\.(?!.*\\.).*") %>% 
             mdy_hms()) %>% 
    filter(between(datetime,
                   lower = as_datetime("2019-07-05 13:29:47"),
                   upper = as_datetime("2019-07-05 13:39:47"),
                   incbounds = TRUE)) %>% 
    select(-datetime) %>% 
    as.data.table()
  
  if (dim(df_raw)[1] != 60100L) stop()
  
  fnm_10minute <- 
    fnm_raw %>% 
    str_remove(pattern = ".csv$") %>% 
    paste0("_10minute.csv")
  data.table::fwrite(
    df_raw,
    file = file.path(path,
                     fnm_10minute),
    sep = ","
  )
}

fls_1sec <- 
  list.files(path = path,
             pattern = "1sec.csv$")

for (i in seq_along(fls_1sec)) {
  fnm_1sec <- 
    fls_1sec[i]
  message(i, ": ", fnm_1sec)
  df_1sec <- 
    data.table::fread(
      file = file.path(path,
                       fnm_1sec),
      sep = ",",
      skip = 10
    ) %>% 
    tidyr::unite(col = datetime,
                 Date, Time,
                 sep = " ",
                 remove = FALSE) %>% 
    mutate(datetime = 
             datetime %>% 
             mdy_hms()) %>% 
    filter(between(datetime,
                   lower = as_datetime("2019-07-05 13:29:47"),
                   upper = as_datetime("2019-07-05 13:39:47"),
                   incbounds = TRUE)) %>% 
    select(-datetime) %>% 
    as.data.table()
  
  if (dim(df_1sec)[1] != 601L) stop()
  
  fnm_10minute <- 
    fnm_1sec %>% 
    str_remove(pattern = ".csv$") %>% 
    paste0("_10minute.csv")
  data.table::fwrite(
    df_1sec,
    file = file.path(path,
                     fnm_10minute),
    sep = ","
  )
}

df_raw <- 
  df_raw[
    , `:=`(datetime = 
             Timestamp %>% 
             str_remove(pattern = "\\.(?!.*\\.).*") %>% 
             mdy_hms())
  ][
    between(datetime, 
            lower = as_datetime("2019-07-05 13:29:47"), 
            upper = as_datetime("2019-07-05 13:39:47"), 
            incbounds = TRUE), 
  ]

df_raw %>% 
  mutate(datetime = 
           Timestamp %>% 
           str_remove(pattern = "\\.(?!.*\\.).*") %>% 
           mdy_hms()) %>% 
  filter(between(datetime,
                 lower = as_datetime("2019-07-05 13:29:47"),
                 upper = as_datetime("2019-07-05 13:39:47"),
                 incbounds = TRUE)) %>% 
  as.data.table()

df_raw[, datetime := str_remove(Timestamp,
                                pattern = "\\.(?!.*\\.).*")]
df_raw[, datetime := lubridate::mdy_hms(datetime)]

df_raw <- 
  df_raw[between(datetime,
                 lower = as_datetime("2019-07-05 13:29:47"),
                 upper = as_datetime("2019-07-05 13:39:47"),
                 incbounds = TRUE), ]
