pacman::p_load(
  tidyverse,
  fs
)

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

zz <- 
  file(
    paste0("S:/_V_PAHRL/FLAC/R/task_logs/copy_and_move_odx/", 
              format(Sys.time(), "%Y-%m-%d"), 
              ".txt"), 
       open = "wt"
  )
sink(zz)
sink(zz, type = "message")

cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
           "\n\n\n"))

cat("FLAC - Aim 1\n\n")
copy_and_move_odx(
  project = "FLAC - Aim 1"
)

cat("\n\n\nFLAC - Aim 2\n\n")
copy_and_move_odx(
  project = "FLAC - Aim 2"
)

sink(type = "message")
sink()


# ## capture all the output to a file. Do a sink for output and another for messages/errors.
# zz <- 
#   file("sink_examp.txt", open = "wt")
# sink(zz)
# sink(zz, type = "message")
# i <- 1:10
# outer(i, i, "*")
# try(log("a"))
# ## revert output back to the console -- only then access the file!
# sink(type = "message")
# sink()
# file.show("sink_examp.txt")
# unlink("sink_examp.txt")
