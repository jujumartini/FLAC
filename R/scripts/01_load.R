# Ctrl + Shift + F10      :  restart session
# Ctrl + A, Ctrl + Enter  :  Select whole script and run whole script
if (!require("pacman")) install.packages("pacman"); library(pacman)
if (!p_exists("tidyverse")) install.packages("tidyverse")

pacman::p_load(
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                             R INFRASTRUCTURE                             ::
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  cli,                    # Create diagnostic messages.
  devtools,               # Tools to make an R developer's life easier.
  fs,                     # Cross platform file operations based on libuv.
  prettyunits,            # Human readable formatting of quantities
  rlang,                  # Low-level API for programming with R.
  sessioninfo,            # Print session information in a neater way.
  styler,                 # Non-invasive pretty printing of R code.
  tools,                  # Base tools for package dev, admin and documentation.
  usethis,                # Set up commonly used package components
  vctrs,                  # Generic programming with typed R vectors
  zeallot,                # Multiple/unpacking variable assignment in R.
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                                 UTILITY                                  ::
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  bannerCommenter,        # Make banners/headings within code.
  pdftools,               # Text extraction, rendering and converting of PDFs.
  sys,                    # Powerful replacements for running system commands.
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                                 ANALYSIS                                 ::
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  broom,                  # Summarize models into tibbles.
  irr,                    # Various coefficients of reliability and agreement.
  lme4,                   # Mixed-effects models.
  modelr,                 # Elegant pipelines during modeling.
  skimr,                  # Frictionless, pipeable summary statistics.
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                            DATA VISUALIZATION                            ::
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ggplot2,                # Grammar of graphics plots.
  janitor,                # Format frequency tables; tabulation.
  knitr,                  # Dynamic documents.
  lemon,                  # Brackets for axis ticks in ggplot2 graphs.
  scales,                 # Tools for ggplot2 scales.
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                             WRANGLE - IMAGES                             ::
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  magick,                 # Image wrangling.
  tesseract,              # Get timestamps from images.
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                              WRANGLE - DATA                              ::
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Tidyverse
  dplyr,                  # Data manipulation.
  dtplyr,                 # dplyr functions with the speed of data.table.
  forcats,                # Factor wrangling.
  haven,                  # Read SPSS, Stata and SAS files from R.
  lubridate,              # Date & times wrangling.
  purrr,                  # Mapping functions.
  readxl,                 # Read Excel sheets.
  stringi,                # Character string wrangling.
  stringr,                # Character string wrangling but simpler.
  tibble,                 # Data & list wrangling,
  tidyr,                  # Data tidying.
  # Other
  data.table,             # Fast read/write of data & elegant wrangle semantics.
  # janitor               # convert_*(), get_dupes(), row_to_names()
  padr                    # Fill in discontinous time series.
)
pacman::p_load_gh(
  "gadenbuie/regexplain"  # An RStudio addin slash regex utility belt.
)
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                                   OPTIONS                                   ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Show warnings for partial matches.
options(warnPartialMatchDollar = TRUE)
options(warnPartialMatchArgs = TRUE)
options(datatable.print.class = TRUE)
options(datatable.print.keys = TRUE)
# When condition in if (condition) has length > 1, throws error.
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

tidyverse::tidyverse_conflicts()
sessioninfo::session_info(info = "all",
                          include_base = TRUE)
