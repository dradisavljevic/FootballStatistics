# Set working directory to the current files directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# loading in the required packages
install.packages('tidyr')
install.packages('magrittr')
install.packages('tidyquant')
install.packages('purrr')
suppressWarnings(suppressPackageStartupMessages({
  library(tidyr)
  library(magrittr)
  library(tidyquant)
  library(purrr)
}))
serbia_csv <- list.files(path = "SRB", full.names = TRUE)

serbia_raw_data <- map(serbia_csv, read.csv)

serbia_data <- do.call(rbind, serbia_raw_data) %>% as_tibble()
