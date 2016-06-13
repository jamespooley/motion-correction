source("code/functions.R")


#devtools::install_github(repo = "jamespooley/motion-correction", quiet = TRUE)

# analysis <- function() {
# 
#   library(readr)
#   library(dplyr)
#   library(purrr)
#   library(ggplot2)
#   
#   source("functions.R")  # Read in helper functions for the analyses
#   
#   adhd200 <- read_csv("data/adhd200.csv")
#   qap_func <- read_csv("data/qap_func.csv")
#   qap_struct <- read_csv("data/qap_struct.csv")
#   
#   
#   roi_list <- names(adhd200)[16:80]
#   
#   # Effect of Motion on Neurodevelopmental Trajectories
#   
#   
#   
#   results <- map2(roi_list, blah_blah_blah)
#   results_df <- results %>% tbl_df
# 
# }

qap_measures <- c("EFC", "FWHM", "Qi1", "SNR", "FBER", "CNR")
roi_names <- names(pardoe_df)[16:80]
roi_list <- as.list(roi_names)

map(roi_list, get_peak_age, 
    dplyr::filter(analysis_df, sex == "female", site == "NYUCSC", diagnosis == "adhd"))

map_dbl(roi_list, get_peak_age, subset(analysis_df, sex == "male"))
map_dbl(roi_list, get_peak_age, subset(analysis_df, sex == "female"))

no_motion <- map_dbl(roi_list, get_peak_age, subset(analysis_df, diagnosis == "adhd"))
motion <- map_dbl(roi_list, get_peak_age, subset(analysis_df, diagnosis == "adhd"), "qi1")

sum(no_motion != motion)
