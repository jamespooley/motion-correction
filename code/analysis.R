library(readr)
library(dplyr)
library(purrr)
library(ggplot2)

source("code/functions.R")

qap_measures <- c("EFC", "FWHM", "Qi1", "SNR", "FBER", "CNR")
roi_names <- names(pardoe_df)[16:80]
roi_list <- as.list(roi_names)

map(roi_list, get_peak_age, 
    dplyr::filter(analysis_df, sex == "female", site == "NYUCSC", diagnosis == "adhd"))

peak_ages <- data_frame(
  male = map_dbl(roi_list, get_peak_age, dplyr::filter(analysis_df, sex == "male")),
  female = map_dbl(roi_list, get_peak_age, dplyr::filter(analysis_df, sex == "female")) 
)

peak_ages %>% View

peak_ages_males <- data_frame(
  no_motion = map_dbl(roi_list, get_peak_age, dplyr::filter(analysis_df, sex == "male", diagnosis == "adhd")),
  motion_qi1 = map_dbl(roi_list, get_peak_age, dplyr::filter(analysis_df, sex == "male", diagnosis == "adhd"), "Qi1"),
  motion_snr = map_dbl(roi_list, get_peak_age,
                       dplyr::filter(analysis_df, sex == "male", diagnosis == "adhd"), "SNR"),
  motion_qap.mean.rms = map_dbl(roi_list, get_peak_age,
                                dplyr::filter(analysis_df, sex == "male", diagnosis == "adhd"), "qap.mean.rms"),
  motion_mean.rms = map_dbl(roi_list, get_peak_age,
                            dplyr::filter(analysis_df, sex == "male", diagnosis == "adhd"), "mean.rms")
)

no_motion <- map_dbl(roi_list, get_peak_age, subset(analysis_df, diagnosis == "adhd"))
motion <- map_dbl(roi_list, get_peak_age, subset(analysis_df, diagnosis == "adhd"), "qi1")

sum(no_motion != motion)
