library(readr)
library(stringr)
library(dplyr)

##############################################################################
# ORGANIZE THE VARIOUS DATASETS ##############################################
##############################################################################
#
# TODO: Wrap all the code to organize the datasets into a function

# Load the Pardoe et al. data
pardoe_file <- "data/pardoe.motion.morphometry.20160416.csv"
pardoe_df <- read.csv(pardoe_file) %>% 
  filter(study == "adhd")
pardoe_df <- pardoe_df %>% tbl_df

func_temp_df <- read_csv("data/qap_ADHD200_functional_temporal.csv") %>% 
  rename(id = Participant,
         session = Session,
         run = Series) 
# %>% 
#   mutate(id = as.character(id),
#          id = str_pad(id, 7, pad = "0"))

anat_spat_df <- read_csv("data/qap_ADHD200_anatomical_spatial.csv") %>% 
  rename(id = Participant,
         session = Session,
         run = Series) 
# %>% 
#   mutate(id = as.character(id),
#          id = str_pad(id, 7, pad = "0"))

df <- func_temp_df %>%
  merge(pardoe_df, by = c("id", "session", "run"))

analysis_df <- inner_join(df[, c("id", "diagnosis", "session", "run", "gender", "age", "site", "mean.rms", "motion.artifact", "RMSD (Mean)", 
                                 "freesurfer.EstimatedTotalIntraCranialVol", roi_names)], 
                          anat_spat_df[, c("id", "session", "run", qap_measures)], 
                          by = c("id", "session")) %>% 
  rename(sex = gender) %>% 
  mutate(age_cent = age - mean(age, na.rm = TRUE),
         age_cent2 = age_cent^2,
         age_cent3 = age_cent^3)

names(analysis_df)[9] <- "qap.mean.rms"
