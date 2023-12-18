# This script was written by Alaina Pearce in March 2023
# to set up tables for the a paper examining structural differences
# in children with LOC-eating
#
#     Copyright (C) 2023 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

############ Basic Data Load/Setup ############


#### set up ####

# 1 - Load Data ####
## a) dem
loc_demo <- read.csv("Data/loc_covars.csv")
loc_demo$study_id <- tolower(loc_demo$study_id)

loc_matched_demo <- read.csv("Data/loc-matched_covars.csv")
loc_matched_demo$study_id <- tolower(loc_matched_demo$study_id)

## b) neuromorphometrics 
loc_vgm_nm <- read.csv("Data/loc_roi-vgm_neuromorphometrics.csv")
names(loc_vgm_nm)[1] <- 'study_id'

## c) cobra
loc_vgm_cobra <- read.csv("Data/loc_roi-vgm_cobra.csv")
names(loc_vgm_cobra)[1] <- 'study_id'

## d) cort complexity
loc_cortcomplixity <- read.csv("Data/loc_roi-fractaldimension_aparc_DK40.csv")
names(loc_cortcomplixity)[1] <- 'study_id'

## e) sulci depth
loc_sd <- read.csv("Data/loc_roi-sqrtdepth_aparc_DK40.csv")
names(loc_sd)[1] <- 'study_id'

# 2 - Merge ####

## fix ids
#fix TRTA/B
fix_id <- function(id){
  
  if (grepl('trt', id)){
    id_short <- gsub('A', '', id)
    id_short <- gsub('B', '', id_short)
  } else if (grepl('testretest', id)){
    id_short <- gsub('testretest', 'trt', id)
  } else if (grepl('port', id)){
    id_short <- gsub('port', 'fbs', id)
  } else {
    id_short <- id
  }
  
  id_short <- gsub('_T1', '', id_short)
  
  return(id_short)
}

loc_demo[['study_id']] <- sapply(loc_demo[['study_id']], function(x) fix_id(x), USE.NAMES = FALSE)

loc_vgm_nm[['study_id']] <- sapply(loc_vgm_nm[['study_id']], function(x) fix_id(x), USE.NAMES = FALSE)

loc_vgm_cobra[['study_id']] <- sapply(loc_vgm_cobra[['study_id']], function(x) fix_id(x), USE.NAMES = FALSE)

loc_cortcomplixity[['study_id']] <- sapply(loc_cortcomplixity[['study_id']], function(x) fix_id(x), USE.NAMES = FALSE)

loc_sd[['study_id']] <- sapply(loc_sd[['study_id']], function(x) fix_id(x), USE.NAMES = FALSE)

## add measure onto variable names
names(loc_vgm_nm)[2:137] <- sapply(names(loc_vgm_nm)[2:137], function(x) paste0('vgm_', x), USE.NAMES = FALSE)

names(loc_vgm_cobra)[2:53] <- sapply(names(loc_vgm_cobra)[2:53], function(x) paste0('vgm_', x), USE.NAMES = FALSE)

names(loc_cortcomplixity)[2:73] <- sapply(names(loc_cortcomplixity)[2:73], function(x) paste0('fd_', x), USE.NAMES = FALSE)

names(loc_sd)[2:73] <- sapply(names(loc_sd)[2:73], function(x) paste0('sd_', x), USE.NAMES = FALSE)

## merge
loc_dat <- merge(loc_demo, loc_vgm_nm, by = 'study_id')
loc_dat <- merge(loc_dat, loc_vgm_cobra, by = 'study_id')
loc_dat <- merge(loc_dat, loc_cortcomplixity, by = 'study_id')
loc_dat <- merge(loc_dat, loc_sd, by = 'study_id')

loc_matched_dat <- merge(loc_matched_demo, loc_vgm_nm, by = 'study_id', all.x = TRUE, all.y = FALSE)
loc_matched_dat <- merge(loc_matched_dat, loc_vgm_cobra, by = 'study_id', all.x = TRUE, all.y = FALSE)
loc_matched_dat <- merge(loc_matched_dat, loc_cortcomplixity, by = 'study_id', all.x = TRUE, all.y = FALSE)
loc_matched_dat <- merge(loc_matched_dat, loc_sd, by = 'study_id', all.x = TRUE, all.y = FALSE)

# 3 - reduce categories ####
loc_dat[['mom_ed']] <- ifelse(loc_dat[['mom_ed']] == 'GradSchool' | loc_dat[['mom_ed']]  == 'MastersDegree' | loc_dat[['mom_ed']]  == 'DoctoralDegree' | loc_dat[['mom_ed']]  == 'SomeGraduateSchool', '>BA', ifelse(loc_dat[['mom_ed']]  == 'Associates/Technical' | loc_dat[['mom_ed']]  == 'SomeCollege' | loc_dat[['mom_ed']]  == 'HighSchool', '<BA', ifelse(loc_dat[['mom_ed']]  == 'BachelorDegree', 'BA', NA)))

loc_dat[['income']] <- ifelse(loc_dat[['income']] == '<$20K' | loc_dat[['income']] == '$21-35K' | loc_dat[['income']] == '$36-50K', '<$51,000', ifelse(loc_dat[['income']] == '$51-75K' | loc_dat[['income']] == '$76-100K', '$51,000-$100,000', ifelse(loc_dat[['income']] == '>$100K', '>$100,000', as.character(loc_dat[['income']]))))

loc_dat[['ethnicity']] <- ifelse(loc_dat[['ethnicity']] == 'check', NA, as.character(loc_dat[['ethnicity']]))


loc_matched_dat[['mom_ed']] <- ifelse(loc_matched_dat[['mom_ed']] == 'GradSchool' | loc_matched_dat[['mom_ed']]  == 'MastersDegree' | loc_matched_dat[['mom_ed']]  == 'DoctoralDegree' | loc_matched_dat[['mom_ed']]  == 'SomeGraduateSchool', '>BA', ifelse(loc_matched_dat[['mom_ed']]  == 'Associates/Technical' | loc_matched_dat[['mom_ed']]  == 'SomeCollege' | loc_matched_dat[['mom_ed']]  == 'HighSchool', '<BA', ifelse(loc_matched_dat[['mom_ed']]  == 'BachelorDegree', 'BA', NA)))

loc_matched_dat[['income']] <- ifelse(loc_matched_dat[['income']] == '<$20K' | loc_matched_dat[['income']] == '$21-35K' | loc_matched_dat[['income']] == '$36-50K', '<$51,000', ifelse(loc_matched_dat[['income']] == '$51-75K' | loc_matched_dat[['income']] == '$76-100K', '$51,000-$100,000', ifelse(loc_matched_dat[['income']] == '>$100K', '>$100,000', as.character(loc_matched_dat[['income']]))))

loc_matched_dat[['ethnicity']] <- ifelse(loc_matched_dat[['ethnicity']] == 'check', NA, as.character(loc_matched_dat[['ethnicity']]))

# restrict to IQR ratio > 80
loc_dat <- loc_dat[loc_dat[['iqr_ratio']] >= 80, ]
loc_dat$bmi_z <- sds(loc_dat$bmi, age = loc_dat$age_yr, sex = loc_dat$sex, male = "Male", female = "Female", ref = cdc.ref, item = "bmi", type = "SDS")

loc_matched_dat <- loc_matched_dat[loc_matched_dat[['iqr_ratio']] >= 80, ]
