############ Basic Data Load/Setup########

#set working directory to location of script--not needed when called 
#through Rmarkdown doc. Uncomment below if running locally/manually
#this.dir = getActiveDocumentContext()$path
#setwd(dirname(this.dir))

source('functions.R')

##load datasets
all_dat = read.csv('Data/All_data.csv', header = TRUE, na.strings = c("NA", "", "<NA>", "#NULL!", " "))
LOC_dat = all_dat[!is.na(all_dat$loc1), c(1:66, 102:109, 219:243)]
LOC_dat$lab_id = factor(LOC_dat$lab_id)
LOC_dat$study_id = factor(LOC_dat$study_id)
LOC_dat$keep = 'N'

#Load QC data
TPMall_QC = read.csv('Data/tpm-all_qc_data.csv', header = TRUE)

#merge lab id
LOC_dat$study_id_caps = toupper(LOC_dat$study_id)
TPMall_QC$study_id_caps = toupper(TPMall_QC$parID)

#fix TRTA/B
rm_AB <- function(id){
  if (grepl('TRT', id)){
    id_short <- gsub('A', '', id)
    id_short <- gsub('B', '', id_short)
  } else if(grepl('CCEB', id)){
    id_short <- gsub('_TOY', '', id)
    id_short <- gsub('_FOOD', '', id_short)
  } else if (grepl('TESTRETEST', id)){
    id_short <- gsub('TESTRETEST', 'TRT', id)
  } else if (grepl('FBS', id)){
    id_short <- gsub('FBS', 'PORT', id)
  } else {
    id_short <- id
  }
  return(id_short)
}

LOC_dat$study_id_caps <- sapply(LOC_dat$study_id_caps, function(x) rm_AB(x), USE.NAMES = FALSE)

TPMall_QC$study_id_caps <- sapply(TPMall_QC$study_id_caps, function(x) rm_AB(x), USE.NAMES = FALSE)

LOC_QC <- merge(LOC_dat, TPMall_QC[c(10, 12:16)], by = 'study_id_caps')

#Check IQR Load Rating

visit_cmp <- function(data, id){
  labid <- data[data$parID == id, 'lab_id']
  
  if(nrow(data[data$lab_id == labid, ]) > 1){
    if (length(unique(data[data$lab_id == labid, 'iqr_ratio'])) > 1) {
      
      best_iqr <- data[data$parID == id, 'iqr_ratio'] == max(data[data$lab_id == labid, 'iqr_ratio'])
      
      if (length(best_iqr) > 1) {
        
        if (data[data$parID == id, 'visit_order'] == min(data[data$lab_id == labid, 'visit_order'])) {
          if (grepl('trt', id)) {
            best_iqr <- grepl('A', id)
          } else {
            best_iqr <- TRUE
          }
        } else {
          best_iqr <- FALSE
        }
      } 
    } else {
      
      if (grepl('trt', id)) {
        best_iqr <- grepl('A', id)
      } else {
        best_iqr <- data[data$parID == id, 'visit_order'] == min(data[data$lab_id == labid, 'visit_order'])
      }
    }
    
  } else {
    
    best_iqr <- TRUE
  }
  
  return(as.numeric(best_iqr))
}

LOC_QC$best_iqr <- sapply(LOC_QC$parID, function(x) visit_cmp(LOC_QC, x), USE.NAMES = FALSE)

#make covariate database
LOC_QC$age_yr <- as.numeric(LOC_QC$age_mo)/12
LOCstructural_covars = LOC_QC[LOC_QC$best_iqr == 1, c(2:3, 102, 8, 10:12, 38, 13, 39, 14:16, 103, 105:106, 45:46, 30:33, 50, 68:76)]

##get percent above overweight
library(childsds)

pow_fn <- function(data, id, level){
  dat <- data[data$study_id == id, ]
  
  sex_cor <- ifelse(dat$sex == 'Male', 'male', 'female')
  
  cdc_bmi8595_tab = make_percentile_tab(cdc.ref, item = "bmi", perc = c(85, 95), 
                                        age = dat$age_yr, 
                                        sex = sex_cor, stack = FALSE)
  
  if (level == 85){
    pow <- dat$bmi/cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_85_0
  } else if (level == 95){
    pow <- dat$bmi/cdc_bmi8595_tab[cdc_bmi8595_tab$sex == sex_cor, ]$perc_95_0
  }
  
  return(pow)
}

LOCstructural_covars$cdc_bmi85 <- sapply(LOCstructural_covars$study_id, function(x) pow_fn(LOCstructural_covars, x, 85), USE.NAMES = FALSE)
LOCstructural_covars$cdc_bmi95 <- sapply(LOCstructural_covars$study_id, function(x) pow_fn(LOCstructural_covars, x, 95), USE.NAMES = FALSE)

## add study and loc dummy and sex dummy and bmi class dummy??
LOCstructural_covars$bmi_class <- ifelse(LOCstructural_covars$bmi_p >= 95, 'OB', ifelse(LOCstructural_covars$bmi_p >= 85, 'OW', ifelse(!is.na(LOCstructural_covars$bmi_p), 'HW', NA)))
LOCstructural_covars$loc1_dummy <- ifelse(LOCstructural_covars$loc1 == 'Yes', 1, 0)
LOCstructural_covars$sex_dummy <- ifelse(LOCstructural_covars$sex == 'Male', 1, 0)
LOCstructural_covars$bmi_class_dummy <- ifelse(LOCstructural_covars$bmi_class == 'OB', 2, ifelse(LOCstructural_covars$bmi_class == 'OW', 1, 0))
LOCstructural_covars$bmi_hw_dummy <- ifelse(LOCstructural_covars$bmi_class == 'HW', 1, 0)

## study
#fix TRTA/B
fix_study <- function(id){
  
  if (grepl('Brand', id)){
    study <- 'brand'
  } else if (grepl('DMK', id)){
    study <- 'dmk'
  } else if (grepl('FBS', id)){
    study <- 'fbs'
  } else if (grepl('R01', id)){
    study <- 'r01'
  } else if (grepl('TestRetest', id)){
    study <- 'trt'
  } else {
    study <- id
  }
  
  return(study)
}
LOCstructural_covars$study <- sapply(LOCstructural_covars$study_id, function(x) fix_study(x), USE.NAMES = FALSE)

LOCstructural_covars$study_dummy <- ifelse(LOCstructural_covars$study == 'brand', 1, ifelse(LOCstructural_covars$study == 'fbs', 2, ifelse(LOCstructural_covars$study == 'dmk', 3, ifelse(LOCstructural_covars$study == 'r01', 4, ifelse(LOCstructural_covars$study == 'trt', 5, NA)))))

## remove those with iqr ratio < 70
LOCstructural_covars <- LOCstructural_covars[LOCstructural_covars$iqr_ratio >= 70, ]


#reduce to 'keep' and write out dsets
write.csv(LOCstructural_covars[c('lab_id', 'parID')], 'Data/loc_parlist.csv', row.names = F)
write.csv(LOCstructural_covars, 'Data/loc_covars.csv', row.names = F)
