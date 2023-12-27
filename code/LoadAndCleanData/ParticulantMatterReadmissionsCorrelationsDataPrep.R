############################################################################################################################*
################################################file title###################################################################
##                                                                                                                         ##
##                              This file loads the cleaned data and prepares it for the                                   ##
##                                  analysis to measure correlation between particulant                                    ##
##                                  matter measures and readmissions rates for various                                     ##
##                                  heart and lung conditions in California                                                ##
############################################################################################################################*


##### Load the cleaned data                                                                                                     #####
#####*

unplanned_hospital_visits <- read_csv("data/cleaned/unplanned_hospital_visits.csv")
zip_cbsa <- read_csv("data/cleaned/zip_cbsa.csv")

##### --
#####*




unplanned_hospital_visits <- read_csv("data/cleaned/unplanned_hospital_visits.csv")

unplanned_hospital_visits %>% 
  filter(measure_name %in% c("Acute Myocardial Infarction", "CABG", 
                             "Rate of readmission for chronic obstructive pulmonary disease (COPD) patients",
                             "Heart failure (HF) 30-Day Readmission Rate", "Pneumonia (PN) 30-Day Readmission Rate")) %>%
  distinct(end_date)
