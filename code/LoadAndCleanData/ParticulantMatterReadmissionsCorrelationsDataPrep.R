############################################################################################################################*
################################################ file title###################################################################
##                                                                                                                         ##
##                              This file loads the cleaned data and prepares it for the                                   ##
##                                  analysis to measure correlation between particulant                                    ##
##                                  matter measures and readmissions rates for various                                     ##
##                                  heart and lung conditions in California                                                ##
############################################################################################################################*


##### Load the cleaned data                                                                                             #####
#####*

unplanned_hospital_visits <- read_csv("data/cleaned/unplanned_hospital_visits.csv")

##### --
#####*


##### Load EPA data                                                                                                     #####
#####*
### set the credentials to access the EPA api
source("code/api_key.R")

### pass the user credentials from the sourced file as arguments to be used
aqs_credentials(
  username = email,
  key = api_key
)

### create a list of all the county fips in CA
county_list <- aqs_counties_by_state("06", return_header = FALSE) %>%
  pull(county_code)

### make the request, parameter code list can be found at https://aqs.epa.gov/aqsweb/documents/codetables/methods_all.html
epa_county <- aqs_annualsummary_by_county(
  parameter = c("88128", "42101"), stateFIPS = "06", countycode = county_list,
  bdate = as.Date("2022-01-01"), edate = as.Date("2022-12-31")
)

##### --
#####*

##### Select the columns we are interested in and join the data frames                                                  #####
#####*
### for the epa data
epa_county <- epa_county %>%
  distinct(parameter, sample_duration,  arithmetic_mean,  county) %>%
  filter(
    (parameter == "Lead PM2.5 LC") | 
      (parameter == "Carbon monoxide" & sample_duration == "8-HR RUN AVG END HOUR")
  ) %>%
  ### there are multiple measuring sites in some counties, the average is calculated by county for each parameter
  group_by(county, parameter) %>%
  summarize(mean = mean(arithmetic_mean, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  clean_names()

### create a measures table to be able to refer to the measures
measures_table <- unplanned_hospital_visits %>%
  filter(measure_name %in% c(
    "Acute Myocardial Infarction (AMI) 30-Day Readmission Rate", 
    "Rate of readmission for CABG",
    "Rate of readmission for chronic obstructive pulmonary disease (COPD) patients",
    "Heart failure (HF) 30-Day Readmission Rate", 
    "Pneumonia (PN) 30-Day Readmission Rate"
  )) %>%
  distinct(measure_id, measure_name)

### for the unplanned hospital visits data
unplanned_hospital_visits <- unplanned_hospital_visits %>%
  distinct(
    facility_id, facility_name, county_parish, measure_id, measure_name,
    denominator, score
  ) %>%
  filter(measure_name %in% c(
    "Acute Myocardial Infarction (AMI) 30-Day Readmission Rate", 
    "Rate of readmission for CABG",
    "Rate of readmission for chronic obstructive pulmonary disease (COPD) patients",
    "Heart failure (HF) 30-Day Readmission Rate", 
    "Pneumonia (PN) 30-Day Readmission Rate"
  )) %>%
  ### calculate the return rate by county as a weighted mean to account for the number of patients at each hospital
  group_by(county_parish, measure_id) %>%
  summarize(score = weighted.mean(score, denominator, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = measure_id, values_from = score)

### join the epa dat and unplanned hospital visits data
pm_readmissions_regression <- unplanned_hospital_visits %>%
  left_join(
    epa_county %>% 
      mutate(county = str_to_upper(county)), 
    by = c("county_parish" = "county")
  )

##### --
#####*


##### save the data we will need                                                                                        #####
#####*

write_csv(
  pm_readmissions_regression,
  file = "data/final/particulantMatterReadmissionsCorrelation/pm_readmissions_regression.csv"
)

write_csv(measures_table, file = "data/final/particulantMatterReadmissionsCorrelation/measures_table.csv")

### clean our environment
rm(list = str_remove_all(ls(), "LoadData|CleanRawDataAndSave|HospitalQualityHypothesisTestingDataPrep"))

##### --
#####*


beep("mario")
print("End of: ParticulantMatterReadmissionsCorrelationsDataPrep.R")
