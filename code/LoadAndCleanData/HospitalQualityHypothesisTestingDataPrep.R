############################################################################################################################*
################################################file title###################################################################
##                                                                                                                         ##
##                              This file loads the cleaned data and prepares it for the                                   ##
##                                  analysis to use hypothesis testing to measure                                          ##
##                                  differences in quality of care between Inland Empire                                   ##
##                                  hospitals and elsewhere in California                                                  ##
############################################################################################################################*


##### Load the data                                                                                                     #####
#####*

healthcare_associated_infections <- read_csv("data/cleaned/healthcare_associated_infections.csv")
timely_and_effective_care <- read_csv("data/cleaned/timely_and_effective_care.csv")
patient_survey_hcahps <- read_csv("data/cleaned/patient_survey_hcahps.csv")
zip_cbsa <- read_csv("data/cleaned/zip_cbsa.csv")

##### --
#####*


##### Join the tables together                                                                                          #####
#####*

### Make convenient tables to refer to later for understanding what each measure is
measures_table <- bind_rows(
    healthcare_associated_infections %>%
      filter(measure_name %in% c("Central Line Associated Bloodstream Infection (ICU + select Wards)",
                                 "Catheter Associated Urinary Tract Infections (ICU + select Wards)",
                                 "SSI - Colon Surgery",
                                 "SSI - Abdominal Hysterectomy",
                                 "MRSA Bacteremia",
                                 "Clostridium Difficile (C.Diff)")) %>% 
      distinct(measure_name, measure_id),
    
    patient_survey_hcahps %>%
      filter(hcahps_question %in% c("Nurse communication - star rating",
                                    "Doctor communication - star rating",
                                    "Staff responsiveness - star rating")) %>%
      distinct(hcahps_question, hcahps_measure_id) %>%
      rename(measure_name = hcahps_question, measure_id = hcahps_measure_id)
  )

### Make sure we have only the columns and rows of interest and they are ready for a one-to-one match
healthcare_associated_infections <- healthcare_associated_infections %>%
  filter(measure_name %in% c("Central Line Associated Bloodstream Infection (ICU + select Wards)",
                             "Catheter Associated Urinary Tract Infections (ICU + select Wards)",
                             "SSI - Colon Surgery",
                             "SSI - Abdominal Hysterectomy",
                             "MRSA Bacteremia",
                             "Clostridium Difficile (C.Diff)")) %>%
  select(-c(measure_name, score, footnote, start_date, end_date, address, state, county_parish, telephone_number)) %>%
  pivot_wider(names_from = measure_id, values_from = compared_to_national)

patient_survey_hcahps <- patient_survey_hcahps %>%
  filter(hcahps_question %in% c("Nurse communication - star rating",
                                "Doctor communication - star rating",
                                "Staff responsiveness - star rating")) %>%
  select(-c(address, state, county_parish:telephone_number, hcahps_question:hcahps_answer_description, 
            patient_survey_star_rating_footnote:end_date)) %>%
  mutate(patient_survey_star_rating = case_when(
    patient_survey_star_rating < 2                                    ~ "poor",
    patient_survey_star_rating >= 2 & patient_survey_star_rating < 5  ~ "medium",
    patient_survey_star_rating > 4                                    ~ "excellent",
  )) %>%
  pivot_wider(names_from = hcahps_measure_id, values_from = patient_survey_star_rating)

timely_and_effective_care <- timely_and_effective_care %>%
  filter(measure_name == "Emergency department volume") %>%
  select(-c(address, state, county_parish:telephone_number, condition:measure_name, sample:end_date)) %>%
  rename(emergency_department_volume = score)

### Join the table together
hypothesis_data <- healthcare_associated_infections %>%
  left_join(patient_survey_hcahps, by = c("facility_id", "facility_name", "city_town", "zip_code")) %>%
  left_join(timely_and_effective_care, by = c("facility_id", "facility_name", "city_town", "zip_code")) %>%
  mutate(zip_code = as.character(zip_code)) %>%
  left_join(zip_cbsa %>% mutate(zip = as.character(zip)), by = c("zip_code" = "zip")) %>%
  ### reorganize columns
  select(facility_id, HAI_1_SIR:emergency_department_volume, everything()) %>%
  ### create Inland Empire variable
  mutate(inland_empire = if_else(
    cbsa_title == "Riverside-San Bernardino-Ontario, CA" &
      ua_title %in% c("Riverside--San Bernardino, CA", "Los Angeles--Long Beach--Anaheim, CA",
                      "Victorville--Hesperia--Apple Valley, CA", "Indio--Palm Desert--Palm Springs, CA",
                      "Temecula--Murrieta--Menifee, CA"),
    "yes",
    "no"
  ))

##### --
#####*


##### Save the data                                                                                                     #####
#####*

write_csv(hypothesis_data, file = "data/final/hospitalQualityHypothesisTesting/hypothesis_data.csv")
write_csv(measures_table, file = "data/final/hospitalQualityHypothesisTesting/measures_table.csv")

### clean our environment
rm(list = str_remove_all(ls(), "LoadData|CleanRawDataAndSave|HospitalQualityHypothesisTestingDataPrep"))

##### --
#####*


beep("ping")
print("End of: HospitalQualityHypothesisTestingDataPrep.R")