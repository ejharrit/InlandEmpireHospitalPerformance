############################################################################################################################*
################################################file title###################################################################
##                                                                                                                         ##
##                              This file loads the data we pulled from the Data.CMS.gov                                   ##
##                                  API, cleans it and saves the new data format                                           ##
##                                                                                                                         ##
##                                                                                                                         ##
############################################################################################################################*

##### Notes                                                                                                             #####

##### Load the data                                                                                                     #####
#####*

load("data/raw/hospital_data_list.RData")

##### --
#####*


##### Custom functions                                                                                                  #####
#####*

separate_multiple_cols <- function(x, criteria){
  if(is.character(criteria)){
    x %>%
    mutate(across(matches(criteria), 
                  ~ list(tibble(col1 = .) %>% 
                           separate(col1, into = str_c(cur_column(), c("", "_base")), sep = " ", extra = "merge"))))
  }else{
    print("criteria must be a character vector to pass to matches() and select which columns to separate.")
  }
}

basic_cleaning <- function(x){
  x %>%
    distinct() %>%
    remove_empty(c("rows", "cols")) %>%
    clean_names()
}

##### --
#####*


##### Check for missing values                                                                                          #####
#####*

map(hospital_data, skim)

### adjust missing values
hospital_data <- map(hospital_data, ~ .x %>% 
                       mutate(across(everything(), \(x) na_if(x, "N/A"))) %>%
                       mutate(across(everything(), \(x) na_if(x, "Not Available"))) %>%
                       mutate(across(everything(), \(x) na_if(x, "Not Applicable"))))


##### --
#####*


##### Split the data into individual data frames                                                                        #####
#####*

### split our list
iwalk(
  .x = hospital_data,
  .f = function(x, y) {
    x <- as_tibble(x)
    assign(y, x, envir = globalenv())
  }
)

### get the names of our list to save for later
list_names <- names(hospital_data)

### remove the list to avoid confusion
remove(hospital_data)

##### --
#####*


##### ambulatory_surgical_center_quality data set                                                                       #####
#####*

glimpse(ambulatory_surgical_center_quality)

ambulatory_surgical_center_quality <- ambulatory_surgical_center_quality %>%
  basic_cleaning() %>%
  mutate(across(matches("rate|cases|limit|sample"), \(x) as.numeric(x)))

##### --
#####*


##### cms_medicare_psi_90 data set                                                                                      #####
#####*

glimpse(cms_medicare_psi_90)

### clean cms_medicare_psi_90
cms_medicare_psi_90 <- cms_medicare_psi_90 %>%
  basic_cleaning() %>%
  mutate(rate = as.numeric(rate)) %>%
  mutate(across((matches("date")), \(x) mdy(x))) 
  

##### --
#####*


##### complications_and_deaths dataset                                                                                  #####
#####*

glimpse(complications_and_deaths)

### clean complications_and_deaths
complications_and_deaths <- complications_and_deaths %>%
  basic_cleaning() %>%
  mutate(across(matches("denominator|score|estimate"), \(x) as.numeric(x))) %>% 
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### general_information data set                                                                                      #####
#####*

glimpse(general_information)

general_information <- general_information %>%
  basic_cleaning() %>%
  mutate(across((matches("count_|_count|raiting")&!matches("footnote")), \(x) as.numeric(x))) 

##### --
#####*


##### healthcare_associated_infections dataset                                                                          #####
#####*

glimpse(healthcare_associated_infections)

### clean healthcare_associated_infections
healthcare_associated_infections <- healthcare_associated_infections %>%
  basic_cleaning() %>%
  mutate(score = na_if(score, "--")) %>%
  mutate(across(matches("score"), \(x) as.numeric(x))) %>% 
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### hvbp_clinical_outcomes_domain dataset                                                                             #####
#####*

glimpse(hvbp_clinical_outcomes_domain)

hvbp_clinical_outcomes_domain <- hvbp_clinical_outcomes_domain %>%
  basic_cleaning() %>%
  ### A tricky part here. All the columns that are scores on a scale (e.g. "5 out of 9", "4 out of 10") we convert
  ### to two columns. One column to keep track of the score received, and the other to keep track of the scale 
  group_by(facility_id) %>%
  separate_multiple_cols("points|score") %>% 
  unnest(cols = c(mort_30_ami_achievement_points,     mort_30_ami_improvement_points,   mort_30_ami_measure_score,
                  mort_30_hf_achievement_points,      mort_30_hf_improvement_points,    mort_30_hf_measure_score, 
                  mort_30_copd_achievement_points,    mort_30_copd_improvement_points,  mort_30_copd_measure_score, 
                  mort_30_cabg_achievement_points,    mort_30_cabg_improvement_points,  mort_30_cabg_measure_score, 
                  comp_hip_knee_achievement_points,   comp_hip_knee_improvement_points, comp_hip_knee_measure_score)) %>%
  ungroup() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate")&!matches("_base$")), \(x) as.numeric(x)))

##### --
#####*


##### hvbp_efficiency_scores data set                                                                                   #####
#####*

glimpse(hvbp_efficiency_scores)

hvbp_efficiency_scores <- hvbp_efficiency_scores %>%
  basic_cleaning() %>%
  ### A tricky part here. All the columns that are scores on a scale (e.g. "5 out of 9", "4 out of 10") we convert
  ### to two columns. One column to keep track of the score received, and the other to keep track of the scale 
  group_by(facility_id) %>%
  separate_multiple_cols("points|score") %>%
  unnest(cols = c(mspb_1_achievement_points, mspb_1_improvement_points, mspb_1_measure_score)) %>%
  ungroup() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate")&!matches("_base$")), \(x) as.numeric(x)))

##### --
#####*


##### hvbp_person_and_community dataset                                                                                 #####
#####*

glimpse(hvbp_person_and_community)

hvbp_person_and_community <- hvbp_person_and_community %>%
  basic_cleaning() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate|floor")&!matches("_base$")), 
                \(x) as.numeric(str_remove(x, "%"))))

##### --
#####*


##### hvbp_safety data set                                                                                              #####
#####*

glimpse(hvbp_safety)

hvbp_safety <- hvbp_safety %>%
  basic_cleaning() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate|floor")&!matches("_base$")), 
                \(x) as.numeric(str_remove(x, "%"))))

##### --
#####*


##### hvbp_total_performance_score data set                                                                             #####
#####*

glimpse(hvbp_total_performance_score)

hvbp_total_performance_score <- hvbp_total_performance_score %>%
  basic_cleaning() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate|floor")&!matches("_base$")), \(x) as.numeric(x))) 
##### --
#####*


##### inpatient_psychiatric_facility data set                                                                           #####
#####*

glimpse(inpatient_psychiatric_facility)

inpatient_psychiatric_facility <- inpatient_psychiatric_facility %>%
  basic_cleaning() %>%
  mutate(across((matches("rate|num|den|percent|denominator|estimate")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))


##### --
#####*


##### maternal_health data set                                                                                          #####
#####*

glimpse(maternal_health)

### clean maternal_health data set
maternal_health <- maternal_health %>%
  basic_cleaning() %>% 
  mutate(
         score = na_if(score, "Not Applicable (our hospital does not provide inpatient labor/delivery care)"),
         score = na_if(score, "")
         ) %>%
  ### most values are numeric but measure SM_7 is measured with "Yes" or "No". Information on this variable would be lost
  ### by converting values to numeric right now even though most of them are numeric. 
  # mutate(across((matches("score|sample")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### medicare_hospital_spending_by_claim data set                                                                      #####
#####*

glimpse(medicare_hospital_spending_by_claim)

medicare_hospital_spending_by_claim <- medicare_hospital_spending_by_claim %>%
  basic_cleaning() %>%
  mutate(across((matches("spndg")), \(x) as.numeric(str_remove(x, "%")))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### medicare_spending_per_beneficiary data set                                                                        #####
#####*

glimpse(medicare_spending_per_beneficiary)

medicare_spending_per_beneficiary <- medicare_spending_per_beneficiary %>%
  basic_cleaning() %>%
  mutate(across((matches("MSPB-1")), \(x) as.numeric(x)))  %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### OAS_CAHPS_ambulatory_surgical_centers data set                                                                    #####
#####*

glimpse(oas_cahps_ambulatory_surgical_centers)

oas_cahps_ambulatory_surgical_centers <- oas_cahps_ambulatory_surgical_centers %>%
  basic_cleaning() %>%
  mutate(across((matches("patients|score|survey")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### oas_cahps_survey_outpatient dataset                                                                               #####
#####*

glimpse(oas_cahps_survey_outpatient)

oas_cahps_survey_outpatient <- oas_cahps_survey_outpatient %>%
  basic_cleaning() %>%
  mutate(across((matches("patients|score|survey")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### oncology_care_ceasures_exempt_cancer data set                                                                     #####
#####*

glimpse(oncology_care_ceasures_exempt_cancer)

### clean the oncology_care_ceasures_exempt_cancer data set
oncology_care_ceasures_exempt_cancer <- oncology_care_ceasures_exempt_cancer %>%
  basic_cleaning() %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  mutate(across((matches("performance|denominator")), \(x) as.numeric(x))) %>% print(width = Inf)

##### --
#####*


##### outpatient_imaging_efficiency data set                                                                            #####
#####*

glimpse(outpatient_imaging_efficiency)

### clean the outpatient_imaging_efficiency data set
outpatient_imaging_efficiency <- outpatient_imaging_efficiency %>%
  basic_cleaning() %>%
  mutate(score = as.numeric(score)) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### patient_survey_hcahps data set                                                                                    #####
#####*

glimpse(patient_survey_hcahps)

### clean the patient_survey_hcahps data set
patient_survey_hcahps <- patient_survey_hcahps %>%
  basic_cleaning() %>%
  mutate(across((matches("survey|linear|percent")&!matches("footnote")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>% print(width = Inf)

##### --
#####*



##### payment_and_value_of_care data set                                                                                #####
#####*

glimpse(payment_and_value_of_care)

### clean the payment_and_value_of_care data set
payment_and_value_of_care <- payment_and_value_of_care %>%
  basic_cleaning() %>% 
  mutate(across((matches("denominator|payment|estimate")&!matches("footnote|category|_id|_name")), 
                \(x) as.numeric(str_remove_all(x, "\\$|,")))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### pch_hcahps_pps_exempt_cancer data set                                                                             #####
#####*

glimpse(pch_hcahps_pps_exempt_cancer)

pch_hcahps_pps_exempt_cancer <- pch_hcahps_pps_exempt_cancer %>%
  basic_cleaning() %>% 
  mutate(across((matches("rating|percent|linear|completed")&!matches("footnote")), 
                \(x) as.numeric(str_remove_all(x, "\\$|,")))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### safety_and_healthcare_associated data set                                                                         #####
#####*

glimpse(safety_and_healthcare_associated)

safety_and_healthcare_associated <- safety_and_healthcare_associated %>%
  basic_cleaning() %>% 
  mutate(
    score = na_if(score, "--"),
    score = as.numeric(score)
    ) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### timely_and_effective_care data set                                                                                #####
#####*

glimpse(timely_and_effective_care)

timely_and_effective_care <- timely_and_effective_care %>%
  basic_cleaning() %>% 
  ### most values are numeric but measure EDV is measured with values such as "medium", "high" and "very high".
  ### Information on this variable would be lost by converting values to numeric right now even though most of 
  ### them are numeric. 
  # mutate(score = as.numeric(score)) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### unplanned_hospital_visits data set                                                                                #####
#####*

glimpse(unplanned_hospital_visits)

unplanned_hospital_visits <- unplanned_hospital_visits %>%
  basic_cleaning() %>% 
  mutate(across(matches("denominator|score|estimate|number_of"), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x)))

##### --
#####*


##### unplanned_hospital_visits_exempt data set                                                                         #####
#####*

glimpse(unplanned_hospital_visits_exempt)

unplanned_hospital_visits_exempt %>%
  basic_cleaning() %>%
  mutate(across(matches("total|rate|limit"), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  print(width=Inf)

##### --
#####*


##### create a table to convert zip to cbsa and zip to urban area                                                                             #####
#####*

zip_cbsa <- read_excel("data/raw/ZIP_CBSA_092023.xlsx", sheet = "Export Worksheet") %>%
  clean_names()

cbsa_csa <- read_excel("data/raw/Census_CBSA_CSA_2023.xlsx", skip = 2, col_names = TRUE) %>%
  clean_names()

zip_cbsa <- zip_cbsa %>%
  filter(usps_zip_pref_state == "CA") %>%
  ### zip codes map to multiple CBSAs, the code below assigns a zipcode to the CBSA with the highest ratio
  ### of residential addresses for that zipcode
  group_by(zip) %>%
  arrange(desc(res_ratio)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  distinct(zip, cbsa) %>%
  ### Join the zip_cbsa file to the cbsa_csa file to get the cbsa_title
  left_join(
    cbsa_csa %>%
      filter(state_name == "California") %>%
      distinct(cbsa_code, cbsa_title),
    by = c("cbsa" = "cbsa_code")
  ) %>%
  left_join(
    read_excel("data/raw/2020_Census_ua_list_all.xlsx") %>%
      filter(str_detect(NAME, ", CA")) %>%
      select(UACE, NAME) %>%
      rename(ua_title = NAME, urban_area = UACE) %>%
      left_join(
        read_delim("https://www2.census.gov/geo/docs/maps-data/data/rel2020/ua/tab20_ua20_zcta520_natl.txt") %>%
          filter(str_detect(NAMELSAD_UA_20, ", CA")) %>%
          group_by(GEOID_ZCTA5_20) %>%
          arrange(desc(AREALAND_PART)) %>%
          slice_head(n = 1) %>%
          ungroup() %>%
          select(GEOID_UA_20, GEOID_ZCTA5_20),
        by = c("urban_area" = "GEOID_UA_20")
      ),
    by = c("zip" = "GEOID_ZCTA5_20")
  ) %>%
  mutate(zip = as.character(zip))

remove(cbsa_csa)

##### --
#####*


##### save all the files                                                                                                #####
#####*

### save all data frames individually
dataframes <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

walk(dataframes, ~ write_csv(.GlobalEnv[[.x]], file = paste0("data/cleaned/", .x, ".csv")))

### Sometimes it might be easier to load all the data in a list and select the data frames needed from that.
### The code below combines all the dataframes into a list and saves it
hospital_data <- map(dataframes, ~.GlobalEnv[[.x]])
names(hospital_data) <- dataframes

save(hospital_data, file = "data/cleaned/hospital_data.RData")

### clean the environment
rm(list = str_remove_all(ls(), "LoadData|CleanRawDataAndSave"))

##### --
#####*



beep("treasure")
print("End of: CleanRawDataAndSave.R")