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
  distinct() %>%
  mutate(across(matches("rate|cases|limit|sample"), \(x) as.numeric(x))) %>% 
  select(where(~!all(is.na(.x)))) %>%
  clean_names()

##### --
#####*


##### cms_medicare_psi_90 data set                                                                                      #####
#####*

glimpse(cms_medicare_psi_90)

### create a new table of the measures
psi_90_measures <- cms_medicare_psi_90 %>%
  distinct(measure_id, measure_name)

list_names <- c(list_names, "psi_90_measures")

### clean cms_medicare_psi_90
cms_medicare_psi_90 <- cms_medicare_psi_90 %>%
  distinct() %>%
  mutate(
    rate = as.numeric(rate)) %>%
  select(-measure_name) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x)))) %>%
  clean_names()
  

##### --
#####*


##### complications_and_deaths dataset                                                                                  #####
#####*

glimpse(complications_and_deaths)

### create a new table of the measures
complications_and_deaths_measures <- complications_and_deaths %>%
  distinct(measure_id, measure_name)

list_names <- c(list_names, "complications_and_deaths_measures")

### clean complications_and_deaths
complications_and_deaths <- complications_and_deaths %>%
  distinct() %>%
  select(-measure_name) %>%
  mutate(across(matches("denominator|score|estimate"), \(x) as.numeric(x))) %>% 
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x)))) %>%
  clean_names()

##### --
#####*


##### general_information data set                                                                                      #####
#####*

glimpse(general_information)

general_information <- general_information %>%
  distinct()%>%
  mutate(across(matches("count_|_count"), \(x) as.numeric(x)),
         hospital_overall_rating = as.numeric(hospital_overall_rating)) %>% 
  select(where(~!all(is.na(.x)))) %>%
  clean_names()

##### --
#####*


##### healthcare_associated_infections dataset                                                                          #####
#####*

glimpse(healthcare_associated_infections)

### create a new table for the measures
healthcare_associated_infections_measures <- healthcare_associated_infections %>%
  distinct(measure_id, measure_name)

list_names <- c(list_names, "healthcare_associated_infections_measures")

### clean healthcare_associated_infections
healthcare_associated_infections <- healthcare_associated_infections %>%
  distinct() %>%
  mutate(score = na_if(score, "--")) %>%
  select(-measure_name) %>%
  mutate(across(matches("score"), \(x) as.numeric(x))) %>% 
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x)))) %>%
  clean_names()

##### --
#####*


##### hvbp_clinical_outcomes_domain dataset                                                                             #####
#####*

glimpse(hvbp_clinical_outcomes_domain)

hvbp_clinical_outcomes_domain <- hvbp_clinical_outcomes_domain %>%
  distinct() %>% 
  ### A tricky part here. All the columns that are scores on a scale (e.g. "5 out of 9", "4 out of 10") we convert
  ### to two columns. One column to keep track of the score received, and the other to keep track of the scale 
  select(where(~!all(is.na(.x)))) %>%
  group_by(facility_id) %>%
  mutate(across(matches("points|score"), ~ list(tibble(col1 = .) %>% 
                  separate(col1, into = str_c(cur_column(), c("", "_base")), sep = " ", extra = "merge")))) %>% 
  unnest(cols = c(mort_30_ami_achievement_points,     mort_30_ami_improvement_points,   mort_30_ami_measure_score,
                  mort_30_hf_achievement_points,      mort_30_hf_improvement_points,    mort_30_hf_measure_score, 
                  mort_30_copd_achievement_points,    mort_30_copd_improvement_points,  mort_30_copd_measure_score, 
                  mort_30_cabg_achievement_points,    mort_30_cabg_improvement_points,  mort_30_cabg_measure_score, 
                  comp_hip_knee_achievement_points,   comp_hip_knee_improvement_points, comp_hip_knee_measure_score)) %>%
  ungroup() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate")&!matches("_base$")), \(x) as.numeric(x))) %>% 
  select(where(~!all(is.na(.x)))) %>%
  print(width = Inf)

##### --
#####*


##### hvbp_efficiency_scores data set                                                                                   #####
#####*

glimpse(hvbp_efficiency_scores)

hvbp_efficiency_scores <- hvbp_efficiency_scores %>%
  distinct() %>%
  ### A tricky part here. All the columns that are scores on a scale (e.g. "5 out of 9", "4 out of 10") we convert
  ### to two columns. One column to keep track of the score received, and the other to keep track of the scale 
  mutate(across(matches("points|score"), ~ tibble(col1 = .) %>% 
                  separate(col1, into = str_c(cur_column(), c("", "_base")), sep = " ", extra = "merge"))) %>% 
  unnest(cols = matches("points|score")) %>%
  mutate(across((matches("points|score|threshold|benchmark|rate")&!matches("_base$")), \(x) as.numeric(x))) %>% 
  select(where(~!all(is.na(.x))))

##### --
#####*


##### hvbp_person_and_community dataset                                                                                 #####
#####*

glimpse(hvbp_person_and_community)

hvbp_person_and_community <- hvbp_person_and_community %>%
  distinct() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate|floor")&!matches("_base$")), 
                \(x) as.numeric(str_remove(x, "%")))) %>% 
  select(where(~!all(is.na(.x))))

##### --
#####*


##### hvbp_safety data set                                                                                              #####
#####*

glimpse(hvbp_safety)

hvbp_safety <- hvbp_safety %>%
  distinct() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate|floor")&!matches("_base$")), 
                \(x) as.numeric(str_remove(x, "%")))) %>% 
  select(where(~!all(is.na(.x))))

##### --
#####*


##### hvbp_total_performance_score data set                                                                             #####
#####*

glimpse(hvbp_total_performance_score)

hvbp_total_performance_score <- hvbp_total_performance_score %>%
  distinct() %>%
  mutate(across((matches("points|score|threshold|benchmark|rate|floor")&!matches("_base$")), \(x) as.numeric(x))) %>% 
  select(where(~!all(is.na(.x))))

##### --
#####*


##### inpatient_psychiatric_facility data set                                                                           #####
#####*

glimpse(inpatient_psychiatric_facility)

inpatient_psychiatric_facility <- inpatient_psychiatric_facility %>%
  distinct() %>%
  mutate(across((matches("rate|num|den|percent|denominator|estimate")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x))))

##### --
#####*


##### maternal_health data set                                                                                          #####
#####*

glimpse(maternal_health)

### table of measures
maternal_health_measures <- maternal_health %>%
  distinct(measure_id, measure_name)

### clean maternal_health data set
maternal_health <- maternal_health %>%
  distinct() %>%
  select(-measure_name) %>%
  mutate(score = na_if(score, "Yes"),
         score = na_if(score, "Not Applicable (our hospital does not provide inpatient labor/delivery care)"),
         score = na_if(score, "No"),
         score = na_if(score, "")) %>%
  pivot_wider(
    names_from = measure_id,
    values_from = c(score, sample)) %>%
  mutate(across((matches("score|sample")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x))))

##### --
#####*


##### medicare_hospital_spending_by_claim data set                                                                      #####
#####*

glimpse(medicare_hospital_spending_by_claim)

medicare_hospital_spending_by_claim <- medicare_hospital_spending_by_claim %>%
  distinct() %>%
  mutate(across((matches("spndg")), \(x) as.numeric(str_remove(x, "%")))) %>%
  pivot_wider(
    names_from = c(period, claim_type),
    values_from = c(avg_spndg_per_ep_hospital,  avg_spndg_per_ep_state, avg_spndg_per_ep_national, 
                    percent_of_spndg_hospital,  percent_of_spndg_state, percent_of_spndg_national)
  ) %>%
  clean_names() %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x))))

##### --
#####*


##### medicare_spending_per_beneficiary data set                                                                        #####
#####*

glimpse(medicare_spending_per_beneficiary)

### build a table for the measures
medicare_spending_per_beneficiary_measures <- medicare_spending_per_beneficiary %>%
  distinct(measure_name, measure_id)

medicare_spending_per_beneficiary <- medicare_spending_per_beneficiary %>%
  distinct() %>%
  select(-measure_name) %>%
  pivot_wider(names_from = measure_id, values_from = score) %>%
  mutate(across((matches("MSPB-1")), \(x) as.numeric(x)))  %>%
  clean_names() %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x))))

##### --
#####*


##### OAS_CAHPS_ambulatory_surgical_centers data set                                                                    #####
#####*

glimpse(OAS_CAHPS_ambulatory_surgical_centers)

oas_cahps_ambulatory_surgical_centers <- OAS_CAHPS_ambulatory_surgical_centers %>%
  distinct() %>%
  mutate(across((matches("patients|score|survey")), \(x) as.numeric(x))) %>%
  clean_names() %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x))))

##### --
#####*


##### oas_cahps_survey_outpatient dataset                                                                               #####
#####*

glimpse(oas_cahps_survey_outpatient)

oas_cahps_survey_outpatient <- oas_cahps_survey_outpatient %>%
  distinct() %>%
  mutate(across((matches("patients|score|survey")), \(x) as.numeric(x))) %>%
  clean_names() %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x))))

##### --
#####*


##### oncology_care_ceasures_exempt_cancer data set                                                                     #####
#####*

glimpse(oncology_care_ceasures_exempt_cancer)

### create a measure table
oncology_care_ceasures_exempt_cancer_measures <- oncology_care_ceasures_exempt_cancer %>%
  distinct(measure_id, measure_description)

### clean the oncology_care_ceasures_exempt_cancer data set
oncology_care_ceasures_exempt_cancer <- oncology_care_ceasures_exempt_cancer %>%
  distinct() %>%
  select(-measure_description) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  mutate(across((matches("performance|denominator")), \(x) as.numeric(x)))

##### --
#####*


##### outpatient_imaging_efficiency data set                                                                            #####
#####*

glimpse(outpatient_imaging_efficiency)

### create a measure table
outpatient_imaging_efficiency_measures <- outpatient_imaging_efficiency %>%
  distinct(measure_id, measure_name)

### clean the outpatient_imaging_efficiency data set
outpatient_imaging_efficiency <- outpatient_imaging_efficiency %>%
  distinct() %>%
  select(-measure_name) %>%
  pivot_wider(names_from = measure_id, values_from = score) %>%
  mutate(across((matches("OP")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x)))) %>%
  clean_names()

##### --
#####*


##### patient_survey_hcahps data set                                                                                    #####
#####*

glimpse(patient_survey_hcahps)

### create a measures table
patient_survey_hcahps_measures <- patient_survey_hcahps %>%
  distinct(hcahps_measure_id, hcahps_question, hcahps_answer_description)

### clean the patient_survey_hcahps data set
patient_survey_hcahps <- patient_survey_hcahps %>%
  distinct() %>%
  select(-c(hcahps_question, hcahps_answer_description)) %>%
  pivot_wider(
    names_from = hcahps_measure_id,
    values_from = c(patient_survey_star_rating,           hcahps_answer_percent, hcahps_linear_mean_value,
                    patient_survey_star_rating_footnote,  hcahps_answer_percent_footnote)
  ) %>%
  mutate(across((matches("survey|linear|percent")&!matches("footnote")), \(x) as.numeric(x))) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x)))) %>%
  clean_names()

##### --
#####*



##### payment_and_value_of_care data set                                                                                #####
#####*

glimpse(payment_and_value_of_care)

### create a measures table 
payment_and_value_of_care_measures <- payment_and_value_of_care %>%
  distinct(payment_measure_id, payment_measure_name)

### clean the payment_and_value_of_care data set
payment_and_value_of_care <- payment_and_value_of_care %>%
  distinct() %>%
  select(-payment_measure_name) %>%
  pivot_wider(
    names_from = payment_measure_id,
    values_from = c(payment_category, denominator,      payment,                  lower_estimate,
                    higher_estimate,  payment_footnote, value_of_care_display_id, value_of_care_category,
                    value_of_care_display_name,         value_of_care_footnote)
  ) %>% 
  mutate(across((matches("denominator|payment|estimate")&!matches("footnote|category")), 
                \(x) as.numeric(str_remove_all(x, "\\$|,")))) %>%
  mutate(across((matches("date")), \(x) mdy(x))) %>%
  select(where(~!all(is.na(.x)))) %>%
  clean_names()
  

##### --
#####*


##### pch_hcahps_pps_exempt_cancer data set                                                                             #####
#####*

glimpse(pch_hcahps_pps_exempt_cancer)

%>%
  print(width=Inf)

##### --
#####*


##### chunk name here                                                                                                   #####
#####*

%>%
  print(width=Inf)

##### --
#####*


