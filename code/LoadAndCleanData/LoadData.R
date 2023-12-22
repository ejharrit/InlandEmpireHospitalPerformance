############################################################################################################################*
############################################################################################################################*
##### File Title                                                                                                        #####
##                              This file loads the many hospital level data files from                                    ##
##                                    the API  made available at data.csm.gov                                              ##
##                                                                                                                         ##
##                                                                                                                         ##
############################################################################################################################*


##### Build a function to pull the data                                                                                 #####
#####*

### Create a function that GETs the data from the url and parses it into a dataframe
get_content <- function(url, year) {
  request(url) %>%
    ### the requests seem to only pull 1000 records, this adds an iterative request to *repeatedly* pull
    ### 1000 records at a time
    req_perform_iterative(next_req = iterate_with_offset("offset", start = 1, offset = 1000)) %>%
    map_dfr(resp_body_json, simplifyVector = TRUE) %>%
    as_tibble() %>%
    clean_names()
}


##### --
#####*


##### Set up the vectors for mapping in purr                                                                            #####
#####*

### dataset links
### vector of distribution IDs for the dataset links
distribution_ids <- c(
  ### Maternal Health                     ### General Information                 ### OAS CAHPS ambulatory surgical centers
  "f1e19d8e-0d68-50ca-bb4d-2171c0ef30d4", "dca90d1c-047a-5377-ad18-bb78e1b24050", "6773490c-7db1-5842-ba68-6d2764ff9c81",
  ### Ambulatory Surgical Center Quality  ### Unplanned Hospital Visits           ### Healthcare Associated Infections
  "ed8daedb-7687-58f8-b87d-794e938f4e90", "db473936-bd75-5c86-863b-1c2bb00d06ee", "ee97b48d-5453-596e-9674-d5cdbc35263c",
  ### Payment and value of care           ### Patient survey (HCAHPS)             ### Oncology Care Measures - Exempt Cancer
  "6b645263-04ac-57dd-95be-8497245405d3", "37ce9cc6-e255-5aae-ae56-16bee7fbaf16", "04f1ee7b-18f3-5caf-abd1-8d2bf697d8d9",
  ### (PCH - HCAHPS) PPS-Exempt Cancer    ### Safety and Healthcare-Associated    ### CMS Medicare PSI-90
  "4fd40c99-8d3a-50af-86fd-6a74946d58df", "6191c058-5230-5725-9d87-bec5f8c8cf8a", "a2e5b4e6-34e1-5c04-88e4-57f7a5e1937c",
  ### Inpatient Psychiatric Facility      ### Medicare Spending Per Beneficiary   ### Outpatient Imaging Efficiency
  "7656a979-d2e2-5099-951c-8088b5498485", "db4357d5-2599-5be2-b0bc-8f82e4a3e186", "ee5f4da2-65e1-5e3a-8900-ad37de1e53f3",
  ### (OAS CAHPS) survey for outpatient   ### Complications and Deaths            ### Timely and Effective Care
  "d89e7608-aaa0-53d6-81d0-e935eb20323c", "c3a34dc7-b8a2-5a08-8335-11b4471ca6e4", "b65f96ef-dddb-5c14-bfe5-0bd8f9cfcd64",
  ### Unplanned Hospital Visits-Exempt    ### (HVBP) - Person and Community       ### (HVBP) - Safety
  "b455e81a-3fa7-5f7b-9f96-24b40e70a176", "b378f9f7-0359-503f-b0c1-4e873c296d2b", "6c4acb6b-5606-53d7-a176-dcbc345b52c3",
  ### Medicare Hospital Spending by Claim ### (HVBP) - Total Performance Score    ### (HVBP) - Efficiency Scores
  "39b7b4e2-f810-5b73-9786-75535bae10c9", "ffba8cce-1122-59fa-93d4-2e9a475bb124", "ecfcc479-61d9-5efe-908c-0b82e6f108e9",
  ### (HVBP) - Clinical Outcomes Domain
  "f4c09337-10d1-5361-aeb4-876561254c4c"
)

data_links <- map_chr(
  distribution_ids,
  function(x) {
    paste0(
      "https://data.cms.gov/provider-data/api/1/datastore/sql?query=%5BSELECT%20%2A%20FROM%20",
      x,
      "%5D%5BWHERE%20State%20%3D%20%22CA%22%5D"
    )
  }
)
### Create a vector of names for the individual datasets
dataset_names <- c(
  "maternal_health",                      "general_information",                  "oas_cahps_ambulatory_surgical_centers",
  "ambulatory_surgical_center_quality",   "unplanned_hospital_visits",            "healthcare_associated_infections",
  "payment_and_value_of_care",            "patient_survey_hcahps",                "oncology_care_ceasures_exempt_cancer",
  "pch_hcahps_pps_exempt_cancer",         "safety_and_healthcare_associated",     "cms_medicare_psi_90",
  "inpatient_psychiatric_facility",       "medicare_spending_per_beneficiary",    "outpatient_imaging_efficiency",
  "oas_cahps_survey_outpatient",          "complications_and_deaths",             "timely_and_effective_care",
  "unplanned_hospital_visits_exempt",     "hvbp_person_and_community",            "hvbp_safety",
  "medicare_hospital_spending_by_claim",  "hvbp_total_performance_score",         "hvbp_efficiency_scores",
  "hvbp_clinical_outcomes_domain"
)


##### --
#####*


##### map over the vector of links and download the data from the api                                                   #####
#####*

### map the vector to the function and load the data
hospital_data <- map(data_links, get_content)

### Name the elements of the list so that what will eventually become individual data frames will be named
names(hospital_data) <- dataset_names

##### --
#####*


##### Download the documentation for the data we downloaded                                                             #####
#####*

download.file("https://data.cms.gov/provider-data/sites/default/files/data_dictionaries/hospital/HOSPITAL_Data_Dictionary.pdf",
              destfile = "data/Documentation/HOSPITAL_Data_Dictionary.pdf")

##### --
#####*


##### Download the zip to core-based statistical area                                                                   #####
#####*
### This File was downloaded manually at https://www.huduser.gov/apps/public/uspscrosswalk/home
### a profile needed to be created but the download was free and accessible
### The data set for the third quarter of 2023 was the on downloaded and used in this project
### https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
### --
###

##### --
#####*


##### save the data                                                                                                     #####
#####*

### save our list
save(hospital_data, file = "data/raw/hospital_data_list.RData")

### clean our environment
rm(list = str_remove_all(ls(), "LoadData|CleanRawDataAndSave"))

##### --
#####*


beep("mario")
print("End of: LoadData.R")