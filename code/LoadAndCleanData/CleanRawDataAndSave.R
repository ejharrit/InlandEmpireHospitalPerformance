############################################################################################################################*
################################################file title###################################################################
##                                                                                                                         ##
##                              This file loads the data we pulled from the Data.CMS.gov                                   ##
##                                  API, cleans it and saves the new data format                                           ##
##                                                                                                                         ##
##                                                                                                                         ##
############################################################################################################################*

##### Notes                                                                                                             #####
### Certain things to take note of regarding cleaning the data. This is not preparing the data for any specific
### analysis. Rather, this code is meant to tidy the data so that it is in a usable format for any analysis, making
### sure that every row refers to one record, every column measures one variable and all variables refer to the same 
### unit of observation, in this case, medical facility.
###
### One common issue across all datasets is that the data with NA or missing values are not being read in that way.
### They are often coded as "Not Available" which causes R to read them in as part of a character vector. Since "Not
### Available" means the data is not available or missing, I recode these as NA.
### 
### Another common issue with our data sets that I fix in this code is that I have "long" data sets where some columns
### show measurements for more than one variable. I use pivot_wider on these data sets to make them wider and make sure 
### every column is only measuring one variable.
###
### There also seem to be many duplicates in each dataframe. It's reach the point that when I clean every dataframe the 
### first thing I do is run it through the distinct() function to get only unique rows. This almost always results in a 
### significant decrease in the number of rows in the data.

##### Load the data                                                                                                     #####
#####*

load("data/raw/hospital_data_list.RData")

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
  mutate(across(matches("rate|cases|limit|sample"), \(x) as.numeric(x)))

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
  pivot_wider(names_from = measure_id, values_from = rate)

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
  pivot_wider(names_from = measure_id, 
              values_from = c(compared_to_national, denominator, score, lower_estimate, higher_estimate)) %>%
  mutate(across(matches("denominator|score|estimate"), \(x) as.numeric(x)))

##### --
#####*


##### general_information data set                                                                                      #####
#####*

glimpse(general_information)

general_information <- general_information %>%
  distinct()%>%
  mutate(across(matches("count_|_count"), \(x) as.numeric(x)),
         hospital_overall_rating = as.numeric(hospital_overall_rating))

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
  pivot_wider(
    names_from = measure_id,
    values_from = c(compared_to_national, score)
  ) %>%
  mutate(across(matches("score"), \(x) as.numeric(x)))

##### --
#####*


##### hvbp_clinical_outcomes_domain dataset                                                                             #####
#####*

glimpse(hvbp_clinical_outcomes_domain)

hvbp_clinical_outcomes_domain <- hvbp_clinical_outcomes_domain %>%
  distinct() %>%
  ### A tricky part here. All the columns that are scores on a scale (e.g. "5 out of 9", "4 out of 10") we convert
  ### to two columns. One column to keep track of the score received, and the other to keep track of the scale 
  mutate(across(matches("points|score"), ~ tibble(col1 = .) %>% 
                             separate(col1, into = str_c(cur_column(), c("", "_base")), sep = " ", extra = "merge"))) %>% 
  unnest(cols = matches("points|score")) %>%
  mutate(across((matches("points|score|threshold|benchmark|rate")&!matches("_base$")), \(x) as.numeric(x)))

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
  print(width=Inf)

##### --
#####*


##### chunk name here                                                                                                   #####
#####*

%>%
  print(width=Inf)

##### --
#####*


