############################################################################################################################*
################################################ file title###################################################################
##                                                                                                                         ##
##                              This file conducts Exploratory Data Analysis of the data                                   ##
##                                  prepared for hypothesis testing on the performance                                     ##
##                                  of hospitals in infections from care, communication                                    ##
##                                  from hosptial staff, and ER volume in California and the Inland Empire                 ##
############################################################################################################################*


##### Load the data                                                                                                     #####
#####*

hypothesis_data <- read_csv("data/final/hospitalQualityHypothesisTesting/hypothesis_data.csv")
measures_table <- read_csv("data/final/hospitalQualityHypothesisTesting/measures_table.csv")

##### --
#####*


##### Custom functions                                                                                                  #####
#####*

expected_frequencies <- function(x, variable) {
  x %>%
    select(inland_empire, all_of({{ variable }})) %>%
    drop_na() %>%
    table() %>%
    ExpFreq("abs")
}

##### --
#####*


##### overview the data                                                                                                 #####
#####*

measures_table

skim(hypothesis_data)
glimpse(hypothesis_data)

##### --
#####*


##### exploratory graphs                                                                                                #####
#####*

hypothesis_data %>%
  select(HAI_1_SIR:HAI_6_SIR, inland_empire) %>%
  pivot_longer(-inland_empire, names_to = "name", values_to = "value") %>%
  drop_na() %>%
  ggplot(aes(x = name, fill = value)) +
  geom_bar(position = "dodge") +
  facet_wrap(~inland_empire)

hypothesis_data %>%
  select(H_COMP_1_STAR_RATING:H_COMP_3_STAR_RATING, inland_empire) %>%
  pivot_longer(-inland_empire, names_to = "name", values_to = "value") %>%
  drop_na() %>%
  ggplot(aes(x = name, fill = value)) +
  geom_bar(position = "dodge") +
  facet_wrap(~inland_empire)

### with a different inland_empire criteria
hypothesis_data %>%
  mutate(
    inland_empire = if_else(cbsa_title == "Riverside-San Bernardino-Ontario, CA", "yes", "no"),
    inland_empire = replace_na(inland_empire, "no")
  ) %>%
  select(HAI_1_SIR:HAI_6_SIR, inland_empire) %>%
  pivot_longer(-inland_empire, names_to = "name", values_to = "value") %>%
  drop_na() %>%
  ggplot(aes(x = name, fill = value)) +
  geom_bar(position = "dodge") +
  facet_wrap(~inland_empire)

hypothesis_data %>%
  mutate(
    inland_empire = if_else(cbsa_title == "Riverside-San Bernardino-Ontario, CA", "yes", "no"),
    inland_empire = replace_na(inland_empire, "no")
  ) %>%
  select(H_COMP_1_STAR_RATING:H_COMP_3_STAR_RATING, inland_empire) %>%
  pivot_longer(-inland_empire, names_to = "name", values_to = "value") %>%
  drop_na() %>%
  ggplot(aes(x = name, fill = value)) +
  geom_bar(position = "dodge") +
  facet_wrap(~inland_empire)

##### --
#####*


##### tables and expected frequencies                                                                                   #####
#####*

exp_columns <- names(hypothesis_data)[2:11]

map(exp_columns, ~ hypothesis_data %>%
  select(all_of(c("inland_empire", .x))) %>%
  table())

map(exp_columns, ~ hypothesis_data %>%
  expected_frequencies(.x))


##### --
#####*

rm(list = str_remove_all(ls(), "LoadData|CleanRawDataAndSave|HospitalQualityHypothesisTestingDataPrep|
                         HospitalQualityHypothesisTestingEDA"))
