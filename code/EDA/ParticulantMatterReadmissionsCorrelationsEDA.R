############################################################################################################################*
################################################ file title###################################################################
##                                                                                                                         ##
##                              This file performes an Exploratory Analysis of the data                                    ##
##                                for measuring correlation through linear regressions                                     ##
##                                of hospital readdmission data and EPA measures by county                                 ##
##                                                                                                                         ##
############################################################################################################################*


##### Load the data                                                                                                     #####
#####*

pm_readmissions_regression <- read_csv("data/final/particulantMatterReadmissionsCorrelation/pm_readmissions_regression.csv")
measures_table <- read_csv("data/final/particulantMatterReadmissionsCorrelation/measures_table.csv")

##### --
#####*


##### glimpse and skim the data                                                                                         #####
#####*

measures_table
glimpse(pm_readmissions_regression)
skim(pm_readmissions_regression)

##### --
#####*


##### summary statistics                                                                                                #####
#####*

summary(pm_readmissions_regression)

##### --
#####*


##### histograms of the variables                                                                                       #####
#####*

histograms <- map(
  names(
    pm_readmissions_regression %>%
      select(where(is.numeric))
  ),
  ~ ggplot(pm_readmissions_regression, aes(x = !!as.symbol(.x))) +
    geom_histogram() +
    geom_density()
)

names(histograms) <- names(
  pm_readmissions_regression %>%
    select(where(is.numeric))
)

### most of the variables follow patterns relatively close to normal distributions. Two should be transformed to better
### fit a normal distribution
### taking the square root of carbon monoxide greatly reduces the skewness and brings the data a little closer to a normal
### distribution
pm_readmissions_regression %>%
  mutate(carbon_monoxide = carbon_monoxide^2) %>%
  ggplot(aes(x = carbon_monoxide)) +
  geom_histogram() +
  geom_density()

skewness(pm_readmissions_regression$carbon_monoxide, na.rm = TRUE)
kurtosis(pm_readmissions_regression$carbon_monoxide, na.rm = TRUE)

skewness(sqrt(pm_readmissions_regression$carbon_monoxide), na.rm = TRUE)
kurtosis(sqrt(pm_readmissions_regression$carbon_monoxide), na.rm = TRUE)

### Taking the log of the lead measure greatly reduces our skewness and kurtosis, bringing both closer to reflecting a normal
### distribution
pm_readmissions_regression %>%
  mutate(lead_pm2_5_lc = log(lead_pm2_5_lc)) %>%
  ggplot(aes(x = lead_pm2_5_lc)) +
  geom_histogram() +
  geom_density()

skewness(pm_readmissions_regression$lead_pm2_5_lc, na.rm = TRUE)
kurtosis(pm_readmissions_regression$lead_pm2_5_lc, na.rm = TRUE)

skewness(log(pm_readmissions_regression$lead_pm2_5_lc), na.rm = TRUE)
kurtosis(log(pm_readmissions_regression$lead_pm2_5_lc), na.rm = TRUE)

##### --
#####*


##### clean our environment                                                                                             #####
#####*

rm(list = str_remove_all(ls(), "LoadData|CleanRawDataAndSave|HospitalQualityHypothesisTestingDataPrep"))

##### --
#####*


beep("sword")
print("End of: ParticulantMatterReadmissionsCorrelationsEDA.R")
