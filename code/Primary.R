############################################################################################################################*
############################################################################################################################*
##### File Title                                                                                                        #####
##                              The Primary File will serve as a script to manage settings,                                ##
##                                determine which of the other scripts need to be ran                                      ##
##                                and which don't, and any other over-arching decisions                                    ##
##                                  regarding the running of code for this analysis                                        ##
############################################################################################################################*

##### Set the working directory                                                                                         #####
#####*

### Sharing this project might mean working directories will differ from one machine to another
### the code below allows a user to set the working directory to the folder that the project is
### located in on their machine, allowing all other code to run smoothly and all other folder
### references in the code to be accurate relative to the machine it is running on.
setwd("C:/Users/Owner/Desktop/Portfolio_Of_Projects/InlandEmpireHospitalPerformance")

##### --
#####*


##### Load the necessary packages                                                                                       #####
#####*

### This will allow the user to see all necessary packages required to run all the code in one place,
### making it easier to install necessary dependencies to run the code. situations where functions
### from one package are preferred over another loaded package can be handled within the individual
### files (e.g. adding packagename:: before the individual function call if there is a redundancy)
library(tidyverse)
library(stringr)
library(janitor)
library(httr2)
library(skimr)
library(beepr)

##### --
#####*


##### Set which files need to run                                                                                       #####
#####*

### This section allows a user to set which code files need to be run.
### For example, loading the data repeatedly is not necessary. Once that
### code has been run and the data is saved to the machine it need not be
### run again, even though it is included in this Primary.R file. For
### files that should *NOT* be run again, set them to 0 and for files that
### *DO* need run, set them to 1. If this is your first time running any thing
### from this project, it is recommended that you set all the files below to
### 1. They are divided up into general groups to make it easier to navigate
### which files need to run again.

### loading, cleaning, and preparing the data for analysis
LoadData <- 1
CleanRawDataAndSave <- 1

##### --
#####*


##### Run the files                                                                                                     #####
#####*

### The code below runs the code in the other file of this project.
### They are divided up into general groups to make it easier to
### navigate which files will run in what order.

### loading, cleaning, and preparing the data for analysis
if (LoadData == 1) {
  source("code/LoadAndCleanData/LoadData.R")
}

if (CleanRawDataAndSave == 1) {
  source("code/LoadAndCleanData/CleanRawDataAndSave.R")
}

##### --
#####*


##### Clean the environment                                                                                             #####
#####*
rm(list = ls())

##### --
#####*
