############################################################################################################################*
############################################################################################################################*
##### File Title                                                                                                        #####
##                              This file loads the many hospital level data files from                                    ##
##                                    the APIs made available at data.csm.gov                                              ##
##                                                                                                                         ##
##                                                                                                                         ##
############################################################################################################################*


##### Load the necessary packages                                                                                       #####
#####*

library(tidyverse)
library(httr2)

##### --
#####*

req <- request("https://data.cms.gov/provider-data/api/1/datastore/query/nrdb-3fcy") %>%
  req_perform() %>%
  resp_body_json() %>%
  as_tibble()