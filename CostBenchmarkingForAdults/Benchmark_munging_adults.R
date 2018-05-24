#### SCRIPT TO MUNGE DATA FOR SHINY APP ####
## This script will read in the newest data pulls and calculate initial benchmarks
## February 13, 2018

## --------------------------< Set Up Workspace >--------------------------
setwd("S:/Users/sxie/Adult Rshiny App")

rm(list = ls())

library(tidyverse)
library(dplyr)
library(readr)
library(cowplot)
library(ggplot2)
#library(lazyeval)
library(rlang)
library(civis)
#library(civis.deckR)

## --------------------------< Connect to Civis Platform >------------------
install.packages('civis')
library (civis)

devtools::install_github("civisanalytics/civis-r")

## good way to check that the API key is installed properly:
civis::users_list_me()

## -----------< Load Helper Functions and Data >-----------
source("S:/Users/sxie/Adult Rshiny App/StrataFunctions_adults.R")


## --------------------------< Read in Data >--------------------------
customer1 <- read_civis("customer1_adult1", database="Strata Decision Technologies")
customer2 <- read_civis("customer2_adult", database="Strata Decision Technologies")
customer3 <- read_civis("customer3_adult", database="Strata Decision Technologies")
customer4 <- read_civis("customer4_adult", database="Strata Decision Technologies")
customer6 <- read_civis("customer6_adult", database="Strata Decision Technologies")
customer7 <- read_civis("customer7_adult", database="Strata Decision Technologies")
customer8 <- read_civis("customer8_adult", database="Strata Decision Technologies")
customer9 <- read_civis("customer9_adult", database="Strata Decision Technologies")
customer10 <- read_civis("customer10_adult",database="Strata Decision Technologies")

# for customer 1, delete rows where patienttyperollup="Not Specified"; also include "Reoccuring outpaient" to "outpatient" group.
customer1<- subset(customer1, patienttyperollup!="Not Specified")
customer1$patienttyperollup[customer1$patienttyperollup=="Recurring Outpatient"]<-"Outpatient"
customer1$patienttyperollup<-factor(customer1$patienttyperollup)

## grab all customer tables
dfs_customers <- grep("customer", ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], value = TRUE)
encounter_data<- do.call("bind_rows", mget(dfs_customers))

hospital_info <- read_civis("hospital_info_adult",database="Strata Decision Technologies")
hospital_info$customer_entity<-paste0("Customer ", hospital_info$customerid, ", ", "Entity ", hospital_info$entityid)
                          
## --------------------------< Join Data >--------------------------

## grab all customer tables
calc_groups <- c("region", "beds", "specialty",     # benchmark parameters
                 "customerid", "entityid", "MSDRGGROUP", "customer_entity", "isstratastandardcost",  # hospital parameters
                 "encounterid", "rom", "soi", "AgeBucket", "patienttyperollup", "dischargestatusgroup", "costdriver", "hospitalacqcondition",  # benchmark breakdowns
                 "lengthofstay")

## create full table for Shiny app
full <- left_join(encounter_data, hospital_info, by = c("customerid" = "customerid", "entityid" = "entityid")) %>%
  filter(!is.na(msdrgcode)) %>%
  # group ages into buckets
  mutate(AgeBucket = case_when(
    age < 1 ~ "Infant",
    age >= 1 & age < 18 ~ "Pediatric",
    age >= 18 & age < 65 ~ "Adult",
    age >= 65 ~ "Senior",
    TRUE ~ as.character(NA)),
    
    MSDRGGROUP= case_when(
      msdrgcode>= 231 & msdrgcode<= 236 ~ "CABG",
      msdrgcode>= 280 & msdrgcode<= 282 ~ "AMI",
      msdrgcode>= 469 & msdrgcode<= 470 ~ "THA TKA",
      msdrgcode>= 480 & msdrgcode<= 482 ~ "Hip Fracture",
      TRUE ~ as.character(NA)
    ),
    
    # replace NAs in DischargeStatusGroup with "Not Specified"
    dischargestatusgroup = ifelse(is.na(dischargestatusgroup), "Not Specified", dischargestatusgroup),
    customer_entity = paste0("Customer ", customerid, ", ", "Entity ", entityid))%>% 
    groupingCalcs(grouping_vars = calc_groups)

# for ROM and SOI, exclude cases with 0  
full<-filter(full,rom>0 & soi>0)


# save as R dataframe
save(encounter_data,file="encounter_data_adult.Rdata")
save(full,file="full_adult.Rdata")

# write dataframe to Civis platform 
write_civis(full,"public.full_adult", database = "Strata Decision Technologies")
