#### SCRIPT TO MUNGE DATA FOR SHINY APP ####
## This script will read in the newest data pulls and calculate initial benchmarks
## February 13, 2018

## --------------------------< Set Up Workspace >--------------------------

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


## load helper functions
source("/Users/cwang/Desktop/Strata/StrataPIlotPrototype/StrataFunctions.R")


## --------------------------< Read in Data >--------------------------

setwd("~/Desktop/Strata/StrataData")

customer1 <- readStrataFile("Customer1.csv")
customer3 <- readStrataFile("Customer3.csv")
customer4 <- readStrataFile("Customer4.csv")
customer5 <- readStrataFile("Customer5.csv")
customer6 <- readStrataFile("Customer6.csv")
customer7 <- readStrataFile("Customer7.csv")
customer9 <- readStrataFile("Customer9.CSV")
customer11 <- readStrataFile("Customer11.CSV")
customer12 <- readStrataFile("Customer12.CSV")

hospital_info <- read_csv("hospital_data.csv", col_types = cols(.default = col_character(), "Beds" = col_double())) %>%
  mutate(EntityID_fixed = case_when(
    CustomerID == "1" & EntityID == "3" ~ "8",
    CustomerID == "4" & EntityID == "13" ~ "26",
    TRUE ~ as.character(EntityID)),
    Beds_fixed = ifelse(Beds >= 200, "200+", "less than 200"),
    Specialty = "Pediatric",
    customer_entity = paste0("Customer ", CustomerID, ", Entity ", EntityID_fixed)
  )


## change working directoryy
setwd("~/Desktop/Strata/PIlotPrototype/")

options(civis.default_db = "Strata Decision Technologies")

## --------------------------< Join Data >--------------------------

## grab all customer tables
dfs_customers <- grep("customer", ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], value = TRUE)


calc_groups <- c("Region", "Beds_fixed", "Specialty",     # benchmark parameters
                 "CustomerID", "EntityID", "APRDRGCODE", "customer_entity", "IsStrataStandardCost", # hospital parameters
                 "EncounterID", "ROM", "SOI", "AgeBucket", "PatientTypeRollup", "DischargeStatusGroup", "CostDriver", "HospitalAcqCondition",  # benchmark breakdowns
                 "LengthOfStay")  # extra, necessary columns


## create full table for Shiny app
full <- do.call("bind_rows", mget(dfs_customers)) %>%  # con
  left_join(hospital_info, by = c("CustomerID" = "CustomerID", "EntityID" = "EntityID_fixed")) %>%
  filter(!is.na(APRDRGCODE)) %>%
  # group ages into buckets
  mutate(AgeBucket = case_when(
    Age < 1 ~ "Infant",
    Age >= 1 & Age < 2 ~ "Toddler",
    Age >= 2 & Age <= 5 ~ "Early Childhood",
    Age >= 6 & Age <= 11 ~ "Middle Childhood",
    Age >= 12 & Age <= 17 ~ "Adolescence",
    Age >= 18 ~ "Adult",
    TRUE ~ as.character(NA)),
    # clean patient type roll up
    PatientTypeRollup = case_when(
      grepl("in", tolower(PatientTypeRollup)) ~ "Inpatient",
      grepl("out", tolower(PatientTypeRollup)) ~ "Outpatient",
      PatientTypeRollup == "Emergency" ~ "Emergency",
      TRUE ~ as.character(NA)
    ),
    # replace NAs in DischargeStatusGroup with "Not Specified"
    DischargeStatusGroup = ifelse(is.na(DischargeStatusGroup), "Not Specified", DischargeStatusGroup),
    customer_entity = paste0("Customer ", CustomerID, ", ", "Entity ", EntityID)) %>% 
  groupingCalcs(grouping_vars = calc_groups) 


# # # write dataframes to CSV
 write.csv(hospital_info, file = "hospitals.csv")
 write.csv(full, file = "full.csv")
# 
# # write dataframes to S3 buckets
 write_civis_file(x = full, name = "strata_full")  # 10051504
 write_civis_file(x = hospital_info, name = "strata_hospitals")  # 10051505
# 
# # grant permissions on files to Strata robot platform account
 files_put_shares_users(id = 10051504, user_ids = 3896, permission_level = "read")
 files_put_shares_users(id = 10051505, user_ids = 3896, permission_level = "read")


## Write Cleaned Files to Strata Decision Technologies Cluster
 full <- read_civis(x = 10051504)
 write_civis(x = full, tablename = "public.full", distkey = "APRDRGCODE")
# 
 hospital_info <- read_civis(x = 10051505)
 write_civis(x = hospital_info, tablename = "public.hospital_info", distkey = "customer_entity")