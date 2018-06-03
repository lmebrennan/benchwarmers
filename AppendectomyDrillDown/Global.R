## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

### NOTE: Global.R files run first
### This file contains general set up, data cleaning, global functions


# SET UP ------------------------------------------------------------------

#all r packages needed
packages<-c('rlang',
            'devtools',
            'shiny',
            'shinythemes',
            'shinyWidgets',
            'shinyTree',
            'stringr',
            'cowplot',
            'DT',
            'data.table',
            'ggplot2',
            'civis',
            'dplyr',
            'tidyr',
            'readr',
            'httr',
            'shinydashboard',
            'plotly')

#install each package
install.packages(packages,repos = 'https://cran.rstudio.com/')

devtools::install_github("civisanalytics/civis-r")

#load each package into r library
inst = lapply(packages,require,character.only=TRUE)


# DATA  ---------------------------------------------------------------

#load data from civis
Sys.setenv(CIVIS_API_KEY='6156c21bdeeb714a16511f176b896d568787abf51deb1bd1165d1ee3685f1c2d')

#encounter data from customers(hospitals)
if(!(exists("customer1"))){
  customer1<-read_civis("public.customer1_adult1",database="Strata Decision Technologies")
}
if(!(exists("customer2"))){
  customer2<-read_civis("public.customer2_adult",database="Strata Decision Technologies")
}
if(!(exists("customer3"))){
  customer3<-read_civis("public.customer3_adult",database="Strata Decision Technologies")
}
if(!(exists("customer4"))){
  customer4<-read_civis("public.customer4_adult",database="Strata Decision Technologies")
}
if(!(exists("customer6"))){
  customer6<-read_civis("public.customer6_adult",database="Strata Decision Technologies")
}
if(!(exists("customer7"))){
  customer7<-read_civis("public.customer7_adult",database="Strata Decision Technologies")
}
if(!(exists("customer8"))){
  customer8<-read_civis("public.customer8_adult",database="Strata Decision Technologies")
}
if(!(exists("customer9"))){
customer9<-read_civis("public.customer9_adult",database="Strata Decision Technologies")
}
if(!(exists("customer10"))){
  customer10<-read_civis("public.customer10_adult",database="Strata Decision Technologies")
}

#hospital data
if(!(exists("hospital_df"))){
  hospital_df <- read_civis("public.hospital_info_adult",database="Strata Decision Technologies")
}


#combine all customer data to create encounter dataframe
encounter_df<-rbind(customer1,customer2,customer3,customer4,customer6,customer7,customer8,customer9,customer10)

#add customer_entity field to hospital dataframe
hospital_df$customer_entity<-paste0("Customer ", hospital_df$customerid, ", ", "Entity ", hospital_df$entityid)


#build full data table to use in application

#list columns to include in df
# columns<-c('region',
#            'beds',
#            'specialty',
#            'customerid',
#            'entityid',
#            'MSDRGGROUP',
#            'customer_entity',
#            'isstratastandardcost',
#            'encounterid'
#            )

#build dataframe
full_df<-left_join(encounter_df,hospital_df,by=c('customerid'='customerid','entityid'='entityid')) %>%
  filter(!is.na(msdrgcode)) %>%
  mutate(
    customer_entity = paste0("Customer ", customerid, ", ", "Entity ", entityid)
  )

#save as dataframe
save(encounter_df,file='encounter_df.Rdata')
save(hospital_df,file='hospital_df.Rdata')
save(full_df,file='full_df.Rdata')


# FUNCTIONS ---------------------------------------------------------------

#Function to label APR-DRG codes
labelMSDRG <- function(codes, values = FALSE){
  labelled_codes <- c("Coronary Bypass (CABG)" = "CABG",
                      "Acute Myocardial Infarction (AMI)" = "AMI",
                      "Major Joint Replacement Or Reattachment Of Lower Extremity (THA TKA)" = "THA TKA",
                      "Hip and Femur Procedures (Hip Fracture)" = "Hip Fracture")
  x <- match(codes, labelled_codes)
  if(values == FALSE){
    return(labelled_codes[x])
  }
  else if(values == TRUE){
    return(names(labelled_codes[x]))
  }
}



# LOAD FILES --------------------------------------------------------------
source('header.R')
source('sidebar.R')
source('body.R')




