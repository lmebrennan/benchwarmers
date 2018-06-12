## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

### NOTES:
### * Global.R files run first
### * This file contains general set up, data cleaning, global functions
### * To make regions all the same length, CTRL+SHIFT+R then before the region name, paste the following
###   '# ----------------------------------------------------'


# ---------------------------------------------------- 1. SET UP R PACKAGES --------------------------------------------------------

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

#make sure civis is intalled
devtools::install_github("civisanalytics/civis-r")

#load each package into r library
inst = lapply(packages,require,character.only=TRUE)



# ---------------------------------------------------- 2. SET UP WORKSPACE -----------------------------------------------------

# load all other files in the project
source('header.R')
source('sidebar.R')
source('body.R')


# ---------------------------------------------------- 3. GET DATA -------------------------------------------------------------


# ---------------------------------------------------- _ a. set up civis api key  ----------------------------------------------

Sys.setenv(CIVIS_API_KEY='6156c21bdeeb714a16511f176b896d568787abf51deb1bd1165d1ee3685f1c2d')


# ---------------------------------------------------- _ b. load data ----------------------------------------------------------

#####-NOTE: THE FOLLOWING CODE WAS USED TO CREATE THE FULL_CHILDREN TABLE IN CIVIS
######-

# load all customer data from S drive
if(!exists("raw_customer1")){
  raw_customer1<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer1.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer3")){
  raw_customer3<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer3.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer4")){
  raw_customer4<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer4.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer5")){
  raw_customer5<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer5.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer6")){
  raw_customer6<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer6.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer7")){
  raw_customer7<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer7.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer9")){
  raw_customer9<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer9.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer11")){
  raw_customer11<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer11.csv",fileEncoding="UTF-8-BOM")
}
if(!exists("raw_customer12")){
  raw_customer12<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer12.csv",fileEncoding="UTF-8-BOM")
}

# get hospital data from civis
if(!(exists("raw_hospital_df"))){
  raw_hospital_df <- read_civis("public.hospital_info",database="Strata Decision Technologies")
}


# ---------------------------------------------------- _ c. create full df ------------------------------------------------------

# maintain raw hospital df, and raw customer dfs untouched - we are going to manipulate the data a lot
hospital_df<-raw_hospital_df

# combine all customer data into an encounter df
dfs_customers <- grep("raw_customer", ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], value = TRUE)
encounter_df <- do.call("bind_rows",mget(dfs_customers))

# create a df to hold all information, merging hospital and customer info
full_df <- merge(x=encounter_df,
                   y=raw_hospital_df,
                   by.x = c("CustomerID","EntityID"),
                   by.y = c("customerid","entityid_fixed"))


# ---------------------------------------------------- _ d. edit full df -------------------------------------------------------

# remove all data that does not have APRDRGCODE = 225 (Appendectomy)
full_df <- full_df[full_df$APRDRGCODE=="225",]

# function to use to manipulate UBRevCode column
getUBRevCode_Short <- function(ubrevcodefull){
  unlist(strsplit(ubrevcodefull," - "))[1]
}

# add a column for just ub rev code (and not description)
full_df$UBRevCode_Short <-  sapply(full_df$UBRevCode, getUBRevCode_Short)

# list of groupings for grouping costs
calc_groups <- c("region", "beds", "specialty", #benchmark parameters
                 "CustomerID", "entityid", "customer_entity", "isstratastandardcost",  # hospital parameters
                 "EncounterID", "ROM", "SOI", "agebucket", "PatientTypeRollup", "DischargeStatusGroup", "CostDriver", "HospitalAcqCondition",  # benchmark breakdowns
                 "LengthOfStay", "imaging_ubrev_group","los_ubrev_group", "UBRevCode")

# function to caclulate Sums info
groupingCalcs <- function(df, grouping_vars){
  out <- df %>%
    group_by(.dots = grouping_vars) %>%
    summarise(FixedDirectCost = sum(FixedDirectCost, na.rm = TRUE),
              FixedIndirectCost = sum(FixedIndirectCost, na.rm = TRUE),
              VariableDirectCost = sum(VariableDirectCost, na.rm = TRUE),
              VariableIndirectCost = sum(VariableIndirectCost, na.rm = TRUE)
    ) %>% ungroup() %>%
    gather("CostKey", "Costs", (ncol(.)-3):ncol(.)) %>%
    mutate(
      FixedVariable = case_when(
        CostKey == "VariableDirectCost" | CostKey == "VariableIndirectCost" ~ "Variable",
        CostKey == "FixedDirectCost" | CostKey == "FixedIndirectCost" ~ "Fixed"),
      DirectIndirect = case_when(
        CostKey == "VariableDirectCost" | CostKey == "FixedDirectCost" ~ "Direct",
        CostKey == "VariableIndirectCost" | CostKey == "FixedIndirectCost" ~ "Indirect")
    )
  return(out)
}

# ---------------------------------------------------- _ _ Edit Full_df columns ------------------------------------------------
full_df <- full_df %>%
  mutate(
    # _ _ * UPDATE: Patient Type Rollup ---------------------------------------
    PatientTypeRollup = case_when(
      grepl("in", tolower(PatientTypeRollup)) ~ "Inpatient",
      grepl("out", tolower(PatientTypeRollup)) ~ "Outpatient",
      PatientTypeRollup == "Emergency" ~ "Emergency",
      TRUE ~ as.character(NA)
    ),

    # _ _ * UPDATE: Discharge Status Group ------------------------------------
    DischargeStatusGroup = ifelse(is.na(DischargeStatusGroup), "Not Specified", DischargeStatusGroup),

    # _ _ * ADD: Age Bucket ---------------------------------------------------
    agebucket = case_when(
      Age < 1 ~ "Infant",
      Age >= 1 & Age < 2 ~ "Toddler",
      Age >= 2 & Age <= 5 ~ "Early Childhood",
      Age >= 6 & Age <= 11 ~ "Middle Childhood",
      Age >= 12 & Age <= 17 ~ "Adolescence",
      Age >= 18 ~ "Adult",
      TRUE ~ as.character(NA)
    ),

    # _ _ * ADD: Imaging UBRev Group ------------------------------------------
    imaging_ubrev_group = case_when(
      UBRevCode_Short=="0341" | UBRevCode_Short=="0343" | UBRevCode_Short=="0340" | UBRevCode_Short=="0342" | UBRevCode_Short=="0344" ~ "Nuclear Med",
      UBRevCode_Short=="0610" | UBRevCode_Short=="0615" | UBRevCode_Short=="0616" | UBRevCode_Short=="0618" | UBRevCode_Short=="0614" | UBRevCode_Short=="0612" | UBRevCode_Short=="0619" ~ "MRT",
      UBRevCode_Short=="0352" | UBRevCode_Short=="0359" | UBRevCode_Short=="0350" | UBRevCode_Short=="0351" ~ "CT",
      UBRevCode_Short=="0401" | UBRevCode_Short=="0400" | UBRevCode_Short=="0409" | UBRevCode_Short=="0404" | UBRevCode_Short=="0403" | UBRevCode_Short=="0402" ~ "Other Imaging",
      UBRevCode_Short=="0321" | UBRevCode_Short=="0323" | UBRevCode_Short=="0322" | UBRevCode_Short=="0324" | UBRevCode_Short=="0320" | UBRevCode_Short=="0329" ~ "Radiology",
      UBRevCode_Short=="0860" | UBRevCode_Short=="0861" ~ "MEG",
      UBRevCode_Short=="0740" ~ "EEG",
      TRUE ~ ""
    ),

    # _ _ * ADD: LOS UBRev Group ----------------------------------------------
    los_ubrev_group = case_when(
      UBRevCode_Short=="0101" ~ "Room and Board",
      UBRevCode_Short>="0110" & UBRevCode_Short<="0114" ~ "Room and Board",
      UBRevCode_Short>="0116" & UBRevCode_Short<="0124" ~ "Room and Board",
      UBRevCode_Short>="0126" & UBRevCode_Short<="0134" ~ "Room and Board",
      UBRevCode_Short>="0136" & UBRevCode_Short<="0144" ~ "Room and Board",
      UBRevCode_Short>="0146" & UBRevCode_Short<="0154" ~ "Room and Board",
      UBRevCode_Short>="0156" & UBRevCode_Short<="0159" ~ "Room and Board",
      UBRevCode_Short=="0164" ~ "Room and Board",
      UBRevCode_Short>="0170" & UBRevCode_Short<="0174" ~ "Nursery",
      UBRevCode_Short=="0719" ~ "Nursery",
      UBRevCode_Short>="0200" & UBRevCode_Short<="0204" ~ "ICU",
      UBRevCode_Short>="0206" & UBRevCode_Short<="0209" ~ "ICU",
      UBRevCode_Short>="0210" & UBRevCode_Short<="0212" ~ "CCU",
      UBRevCode_Short=="0214" ~ "CCU",
      UBRevCode_Short=="0219" ~ "CCU",
      TRUE ~ ""
    )
  ) %>%
  groupingCalcs(grouping_vars=calc_groups)

# ---------------------------------------------------- 3. LOAD FUNCTIONS ----------------------------------------------------------

# ---------------------------------------------------- _ General Functions --------------------------------------------------------

# function to calculate summary info
calcSummary <- function(df, summary_var, outlier_threshold = 2, grouping_vars = NULL){
  summary_var <- parse_quosure(summary_var)
  if(is.null(grouping_vars)){
    out <- df %>%
      summarise(min = round(summary(!!summary_var, na.rm = TRUE)[[1]], 2),
                q1 = round(summary(!!summary_var, na.rm = TRUE)[[2]], 2),
                median = round(summary(!!summary_var, na.rm = TRUE)[[3]], 2),
                mean = round(summary(!!summary_var, na.rm = TRUE)[[4]], 2),
                q3 = round(summary(!!summary_var, na.rm = TRUE)[[5]], 2),
                max = round(summary(!!summary_var, na.rm = TRUE)[[6]], 2),
                sd = round(sd(!!summary_var, na.rm = TRUE), 2),
                total = round(sum(!!summary_var, na.rm = TRUE), 2),
                obs = n()) %>%
      mutate(IQR = round(q3 - q1, 2),
             IQR_outlier_high = round(q3 + (1.5*IQR), 2),
             IQR_outlier_low = round(q1 - (1.5*IQR), 2),
             sd_outlier_high = round(mean + (outlier_threshold*sd), 2),
             sd_outlier_low = round(mean - (outlier_threshold*sd), 2))
  } else {
    out <- df %>%
      group_by(.dots = grouping_vars) %>%
      summarise(min = round(summary(!!summary_var, na.rm = TRUE)[[1]], 2),
                q1 = round(summary(!!summary_var, na.rm = TRUE)[[2]], 2),
                median = round(summary(!!summary_var, na.rm = TRUE)[[3]], 2),
                mean = round(summary(!!summary_var, na.rm = TRUE)[[4]], 2),
                q3 = round(summary(!!summary_var, na.rm = TRUE)[[5]], 2),
                max = round(summary(!!summary_var, na.rm = TRUE)[[6]], 2),
                sd = round(sd(!!summary_var, na.rm = TRUE), 2),
                total = round(sum(!!summary_var, na.rm = TRUE), 2),
                obs = n()) %>% ungroup() %>%
      mutate(IQR = round(q3 - q1, 2),
             IQR_outlier_high = round(q3 + (1.5*IQR), 2),
             IQR_outlier_low = round(q1 - (1.5*IQR), 2),
             sd_outlier_high = round(mean + (outlier_threshold*sd), 2),
             sd_outlier_low = round(mean - (outlier_threshold*sd), 2))
  }
  return(out)
}


# ---------------------------------------------------- _ Graphing Functions ----------------------------------------------------

## Function to add facet to a plot ##
addFacet <- function(plot, facet_formula) {
  plot +
    facet_grid(facet_formula)
}

## Function to adjust plot scales ##
setY_AxisScale <- function(plot, logscale = FALSE){
  if(logscale){
    plot +
      scale_y_log10(name = "Cost per Encounter\n($)",
                    labels = scales::dollar)
  }
  else {
    plot +
      scale_y_continuous(name = "Cost per Encounter\n($)",
                         labels = scales::dollar)
  }

  return(plot)
}

# ---------------------------------------------------- _ Imaging Cost Driver Functions -----------------------------------------


# ---------------------------------------------------- _ LOS Cost Driver Functions ---------------------------------------------


# ---------------------------------------------------- _ Pharmacy Cost Driver Functions ----------------------------------------









