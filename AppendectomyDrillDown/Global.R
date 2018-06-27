## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

### NOTES:
### * Global.R files run first
### * This file contains general set up, data cleaning, global functions
### * To make regions all the same length, CTRL+SHIFT+R then before the region name, paste the following
###   '# ----------------------------------------------------'

###################################################################################################################################-
##### 1. SET UP R WORKSPACE --------------------------------------------------------------------------------------------------------
###################################################################################################################################-

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

# preferences in r environment
options(warn = -1) # suppress warnings in console
Sys.setenv(CIVIS_API_KEY='6156c21bdeeb714a16511f176b896d568787abf51deb1bd1165d1ee3685f1c2d')

# load all other files in the project
source('header.R')
source('sidebar.R')
source('body.R')

###############################################################################################################################-
##### 2. GET DATA --------------------------------------------------------------------------------------------------------------
###############################################################################################################################-


###############################################################################################################################-
##### _ a. load data -----------------------------------------------------------------------------------------------------------
#####-
#####-NOTE: THE FOLLOWING COMMENTED OUT CODE WAS USED TO CREATE THE ENCOUNTER_CHILDREN TABLE IN CIVIS
#####------- KEEPING THIS CODE HERE FOR RECORD KEEPING, BUT NOT NECESSARY ANYMORE
###############################################################################################################################-

    # # load all customer data from S drive
    # if(!exists("raw_customer1")){
    #   raw_customer1<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer1.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer3")){
    #   raw_customer3<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer3.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer4")){
    #   raw_customer4<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer4.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer5")){
    #   raw_customer5<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer5.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer6")){
    #   raw_customer6<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer6.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer7")){
    #   raw_customer7<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer7.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer9")){
    #   raw_customer9<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer9.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer11")){
    #   raw_customer11<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer11.csv",fileEncoding="UTF-8-BOM")
    # }
    # if(!exists("raw_customer12")){
    #   raw_customer12<-read.csv("S:/Product Management/1 - Roadmap Projects/1 - Cross-Solution Projects/Data Services/Civis Project/Customer12.csv",fileEncoding="UTF-8-BOM")
    # }
    #
    # # combine all customer data into an encounter df
    # dfs_customers <- grep("raw_customer", ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)], value = TRUE)
    # encounter_df <- do.call("bind_rows",mget(dfs_customers))
    #
    # # save raw encounter df to civis
    # write_civis(encounter_df,"public.encounter_children",database="Strata Decision Technologies")

###############################################################################################################################-
##### Load data from Civis, then merge into full_df
###############################################################################################################################-

# get ENTITY data from civis
if(!(exists("hospital_info"))){
  hospital_info <- read_civis("public.hospital_info",database="Strata Decision Technologies")
}
# get ENCOUNTER data from civis
if(!(exists("raw_encounter_df"))){
  raw_encounter_df <- read_civis("public.encounter_children",database="Strata Decision Technologies")
}

# safe gaurd the data
entity_df<-hospital_info
encounter_df<-raw_encounter_df

# we can drop this column, because we are using entityid_fixed instead
entity_df$entityid<- NULL

# create a df to hold all information, merging hospital and customer info
full_df <- merge(x=encounter_df,
                   y=entity_df,
                   by.x = c("customerid","entityid"),
                   by.y = c("customerid","entityid_fixed"))

###############################################################################################################################-
###### _ b. edit full_df -------------------------------------------------------------------------------------------------------
###############################################################################################################################-

# remove all data that does not have APRDRGCODE = 225 (Appendectomy)
full_df <- full_df[full_df$aprdrgcode=="225",]

# function to use to manipulate UBRevCode column
getUBRevCode_Short <- function(ubrevcodefull){
  ubrevcodefull<-as.character(ubrevcodefull)
  unlist(strsplit(ubrevcodefull," - "))[1]
}

# add a column for just ub rev code (and not description)
full_df$ubrevcode_short <- sapply(full_df$ubrevcode, getUBRevCode_Short)

# list of groupings for grouping costs
calc_groups <- c("region", "beds", "specialty", "customerid", "entityid", "customer_entity", "isstratastandardcost",  # Entity Filters
                 "encounterid", "rom", "soi", "agebucket", "patienttyperollup", "dischargestatusgroup", "hospitalacqcondition", "costdriver", # Encounter Filters
                 "lengthofstay", "imaging_ubrev_group","los_ubrev_group", "ubrevcode") # Benchmarking Columns

# function to caclulate Sums info
groupingCalcs <- function(df, grouping_vars){
  out <- df %>%
    # group_by(.dots = grouping_vars) %>%
    # summarise(fixeddirectcost = sum(fixeddirectcost, na.rm = TRUE),
    #           fixedindirectcost = sum(fixedindirectcost, na.rm = TRUE),
    #           variabledirectcost = sum(variabledirectcost, na.rm = TRUE),
    #           variableindirectcost = sum(variableindirectcost, na.rm = TRUE)
    # ) %>% ungroup() %>%
    gather("costkey", "costs", (ncol(.)-3):ncol(.)) %>%
    mutate(
      fixedvariable = case_when(
        costkey == "variabledirectcost" | costkey == "variableindirectcost" ~ "variable",
        costkey == "fixeddirectcost" | costkey == "fixedindirectcost" ~ "fixed"),
      directindirect = case_when(
        costkey == "variabledirectcost" | costkey == "fixeddirectcost" ~ "direct",
        costkey == "variableindirectcost" | costkey == "fixedindirectcost" ~ "indirect")
    )
  return(out)
}

###############################################################################################################################-
##### _ c. Add/Edit columns ----------------------------------------------------------------------------------------------------
###############################################################################################################################-

full_df <- full_df %>%
  mutate(
    ##### __ * UPDATE: Patient Type Rollup ---------------------------------------
    patienttyperollup = case_when(
      grepl("in", tolower(patienttyperollup)) ~ "Inpatient",
      grepl("out", tolower(patienttyperollup)) ~ "Outpatient",
      patienttyperollup == "Emergency" ~ "Emergency",
      TRUE ~ as.character(NA)
    ),

    ##### __ * UPDATE: Discharge Status Group ------------------------------------
    dischargestatusgroup = ifelse(is.na(dischargestatusgroup), "Not Specified", dischargestatusgroup),

    ##### __ * ADD: age Bucket ---------------------------------------------------
    agebucket = case_when(
      age < 1 ~ "Infant",
      age >= 1 & age < 2 ~ "Toddler",
      age >= 2 & age <= 5 ~ "Early Childhood",
      age >= 6 & age <= 11 ~ "Middle Childhood",
      age >= 12 & age <= 17 ~ "Adolescence",
      age >= 18 ~ "Adult",
      TRUE ~ as.character(NA)
    ),

    ##### __ * ADD: Imaging UBRev Group ------------------------------------------
    imaging_ubrev_group = case_when(
      ubrevcode_short=="0341" | ubrevcode_short=="0343" | ubrevcode_short=="0340" | ubrevcode_short=="0342" | ubrevcode_short=="0344" ~ "Nuclear Med",
      ubrevcode_short=="0610" | ubrevcode_short=="0615" | ubrevcode_short=="0616" | ubrevcode_short=="0618" | ubrevcode_short=="0614" | ubrevcode_short=="0612" | ubrevcode_short=="0619" ~ "MRT",
      ubrevcode_short=="0352" | ubrevcode_short=="0359" | ubrevcode_short=="0350" | ubrevcode_short=="0351" ~ "CT",
      ubrevcode_short=="0401" | ubrevcode_short=="0400" | ubrevcode_short=="0409" | ubrevcode_short=="0404" | ubrevcode_short=="0403" | ubrevcode_short=="0402" ~ "Other Imaging",
      ubrevcode_short=="0321" | ubrevcode_short=="0323" | ubrevcode_short=="0322" | ubrevcode_short=="0324" | ubrevcode_short=="0320" | ubrevcode_short=="0329" ~ "Radiology",
      ubrevcode_short=="0860" | ubrevcode_short=="0861" ~ "MEG",
      ubrevcode_short=="0740" ~ "EEG",
      TRUE ~ ""
    ),

    ##### __ * ADD: LOS UBRev Group ----------------------------------------------
    los_ubrev_group = case_when(
      ubrevcode_short=="0101" ~ "Room and Board",
      ubrevcode_short>="0110" & ubrevcode_short<="0114" ~ "Room and Board",
      ubrevcode_short>="0116" & ubrevcode_short<="0124" ~ "Room and Board",
      ubrevcode_short>="0126" & ubrevcode_short<="0134" ~ "Room and Board",
      ubrevcode_short>="0136" & ubrevcode_short<="0144" ~ "Room and Board",
      ubrevcode_short>="0146" & ubrevcode_short<="0154" ~ "Room and Board",
      ubrevcode_short>="0156" & ubrevcode_short<="0159" ~ "Room and Board",
      ubrevcode_short=="0164" ~ "Room and Board",
      ubrevcode_short>="0170" & ubrevcode_short<="0174" ~ "Nursery",
      ubrevcode_short=="0719" ~ "Nursery",
      ubrevcode_short>="0200" & ubrevcode_short<="0204" ~ "ICU",
      ubrevcode_short>="0206" & ubrevcode_short<="0209" ~ "ICU",
      ubrevcode_short>="0210" & ubrevcode_short<="0212" ~ "CCU",
      ubrevcode_short=="0214" ~ "CCU",
      ubrevcode_short=="0219" ~ "CCU",
      TRUE ~ ""
    )
  ) %>%
  groupingCalcs(grouping_vars=calc_groups)

##### __ * ADD: Placeholders for Filters ------------------------------------
# these placeholders make future coding easier because they will be associated with the data frames
# these filters will be dynamically updated to TRUE/FALSE values depending on user inputs
full_df$filter_myentity = TRUE
full_df$filter_region = TRUE
full_df$filter_size = TRUE
full_df$filter_specialty = TRUE
full_df$filter_costmodel = TRUE
full_df$filtered_entity = TRUE

full_df$filter_costdriver = TRUE

full_df$filter_rom = TRUE
full_df$filter_soi = TRUE
full_df$filter_age = TRUE
full_df$filter_patienttype = TRUE
full_df$filter_dischargestatus = TRUE
full_df$filter_costtype = TRUE
full_df$filter_qualityincidents = TRUE

###################################################################################################################################-
##### 3. FUNCTIONS -----------------------------------------------------------------------------------------------------------------
###################################################################################################################################-

##### _ a. General Functions ------------------------------------------------

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


##### _ b. Graphing Functions ------------------------------------------------

# theming plots
theme_plot<-function(base_size = 10,legend.pos = 'bottom') {
  t = (theme_foundation(base_size = base_size)
       + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.x = element_text(vjust = -0.2),
               axis.title.y = element_text(angle = 90,vjust = 2),
               axis.text = element_text(),
               axis.line = element_line(colour = "black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour = "#f0f0f0"),
               panel.grid.minor = element_black(),
               legend.key = element_rect(colour=NA),
               legend.position = legend.pos,
               legend.title = element_text(face="italic"),
               plot.margin = unit(c(10,5,5,5),"mm"),
               strip.background = element_rect(colour = "#f0f0f0"),
               strip.text = element_text(face = "italic")
               )
       )
  return(t)
}

scale_fill_plot <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

scale_colour_plot <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

plot_custom <- function(p, saveTo = NULL, palette = 'tableau20', base_size=10, legend.pos = "right", color = TRUE, fill = FALSE) {
  out = p + theme_plot(base_size, legend.pos)
  if(color) out = out + scale_colour_tableau(palette = palette)
  if(fill) out = out + scale_fill_tableau(palette = palette)
  if(is.null(saveTo)) return(out)
  ggsave(saveTo, out)
  return(out)
}


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










