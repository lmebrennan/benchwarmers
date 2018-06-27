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
library(civis)

#make sure civis is intalled
#devtools::install_github("civisanalytics/civis-r")

#load each package into r library
inst = lapply(packages,require,character.only=TRUE)

# load all other files in the project
source('functions.R',local = TRUE)
###############################################################################################################################-
##### Load data from Civis, then merge into full_df
###############################################################################################################################-

# get ENTITY data from civis
if(!(exists("hospital_info"))){
  hospital_info <- read_civis("public.hospital_info",database="Strata Decision Technologies")
}
# get ENCOUNTER data from civis
if(!(exists("raw_encounter_df"))){
  raw_encounter_df <- read_civis("public.encounter_children1",database="Strata Decision Technologies")
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
     #group_by(.dots = grouping_vars) %>%
     #summarise(fixeddirectcost = sum(fixeddirectcost, na.rm = TRUE),
     #          fixedindirectcost = sum(fixedindirectcost, na.rm = TRUE),
     #          variabledirectcost = sum(variabledirectcost, na.rm = TRUE),
     #          variableindirectcost = sum(variableindirectcost, na.rm = TRUE)
     #) %>% ungroup() %>%
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

#### UI ####
dbSidebar<-dashboardSidebar(
  sidebarMenu(
    h4("Entity to Benchmark",align="center"),
    selectizeInput(inputId="customer_entity",label="Select an entity:",choices=NULL),
    hr(),
    
    h4("Benchmark Population", align="center"),
    selectizeInput(inputId="region",label="Region(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="beds",label="Bedsize(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="specialty",label="Specialty(ies):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="costmodel",label="Select Cost model(s):",choices=c(ALL=""),multiple=TRUE),
    uiOutput("benchmark_selector"),
    
    actionBttn(inputId="refresh",label="Refresh")
  )
)

dbBody<-dashboardBody(
  
  fluidRow(
    # Benchmarking Information ----------------------------------------------------
    box(
      title="My Entity",
      status="warning",
      solidHeader=TRUE,
      collapsible=TRUE,
      htmlOutput("selected_entity"),
      htmlOutput("selected_region"),
      htmlOutput("selected_size"),
      htmlOutput("selected_specialty"),
      htmlOutput("selected_costmodel")
    ),
    box(
      title="Benchmark Population",
      status="warning",
      solidHeader=TRUE,
      collapsible=TRUE,
      htmlOutput("comparison_entities"),
      htmlOutput("comparison_regions"),
      htmlOutput("comparison_sizes"),
      htmlOutput("comparison_specialties"),
      htmlOutput("comparison_costmodels")
    )
  ),
  # Benchmarking Results ----------------------------------------------------
  fluidRow(
    box(
      width = 3,
      title="Filter Encounters",
      status="warning",
      solidHeader = TRUE 
    ),
    box(
      width=9,
      title="Cost Driver Overview",
      solidHeader=TRUE,
      status="primary",
      collapsible=TRUE,
      tabsetPanel(type = "tabs",
                  tabPanel(
                    title="Box Plot",
                    plotOutput("costdriver_overview_plot")
                  ),
                  tabPanel(
                    title="Summary",
                    plotlyOutput(outputId="box_Imaging")
                  ),
                  tabPanel(
                    title="Table",
                    tableOutput("table")
                  )
      )
    )
  )
)


ui <-fluidPage(
  fluidRow(
    dashboardPage(
      skin = 'black',
      dashboardHeader(title="Benchmarking"),
      dbSidebar,
      dbBody
    )
  )
)

#### SERVER #### 
server<-function(input, output, session) {
  
  #################################################################################################################################-
  ##### SIDEBAR LOGIC --------------------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  
  ##### _ 1. Populate Dropdowns ----------------------------------------------------------------------------------------------------
  
  # updates each dropdown with values from the database, alphabetized
  updateSelectizeInput(session,"customer_entity",
                       choices=hospital_info[order(hospital_info$customerid,hospital_info$entityid),]$customer_entity,
                       server=TRUE)
  
  updateSelectizeInput(session,"region",
                       choices=as.factor(hospital_info[order(hospital_info$region),]$region),
                       server=TRUE)
  updateSelectizeInput(session,"beds",
                       choices=as.factor(hospital_info[order(hospital_info$beds_fixed),]$beds_fixed),
                       server=TRUE)
  updateSelectizeInput(session,"specialty",
                       choices=as.factor(hospital_info[order(hospital_info$specialty),]$specialty),
                       server=TRUE)
  updateSelectizeInput(session,"costmodel",
                       choices=c("Hospitals with Strata Standardized Cost Models" = "standard",
                                 "Hospitals without Strata Standardized Cost Models" = "non"),
                       server=TRUE)
  
  output$benchmark_selector = renderUI({
    selectizeInput(inputId="customer_entity_benchmark",
                   label="Select Entity(ies):",
                   choices=c(ALL="",hospital_info[hospital_info$customer_entity!=input$customer_entity]$customer_entity),
                   multiple=TRUE)
  })
  
  #################################################################################################################################-
  ##### BODY LOGIC -----------------------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  
  ##### _ 1. My Entity Info --------------------------------------------------------------------------------------------------------
  
  # populate selected entity
  output$selected_entity = renderText({
    paste("<b>Hospital Institution:</b>",input$customer_entity)
  })
  
  # populate selected entity's region
  output$selected_region = renderText({
    paste("<b>Region:</b>", hospital_info$region[hospital_info$customer_entity == input$customer_entity])
  })
  
  # populate selected entity's bed size
  output$selected_size = renderText({
    paste("<b>Bed Size:</b>", hospital_info$beds_fixed[hospital_info$customer_entity == input$customer_entity])
  })
  
  # populate selected entity's specialty
  output$selected_specialty = renderText({
    paste("<b>Specialty:</b>", hospital_info$specialty[hospital_info$customer_entity == input$customer_entity])
  })
  
  # populate selected entity's cost model classification
  output$selected_costmodel = renderText({
    paste("<b>Cost Model:</b>",
          if_else(length(input$customer_entity)==0,
                  "",
                  if(hospital_info$isstratastandardcost[hospital_info$customer_entity == input$customer_entity] == "t") {
                    "Strata Standard Cost Model"
                  }
                  else if(hospital_info$isstratastandardcost[hospital_info$customer_entity == input$customer_entity] == "f"){
                    "Not Strata Standard Cost Model"
                  },
                  ""
          ))
  })
  
  ##### _ 2. Benchmark Population Info -------------------------------------------------------------------------------------------
  
  # populate comparison population entities
  output$comparison_entities = renderText({
    if(length(input$costmodel) == 1 & "standard" %in% input$costmodel){
      df <- hospital_info %>%
        filter(isstratastandardcost == "t")
      
      paste("<b>Benchmark Institution(s):</b>",
            ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$beds) & is.null(input$specialty),
                   # if no inputs, then take all the hospitals that aren't the one selected
                   paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = ", "),
                   ifelse(is.null(input$region) & is.null(input$beds) & is.null(input$specialty),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "; "),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & (df$customer_entity %in% input$customer_entity_benchmark
                                                                       | ((df$region %in% input$region | is.null(input$region))
                                                                          & (df$beds_fixed %in% input$beds | is.null(input$beds))
                                                                          & (df$specialty %in% input$specialty | is.null(input$specialty))))])),
                                collapse = "; ")
                   )))
    }
    else if(length(input$costmodel) == 1 & "non" %in% input$costmodel){
      df <- hospital_info %>%
        filter(isstratastandardcost == "f")
      
      paste("<b>Benchmark Institution(s):</b>",
            ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$beds) & is.null(input$specialty),
                   # if no inputs, then take all the hospitals that aren't the one selected
                   paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = "; "),
                   ifelse(is.null(input$region) & is.null(input$beds) & is.null(input$specialty),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "; "),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & (df$customer_entity %in% input$customer_entity_benchmark
                                                                       | ((df$region %in% input$region | is.null(input$region))
                                                                          & (df$beds_fixed %in% input$beds | is.null(input$beds))
                                                                          & (df$specialty %in% input$specialty | is.null(input$specialty))))])),
                                collapse = "; ")
                   )))
    }
    else {
      df <- hospital_info
      
      paste("<b>Benchmark Institution(s):</b>",
            ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$beds) & is.null(input$specialty),
                   # if no inputs, then take all the hospitals that aren't the one selected
                   paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = "; "),
                   ifelse(is.null(input$region) & is.null(input$beds) & is.null(input$specialty),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "; "),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & (df$customer_entity %in% input$customer_entity_benchmark
                                                                       | ((df$region %in% input$region | is.null(input$region))
                                                                          & (df$beds_fixed %in% input$beds | is.null(input$beds))
                                                                          & (df$specialty %in% input$specialty | is.null(input$specialty))))])),
                                collapse = "; ")
                   )))
    }
  })
  
  # populate comparison population regions
  output$comparison_regions = renderText({
    paste("<b>Region(s):</b>",
          ifelse(is.null(input$region),
                 paste(as.vector(unique(hospital_info$region)),collapse=", "),
                 paste(as.vector(unique(input$region)),collapse=", "))
    )
  })
  
  # populate comparison population bed sizes
  output$comparison_sizes = renderText({
    paste("<b>Bed Size(s):</b>",
          ifelse(is.null(input$beds),
                 paste(as.vector(unique(hospital_info$beds_fixed)),collapse=", "),
                 paste(as.vector(unique(input$beds)),collapse=", "))
    )
  })
  
  # populate comparison population specialties
  output$comparison_specialties = renderText({
    paste("<b>Specialty(ies):</b>",
          ifelse(is.null(input$specialty),
                 paste(as.vector(unique(hospital_info$specialty)),collapse=", "),
                 paste(as.vector(unique(input$specialty)),collapse=", "))
    )
  })
  
  # populate comparison population cost models
  output$comparison_costmodels = renderText({
    paste("<b>Cost Model(s):</b>",
          ifelse(is.null(input$costmodel),
                 "Strata Standard Cost Model, Not Strata Standard Cost Model",
                 if(length(input$costmodel) == 1 & "standard" %in% input$costmodel){
                   "Strata Standard Cost Model"
                 }
                 else if(length(input$costmodel) == 1 & "non" %in% input$costmodel){
                   "Not Strata Standard Cost Model"
                 }
                 else {
                   "Strata Standard Cost Model, Not Strata Standard Cost Model"
                 })
    )
  })
  
  #################################################################################################################################-
  ##### ENTITY DF LOGIC ------------------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  
  # When a user clicks 'refresh' on the BYOBenchmark side bar, the dataframe that is used throughout the application
  #  needs to be refreshed.
  entity_df <- eventReactive(input$refresh, {
    # start with the raw customer data
    entity_df <- full_df
    
    ### B1. filter out selected entity
    entity_df$filter_myEntity <- ifelse(entity_df$customer_entity==input$customer_entity,TRUE,FALSE)
    
    ### update entity filters
    if(!is.null(input$region)){
      entity_df$filter_region<-ifelse(entity_df$region %in% input$region,TRUE,FALSE)
    } else {
      entity_df$filter_region<-TRUE
    }
    if(!is.null(input$size)){
      entity_df$filter_size<-ifelse(entity_df$beds_fixed %in% input$size,TRUE,FALSE)
    } else {
      entity_df$filter_size<-TRUE
    }
    if(!is.null(input$specialty)){
      entity_df$filter_specialty<-ifelse(entity_df$specialty %in% input$specialty,TRUE,FALSE)
    } else {
      entity_df$filter_specialty<-TRUE
    }
    if(length(input$costmodel) == 1){
      if("standard" %in% input$costmodel){
        entity_df$filter_costmodel <- ifelse(entity_df$isstratastandardcost == "t", TRUE, FALSE)
      } else if("non" %in% input$costmodel){
        entity_df$filter_costmodel <- ifelse(entity_df$isstratastandardcost == "f", TRUE, FALSE)
      }
    }
    else {
      entity_df$filter_costmodel <- TRUE
    }
    
    ### Master hospital benchmark filter (filtered_entity)
    ### If Entity matches filters, filtered_entity=TRUE
    ### If Entity does not match filters, filtered_entity=FALSE
    
    # if the only input is customers/entities, then only use that column to filter
    if(all(is.null(input$region), is.null(input$size), is.null(input$specialty), is.null(input$costmodel))
       & !is.null(input$customer_entity_benchmark)){
      entity_df$filtered_entity<-ifelse(entity_df$filter_customer_entity, TRUE, FALSE)
    }
    # if there are input filters, but no customer entity filters, then only use input filters
    else if(any(!is.null(input$region), !is.null(input$size), !is.null(input$specialty), !is.null(input$costmodel))
            & !is.null(input$customer_entity_benchmark))
    {
      entity_df$filtered_entity<-ifelse(entity_df$filter_region & entity_df$filter_size & entity_df$specialty, TRUE, FALSE)
    }
    # if there are input filters and customer entity filters, then
    else if(any(!is.null(input$region),!is.null(input$size),!is.null(input$specialty), !is.null(input$costmodel))
            & !is.null(input$customer_entity_benchmark)){
      entity_df$filtered_entity<-ifelse((entity_df$filter_region & entity_df$size & entity_df$specialty)
                                        | entity_df$filter_customer_entity, TRUE, FALSE)
    }
    # if no filter is selected, then
    else {
      entity_df$filtered_entity<-TRUE
    }
    
    entity_df<-entity_df %>%
      filter(filter_myEntity) %>%
      mutate("Group" = ifelse(filter_myEntity,"Me","Baseline")) %>%
      group_by(region, beds, specialty, customer_entity, costdriver,
               encounterid, rom, soi, agebucket, patienttyperollup,
               dischargestatusgroup, los_ubrev_group, imaging_ubrev_group) %>%
      summarize("Count"=1,
                "costs" = sum(costs)) %>%
      ungroup()
    
    return(entity_df)
  })
  
  #################################################################################################################################-
  ##### ENCOUNTER DF LOGIC ---------------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  
  # Encounter level data frame with benchmark grouping columns and cost grouping columns as well as
  # columns with cost information;
  
  # This code filters the full dataframe of all cost data, based off user inputs about how to filter the data
  encounter_df<-eventReactive(input$refresh, {
    # we can start with the current filtered state of entity_df
    encounter_df <- entity_df[which(entity_df$filtered_entity==TRUE)]
    
    # a. Benchmarking filters
    # # filter ROM
    if(!is.null(input$rom)){
      encounter_df$filter_rom<-ifelse(encounter_df$rom %in% input$rom, TRUE, FALSE)
    } else {
      encounter_df$filter_rom<-TRUE
    }
    # # filter SOI
    if(!is.null(input$soi)){
      encounter_df$filter_soi<-ifelse(encounter_df$soi %in% input$soi, TRUE, FALSE)
    } else {
      encounter_df$filter_soi<-TRUE
    }
    # # filter age
    if(!is.null(input$age)){
      encounter_df$filter_age<-ifelse(encounter_df$age %in% input$age, TRUE, FALSE)
    } else {
      encounter_df$filter_age<-TRUE
    }
    # # filter patient type
    if(!is.null(input$patienttyperollup)){
      encounter_df$filter_patienttype<-ifelse(encounter_df$patienttyperollup %in% input$patienttyperollup, TRUE, FALSE)
    } else {
      encounter_df$filter_patienttype<-TRUE
    }
    # # filter discharge status
    if(!is.null(input$dischargestatusgroup)){
      encounter_df$filter_dischargestatus<-ifelse(encounter_df$dischargestatusgroup %in% input$dischargestatusgroup, TRUE, FALSE)
    } else {
      encounter_df$filter_dischargestatus<-TRUE
    }
    
    # e. Cost Type Filters
    if(length(input$cost) > 0){
      encounter_df$temp1<-ifelse(encounter_df$fixedvariable %in% input$costs, 1, 0)
      encounter_df$temp2<-ifelse(encounter_df$directindirect %in% input$costs, 1, 0)
      encounter_df$temp3<-ifelse(encounter_df$costdriver %in% input$costs, 1, 0)
      encounter_df$temp_all<-encounter_df$temp1 + encounter_df$temp2 + encounter_df$temp3
      if(max(encounter_df$temp_all)==1){
        encounter_df$filter_costtype<-ifelse(encounter_df$temp_all==1, TRUE, FALSE)
      }
      if(max(encounter_df$temp_all)==2){
        encounter_df$filter_costtype<-ifelse(encounter_df$temp_all==2, TRUE, FALSE)
      }
      if(max(encounter_df$temp_all)==3){
        encounter_df$filter_costtype<-ifelse(encounter_df$temp_all==3, TRUE, FALSE)
      }
    } else {
      encounter_df$filter_costtype<-TRUE
    }
    
    
    # f. Quality Incident Filters
    # # if there is only one quality incident filter selected
    if(length(input$qualityincidents)==1){
      # if "Only Keep Encounters without Hospital-Acquired Quality Incidents"
      # filter out hospital - acquired conditions / hospital caused quality incidents
      if(min(input$qualityincidents)=="Remove"){
        encounter_df$filter_qualityincidents<-ifelse(encounter_df$hospitalacqcondition=="0", TRUE, FALSE)
      }
      # if "Only Keep Encounters With Hospital-Acquired Quality Incidents"
      # filter out NON hospital-acquired conditions/hospital-caused quality incidents
      else if(min(input$qualityincidents)=="Keep"){
        encounter_df$filter_qualityincidents<-ifelse(encounter_df$hospitalacqcondition=="1", TRUE, FALSE)
      }
    }
    # if both are selected, or neither selected, keep both
    else if(is.null(input$qualityincidents) | length(input$qualityincidents)==2){
      encounter_df$filter_qualityincidents<-TRUE
    }
    
    # set conditions for filtering
    encounter_conditions<-c(encounter_df$filter_rom & encounter_df$filter_soi & encounter_df$filter_age &
                              encounter_df$filter_patienttype & encounter_df$filter_costtype &
                              encounter_df$filter_qualityincidents & encounter_df$filter_dischargestatus)
    
    encounter_df<-encounter_df[encounter_conditions,] %>%
      mutate(Group=factor(ifelse(filter_myentity,"Me","Baseline"),
                          levels=c("Baseline","Me"),
                          ordered=TRUE),
             name="Benchmark")
    
    
    #check to see if there's still data after all that filtering!!
    validate(
      need(nrow(encounter_df)>0,"The data has zero rows due to filtering. Please adjust your filters.")
    )
    
    
    return(encounter_df)
    
  })
  
  #################################################################################################################################-
  ##### SUMMARY DF BENCHMARK LOGIC -------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  
  # data frame with summary statistics for all the baseline hospitals
  # this data frame is used to create the labels for the boxplots, as well as the data tables
  summary_df_benchmark <- eventReactive(input$refresh, {
    # initialize empty variables to indicate keep/remove ROM/SOI for custom grouping option
    remove_ROM <- c()
    keep_ROM <- c()
    remove_SOI <- c()
    keep_SOI <- c()
    
    # grab all of the possible groupings
    all_groupings <- c(input$benchmarkbreakdowns, keep_ROM, keep_SOI)
    # remove unwanted groupings if specified
    keep_benchmarkgroups <- setdiff(all_groupings, c(remove_ROM, remove_SOI))
    
    groups <- c("Group", "msdrggroup", "CostGrouping", "BenchmarkGrouping", keep_benchmarkgroups, input$costbreakdowns)
    
    summary_df_benchmark <- encounter_df() %>%
      filter(Group == "Baseline") %>%
      calcSummary(df = ., summary_var = "costs", outlier_threshold = 2, grouping_vars = groups)
    
    ## check to see there's still data to benchmark against after filtering encounter_df for just the baseline data
    validate(
      need(nrow(summary_df_benchmark) > 0, "There is no baseline data due to filtering (i.e. there is no data for the 'Baseline'). Please adjust your data filters.")
    )
    
    return(summary_df_benchmark)
  })
  
  
  #################################################################################################################################-
  ##### SUMMARY DF ME LOGIC --------------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  
  # data frame with summary statistics for the hospital of interest
  # this data frame is used to create the labels for the boxplots, as well as the data tables
  summary_df_me <- eventReactive(input$refresh, {
    # initialize empty variables to indicate keep/remove ROM/SOI for custom grouping option
    remove_ROM <- c()
    keep_ROM <- c()
    remove_SOI <- c()
    keep_SOI <- c()
    
    # grab all of the possible groupings
    all_groupings <- c(input$benchmarkbreakdowns, keep_ROM, keep_SOI)
    # remove unwanted groupings if specified
    keep_benchmarkgroups <- setdiff(all_groupings, c(remove_ROM, remove_SOI))
    
    groups <- c("Group", "msdrggroup", "CostGrouping", "BenchmarkGrouping", keep_benchmarkgroups, input$costbreakdowns)
    
    summary_df_me <- encounter_df() %>%
      filter(Group == "Me") %>%
      calcSummary(df = ., summary_var = "costs", outlier_threshold = 2, grouping_vars = groups)
    
    ## check to see there's still data to benchmark after filtering encounter_df for just the "Me" data
    validate(
      need(nrow(summary_df_me) > 0, "There is no data to benchmark due to filtering (i.e. there is no data for 'Me'). Please adjust your data filters.")
    )
    
    return(summary_df_me)
  })
  
  
  #################################################################################################################################-
  ##### SUMMARY DF COMPARE LOGIC ---------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  
  # data frame with the summary information for "Me" and the "Baseline" next to each other in order to calculate differences
  # used to create labels for the difference barplots, as well as the comparison data table
  compare_df <- eventReactive(input$refresh, {
    # initialize empty variables to indicate keep/remove ROM/SOI for custom grouping option
    remove_ROM <- c()
    keep_ROM <- c()
    remove_SOI <- c()
    keep_SOI <- c()
    # grab all of the possible groupings
    all_groupings <- c(input$benchmarkbreakdowns, keep_ROM, keep_SOI)
    # remove unwanted groupings if specified
    keep_benchmarkgroups <- setdiff(all_groupings, c(remove_ROM, remove_SOI))
    
    groups <- c("msdrggroup", "BenchmarkGrouping", "CostGrouping", keep_benchmarkgroups, input$costbreakdowns)
    
    # grab summary df of "Me"
    me <- summary_df_me()
    # append "_ME" to end of summary column names (excluding join keys (i.e. groups))
    colnames(me)[!(colnames(me) %in% groups)] <- paste0(colnames(me)[!(colnames(me) %in% groups)], "_ME")
    
    # full join the summary df of all the benchmark hospitals, and the summary df of "Me"
    # use the groupings as join keys
    compare_out <- summary_df_benchmark() %>%
      full_join(me, by = groups)
    
    compare_out <- compare_out %>%
      mutate(diff_median = round(median_ME - median, 2),                            # difference in medians b/w "Me" and benchmark
             diff_mean = round(mean_ME - mean, 2),                                  # difference in mean b/w "Me" and benchmark
             # percent difference in median b/w "Me" and benchmark
             proport_diff_median = ifelse(is.infinite(diff_median/median_ME),
                                          round(coalesce(diff_median / 1, 0), 2),  # if division by zero, divide by 1 instead
                                          round(coalesce(diff_median / median_ME, 0), 2)),
             # percent difference in mean b/w "Me" and benchmark
             proport_diff_mean = ifelse(is.infinite(diff_mean/mean_ME),
                                        round(coalesce(diff_mean / 1, 0), 2),    # if division by zero, divide by 1 instead
                                        round(coalesce(diff_mean / mean_ME, 0), 2)),
             Difference = "Difference",                                             # column to indicate this is the "difference" data frame; used for faceting
             empty_flag = ifelse(is.na(min) | is.na(min_ME), 1, 0))                 # flag for if missing data
    
    
    return(compare_out)
  })
  
  #################################################################################################################################-
  ##### GRAPHING LOGIC -------------------------------------------------------------------------------------------------------------
  #################################################################################################################################-
  # _ II. Plots ------------------------------------------------------------
  
  # _ _ 0. General ----------------------------------------------------------
  costdriver_overview_plot<-eventReactive(input$refresh, {
    #grab reactive data frames
    encounter_df<-encounter_df()
    encounter_df<-encounter_df %>%
      group_by(group, costdriver, customer_entity) %>%
      summarise(Count=sum(Count)) %>%
      ungroup()
    
    costdriver_overview_plot<-ggplot() +
      geom_boxplot(data = encounter_df[encounter_df$group == "Me", ])
    geom_boxplot(data = encounter_df[encounter_df$group == "Baseline", ])
    scale_fill_manual(values = c("Baseline" = "#1f78b4",    # blue
                                 "Me" = "#ff7f00"),         # orange
                      name = "") +
      scale_color_manual(values = c("Baseline" = "#1f78b4",    # blue
                                    "Me" = "#ff7f00"),         # orange
                         guide = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      labs(x = "# of Encounters",
           y = "# of Benchmark Institutions") +
      theme(axis.text.y = element_blank(),
            axis.ticks.x = element_blank())
    
    return(costdriver_overview_plot)
  })
  
  ## -----------<< Benchmark Plot >>-----------
  plot <- eventReactive(input$refresh, {
    
    ## grab all reactive data frames
    main_df <- encounter_df()
    benchmark <- summary_df_benchmark()
    me <- summary_df_me()
    comparison <- compare_df()
    
    ## stack together "Baseline" summary df and "Me" summary df for boxplot labels
    all <- union_all(benchmark, me)
    
    
    ## -----------<<< Set Plotting Parameters >>>-----------
    # if no cost grouping, don't facet
    if(is.null(input$costbreakdowns)){
      facet_group <- as.formula(".~Name")             # faceting for "gg"
      facet_diff_group <- as.formula(".~Difference")  # faceting for "diff"
    }
    # if there is cost grouping, facet by cost grouping
    else {
      facet_group <- as.formula("CostGrouping~Name")             # faceting for "gg
      facet_diff_group <- as.formula("CostGrouping~Difference")  # faceting for "diff"
    }
    
    # if no benchmark grouping, set axis name as "MS-DRG Code" for default
    if(is.null(input$benchmarkbreakdowns)){
      axis_name <- "MSDRG"
    }
    # if benchmark grouping, set axis name as combo of all the grouping column names
    else {
      axis_name <- paste0(input$benchmarkbreakdowns, collapse = " & ")
    }
    
    
    ## -----------<<< gg -- "Baseline" vs. "Me" plot >>>-----------
    gg <- ggplot(encounter_df) +
      geom_boxplot(aes(x = BenchmarkGrouping, y = costs, color = Group), position = "dodge") +
      geom_text(data = all,
                aes(x = BenchmarkGrouping, y = median, label = paste0("$", scales::comma(median)), group = Group,
                    hjust = -0.2, vjust = -0.5,
                    fontface = "bold"),
                position = position_dodge(width = 0.75), size = 5) +
      coord_flip() +
      facet_grid(facet_group) +
      scale_x_discrete(name = axis_name) +
      scale_color_manual(values = c("Baseline" = "#1f78b4",  # blue
                                    "Me" = "#ff7f00"),       # orange
                         name = "") +
      # lines that separate different groupings
      # remove first value in sequence (0.5) because don't want one between panel border and first plot
      geom_vline(xintercept = seq(from = 0.5, to = length(unique(comparison[["BenchmarkGrouping"]])) -0.5, by = 1)[-1],
                 color = "black") +
      theme_bw() +
      theme(plot.title = element_text(size = 18, face = "bold"),
            panel.background = element_rect(fill = "white"),
            panel.grid.minor = element_line(color = "lightgray"),
            strip.background = element_blank(),
            strip.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 15),
            strip.text.x = element_text(size = 15),
            axis.title = element_text(size = 15),
            legend.position = "bottom",
            legend.text = element_text(size = 24)) +
      guides(colour = guide_legend(override.aes = list(size = 2))) +
      
      ## set axis for costs to be either normal or log based on user input
      if(input$scale == TRUE){
        gg <- gg +
          scale_y_log10(name = "Cost per Encounter\n($)",
                        labels = scales::dollar)
      }
    # use normal scale if no input or input is "normal" (default)
    if(input$scale == FALSE){
      gg <- gg +
        scale_y_continuous(name = "Cost per Encounter\n($)",
                           labels = scales::dollar)
    }
    
    ## -----------<<< diff -- plot showing differences between "Baseline" and "Me" >>>-----------
    diff <- ggplot(comparison,
                   aes(fill = ifelse(proport_diff_median > 0, 'pos', 'neg'))) +   # if % difference positive, then "pos", else "neg" (for setting colors)
      geom_bar(aes(x = BenchmarkGrouping, y = proport_diff_median),
               stat = 'identity', width = .95) +
      # line at 0 mark
      geom_hline(color = 'black', yintercept = 0) +
      # lines that separate different groupings
      # remove first value in sequence (0.5) because don't want one between panel border and first plot
      geom_vline(xintercept = seq(from = 0.5, to = length(unique(comparison[["BenchmarkGrouping"]]))-0.5, by = 1)[-1],
                 color = "black") +
      geom_text(aes(label = ifelse(empty_flag == 1,
                                   "  NA",                                           # if NA, label "NA" (extra spaces for aesthetic purposes to move it right of vertical line)
                                   paste0(round(proport_diff_median*100, 2), "%")),  # label with %, round to 2 decimal places
                    x = BenchmarkGrouping,
                    y = case_when(diff_median >= 0 ~ 0.12*max(abs(proport_diff_median)),    # if positive %, put it to the right
                                  diff_median < 0 ~ -0.4*max(abs(proport_diff_median)),     # if negative %, put it to the left
                                  is.na(diff_median) ~ 0),                                  # if NA because no comparisons, put it at zero and should have "NA" label
                    fontface = "bold"), size = 5,
                hjust = 0.15) +
      scale_y_continuous(name = "Difference\n(%)",
                         labels = scales::percent,
                         breaks = scales::pretty_breaks(2),
                         limits = c(-max(abs(comparison$proport_diff_median)), max(abs(comparison$proport_diff_median)))) +
      scale_fill_manual(values = c("neg" = "#33a02c",   # green
                                   "pos" = "#e31a1c"),  # red
                        guide = FALSE) +
      scale_color_manual(values = c("big" = 'white', "small" = 'grey20'), guide = FALSE) +
      coord_flip() +
      facet_grid(facet_diff_group) +
      theme_bw() +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size = 15),
            strip.text.x = element_text(size = 15),
            axis.text.x = element_text(size = 15))
    
    ## -----------<<< full -- gg and diff plots together >>>-----------
    full <- plot_grid(gg, diff, ncol = 2, align = "h", axis = "bt", rel_widths = c(1.5, 0.5))
    
    return(full)
  })
  
  output$costdriver_overview_plot <-renderPlot({
    plot()
  })
  
  output$box_imaging <- renderPlotly({
    p = ggplot(full_df, aes(imaging_ubrev_group, Cost, color = imaging_ubrev_group, name = Cost)) +
      stat_boxplot(geom = "errorbar", width = 0.8) + geom_boxplot() + ylim(c(0, 100))
    (plot_custom(p, legend.pos = 'none') + theme(axis.text.x = element_text(angle = 90, hjust = 1))) %>% ggplotly()
  })
  
  box_genre = function(dat) {
    p = ggplot(encounter_df, aes(imaging_ubrev_group, Cost, color = imaging_ubrev_group, name = Cost)) +
      stat_boxplot(geom = "errorbar", width = 0.8) + geom_boxplot() + ylim(c(0, 100))
    (plot_custom(p, legend.pos = 'none') + theme(axis.text.x = element_text(angle = 90, hjust = 1))) %>% ggplotly()
  }
  

  ## -----------< Session >-----------
  session$allowReconnect("force")
}

#### RUN APP #### 
shinyApp(ui = ui, server = server)
  







