#### CREATE ADULT BENCHMARKING APP ####
## R script that renders a Shiny app to do cost benchmarking for Adult Hospitals
## Winter 2018
## Civis Analytics
## R version 3.5.0

install.packages(c('devtools',
                   'shiny',
                   'shinythemes',
                   'shinyWidgets',
                   #    'ggplot2',
                   #    'tidyverse',
                   #    'readr',
                   'shinyTree',
                   'cowplot',
                   #    'lazyeval',
                   'rlang',
                   #     'civis',
                   #    'rsconnect',
                   'DT',
                   'data.table'
),
repos='https://cran.rstudio.com/')

#devtools::install_github("civisanalytics/civis_deckR")


library(ggplot2)
library(tidyverse)
library(readr)
library(cowplot)
#library(lazyeval)
library(rlang)
library(civis)
#library(civis.deckR)
library(shiny)
library(shinyTree)
library(shinythemes)
library(shinyWidgets)
#library(plotly)
#library(viridis)
#library(rsconnect)
library(DT)
library(data.table)
library(stringr)

## -----------< Load Helper Functions and Data >-----------
source("StrataFunctions_adults.R", local = TRUE)

# read in data
full<-read_civis("full_adult",database="Strata Decision Technologies")
names <- c('customer_entity' ,'dischargestatusgroup')
full[,names] <- lapply(full[,names], as.character)

hospital_info <- read_civis("hospital_info_adult",database="Strata Decision Technologies")
hospital_info$customer_entity<-paste0("Customer ", hospital_info$customerid, ", ", "Entity ", hospital_info$entityid)

#### UI #### 
ui <- fluidPage(
  theme = shinythemes::shinytheme("lumen"),
  tabsetPanel(type = "tabs",
              ## -----------< 1. Create Benchmark >-----------
              tabPanel("Create Benchmark",
                       fluidRow(
                         ## -----------<< Column 1.1: Input Hospital and Benchmark Selections >>-----------
                         column(2,
                                # parameters for "Me"
                                h3("Hospital and MSDRG to Benchmark"),
                                # select customer / hospital system
                                selectizeInput("customer_entity", "Select a customer and entity to benchmark:", 
                                               choices =''),
                                # select MSDRG to benchmark (options change based off which customer is selected)
                                uiOutput("MSDRG_selector"),
                                h3(""),
                                # parameters for Baseline / hospitals to bencmark against
                                h3("Benchmark Hospitals"),
                                # select specific hospitals to compare against
                                uiOutput("benchmark_selector"),
                                # select hospital regions to compare against
                                selectizeInput("region", "Select region(s):",
                                               choices = c(ALL = "", "South", "Midwest", "East"),
                                               multiple = TRUE),
                                # select bedsizes to compare against
                                selectizeInput("size", "Select bedsize(s):",
                                               choices = c(ALL = "", "Under 200", "200+"),
                                               multiple = TRUE),
                                # select specialties to compare against
                                selectizeInput("specialty", "Select specialty(ies):",
                                               choices = c(ALL = "", "Rural","Urban"),
                                               multiple = TRUE),
                                selectizeInput("costmodel", "Select cost model(s):",
                                               choices = c(ALL = "", 
                                                           "Hospitals with Strata Standardized Cost Models" = "standard", 
                                                           "Hospitals without Strata Standardized Cost Models" = "non"),
                                               multiple = TRUE),
                          
                                actionButton("hospital_refresh", "Compare Patient Populations")),
                         ## -----------<< Column 1.2: Output Hospital and Benchmark Characteristics >>-----------
                         column(2,
                                h3("Hospital Characteristics"),
                                # output characteristics about the hospital selected as "Me"
                                htmlOutput("hospital_institution"),  # hospital institution you're benchmarking
                                htmlOutput("hospital_region"),       # hospital region (e.g. Midwest))
                                htmlOutput("hospital_size"),         # hospital size (e.g. 200+ beds)
                                htmlOutput("hospital_specialty"),    # specialty (e.g. rural or urban)
                                h3(""),
                                h3("Benchmark Characteristics"),
                                # output characteristics about the benchmark
                                htmlOutput("benchmark_institutions"),  # institutions in the benchmark
                                htmlOutput("benchmark_region"),        # benchmark region
                                htmlOutput("benchmark_size"),          # benchmark size (e.g. 200+ beds)
                                htmlOutput("benchmark_specialty")      # specialty (e.g. rural or urban)
                         ),
                         ## -----------<< Column 1.3: Highlighted Hospitals >>-----------
                         column(2,
                                h3("Highlighted Institutions:"),
                                strong("Select specific dots by dragging your cursor on the plot, and you can see which customer(s)/entity(ies) you've highlighted below."),
                                htmlOutput("plotbrush_output")),
                         ## -----------<< Column 1.4: Distribution Plots >>-----------
                         column(6,
                                tabsetPanel(type = "tabs",
                                            tabPanel("MSDRG", plotOutput("msdrg_plot", 
                                                                                 brush = brushOpts(id = "msdrg_plotbrush", direction = "x"),
                                                                                 width = "100%", height = "800px")),
                                            tabPanel("ROM", plotOutput("rom_plot",
                                                                       brush = brushOpts(id = "rom_plotbrush", direction = "x"),
                                                                       width = "100%", height = "800px")),
                                            tabPanel("SOI", plotOutput("soi_plot",
                                                                       brush = brushOpts(id = "soi_plotbrush", direction = "x"),
                                                                       width = "100%", height = "800px")),
                                            tabPanel("Patient Age", plotOutput("age_plot",
                                                                               brush = brushOpts(id = "age_plotbrush", direction = "x"),
                                                                               width = "100%", height = "800px")),
                                            tabPanel("Patient Type", plotOutput("type_plot",
                                                                                brush = brushOpts(id = "type_plotbrush", direction = "x"),
                                                                                width = "100%", height = "800px")),
                                            tabPanel("Patient Discharge Status", plotOutput("discharge_plot",
                                                                                            brush = brushOpts(id = "discharge_plotbrush", direction = "x"),
                                                                                            width = "100%", height = "800px"))
                                )
                         )
                       )
              ),
              ## -----------< 2. Cost Saving Opportunities >-----------
              tabPanel("Cost Saving Opportunities -- MSDRG",
                       fluidRow(
                         
                         # button to update plot
                         actionButton("view_opportunities", "View Cost Saving Opportunities")),
                       fluidRow(plotOutput("costsavings_plot", width = "100%", height = "800px"))),
              
              ## -----------< 3. View Benchmark >-----------
              tabPanel("Cost Benchmark Drill-Down",
                       fluidRow(
                         ## -----------<< Column 2.1: Benchmark and Cost Breakdowns >>-----------
                         column(2,
                                # breakdowns by benchmarking groups (changes y-axis)
                                h3("Benchmark Breakdowns"),
                                checkboxGroupInput("benchmarkbreakdowns", strong("Select variables to breakdown costs by:"),
                                                   choiceNames = c("Risk of Mortality (ROM)",
                                                                   "Severity of Illness (SOI)",
                                                                   "Patient Age Bucket",
                                                                   "Patient Type",
                                                                   "Patient Discharge Status"),
                                                   choiceValues = c("rom",
                                                                    "soi",
                                                                    "agebucket",
                                                                    "patienttyperollup",
                                                                    "dischargestatusgroup")),
                               
                                # breakdowns by cost (changes faceting)
                                h3("Cost Breakdowns"),
                                checkboxGroupInput("costbreakdowns", strong("Select how to breakdown costs:"),
                                                   choiceNames = c("Fixed/Variable",
                                                                   "Direct/Indirect",
                                                                   "Cost Drivers"),
                                                   choiceValues = c("fixedvariable",
                                                                    "directindirect",
                                                                    "costdriver")),
                                
                                # other options for displaying / breaking down data
                                h3("Other Options"),
                                checkboxInput("scale", "Change x-axis (costs) to log scale? (default is normal)", 
                                              value = FALSE)
                         ),
                         
                         ## -----------<< Column 2.2: Data Filters >>-----------
                         column(3,
                                # options to remove / filter data
                                h3("Filter Data"),
                                selectizeInput("ROM", "Select Risk of Mortality (ROM) value(s):",
                                               choices = c(ALL = "", "1", "2", "3", "4"),
                                               multiple = TRUE),
                                selectizeInput("SOI", "Select Severity of Illness (SOI) value(s):",
                                               choices = c(ALL = "", "1", "2", "3", "4"),
                                               multiple = TRUE),
                                selectizeInput("age", "Select patient age(s):", 
                                               choices = c(ALL = "",
                                                           "Infant (less than 1 yr)" = "Infant",
                                                           "Pediatric (1 yr - 17 yrs)" = "Pediatric", 
                                                           "Adult (18 yrs - 64 yrs)" = "Adult",
                                                           "Senior (65 years or older)" = "Senior"),
                                               multiple = TRUE),
                                selectizeInput("patienttype", "Select patient type(s):", 
                                               choices = c(ALL = "", 
                                                           "Inpatient",
                                                           "Outpatient"),
                                               multiple = TRUE),
                                selectizeInput("dischargestatus", "Select patient discharge status(es):", 
                                               choices = c(ALL = "", 
                                                           "Still a Patient",
                                                           "Discharged to home or other self care",
                                                           "Discharged to home health services",
                                                           "Left against medical advice (AMA)",
                                                           "Died",
                                                           "Transferred to other facility",
                                                           "Transferred to other short-term care facility",
                                                           "Transferred to intermediate care facility",
                                                           "Transferred to skilled nursing facility",
                                                           "Not Specified"),
                                               multiple = TRUE),
                                selectizeInput("costs", "Select cost(s):",
                                               choices = list(ALL = "",
                                                              `Cost Types` = c("Fixed",
                                                                               "Variable",
                                                                               "Direct",
                                                                               "Indirect"),
                                                              `Cost Drivers` = c("Dialysis",
                                                                                 "Excluded",
                                                                                 "Imaging",
                                                                                 "Laboratory",
                                                                                 "LOS",
                                                                                 "OR Time",
                                                                                 "Other Diagnostic Services",
                                                                                 "Pharmacy",
                                                                                 "Supplies",
                                                                                 "Blood",
                                                                                 "Labor and Delivery",
                                                                                 "Therapeutic Services",
                                                                                 "Cardiovascular")),
                                               multiple = TRUE),
                                selectizeInput("qltyincidents", "Select whether to keep/remove hospital-caused quality incidents:",
                                               choices = c(BOTH = "",
                                                           "Only Encounters without Hospital-Caused Quality Incidents" = "Remove",
                                                           "Only Encounters with Hospital-Caused Quality Incidents" = "Keep"),
                                               multiple = TRUE),
                                # option to remove data
                                checkboxGroupInput("otherfilteroptions", strong("Other data filters:"),
                                                   choiceNames = c("Remove Cost Outliers (based off interquartile range (IQR))",
                                                                   "Remove Cost Outliers (based off standard deviation (sd))",
                                                                   "Remove Length of Stay Outliers (based off interquartile range (IQR))",
                                                                   "Remove Length of Stay Outliers (based off standard deviation (sd))"
                                                   ),
                                                   choiceValues = c("cost_IQR",
                                                                    "cost_SD",
                                                                    "LOS_IQR",
                                                                    "LOS_SD")),
                                # button to update data, plot, and tables
                                actionButton("refresh", "Update")
                         ),
                         
                         ## -----------<< Column 2.3: Output >>-----------
                         column(7,
                                "Select benchmarking parameters and hit the 'UPDATE' button at the bottom right to generate benchmarks.",
                                tabsetPanel(type = "tabs",
                                            # tab with the plot
                                            tabPanel("Plot", plotOutput("plot", width = "100%", height = "800px")),
                                            # tab with data tables
                                            tabPanel("Tables", 
                                                     # baseline data / data for other hospitals
                                                     h4(strong("Baseline")),
                                                     dataTableOutput("summary_df_benchmark"),
                                                     # me data / data for hospital being benchmarked
                                                     h4(strong("Me")),
                                                     dataTableOutput("summary_df_me"),
                                                     # comparison data 
                                                     h4(strong("Difference")),
                                                     dataTableOutput("compare_df"))
                                )
                         )
                       )
              )
  )
)

#### SERVER #### 
server <- function(input, output, session){
  
  ## -----------< UI Inputs and Outputs >-----------
  
  ## Dependent UI Inputs
  
  # Select customer and entity ("me")
  updateSelectizeInput(session,"customer_entity",
                       choices=hospital_info[order(hospital_info$customerid,hospital_info$entityid),]$customer_entity,
                       server=TRUE)
  
  # MSDRG Code -- input options change based off which Customer & Entity are selected
  output$MSDRG_selector = renderUI({
    selectizeInput(inputId = "MSDRG", "Select an MSDRG to benchmark:",
                   choices = labelMSDRG(unique(full$msdrggroup[full$customerid == hospital_info$customerid[hospital_info$customer_entity == input$customer_entity] &
                                                                 full$entityid == hospital_info$entityid[hospital_info$customer_entity == input$customer_entity]])))
  })
  
  # Customer ID and Entity ID
  output$benchmark_selector = renderUI({
    selectizeInput(inputId = "customer_entity_benchmark", "Select customer(s) and entity(ies) to benchmark against:",
                   choices = c(ALL = "", hospital_info$customer_entity[hospital_info$customer_entity != input$customer_entity]),
                   multiple = TRUE)
  })
  
  ## UI output hospital information 
  # Hospital Institution -- outputs the institution you're benchmarking
  output$hospital_institution = renderText({
    paste("<b>Hospital Institution:</b><br/>",
          input$customer_entity)
  })
  # Region -- outputs the region of the Customer & Entity selected
  output$hospital_region = renderText({
    paste("<b>Hospital Region:</b><br/>", 
          hospital_info$region[hospital_info$customer_entity == input$customer_entity])
  })
  # Size -- outputs the bedsize of the Customer & Entity selected
  output$hospital_size = renderText({
    paste("<b>Hospital Bed Size:</b><br/>", 
          hospital_info$beds[hospital_info$customer_entity == input$customer_entity])
  })
  # Specialty -- outputs the specialty of the Customer & Entity selected (e.g. Pediatric)
  output$hospital_specialty = renderText({
    paste("<b>Hospital Specialty:</b><br/>", 
          hospital_info$specialty[hospital_info$customer_entity == input$customer_entity])
  })
  
  ## UI output hospital benchmark information
  # Customer and Entity -- outputs the Customers(s) and Entity(ies) that make up the benchmark
  output$benchmark_institutions = renderText({
    if(length(input$costmodel) == 1 & "standard" %in% input$costmodel){
      df <- hospital_info %>%
        filter(isstratastandardcost == "Y")
      
      paste("<b>Benchmark Institution(s):</b><br/>", 
            ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                   # if no inputs, then take all the hospitals that aren't the one selected
                   paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = "<br/>"),
                   ifelse(is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "<br/>"),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & (df$customer_entity %in% input$customer_entity_benchmark
                                                                       | ((df$region %in% input$region | is.null(input$region))
                                                                          & (df$beds %in% input$size | is.null(input$size))
                                                                          & (df$specialty %in% input$specialty | is.null(input$specialty))))])), 
                                collapse = "<br/>")
                   )))
    }
    else if(length(input$costmodel) == 1 & "non" %in% input$costmodel){
      df <- hospital_info %>%
        filter(isstratastandardcost == "N")
      
      paste("<b>Benchmark Institution(s):</b><br/>", 
            ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                   # if no inputs, then take all the hospitals that aren't the one selected
                   paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = "<br/>"),
                   ifelse(is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "<br/>"),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & (df$customer_entity %in% input$customer_entity_benchmark
                                                                       | ((df$region %in% input$region | is.null(input$region))
                                                                          & (df$beds %in% input$size | is.null(input$size))
                                                                          & (df$specialty %in% input$specialty | is.null(input$specialty))))])), 
                                collapse = "<br/>")
                   )))
    }
    else {
      df <- hospital_info
      
      paste("<b>Benchmark Institution(s):</b><br/>", 
            ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                   # if no inputs, then take all the hospitals that aren't the one selected
                   paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = "<br/>"),
                   ifelse(is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "<br/>"),
                          paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                    & (df$customer_entity %in% input$customer_entity_benchmark
                                                                       | ((df$region %in% input$region | is.null(input$region))
                                                                          & (df$beds %in% input$size | is.null(input$size))
                                                                          & (df$specialty %in% input$specialty | is.null(input$specialty))))])), 
                                collapse = "<br/>")
                   )))
    }
  })
  # Region -- outputs the region of the Customer(s) & Entity(ies) selected
  output$benchmark_region = renderText({
    paste("<b>Benchmark Region(s):</b><br/>", 
          ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region),
                 paste(as.vector(unique(hospital_info$region[hospital_info$customer_entity != input$customer_entity])), collapse = ", "),
                 paste(as.vector(unique(hospital_info$region[hospital_info$customer_entity %in% input$customer_entity_benchmark | hospital_info$region %in% input$region])), collapse = ", ")))
  })
  # Size -- outputs the bedsize of the Customer(s) & Entity(ies) selected
  output$benchmark_size = renderText({
    paste("<b>Benchmark Bed Size(s):</b><br/>", 
          ifelse(is.null(input$customer_entity_benchmark) & is.null(input$size),
                 paste(as.vector(unique(hospital_info$beds[hospital_info$customer_entity != input$customer_entity])), collapse = ", "),
                 paste(as.vector(unique(hospital_info$beds[hospital_info$customer_entity %in% input$customer_entity_benchmark | hospital_info$beds %in% input$size])), collapse = ", ")))
  })
  # Specialty -- outputs the specialty of the Customer(s) & Entity(ies) selected
  output$benchmark_specialty = renderText({
    paste("<b>Benchmark Specialty(ies):</b><br/>", 
          ifelse(is.null(input$customer_entity_benchmark) & is.null(input$specialty),
                 paste(as.vector(unique(hospital_info$specialty[hospital_info$customer_entity != input$customer_entity])), collapse = ", "),
                 paste(as.vector(unique(hospital_info$specialty[hospital_info$customer_entity %in% input$customer_entity_benchmark | hospital_info$specialty %in% input$specialty])), collapse = ", ")))
  })
  # Cost Model -- outputs the cost models of the hospitals
  output$benchmark_specialty = renderText({
    if(length(input$costmodel) == 1 & "standard" %in% input$costmodel){
      out <- c("Strata Standard Cost Model")
    }
    else if(length(input$costmodel) == 1 & "non" %in% input$costmodel){
      out <- c("Not Strata Standard Cost Model")
    }
    else {
      out <- c("Strata Standard Cost Model", "Not Strata Standard Cost Model")
    }
    paste("<b>Benchmark Cost Model(s):</b><br/>", 
          paste(out, collapse = ", "))
  })
  
  ## -----------< Data Munging >-----------
  
  ## -----------<< hospital_df >>-----------
  hospital_df <- eventReactive(input$hospital_refresh | input$view_opportunities, {
    hospital_df <- full
    
    ## MSDRG code filter
    hospital_df$m1 <- ifelse(hospital_df$msdrggroup == input$MSDRG, TRUE, FALSE)
    
    ## "me" / hospital filter
    hospital_df$h1 <- ifelse(hospital_df$customer_entity == input$customer_entity, TRUE, FALSE)  # filter for input Customer ID and Entity ID
    
    
    ## hospital comparison filters
    if(!is.null(input$region)){
      hospital_df$c1 <- ifelse(hospital_df$region %in% input$region, TRUE, FALSE)        # filter for hospital region
    } else {
      hospital_df$c1 <- TRUE
    }
    if(!is.null(input$size)){
      hospital_df$c2 <- ifelse(hospital_df$beds %in% input$size, TRUE, FALSE)      # filter for hospital size
    } else {
      hospital_df$c2 <- TRUE
    }
    if(!is.null(input$specialty)){
      hospital_df$c3 <- ifelse(hospital_df$specialty %in% input$specialty, TRUE, FALSE)  # filter for hospital specialty
    } else {
      hospital_df$c3 <- TRUE
    }
    # filter for specific hospital inputs
    if(!is.null(input$customer_entity_benchmark)){
      hospital_df$c4 <- ifelse(hospital_df$customer_entity %in% input$customer_entity_benchmark, TRUE, FALSE)
    } else {
      hospital_df$c4 <- TRUE
    }
    # if only select one of the two options for input costmodel, then it's standard or non-standard
    if(length(input$costmodel) == 1){
      if("standard" %in% input$costmodel){
        hospital_df$c_costmodel <- ifelse(hospital_df$isstratastandardcost == "Y", TRUE, FALSE)
      } else if("non" %in% input$costmodel){
        hospital_df$c_costmodel <- ifelse(hospital_df$isstratastandardcost == "N", TRUE, FALSE)
      }
    } 
    # if select none or both of the two options for input cost model, then it's all of them
    else {
      hospital_df$c_costmodel <- TRUE
    }
    # master hospital benchmark filter
    # if only input customers/entities to benchmark against, only use that column to filter
    # all of them need to meet the hospital_df$c_costmodel condition
    if(all(is.null(input$region), is.null(input$size), is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      hospital_df$c5 <- ifelse(hospital_df$c4, TRUE, FALSE)
    } 
    # if input region/size/specialty filters, but not customer entity filters, then only use those filters
    else if(any(!is.null(input$region), !is.null(input$size), !is.null(input$specialty)) & is.null(input$customer_entity_benchmark)){
      hospital_df$c5 <- ifelse(hospital_df$c1 & hospital_df$c2 & hospital_df$c3, TRUE, FALSE)
    }
    # if input region/size/specialty filters and customer entity filters, then
    else if(any(!is.null(input$region), !is.null(input$size), !is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      hospital_df$c5 <- ifelse((hospital_df$c1 & hospital_df$c2 & hospital_df$c3) | hospital_df$c4, TRUE, FALSE)
    }
    # if none selected; then else
    else {
      hospital_df$c5 <- TRUE
    }
    
    
    # filter for only hospital to benchmark & benchmark hospitals
    hospital_df <- hospital_df %>%
      filter(h1 | (c5 & c_costmodel)) %>%
      mutate("Group" = ifelse(h1, "Me", "Baseline"),
             "MSDRG_benchmark" = ifelse(m1, msdrggroup, NA)) %>%
      group_by(region, beds, specialty, customer_entity, msdrggroup, encounterid, rom, soi, agebucket, patienttyperollup, dischargestatusgroup,
               Group, MSDRG_benchmark) %>%
      summarise("Count" = 1,
                "costs" = sum(costs)) %>% ungroup()
    
    return(hospital_df)
  })
  
  ## -----------<< main_df >>-----------
  # Encounter-level dataframe with benchmark grouping columns and cost grouping columns as well as columns with cost information;
  # the code below filters the full dataframe of all cost data, based off user inputs about how to filter the data
  # the data is also labeled as "Me" or "Baseline" to indicate which costs go towards the benchmark, and which go to the hospital of interest
  main_df <- eventReactive(input$refresh, {
    ## grab full dataframe of customer data from global environment; summarised at the most granular level of grouping
    main_df <- full
    
    ## MSDRG filter
    main_df$m1 <- ifelse(main_df$msdrggroup == input$MSDRG, TRUE, FALSE)
    
    
    ## "me" / hospital filter
    main_df$h1 <- ifelse(main_df$customer_entity == input$customer_entity, TRUE, FALSE)  # filter for input Customer ID and Entity ID
    
    
    ## hospital comparison filters
    if(!is.null(input$region)){
      main_df$c1 <- ifelse(main_df$region %in% input$region, TRUE, FALSE)        # filter for hospital region
    } else {
      main_df$c1 <- TRUE
    }
    if(!is.null(input$size)){
      main_df$c2 <- ifelse(main_df$beds %in% input$size, TRUE, FALSE)      # filter for hospital size
    } else {
      main_df$c2 <- TRUE
    }
    if(!is.null(input$specialty)){
      main_df$c3 <- ifelse(main_df$specialty %in% input$specialty, TRUE, FALSE)  # filter for hospital specialty
    } else {
      main_df$c3 <- TRUE
    }
    # filter for specific hospital inputs
    if(!is.null(input$customer_entity_benchmark)){
      main_df$c4 <- ifelse(main_df$customer_entity %in% input$customer_entity_benchmark, TRUE, FALSE)
    } else {
      main_df$c4 <- TRUE
    }
    # if only select one of the two options for input costmodel, then it's standard or non-standard
    if(length(input$costmodel) == 1){
      if("standard" %in% input$costmodel){
        main_df$c_costmodel <- ifelse(main_df$isstratastandardcost == "Y", TRUE, FALSE)
      } else if("non" %in% input$costmodel){
        main_df$c_costmodel <- ifelse(main_df$isstratastandardcost == "N", TRUE, FALSE)
      }
    } 
    # if select none or both of the two options for input cost model, then it's all of them
    else {
      main_df$c_costmodel <- TRUE
    }
    # master hospital benchmark filter
    # if only input customers/entities to benchmark against, only use that column to filter
    if(all(is.null(input$region), is.null(input$size), is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      main_df$c5 <- ifelse(main_df$c4, TRUE, FALSE)
    } 
    # if input region/size/specialty filters, but not customer entity filters, then only use those filters
    else if(any(!is.null(input$region), !is.null(input$size), !is.null(input$specialty)) & is.null(input$customer_entity_benchmark)){
      main_df$c5 <- ifelse(main_df$c1 & main_df$c2 & main_df$c3, TRUE, FALSE)
    }
    # if input region/size/specialty filters and customer entity filters, then
    else if(any(!is.null(input$region), !is.null(input$size), !is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      main_df$c5 <- ifelse((main_df$c1 & main_df$c2 & main_df$c3) | main_df$c4, TRUE, FALSE)
    }
    # if none selected; then else
    else {
      main_df$c5 <- TRUE
    }
    
    
    ## benchmark filters
    if(!is.null(input$ROM)){
      main_df$m2 <- ifelse(main_df$rom %in% input$ROM, TRUE, FALSE)  # filter ROM
    } else {
      main_df$m2 <- TRUE
    }
    if(!is.null(input$SOI)){
      main_df$m3 <- ifelse(main_df$soi %in% input$SOI, TRUE, FALSE)  # filter SOI
    } else {
      main_df$m3 <- TRUE
    }
    if(!is.null(input$age)){
      main_df$m4 <- ifelse(main_df$agebucket %in% input$age, TRUE, FALSE)  # filter patient age buckets
    } else {
      main_df$m4 <- TRUE
    }
    if(!is.null(input$patienttype)){
      main_df$m5 <- ifelse(main_df$patienttyperollup %in% input$patienttype, TRUE, FALSE)  # filter patient types
    } else {
      main_df$m5 <- TRUE
    }
    if(!is.null(input$dischargestatus)){
      main_df$m6 <- ifelse(main_df$dischargestatusgroup %in% input$dischargestatus, TRUE, FALSE)  # filter patient discharge statuses
    } else {
      main_df$m6 <- TRUE
    }
    ## cost filters
    if(length(input$costs) > 0){ 
      main_df$temp1 <- ifelse(main_df$fixedvariable %in% input$costs, 1, 0)   # if filtering Fixed/Variable costs, mark with 1
      main_df$temp2 <- ifelse(main_df$directindirect %in% input$costs, 1, 0)  # if filtering Direct/Indirect costs, mark with 1
      main_df$temp3 <- ifelse(main_df$costdriver %in% input$costs, 1, 0)      # if filtering CostDrivers, mark with 1
      main_df$temp_all <- main_df$temp1 + main_df$temp2 + main_df$temp3       # create column with sum of all cost filters (min 0, max 3)
      if(max(main_df$temp_all) == 1){
        main_df$m7<- ifelse(main_df$temp_all == 1, TRUE, FALSE)   # filtering by 1 of the 3 cost column filters
      }
      if(max(main_df$temp_all) == 2){
        main_df$m7 <- ifelse(main_df$temp_all == 2, TRUE, FALSE)  # filtering by 2 of the 3 cost column filters
      }
      if(max(main_df$temp_all) == 3){
        main_df$m7 <- ifelse(main_df$temp_all == 3, TRUE, FALSE)  # filtering by 3 of the 3 cost column filters
      }
    } else {
      main_df$m7 <- TRUE
    }
    ## hospital-acquired quality incident filters
    # if only one quality incident filter selected
    if(length(input$qltyincidents) == 1){                     
      # if only have "Only Keep Encounters without Hospital-Acquired Quality Incidents"
      if(min(input$qltyincidents) == "Remove"){
        main_df$m8 <- ifelse(main_df$hospitalacqcondition == "0", TRUE, FALSE)  # filter out hospital-acquired conditions/hospital-caused quality incidents
      } 
      # if only have "Only Keep Encounters with Hospital-Acquired Quality Incidents"
      else if(min(input$qltyincidents) == "Keep"){
        main_df$m8 <- ifelse(main_df$hospitalacqcondition == "1", TRUE, FALSE)  # filter out NON hospital-acquired conditions/hospital-caused quality incidents
      }
    }
    # if both selected, or neither selected, keep both
    else if(is.null(input$qltyincidents) | length(input$qltyincidents) == 2){
      main_df$m8 <- TRUE
    }
    
    
    ## set conditions for filtering
    hospital_conditions <- c((main_df$c5 & main_df$c_costmodel) | main_df$h1)  # filters for "Me" and "Baseline"
    filter_conditions <- c(main_df$m1 & main_df$m2 & main_df$m3 & main_df$m4 & main_df$m5 & main_df$m6 & main_df$m7 & main_df$m8)  # parameter filters
    
    
    ## filter data frame
    main_df <- main_df[hospital_conditions & filter_conditions,] %>%
      mutate(Group = factor(ifelse(h1, "Me", "Baseline"),  # create variable to facet Baseline vs. Hospital to Compare (Me)
                            levels = c("Baseline", "Me"),
                            ordered = TRUE),
             Name = "Benchmark")
    
    ## check to see if there's still data after filtering
    validate(
      need(nrow(main_df) > 0, "There data has zero rows due to filtering. Please adjust your filters.")
    )
    
    ## add column for benchmark breakdowns; if multiple benchmark breakdowns selected, concatenate columns with "&" in between columns
    if(!is.null(input$benchmarkbreakdowns)){
      # initialize empty variables to indicate keep/remove ROM/SOI for custom grouping option
      remove_ROM <- c()
      keep_ROM <- c()
      remove_SOI <- c()
      keep_SOI <- c()
      
      # grab all of the possible groupings
      all_groupings <- c(input$benchmarkbreakdowns, keep_ROM, keep_SOI)
      # remove unwanted groupings if specified
      keep_benchmarkgroups <- setdiff(all_groupings, c(remove_ROM, remove_SOI))
      
      # coalesce all of the grouping column values into one column for axis naming purposes
      main_df <- tidyr::unite_(main_df, "BenchmarkGrouping", keep_benchmarkgroups, sep = " & ", remove = FALSE)
    } else {
      main_df$BenchmarkGrouping <- main_df$msdrggroup  # if no breakdowns selected, use MSDRGGROUP as default y-axis
    }
    ## add column for cost breakdowns; if multiple cost breakdowns selected, concatenate columns with "&" in between columns
    if(!is.null(input$costbreakdowns)){
      main_df <- tidyr::unite_(main_df, "CostGrouping", input$costbreakdowns, sep = " & ", remove = FALSE)
    } else {
      main_df$CostGrouping <- NA  # if no cost breakdowns selected, default is NA
    }
    
    ## group data based off benchmark breakdowns and cost breakdowns
    # if inputs for both benchmark and cost breakdowns
    if(!is.null(input$benchmarkbreakdowns) & !is.null(input$costbreakdowns)){
      groupings <- c("Name", "region", "beds", "specialty", "customerid", "entityid", "Group", "msdrggroup", "lengthofstay",
                     "CostGrouping", "BenchmarkGrouping", "encounterid", keep_benchmarkgroups, input$costbreakdowns)
      outlier_groupings <- c("Name", "Group", "msdrggroup", "CostGrouping", "BenchmarkGrouping", keep_benchmarkgroups, input$costbreakdowns)
    }
    # if inputs for only benchmark breakdowns
    if(!is.null(input$benchmarkbreakdowns) & is.null(input$costbreakdowns)){
      groupings <- c("Name", "region", "beds", "specialty", "customerid", "entityid", "Group", "msdrggroup", "lengthofstay",
                     "CostGrouping", "BenchmarkGrouping", "encounterid", keep_benchmarkgroups)
      outlier_groupings <- c("Name", "Group", "msdrggroup", "CostGrouping", "BenchmarkGrouping", keep_benchmarkgroups)
    }
    # if inputs for only cost breakdowns
    if(!is.null(input$costbreakdowns) & is.null(input$benchmarkbreakdowns)){
      groupings <- c("Name", "region", "beds", "specialty", "customerid", "entityid", "Group", "msdrggroup", "lengthofstay",
                     "CostGrouping", "BenchmarkGrouping", "encounterid", input$costbreakdowns)
      outlier_groupings <- c("Name", "Group", "msdrggroup", "CostGrouping", "BenchmarkGrouping", input$costbreakdowns)
    }
    # if no inputs for both benchmark and cost breakdowns
    if(is.null(input$costbreakdowns) & is.null(input$benchmarkbreakdowns)){
      groupings <- c("Name", "region", "beds", "specialty", "customerid", "entityid", "Group", "msdrggroup", "lengthofstay",
                     "CostGrouping", "BenchmarkGrouping", "encounterid")
      outlier_groupings <- c("Name", "Group", "msdrggroup", "CostGrouping", "BenchmarkGrouping")
    }
    
    
    ## group by input groupings and re-calculate so that data is at the most granular grouping specified by the user
    # if no grouping parameters specified (e.g. no benchmark or cost breakdowns), most granular level is Encounter level
    main_df <- main_df %>%
      group_by(.dots = groupings) %>%
      summarise(costs = sum(costs)) %>% ungroup()
    
    
    ## remove length of stay outliers if selected
    if(TRUE %in% (grepl("LOS", input$otherfilteroptions))){
      # grab current column names of main df; will use these later to select original columns 
      # (to avoid duplicate column name issues if user selects to remove both LOS and cost outliers)
      save <- colnames(main_df)
      
      # calculate LOS summary statistics and outlier cutoffs based off IQR and standard deviation
      LOS_filters <- main_df %>%
        calcSummary(df = ., summary_var = "lengthofstay", outlier_threshold = 2, grouping_vars = outlier_groupings)
      
      # join summary statistics and outlier cutoffs to main df
      main_df <- main_df %>%
        left_join(LOS_filters, by = outlier_groupings)
      
      # remove LOS IQR outliers if selected
      if("LOS_IQR" %in% input$otherfilteroptions){
        main_df$o1_los <- case_when(
          main_df$obs == 1 ~ TRUE,  # if only one observation, keep (can't be an outlier if you're solo)
          main_df$lengthofstay > main_df$IQR_outlier_high | main_df$lengthofstay < main_df$IQR_outlier_low ~ FALSE,  # IQR outliers
          TRUE ~ TRUE)  # keep non-outliers
        
      } 
      else {
        main_df$o1_los <- TRUE
      }
      # remove LOS standard deviation outliers if selected
      if("LOS_SD" %in% input$otherfilteroptions){
        main_df$o2_los <- case_when(
          main_df$obs == 1 ~ TRUE,  # if only one observation, keep (can't be an outlier if you're solo)
          main_df$lengthofstay > main_df$IQR_outlier_high | main_df$lengthofstay < main_df$IQR_outlier_low ~ FALSE,  # IQR outliers
          TRUE ~ TRUE)  # keep non-outliers
      }
      else {
        main_df$o2_los <- TRUE
      }
      
      # remove LOS outliers
      main_df <- main_df[c(main_df$o1_los & main_df$o2_los), save]
    }
    
    
    ## remove cost outliers if selected
    if(TRUE %in% (grepl("cost", input$otherfilteroptions))){
      # grab current column names of main df; will use these later to select original columns 
      # (to avoid duplicate column name issues if user selects to remove both LOS and cost outliers)
      save <- colnames(main_df)
      
      # calculate LOS summary statistics and outlier cutoffs based off IQR and standard deviation
      cost_filters <- main_df %>%
        calcSummary(df = ., summary_var = "costs", outlier_threshold = 2, grouping_vars = outlier_groupings)
      
      # join summary statistics and outlier cutoffs to main df
      main_df <- main_df %>%
        left_join(cost_filters, by = outlier_groupings)
      
      # remove cost IQR outliers if selected
      if("cost_IQR" %in% input$otherfilteroptions){
        main_df$o1_cost <- case_when(
          main_df$obs == 1 ~ TRUE,  # if only one observation, keep (can't be an outlier if you're solo)
          main_df$costs > main_df$IQR_outlier_high | main_df$costs < main_df$IQR_outlier_low ~ FALSE,  # IQR outliers
          TRUE ~ TRUE)  # keep non-outliers
      } 
      else {
        main_df$o1_cost <- TRUE
      }
      # remove cost standard deviation outliers if selected
      if("cost_SD" %in% input$otherfilteroptions){
        main_df$o2_cost <- case_when(
          main_df$obs == 1 ~ TRUE,  # if only one observation, keep (can't be an outlier if you're solo)
          main_df$costs > main_df$sd_outlier_high | main_df$costs < main_df$sd_outlier_low ~ FALSE,  # standard deviation outliers
          TRUE ~ TRUE)  # keep non-outliers
      }
      else {
        main_df$o2_cost <- TRUE
      }
      
      # remove cost outliers
      main_df <- main_df[c(main_df$o1_cost & main_df$o2_cost), save]
    }
    
    
    ## check to see if there's still data after filtering for outliers
    validate(
      need(nrow(main_df) > 0, "The data has zero rows due to outlier filtering. Please adjust your filters.")
    )
    
    return(main_df)
  })
  
  
  ## -----------<< summary_df_benchmark >>-----------
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
    
    summary_df_benchmark <- main_df() %>%
      filter(Group == "Baseline") %>%
      calcSummary(df = ., summary_var = "costs", outlier_threshold = 2, grouping_vars = groups)
    
    ## check to see there's still data to benchmark against after filtering main_df for just the baseline data
    validate(
      need(nrow(summary_df_benchmark) > 0, "There is no baseline data due to filtering (i.e. there is no data for the 'Baseline'). Please adjust your data filters.")
    )
    
    return(summary_df_benchmark)
  })
  
  
  ## -----------<< summary_df_me >>-----------
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
    
    summary_df_me <- main_df() %>%
      filter(Group == "Me") %>%
      calcSummary(df = ., summary_var = "costs", outlier_threshold = 2, grouping_vars = groups)
    
    ## check to see there's still data to benchmark after filtering main_df for just the "Me" data
    validate(
      need(nrow(summary_df_me) > 0, "There is no data to benchmark due to filtering (i.e. there is no data for 'Me'). Please adjust your data filters.")
    )
    
    return(summary_df_me)
  })
  
  
  ## -----------<< compare_df >>-----------
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
  
  
  ## -----------< Reactive Plotting >-----------
  
  ## -----------<< Patient/Benchmark Comparison Plots >>-----------
  
  ## -----------<<< MSDRG Volume Distribution >>>-----------
  msdrg_plot <- eventReactive(input$hospital_refresh, {
    hospital_df <- hospital_df()
    hospital_df <- hospital_df %>%
      group_by(Group, msdrggroup, customer_entity) %>%
      summarise(Count = sum(Count)) %>% ungroup() %>%
      mutate(msdrggroup = str_wrap(labelMSDRG(msdrggroup, values = TRUE), width = 20))
    
    msdrg_plot <- ggplot() +
      geom_vline(data = hospital_df[hospital_df$Group == "Me", ],
                 aes(xintercept = Count, color = Group, fill = Group), size = 3, alpha = 0.75) +
      geom_dotplot(data = hospital_df[hospital_df$Group == "Baseline", ],
                   aes(x = Count, fill = Group, color = Group), dotsize = 1) +
      scale_fill_manual(values = c("Baseline" = "#1f78b4",    # blue
                                   "Me" = "#ff7f00"),         # orange
                        name = "") +
      scale_color_manual(values = c("Baseline" = "#1f78b4",    # blue
                                    "Me" = "#ff7f00"),         # orange
                         guide = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap("msdrggroup") +
      labs(x = "# of Encounters",
           y = "# of Benchmark Institutions") +
      theme(axis.text.y = element_blank(),
            axis.ticks.x = element_blank())
    
    
    return(msdrg_plot)
  })
  
  ## -----------<<< SOI Distribution >>>-----------
  soi_plot <- eventReactive(input$hospital_refresh, {
    hospital_df <- hospital_df()
    hospital_df <- hospital_df %>%
      filter(!is.na(MSDRG_benchmark)) %>%
      group_by(Group, soi, customer_entity) %>%
      summarise(Count = sum(Count)) %>% ungroup()
    
    soi_plot <- ggplot() +
      geom_vline(data = hospital_df[hospital_df$Group == "Me", ],
                 aes(xintercept = Count, color = Group, fill = Group), size = 3, alpha = 0.75) +
      geom_dotplot(data = hospital_df[hospital_df$Group == "Baseline", ],
                   aes(x = Count, fill = Group, color = Group), dotsize = 1) +
      scale_fill_manual(values = c("Baseline" = "#1f78b4",    # blue
                                   "Me" = "#ff7f00"),         # orange
                        name = "") +
      scale_color_manual(values = c("Baseline" = "#1f78b4",    # blue
                                    "Me" = "#ff7f00"),         # orange
                         guide = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap("soi") +
      labs(x = "# of Encounters",
           y = "# of Benchmark Institutions") +
      theme(axis.text.y = element_blank(),
            axis.ticks.x = element_blank())
    
    return(soi_plot)
    
  })
  
  ## -----------<<< ROM Distribution >>>-----------
  rom_plot <- eventReactive(input$hospital_refresh, {
    hospital_df <- hospital_df()
    hospital_df <- hospital_df %>%
      filter(!is.na(MSDRG_benchmark)) %>%
      group_by(Group, rom, customer_entity) %>%
      summarise(Count = sum(Count)) %>% ungroup()
    
    rom_plot <- ggplot() +
      geom_vline(data = hospital_df[hospital_df$Group == "Me", ],
                 aes(xintercept = Count, color = Group, fill = Group), size = 3, alpha = 0.75) +
      geom_dotplot(data = hospital_df[hospital_df$Group == "Baseline", ],
                   aes(x = Count, fill = Group, color = Group), dotsize = 1) +
      scale_fill_manual(values = c("Baseline" = "#1f78b4",    # blue
                                   "Me" = "#ff7f00"),         # orange
                        name = "") +
      scale_color_manual(values = c("Baseline" = "#1f78b4",    # blue
                                    "Me" = "#ff7f00"),         # orange
                         guide = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap("rom") +
      labs(x = "# of Encounters",
           y = "# of Benchmark Institutions") +
      theme(axis.text.y = element_blank(),
            axis.ticks.x = element_blank())
    
    return(rom_plot)
    
  })
  
  ## -----------<<< Patient Age Distribution >>>-----------
  age_plot <- eventReactive(input$hospital_refresh, {
    hospital_df <- hospital_df()
    hospital_df <- hospital_df %>%
      filter(!is.na(MSDRG_benchmark)) %>%
      group_by(Group, agebucket, customer_entity) %>%
      summarise(Count = sum(Count)) %>% ungroup() %>%
      mutate(agebucket = case_when(agebucket == "Infant" ~ "Infant (less than 1 yr)",
                                   agebucket == "Pediatric" ~ "Pediatric (1 yr - 17 yrs)",
                                   agebucket == "Adult" ~ "Adult (18 yrs - 64 yrs)",
                                   agebucket == "Senior" ~ "Senior (65 years or older)"))
    
    age_plot <- ggplot() +
      geom_vline(data = hospital_df[hospital_df$Group == "Me", ],
                 aes(xintercept = Count, color = Group, fill = Group), size = 3, alpha = 0.75) +
      geom_dotplot(data = hospital_df[hospital_df$Group == "Baseline", ],
                   aes(x = Count, fill = Group, color = Group), dotsize = 1) +
      scale_fill_manual(values = c("Baseline" = "#1f78b4",    # blue
                                   "Me" = "#ff7f00"),         # orange
                        name = "") +
      scale_color_manual(values = c("Baseline" = "#1f78b4",    # blue
                                    "Me" = "#ff7f00"),         # orange
                         guide = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap("agebucket") +
      labs(x = "# of Encounters",
           y = "# of Benchmark Institutions") +
      theme(axis.text.y = element_blank(),
            axis.ticks.x = element_blank())
    
    return(age_plot)
    
  })
  
  ## -----------<<< Patient Type Distribution >>>-----------
  type_plot <- eventReactive(input$hospital_refresh, {
    hospital_df <- hospital_df()
    hospital_df <- hospital_df %>%
      filter(!is.na(MSDRG_benchmark)) %>%
      group_by(Group, patienttyperollup, customer_entity) %>%
      summarise(Count = sum(Count)) %>% ungroup()
    
    type_plot <- ggplot() +
      geom_vline(data = hospital_df[hospital_df$Group == "Me", ],
                 aes(xintercept = Count, color = Group, fill = Group), size = 3, alpha = 0.75) +
      geom_dotplot(data = hospital_df[hospital_df$Group == "Baseline", ],
                   aes(x = Count, fill = Group, color = Group), dotsize = 1) +
      scale_fill_manual(values = c("Baseline" = "#1f78b4",    # blue
                                   "Me" = "#ff7f00"),         # orange
                        name = "") +
      scale_color_manual(values = c("Baseline" = "#1f78b4",    # blue
                                    "Me" = "#ff7f00"),         # orange
                         guide = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap("patienttyperollup") +
      labs(x = "# of Encounters",
           y = "# of Benchmark Institutions") +
      theme(axis.text.y = element_blank(),
            axis.ticks.x = element_blank())
    
    return(type_plot)
  })
  
  ## -----------<<< Patient Discharge Status Distribution >>>-----------
  discharge_plot <- eventReactive(input$hospital_refresh, {
    hospital_df <- hospital_df()
    hospital_df <- hospital_df %>%
      filter(!is.na(MSDRG_benchmark)) %>%
      group_by(Group, dischargestatusgroup, customer_entity) %>%
      summarise(Count = sum(Count)) %>% ungroup() %>%
      mutate(dischargestatusgroup = ifelse(dischargestatusgroup == "Still a Patient", "Inhouse", dischargestatusgroup))
    
    
    discharge_plot <- ggplot() +
      geom_vline(data = hospital_df[hospital_df$Group == "Me", ],
                 aes(xintercept = Count, color = Group, fill = Group), size = 3, alpha = 0.75) +
      geom_dotplot(data = hospital_df[hospital_df$Group == "Baseline", ],
                   aes(x = Count, fill = Group, color = Group), dotsize = 1) +
      scale_fill_manual(values = c("Baseline" = "#1f78b4",    # blue
                                   "Me" = "#ff7f00"),         # orange
                        name = "") +
      scale_color_manual(values = c("Baseline" = "#1f78b4",    # blue
                                    "Me" = "#ff7f00"),         # orange
                         guide = FALSE) +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap("dischargestatusgroup") +
      labs(x = "# of Encounters",
           y = "# of Benchmark Institutions") +
      theme(axis.text.y = element_blank(),
            axis.ticks.x = element_blank())
    
    return(discharge_plot)
  })
  
  ## -----------<< Benchmark Plot >>-----------
  plot <- eventReactive(input$refresh, {
    
    ## grab all reactive data frames
    main_df <- main_df()
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
    gg <- ggplot(main_df) +
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
      labs(title = paste0("MS-DRG: ", 
                          case_when(input$MSDRG == "CABG" ~ "Coronary Bypass (CABG)",
                                    input$MSDRG == "AMI" ~ "Acute Myocardial Infarction (AMI)",
                                    input$MSDRG == "THA TKA" ~ "Major Joint Replacement Or Reattachment Of Lower Extremity (THA TKA)",
                                    input$MSDRG == "Hip Fracture" ~ "Hip and Femur Procedures (Hip Fracture)")))
    
    
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
  
  ## -----------<< Cost Savings Plot >>-----------
  costsavings_plot <- eventReactive(input$view_opportunities, {
    df <- hospital_df()
    
    df <- df %>%
      group_by(msdrggroup, Group) %>%
      summarise(MedianCost = median(costs),
                N = sum(Count)) %>% ungroup()
    df <- data.table::dcast(setDT(df), msdrggroup ~ Group, value.var = c("MedianCost", "N")) %>%
      mutate(MedianCost_Diff = MedianCost_Me - MedianCost_Baseline,
             N_Diff = N_Me - N_Baseline,
             Impact = N_Me * MedianCost_Diff,
             Direction = ifelse(Impact < 0, "Below the Benchmark", "Cost Savings Opportunity"),
             msdrggroup = labelMSDRG(msdrggroup, values = TRUE)) %>%
      filter(!is.na(MedianCost_Diff))
    
    df$msdrggroup <- factor(df$msdrggroup, levels = df$msdrggroup[order(df$Impact)], ordered = TRUE)
    
    costsavings_plot <- ggplot(df) +
      geom_bar(aes(x = msdrggroup, y = Impact, fill = Direction),
               stat = 'identity', width = .95) +
      # line at 0 mark
      geom_hline(color = 'black', yintercept = 0) +  
      # lines that separate different groupings
      # remove first value in sequence (0.5) because don't want one between panel border and first plot
      geom_vline(xintercept = seq(from = 0.5, to = length(unique(df[["msdrggroup"]]))-0.5, by = 1)[-1], 
                 color = "black") +
      geom_text(aes(label = ifelse(Impact >= 0,
                                   paste0("Potential Savings: ", scales::dollar(Impact), "\n# of Encounters: ", N_Me, "\nCost Difference per Encounter: ", scales::dollar(MedianCost_Diff)),
                                   paste0("Current Savings: ", scales::dollar(Impact), "\n# of Encounters: ", N_Me, "\nCost Difference per Encounter: ", scales::dollar(MedianCost_Diff))),
                    x = msdrggroup,
                    y = case_when(is.na(MedianCost_Diff) ~ 0,   # if NA because no comparisons, put it at zero and should have "NA" label
                                  Impact >= 0 ~ 0.15*(min(abs(coalesce(df$Impact, 0)))),
                                  Impact < 0 ~ -0.15*(min(abs(coalesce(df$Impact, 0))))),
                    fontface = "bold"), size = 5, hjust = ifelse(df$Impact >= 0, 0, 1)) + 
      scale_fill_manual(values = c("Below the Benchmark" = "#dadaeb",        # light purple
                                   "Cost Savings Opportunity" = "#807dba"),  # darker purple
                        guide = FALSE) +
      scale_y_continuous(name = paste0("Cost Difference between Me and the Benchmark\n(My Cost per Encounter - Benchmark Cost per Encounter) * My Volume of Encounters"),
                         labels = scales::dollar,
                         expand = c(0.8, 0.8)
      ) +
      coord_flip() +
      labs(x = "MSDRG")
    
    return(costsavings_plot)
    
  })
  
  ## -----------< Reactive Tables >-----------
  # table for comparison hospitals / benchmark
  benchmark_table <- eventReactive(input$refresh, {
    # initialize empty variables to indicate keep/remove ROM/SOI for custom grouping option
    remove_ROM <- c()
    keep_ROM <- c()
    remove_SOI <- c()
    keep_SOI <- c()
    
    # grab all of the possible groupings
    all_groupings <- c(input$benchmarkbreakdowns, keep_ROM, keep_SOI)
    # remove unwanted groupings if specified
    keep_benchmarkgroups <- setdiff(all_groupings, c(remove_ROM, remove_SOI))
    
    benchmark_df <- summary_df_benchmark()
    # only select grouping parameters, median, mean, and number of observations
    select <- c(keep_benchmarkgroups, input$costbreakdowns, "median", "mean", "obs")
    benchmark_df <- benchmark_df[,select]
    # rename columns
    colnames(benchmark_df) <- c(keep_benchmarkgroups, input$costbreakdowns, "Median", "Mean", "N")
    
    return(benchmark_df)
  })
  
  # table for hospital to benchmark / "Me"
  me_table <- eventReactive(input$refresh, {
    # initialize empty variables to indicate keep/remove ROM/SOI for custom grouping option
    remove_ROM <- c()
    keep_ROM <- c()
    remove_SOI <- c()
    keep_SOI <- c()
    
    # grab all of the possible groupings
    all_groupings <- c(input$benchmarkbreakdowns, keep_ROM, keep_SOI)
    # remove unwanted groupings if specified
    keep_benchmarkgroups <- setdiff(all_groupings, c(remove_ROM, remove_SOI))
    
    me_df <- summary_df_me()
    # only select grouping parameters, median, mean, and number of observations
    select <- c(keep_benchmarkgroups, input$costbreakdowns, "median", "mean", "obs")
    me_df <- me_df[,select]
    # rename columns
    colnames(me_df) <- c(keep_benchmarkgroups, input$costbreakdowns, "Median", "Mean", "N")
    
    return(me_df)
  })
  
  # table with comparison information between benchmarks and "Me"
  compare_table <- eventReactive(input$refresh, {
    # initialize empty variables to indicate keep/remove ROM/SOI for custom grouping option
    remove_ROM <- c()
    keep_ROM <- c()
    remove_SOI <- c()
    keep_SOI <- c()
    
    # grab all of the possible groupings
    all_groupings <- c(input$benchmarkbreakdowns, keep_ROM, keep_SOI)
    # remove unwanted groupings if specified
    keep_benchmarkgroups <- setdiff(all_groupings, c(remove_ROM, remove_SOI))
    
    compare_df <- compare_df() %>%
      mutate(proport_diff_median = proport_diff_median*100,  # calculate % diff in medians
             proport_diff_mean = proport_diff_mean*100)       # calculate % diff in means
    # only select grouping parameters, difference in medians, % difference in medians, difference in means, and % difference in means
    select <- c(keep_benchmarkgroups, input$costbreakdowns, "diff_median", "proport_diff_median", "diff_mean", "proport_diff_mean")
    compare_df <- compare_df[,select]
    # rename columns
    colnames(compare_df) <- c(keep_benchmarkgroups, input$costbreakdowns, 
                              "Difference in Medians", "% Difference in Median",  "Difference in Means", "% Difference in Mean")
    return(compare_df)
  })
  
  
  ## -----------< Stable Outputs >-----------
  
  output$msdrg_plot <- renderPlot({
    msdrg_plot()
  })
  
  output$rom_plot <- renderPlot({
    rom_plot()
  })
  
  output$soi_plot <- renderPlot({
    soi_plot()
  })
  
  output$age_plot <- renderPlot({
    age_plot()
  })
  
  output$type_plot <- renderPlot({
    type_plot()
  })
  
  output$discharge_plot <- renderPlot({
    discharge_plot()
  })
  
  
  output$plotbrush_output <- renderText({
    # if haven't created benchmark, can't select points so output empty string
    if(input$hospital_refresh == FALSE){
      out <- ""
    }
    
    # if created benchmark, can start selecting points
    else {
      df <- hospital_df()
      
      if(any(!is.null(input$msdrg_plotbrush), !is.null(input$rom_plotbrush), !is.null(input$soi_plotbrush),
             !is.null(input$age_plotbrush), !is.null(input$type_plotbrush), !is.null(input$discharge_plotbrush))){
        
        out_msdrg <- c()
        out_rom <- c()
        out_soi <- c()
        out_age <- c()
        out_type <- c()
        out_discharge <- c()
        
        # if brushed over MSDRG distribution plot
        if(!is.null(input$msdrg_plotbrush)){
          df1 <- df %>%
            filter(Group == "Baseline") %>%
            group_by(Group, msdrggroup, customer_entity) %>%
            summarise(Count = sum(Count)) %>% ungroup() %>%
            mutate(msdrggroup = str_wrap(labelMSDRG(msdrggroup, values = TRUE), width = 20))
          
          out_msdrg <- brushedPoints(df = df1, brush = input$msdrg_plotbrush, xvar = "Count")$customer_entity
        } 
        # if brushed over ROM distribution plot
        if(!is.null(input$rom_plotbrush)){
          df2 <- df %>%
            filter(Group == "Baseline") %>%
            filter(!is.na(MSDRG_benchmark)) %>%
            group_by(Group, rom, customer_entity) %>%
            summarise(Count = sum(Count)) %>% ungroup()
          
          out_rom <- brushedPoints(df = df2, brush = input$rom_plotbrush, xvar = "Count")$customer_entity
        }
        # if brushed over SOI distribution plot
        if(!is.null(input$soi_plotbrush)){
          df3 <- df %>%
            filter(Group == "Baseline") %>%
            filter(!is.na(MSDRG_benchmark)) %>%
            group_by(Group, soi, customer_entity) %>%
            summarise(Count = sum(Count)) %>% ungroup()
          
          out_soi <- brushedPoints(df = df3, brush = input$soi_plotbrush, xvar = "Count")$customer_entity
        }
        # if brushed over age distribution plot
        if(!is.null(input$age_plotbrush)){
          df4 <- df %>%
            filter(Group == "Baseline") %>%
            filter(!is.na(MSDRG_benchmark)) %>%
            group_by(Group, agebucket, customer_entity) %>%
            summarise(Count = sum(Count)) %>% ungroup() %>%
            mutate(agebucket = case_when(agebucket == "Infant" ~ "Infant (less than 1 yr)",
                                         agebucket == "Pediatric" ~ "Pediatric (1 yr - 17 yrs)",
                                         agebucket == "Adult" ~ "Adult (18 yrs - 64 yrs)",
                                         agebucket == "Senior" ~ "Senior (65 years or older)"))
          
          out_age <- brushedPoints(df = df4, brush = input$age_plotbrush, xvar = "Count")$customer_entity
        }
        # if brushed over patient type distribution plot
        if(!is.null(input$type_plotbrush)){
          df5 <- df %>%
            filter(Group == "Baseline") %>%
            filter(!is.na(MSDRG_benchmark)) %>%
            group_by(Group, patienttyperollup, customer_entity) %>%
            summarise(Count = sum(Count)) %>% ungroup()
          
          out_type <- brushedPoints(df = df5, brush = input$type_plotbrush, xvar = "Count")$customer_entity
        }
        # if brushed over patient discharge status distribution plot
        if(!is.null(input$discharge_plotbrush)){
          df6 <- df %>%
            filter(Group == "Baseline") %>%
            filter(!is.na(MSDRG_benchmark)) %>%
            group_by(Group, dischargestatusgroup, customer_entity) %>%
            summarise(Count = sum(Count)) %>% ungroup() %>%
            mutate(dischargestatusgroup = ifelse(dischargestatusgroup == "Still a Patient", "Inhouse", dischargestatusgroup))
          
          out_discharge <- brushedPoints(df = df6, brush = input$discharge_plotbrush, xvar = "Count")$customer_entity
        }
        
        out <- paste0(unique(c(out_msdrg, out_rom, out_soi, out_age, out_type, out_discharge)), collapse = "<br/>")
      }
      # if all are null
      else {
        out <- ""
      }
    }
    
    return(out)
  })
  
  output$soi_plot <- renderPlot({
    soi_plot()
  })
  
  output$costsavings_plot <- renderPlot({
    costsavings_plot()
  })
  
  # benchmarking plot
  output$plot <- renderPlot({
    plot()
  })
  
  # table for benchmarks
  output$summary_df_benchmark <- renderDataTable({
    benchmark_table()
  })
  
  # table for "Me"
  output$summary_df_me <- renderDataTable({
    me_table()
  })
  
  # table with comparisons between benchmark and "Me"
  output$compare_df <- renderDataTable({
    compare_table()
  })
  
  
  ## -----------< Session >-----------
  session$allowReconnect("force")
}


#### RUN APP #### 
shinyApp(ui = ui, server = server)



