## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018


# SERVER -----------------------------------------------------------------------
shinyServer(function(input, output, session) {

  #  Sidebar logic -------------------------------------------------------------

  # _ I. Populate Dropdowns ----------------------------------------------------

  # updates each dropdown with values from the database, alphabetized
  updateSelectizeInput(session,"customer_entity",
                       choices=hospital_df[order(hospital_df$customerid,hospital_df$entityid),]$customer_entity,
                       server=TRUE)

  output$benchmark_selector = renderUI({
    selectizeInput(inputId="customer_entity_benchmark",
                   label="Select Entity(ies):",
                   choices=c(ALL="",hospital_df[hospital_df$customer_entity!=input$customer_entity]$customer_entity),
                   multiple=TRUE)
  })

  checkboxGroupInput(session,"region",
                       choices=as.factor(hospital_df[order(hospital_df$region),]$region))
  updateSelectizeInput(session,"beds",
                       choices=as.factor(hospital_df[order(hospital_df$beds_fixed),]$beds_fixed),
                       server=TRUE)
  updateSelectizeInput(session,"specialty",
                       choices=as.factor(hospital_df[order(hospital_df$specialty),]$specialty),
                       server=TRUE)
  updateSelectizeInput(session,"costmodel",
                       choices=c("Hospitals with Strata Standardized Cost Models" = "standard",
                                 "Hospitals without Strata Standardized Cost Models" = "non"),
                       server=TRUE)

  # Body logic -------------------------------------------------------------------

  # _ I. Populate Information ----------------------------------------------------

  # _ _ 1. My Entity Info --------------------------------------------------------

  # populate selected entity
  output$selected_entity = renderText({
    paste("<b>Hospital Institution:</b>",input$customer_entity)
  })

  # populate selected entity's region
  output$selected_region = renderText({
    paste("<b>Region:</b>", hospital_df$region[hospital_df$customer_entity == input$customer_entity])
  })

  # populate selected entity's bed size
  output$selected_size = renderText({
    paste("<b>Bed Size:</b>", hospital_df$beds_fixed[hospital_df$customer_entity == input$customer_entity])
  })

  # populate selected entity's specialty
  output$selected_specialty = renderText({
    paste("<b>Specialty:</b>", hospital_df$specialty[hospital_df$customer_entity == input$customer_entity])
  })

  # populate selected entity's cost model classification
  output$selected_costmodel = renderText({
    paste("<b>Cost Model:</b>",
      ifelse(is.null(input$customer_entity),
             "",
             if(hospital_df$isstratastandardcost[hospital_df$customer_entity == input$customer_entity] == "t") {
               "Strata Standard Cost Model"
             }
             else if(hospital_df$isstratastandardcost[hospital_df$customer_entity == input$customer_entity] == "f"){
               "Not Strata Standard Cost Model"
             })
    )
  })

  # _ _ 2. Benchmark Population Info -------------------------------------------

  # populate comparison population entities
  output$comparison_entities = renderText({
      if(length(input$costmodel) == 1 & "standard" %in% input$costmodel){
        df <- hospital_df %>%
          filter(isstratastandardcost == "t")

        paste("<b>Benchmark Institution(s):</b><br/>",
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
        df <- hospital_df %>%
          filter(isstratastandardcost == "f")

        paste("<b>Benchmark Institution(s):</b><br/>",
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
        df <- hospital_df

        paste("<b>Benchmark Institution(s):</b><br/>",
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
                 paste(as.vector(unique(hospital_df$region)),collapse=", "),
                 paste(as.vector(unique(input$region)),collapse=", "))
          )
  })

  # populate comparison population bed sizes
  output$comparison_sizes = renderText({
    paste("<b>Bed Size(s):</b>",
          ifelse(is.null(input$beds),
                 paste(as.vector(unique(hospital_df$beds_fixed)),collapse=", "),
                 paste(as.vector(unique(input$beds)),collapse=", "))
    )
  })

  # populate comparison population specialties
  output$comparison_specialties = renderText({
    paste("<b>Specialty(ies):</b>",
          ifelse(is.null(input$specialty),
                 paste(as.vector(unique(hospital_df$specialty)),collapse=", "),
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


  # _ II. Reactive  Data Refresh -----------------------------------------------

  # _ _ 1. Entity Data Frame ---------------------------------------------------

  # When a user clicks 'refresh' on the BYOBenchmark side bar, the dataframe that is used throughout the application
  #  needs to be refreshed.
  entity_df <- eventReactive(input$refresh, {
    # start with the raw customer data
    entity_df <- full_df


    # _ _ _ a. filter_costdriver ----------------------------------------------
    ### A1. filter selected cost driver
    entity_df$filter_costdriver<-ifelse(entity_df$costdriver==input$costdriver, TRUE, FALSE)

    # _ _ _ b. filter_myEntity ------------------------------------------------
    ### B1. filter selected hospital
    entity_df$filter_myEntity <- ifelse(entity_df$customer_entity==input$customer_entity,TRUE,FALSE)

    # _ _ _ c. Selected Population Filters -----------------------------------
    ### C1. filter_region
    if(!is.null(input$region)){
      entity_df$filter_region<-ifelse(entity_df$region %in% input$region,TRUE,FALSE)
    } else {
      entity_df$filter_region<-TRUE
    }
    ### C2. filter_size
    if(!is.null(input$size)){
      entity_df$filter_size<-ifelse(entity_df$beds_fixed %in% input$size,TRUE,FALSE)
    } else {
      entity_df$filter_size<-TRUE
    }
    ### C3. filter_specialty
    if(!is.null(input$specialty)){
      entity_df$filter_specialty<-ifelse(entity_df$specialty %in% input$specialty,TRUE,FALSE)
    } else {
      entity_df$filter_specialty<-TRUE
    }
    ### C4. filter_costmodel
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
    ### C5. hospital benchmark filter (filter_entities)
    # if the only input is customers/entities, then only use that column to filter
    if(all(is.null(input$region),is.null(input$size),is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      entity_df$filter_entities<-ifelse(entity_df$filter_customer_entity, TRUE, FALSE)
    }
    # if there are input filters, but no customer entity filters, then only use input filters
    else if(any(!is.null(input$region),!is.null(input$size),!is.null(input$specialty)) & !is.null(input$customer_entity_benchmark))
    {
      entity_df$filter_entities<-ifelse(entity_df$filter_region & entity_df$filter_size & entity_df$specialty, TRUE, FALSE)
    }
    # if there are input filters and customer entity filters, then
    else if(any(!is.null(input$region),!is.null(input$size),!is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      entity_df$filter_entities<-ifelse((entity_df$filter_region & entity_df$size & entity_df$specialty) | entity_df$filter_customer_entity, TRUE, FALSE)
    }
    # if no filter is selected, then
    else {
      entity_df$filter_entities<-TRUE
    }

    # _ _ _ d. Complete Filtering ---------------------------------------------
    entity_df<-entity_df %>%
      filter(filter_myEntity | (filter_costmodel)) %>%
      mutate("Group" = ifelse(filter_myEntity,"Me","Baseline"),
             "CostDriver_Benchmark"= ifelse(filter_costdriver,costdriver,NA)) %>%
      group_by(region, beds, specialty, customer_entity, costdriver,
               encounterid, rom, soi, agebucket, patienttyperollup,
               dischargestatusgroup, los_ubrev_group, imaging_ubrev_group,
               CostDriver_Benchmark) %>%
      summarize("Count"=1,
                "costs" = sum(costs)) %>%
      ungroup()
    return(entity_df)
  })

  # _ _ 2. Encounter Data Frame ---------------------------------------------
  # Encounter level data frame with benchmark grouping columns and cost grouping columns as well as
  # columns with cost information;
  #
  # This code filters the full dataframe of all cost data, based off user inputs about how to filter the data
  encounter_df<-eventReactive(input$refresh, {
    encounter_df <- full_df

    # _ _ _ a. Selected Cost Driver Filter ------------------------------------
    encounter_df$filter_costdriver<-ifelse(encounter_df$costdriver==input$costdriver, TRUE, FALSE)

    # _ _ _ b. Selected Entity Filter -----------------------------------------
    ### 1. filter selected hospital
    encounter_df$filter_myEntity <- ifelse(encounter_df$customer_entity==input$customer_entity,TRUE,FALSE)

    # _ _ _ c. Selected Population Filters -----------------------------------
    ### 1. hospital region filter
    if(!is.null(input$region)){
      encounter_df$filter_region<-ifelse(encounter_df$region %in% input$region,TRUE,FALSE)
    } else {
      encounter_df$filter_region<-TRUE
    }
    ### 2. hospital size filter
    if(!is.null(input$size)){
      encounter_df$filter_size<-ifelse(encounter_df$beds_fixed %in% input$size,TRUE,FALSE)
    } else {
      encounter_df$filter_size<-TRUE
    }
    ### 3. hospital specialty filter
    if(!is.null(input$specialty)){
      encounter_df$filter_specialty<-ifelse(encounter_df$specialty %in% input$specialty,TRUE,FALSE)
    } else {
      encounter_df$filter_specialty<-TRUE
    }
    ### 4. hospital entities filters
    if(!is.null(input$customer_entity_benchmark)){
      encounter_df$filter_customer_entity<-ifelse(encounter_df$customer_entity %in% input$customer_entity_benchmark, TRUE, FALSE)
    } else {
      encounter_df$filter_customer_entity<-TRUE
    }
    ### 5. hospital cost model filter
    if(length(input$costmodel) == 1){
      if("standard" %in% input$costmodel){
        encounter_df$filter_costmodel <- ifelse(encounter_df$isstratastandardcost == "t", TRUE, FALSE)
      } else if("non" %in% input$costmodel){
        encounter_df$filter_costmodel <- ifelse(encounter_df$isstratastandardcost == "f", TRUE, FALSE)
      }
    }
    else {
      encounter_df$filter_costmodel <- TRUE
    }
    # hospital benchmark filter
    # if the only input is customers/entities, then only use that column to filter
    if(all(is.null(input$region),is.null(input$size),is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      encounter_df$filter_entities<-ifelse(encounter_df$filter_customer_entity, TRUE, FALSE)
    }
    # if there are input filters, but no customer entity filters, then only use input filters
    else if(any(!is.null(input$region),!is.null(input$size),!is.null(input$specialty)) & !is.null(input$customer_entity_benchmark))
    {
      encounter_df$filter_entities<-ifelse(encounter_df$filter_region & encounter_df$filter_size & encounter_df$specialty, TRUE, FALSE)
    }
    # if there are input filters and customer entity filters, then
    else if(any(!is.null(input$region),!is.null(input$size),!is.null(input$specialty)) & !is.null(input$customer_entity_benchmark)){
      encounter_df$filter_entities<-ifelse((encounter_df$filter_region & encounter_df$size & encounter_df$specialty) | encounter_df$filter_customer_entity, TRUE, FALSE)
    }
    # if no filter is selected, then
    else {
      encounter_df$filter_entities<-TRUE
    }

    # _ _ _ d. Benchmarking filters -------------------------------------------
    # filter ROM
    if(!is.null(input$ROM)){
      encounter_df$filter_rom<-ifelse(encounter_df$rom %in% input$ROM, TRUE, FALSE)
    } else {
      encounter_df$filter_rom<-TRUE
    }
    # filter SOI
    if(!is.null(input$SOI)){
      encounter_df$filter_soi<-ifelse(encounter_df$soi %in% input$ROM, TRUE, FALSE)
    } else {
      encounter_df$filter_soi<-TRUE
    }
    # filter age
    if(!is.null(input$age)){
      encounter_df$filter_age<-ifelse(encounter_df$age %in% input$ROM, TRUE, FALSE)
    } else {
      encounter_df$filter_age<-TRUE
    }
    # filter patient type
    if(!is.null(input$patienttype)){
      encounter_df$filter_patienttype<-ifelse(encounter_df$PatientTypeRollup %in% input$patienttype, TRUE, FALSE)
    } else {
      encounter_df$filter_patienttype<-TRUE
    }

    # _ _ _ e. Cost Type Filters  ---------------------------------------------
    if(length(input$cost) > 0){
      encounter_df$temp1<-ifelse(encounter_df$FixedVariable %in% input$costs, 1, 0)
      encounter_df$temp2<-ifelse(encounter_df$DirectIndirect %in% input$costs, 1, 0)
      encounter_df$temp3<-ifelse(encounter_df$CostDriver %in% input$costs, 1, 0)
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


    # _ _ _ f. Quality Incident Filters ---------------------------------------
    # if there is only one quality incident filter selected
    if(length(input$qualityincidents)==1){
      # if "Only Keep Encounters without Hospital-Acquired Quality Incidents"
      # filter out hospital - acquired conditions / hospital caused quality incidents
      if(min(input$qualityincidents)=="Remove"){
        encounter_df$filter_qualityincidents<-ifelse(encounter_df$HospitalAcqCondition=="0", TRUE, FALSE)
      }
      # if "Only Keep Encounters With Hospital-Acquired Quality Incidents"
      # filter out NON hospital-acquired conditions/hospital-caused quality incidents
      else if(min(input$qualityincidents)=="Keep"){
        encounter_df$filter_qualityincidents<-ifelse(encounter_df$HospitalAcqCondition=="1", TRUE, FALSE)
      }
    }
    # if both are selected, or neither selected, keep both
    else if(is.null(input$qualityincidents) | length(input$qualityincidents)==2){
      encounter_df$filter_qualityincidents<-TRUE
    }

  })



  # _ III. Plots ------------------------------------------------------------


  # _ _ 0. General ----------------------------------------------------------
  costdriver_overview_plot<-eventReactive(input$refresh, {
    #grab reactive data frames
    encounter_df<-encounter_df()




  })



  # _ _ 1. Imaging ----------------------------------------------------------


  # _ _ _ << Plot: Direct Costs by Category >> ------------------------------


  # _ _ _ << Plot: Direct Costs Per Case by Category >> --------------------


  # _ _ 2. LOS --------------------------------------------------------------



  # _ _ _ << Plot: Direct Cost Per Day by Category >> -----------------------


  # _ _ _ << Plot: Avg. Days per Category >> --------------------------------


  # _ _ _ << Plot: Percentage Time per Category >> --------------------------


  # _ _ _ << Plot: Room & Board Days by UBRevCode >> ------------------------
  # day types - ICU, etc


  # _ _ _ << Plot: Direct Cost Per Day by UBRevCode >> ----------------------


  # _ _ _ << Plot: Top 10 Direct Cost Per Case by Day Of Stay >> ------------


  # _ _ _ << Plot: Direct Cost Per Case by Day of Stay >> -------------------


  # _ _ _ << Plot: Direct Cost Per Case for First and Last Day of St --------


  # _ _ _ << Plot: Direct Cost Per Case by Discharge Status >> --------------


  # _ _ _ << Plot: Direct Cost from Admit Date to Surgery Date (<->) --------



  # _ _ 3. Pharmacy ---------------------------------------------------------


  # _ _ _ << Plot: UOS per Case >> ------------------------------------------


  # _ _ _ << Plot: Direct Cost Per Case >> ----------------------------------



})
