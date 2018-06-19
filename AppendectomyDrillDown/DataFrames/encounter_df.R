## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

# Encounter Data Frame ---------------------------------------------
# Encounter level data frame with benchmark grouping columns and cost grouping columns as well as
# columns with cost information;
#
# This code filters the full dataframe of all cost data, based off user inputs about how to filter the data
encounter_df<-eventReactive(input$refresh, {
  encounter_df <- full_df

  # a. Selected Cost Driver Filter ------------------------------------
  encounter_df$filter_costdriver<-ifelse(encounter_df$costdriver==input$costdriver, TRUE, FALSE)

  # b. Selected Entity Filter -----------------------------------------
  ### 1. filter selected hospital
  encounter_df$filter_myentity <- ifelse(encounter_df$customer_entity==input$customer_entity,TRUE,FALSE)

  # c. Selected Population Filters -----------------------------------
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

  # d. Benchmarking filters -------------------------------------------
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
  # filter discharge status
  if(!is.null(input$dischargestatus)){
    encounter_df$filter_dischargestatus<-ifelse(encounter_df$DischargeStatusGroup %in% input$dischargestatus, TRUE, FALSE)
  } else {
    encounter_df$filter_dischargestatus<-TRUE
  }

  # e. Cost Type Filters  ---------------------------------------------
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


  # f. Quality Incident Filters ---------------------------------------
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

  # set conditions for filtering
  entity_conditions<-c((encounter_df$filter_entities & encounter_df$filter_costmodel) | encounter_df$filter_myentity)
  encounter_conditions<-c(encounter_df$filter_rom & encounter_df$filter_soi & encounter_df$filter_age &
                            encounter_df$filter_patienttype & encounter_df$filter_costtype &
                            encounter_df$filter_qualityincidents & encounter_df$filter_dischargestatus)

  encounter_df<-encounter_df[entity_conditions & encounter_conditions,] %>%
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
