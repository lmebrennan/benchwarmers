## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

# Entity Data Frame ---------------------------------------------------

# When a user clicks 'refresh' on the BYOBenchmark side bar, the dataframe that is used throughout the application
#  needs to be refreshed.
entity_df <- eventReactive(input$refresh, {
  # start with the raw customer data
  entity_df <- full_df


  # a. filter_costdriver ----------------------------------------------
  ### A1. filter selected cost driver
  entity_df$filter_costdriver<-ifelse(entity_df$costdriver==input$costdriver, TRUE, FALSE)

  # b. filter_myEntity ------------------------------------------------
  ### B1. filter selected hospital
  entity_df$filter_myEntity <- ifelse(entity_df$customer_entity==input$customer_entity,TRUE,FALSE)

  # c. Selected Population Filters -----------------------------------
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

  # d. Complete Filtering ---------------------------------------------
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
