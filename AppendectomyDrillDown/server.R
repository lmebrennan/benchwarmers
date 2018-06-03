## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018


# SERVER ------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # Sidebar logic ---------------------------------------------------------

  # updates each dropdown with values from the database, alphabetized
  updateSelectizeInput(session,"customer_entity",
                       choices=hospital_df[order(hospital_df$customerid,hospital_df$entityid),]$customer_entity,
                       server=TRUE)
  updateSelectizeInput(session,"region",
                       choices=as.factor(hospital_df[order(hospital_df$region),]$region),
                       server=TRUE)
  updateSelectizeInput(session,"size",
                       choices=as.factor(hospital_df[order(hospital_df$beds),]$beds),
                       server=TRUE)
  updateSelectizeInput(session,"specialty",
                       choices=as.factor(hospital_df[order(hospital_df$specialty),]$specialty),
                       server=TRUE)
  updateSelectizeInput(session,"costmodel",
                       choices=c("Hospitals with Strata Standardized Cost Models" = "standard",
                                 "Hospitals without Strata Standardized Cost Models" = "non"),
                       server=TRUE)

  # output$MSDRG_selector = renderUI({
  #   selectizeInput(inputId="MSDRG","Select an MSDRG to benchmark:",
  #                  choices = labelMSDRG(unique(full_df$msdrggroup[full_df$customerid == hospital_df$customerid[hospital_df$customer_entity == input$customer_entity] &
  #                                                                 full_df$entityid == hospital_df$entityid[hospital_df$customer_entity == input$customer_entity]])))
  # })



})
