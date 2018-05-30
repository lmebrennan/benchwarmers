## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018


# Define server logic required to draw a histogram

# SERVER ------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # Sidebar logic ---------------------------------------------------------
  updateSelectizeInput(session,"customer_entity",
                       choices=hospital_df[order(hospital_df$customerid,hospital_df$entityid),]$customer_entity,
                       server=TRUE)
  output$MSDRG_selector = renderUI({
    selectizeInput(inputId="MSDRG","Select an MSDRG to benchmark:",
                   choices = labelMSDRG(unique(full$msdrggroup[full$customerid == hospital_df$customerid[hospital_df$customer_entity == input$customer_entity] &
                                                                 full$entityid == hospital_df$entityid[hospital_df$customer_entity == input$customer_entity]])))
  })

})
