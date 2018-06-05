## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018


# SERVER ------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # Sidebar logic ---------------------------------------------------------
  # > Populate Dropdowns ----------------------------------------------------

  # updates each dropdown with values from the database, alphabetized
  updateSelectizeInput(session,"customer_entity",
                       choices=hospital_df[order(hospital_df$customerid,hospital_df$entityid),]$customer_entity,
                       server=TRUE)

  # updateSelectizeInput(session,"comparison_entity",
  #                      choices=c(ALL="",hospital_df$customer_entity[hospital_df$customer_entity != input$customer_entity]))
  updateSelectizeInput(session,"region",
                       choices=as.factor(hospital_df[order(hospital_df$region),]$region),
                       server=TRUE)
  updateSelectizeInput(session,"beds",
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

  # Body logic --------------------------------------------------------------

  # > Populate Info for Selected Entity -------------------------------------
  output$selected_entity = renderText({
    paste("<b>Hospital Institution:</b>",input$customer_entity)
  })
  output$selected_region = renderText({
    paste("<b>Region:</b>", hospital_df$region[hospital_df$customer_entity == input$customer_entity])
  })
  output$selected_size = renderText({
    paste("<b>Bed Size:</b>", hospital_df$beds[hospital_df$customer_entity == input$customer_entity])
  })
  output$selected_specialty = renderText({
    paste("<b>Specialty:</b>", hospital_df$specialty[hospital_df$customer_entity == input$customer_entity])
  })

  # > Populate Info for Comparison Population --------------------------------
  output$comparison_entities = renderText({
      if(length(input$costmodel) == 1 & "standard" %in% input$costmodel){
        df <- hospital_df %>%
          filter(isstratastandardcost == "Y")

        paste("<b>Benchmark Institution(s):</b><br/>",
              ifelse(is.null(input$comparison_entities) & is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                     # if no inputs, then take all the hospitals that aren't the one selected
                     paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = ", "),
                     ifelse(is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                            paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                      & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "; "),
                            paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                      & (df$customer_entity %in% input$customer_entity_benchmark
                                                                         | ((df$region %in% input$region | is.null(input$region))
                                                                            & (df$beds %in% input$size | is.null(input$size))
                                                                            & (df$specialty %in% input$specialty | is.null(input$specialty))))])),
                                  collapse = "; ")
                     )))
      }
      else if(length(input$costmodel) == 1 & "non" %in% input$costmodel){
        df <- hospital_df %>%
          filter(isstratastandardcost == "N")

        paste("<b>Benchmark Institution(s):</b><br/>",
              ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                     # if no inputs, then take all the hospitals that aren't the one selected
                     paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = "; "),
                     ifelse(is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                            paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                      & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "; "),
                            paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                      & (df$customer_entity %in% input$customer_entity_benchmark
                                                                         | ((df$region %in% input$region | is.null(input$region))
                                                                            & (df$beds %in% input$size | is.null(input$size))
                                                                            & (df$specialty %in% input$specialty | is.null(input$specialty))))])),
                                  collapse = "; ")
                     )))
      }
      else {
        df <- hospital_df

        paste("<b>Benchmark Institution(s):</b><br/>",
              ifelse(is.null(input$customer_entity_benchmark) & is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                     # if no inputs, then take all the hospitals that aren't the one selected
                     paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity])), collapse = "; "),
                     ifelse(is.null(input$region) & is.null(input$size) & is.null(input$specialty),
                            paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                      & df$customer_entity %in% input$customer_entity_benchmark])), collapse = "; "),
                            paste(as.vector(unique(df$customer_entity[df$customer_entity != input$customer_entity
                                                                      & (df$customer_entity %in% input$customer_entity_benchmark
                                                                         | ((df$region %in% input$region | is.null(input$region))
                                                                            & (df$beds %in% input$size | is.null(input$size))
                                                                            & (df$specialty %in% input$specialty | is.null(input$specialty))))])),
                                  collapse = "; ")
                     )))
      }
  })

  output$comparison_regions = renderText({
    paste("<b>Region(s):</b>",
          ifelse(is.null(input$region),
                 paste(as.vector(unique(hospital_df$region)),collapse=", "),
                 paste(as.vector(unique(input$region)),collapse=", "))
          )
  })

  output$comparison_sizes = renderText({
    paste("<b>Bed Size(s):</b>",
          ifelse(is.null(input$beds),
                 paste(as.vector(unique(hospital_df$beds)),collapse=", "),
                 paste(as.vector(unique(input$beds)),collapse=", "))
    )
  })

  output$comparison_specialties = renderText({
    paste("<b>Specialty(ies):</b>",
          ifelse(is.null(input$specialty),
                 paste(as.vector(unique(hospital_df$specialty)),collapse=", "),
                 paste(as.vector(unique(input$specialty)),collapse=", "))
    )
  })

})
