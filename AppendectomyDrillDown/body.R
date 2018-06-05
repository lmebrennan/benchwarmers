## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

dbBody = dashboardBody(

  fluidRow(
    # Benchmarking Information ----------------------------------------------------
    box(
     title="Selected Entity Information",
     status="warning",
     solidHeader=TRUE,
     collapsible=TRUE,
     htmlOutput("selected_entity"),
     htmlOutput("selected_region"),
     htmlOutput("selected_size"),
     htmlOutput("selected_specialty")
    ),
    box(
      title="Selected Population Information",
      status="warning",
      solidHeader=TRUE,
      collapsible=TRUE,
      # box(
      #   width=12,
      #   title="Comparison Entities",
      #   collapsible=TRUE,
        htmlOutput("comparison_entities"),
      # ),
      htmlOutput("comparison_regions"),
      htmlOutput("comparison_sizes"),
      htmlOutput("comparison_specialties")
    )
  ),
  # Benchmarking Results ----------------------------------------------------
  fluidRow(
    box(
      width=12,
      title="Output",
      status="primary",
      solidHeader=TRUE,
      "Box Content Here",
      br(),
      "MORE CONTENT?!"
    )
  )
)

