## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

dbBody = dashboardBody(

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
      width=12,
      title="Cost Driver Overview",
      solidHeader=TRUE,
      status="primary",
      collapsible=TRUE,
      checkboxGroupInput("costbreakdowns",
                         strong("Breakdown Costs:"),
                         choiceNames=c("Fixed/Variable","Direct/Indirect","Cost Drivers"),
                         choiceValues=c("fixedvariable","directindirect","costdriver")),
      tabsetPanel(type = "tabs",
                  tabPanel(
                    title="Plot",
                    plotOutput("costdriver_overview_plot")
                  ),
                  tabPanel(
                    title="Summary",
                    verbatimTextOutput("summary")
                  ),
                  tabPanel(
                    title="Table",
                    tableOutput("table")
                  )
      )
    )
  )
)
    # box(
    #   width=12,
    #   title="Output",
    #   status="primary",
    #   solidHeader=TRUE,
    #   "Box Content Here",
    #   br(),
    #   "MORE CONTENT?!"
    # )
#   )
# )

