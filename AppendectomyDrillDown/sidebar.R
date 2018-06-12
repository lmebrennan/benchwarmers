## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

dbSidebar <- dashboardSidebar(
  sidebarMenu(
    ### Note: All selectizeInput choices will be populated via server using the df

    h4("Entity to Benchmark",align="center"),
    # select an entity to benchmark
    selectizeInput(inputId="customer_entity",label="Select an entity:",choices=NULL),
    hr(),
    h4("Benchmark Population", align="center"),

    # create population to benchmark against
    uiOutput("benchmark_selector"),
    checkboxGroupInput(inputId="region",label="Region(s):",inline=TRUE),
    selectizeInput(inputId="beds",label="Bedsize(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="specialty",label="Specialty(ies):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="costmodel",label="Select Cost model(s):",choices = c(ALL=""),multiple=TRUE),

    actionBttn(inputId="refresh",label="Refresh")
  )
)
