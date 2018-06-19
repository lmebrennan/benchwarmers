## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018


### Note: All selectizeInput choices will be populated via server using the df

dbSidebar <- dashboardSidebar(
  sidebarMenu(
    h4("Entity to Benchmark",align="center"),
    selectizeInput(inputId="customer_entity",label="Select an entity:",choices=NULL),
    hr(),

    h4("Benchmark Population", align="center"),
    selectizeInput(inputId="region",label="Region(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="beds",label="Bedsize(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="specialty",label="Specialty(ies):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="costmodel",label="Select Cost model(s):",choices=c(ALL=""),multiple=TRUE),
    uiOutput("benchmark_selector"),

    actionBttn(inputId="refresh",label="Refresh")
  )
)
