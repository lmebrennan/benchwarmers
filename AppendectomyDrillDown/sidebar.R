## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

dbSidebar <- dashboardSidebar(
  sidebarMenu(
    ### Note: All selectizeInput choices will be populated via server using the df

    h3("Compare",align="center"),
    # select an entity to benchmark
    selectizeInput(inputId="customer_entity",label="Select an entity:",choices=NULL),
    hr(),
    h3("With",align="center"),
    # create population to benchmark against
    selectizeInput(inputId="comparison_entity",label="Entity(ies):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="region",label="Region(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="beds",label="Bedsize(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="specialty",label="Specialty(ies):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="costmodel",label="Select Cost model(s):",choices = c(ALL=""),multiple=TRUE),

    actionBttn(inputId="hospital_refresh",label="Refresh")
  )
)
