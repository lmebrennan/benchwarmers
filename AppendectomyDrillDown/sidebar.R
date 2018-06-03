## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

dbSidebar <- dashboardSidebar(
  sidebarMenu(
    ### Note: All selectizeInput choices will be populated via server using the df
    h3("Compare",align="center"),
    # Select entity to benchmark
    selectizeInput(inputId="customer_entity",label="Select an entity:",choices=NULL),

    ## uiOutput("MSDRG_selector"),

    hr(),

    h3("With",align="center"),

    ## uiOutput(""),

    # create population to benchmark against
    selectizeInput(inputId="region",label="Region(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="size",label="Bedsize(s):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="specialty",label="Specialty(ies):",choices=c(ALL=""),multiple=TRUE),
    selectizeInput(inputId="costmodel",label="Select Cost model(s):",choices = c(ALL=""),multiple=TRUE),

    actionBttn(inputId="hospital_refresh",label="Refresh")
  )
)
