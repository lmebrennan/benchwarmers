## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

dbSidebar <- dashboardSidebar(
  width=220,
  sidebarMenu(
    #parameters for selected entity
    h3("What you are comparing with:"),
    selectizeInput("customer_entity","Select an entity:", choices=''),
    uiOutput("MSDRG_selector"),

    h3(""),
    h3(""),
    uiOutput(""),
    selectizeInput("region", "Region(s):",
                   choices = c(ALL="","South","Midwest","East"),
                   multiple=TRUE),
    selectizeInput("size","Bedsize(s):",
                   choices = c(ALL="","Under 200", "Over 200"),
                   multiple=TRUE),
    selectizeInput("specialty","Specialty(ies):",
                   choices = c(ALL="","Rural", "Urban"),
                   multiple=TRUE),
    selectizeInput("costmodel","Select Cost model(s):",
                   choices = c(ALL="",
                               "Hospitals with Strata Standardized Cost Models" = "standard",
                               "Hospitals without Strata Standardized Cost Models" = "non"),
                   multiple=TRUE),
    actionBttn("hospital_refresh", "Refresh")
  )
)
