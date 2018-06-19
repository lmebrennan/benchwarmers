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
      width = 3,
      title="Filter Encounters",
      status="warning",
      solidHeader = TRUE #,
      # selectizeInput("ROM", "Select Risk of Mortality (ROM) value(s):",
      #                choices = c(ALL = "", "1", "2", "3", "4"),
      #                multiple = TRUE),
      # selectizeInput("SOI", "Select Severity of Illness (SOI) value(s):",
      #                choices = c(ALL = "", "1", "2", "3", "4"),
      #                multiple = TRUE),
      # selectizeInput("age", "Select patient age(s):",
      #                choices = c(ALL = "",
      #                            "Infant (less than 1 yr)" = "Infant",
      #                            "Pediatric (1 yr - 17 yrs)" = "Pediatric",
      #                            "Adult (18 yrs - 64 yrs)" = "Adult",
      #                            "Senior (65 years or older)" = "Senior"),
      #                multiple = TRUE),
      # selectizeInput("patienttype", "Select patient type(s):",
      #                choices = c(ALL = "",
      #                            "Inpatient",
      #                            "Outpatient"),
      #                multiple = TRUE),
      # selectizeInput("dischargestatus", "Select patient discharge status(es):",
      #                choices = c(ALL = "",
      #                            "Still a Patient",
      #                            "Discharged to home or other self care",
      #                            "Discharged to home health services",
      #                            "Left against medical advice (AMA)",
      #                            "Died",
      #                            "Transferred to other facility",
      #                            "Transferred to other short-term care facility",
      #                            "Transferred to intermediate care facility",
      #                            "Transferred to skilled nursing facility",
      #                            "Not Specified"),
      #                multiple = TRUE),
      # selectizeInput("costs", "Select cost(s):",
      #                choices = list(ALL = "",
      #                               `Cost Types` = c("Fixed",
      #                                                "Variable",
      #                                                "Direct",
      #                                                "Indirect"),
      #                multiple = TRUE),
      # selectizeInput("qltyincidents", "Select whether to keep/remove hospital-caused quality incidents:",
      #                choices = c(BOTH = "",
      #                            "Only Encounters without Hospital-Caused Quality Incidents" = "Remove",
      #                            "Only Encounters with Hospital-Caused Quality Incidents" = "Keep"),
      #                multiple = TRUE),
      # # option to remove data
      # checkboxGroupInput("otherfilteroptions", strong("Other data filters:"),
      #                    choiceNames = c("Remove Cost Outliers (based off interquartile range (IQR))",
      #                                    "Remove Cost Outliers (based off standard deviation (sd))",
      #                                    "Remove Length of Stay Outliers (based off interquartile range (IQR))",
      #                                    "Remove Length of Stay Outliers (based off standard deviation (sd))"
      #                    ),
      #                    choiceValues = c("cost_IQR",
      #                                     "cost_SD",
      #                                     "LOS_IQR",
      #                                      "LOS_SD"))
      # checkboxGroupInput("benchmarkbreakdowns", strong("Select variables to breakdown costs by:"),
      #                    choiceNames = c("Risk of Mortality (ROM)",
      #                                    "Severity of Illness (SOI)",
      #                                    "Patient Age Bucket",
      #                                    "Patient Type",
      #                                    "Patient Discharge Status"),
      #                    choiceValues = c("rom",
      #                                     "soi",
      #                                     "agebucket",
      #                                     "patienttyperollup",
      #                                     "dischargestatusgroup")),
      # checkboxGroupInput("costbreakdowns", strong("Select how to breakdown costs:"),
      #                    choiceNames = c("Fixed/Variable",
      #                                    "Direct/Indirect",
      #                                    "Cost Drivers"),
      #                    choiceValues = c("fixedvariable",
      #                                     "directindirect",
      #                                     "costdriver")),
      # selectizeInput(inputId="rom",label="ROM:",choices=c(ALL=""),multiple=TRUE),
      # selectizeInput(inputId="soi",label="SOI:",choices=c(ALL=""),multiple=TRUE),
      # selectizeInput(inputId="age",label="Age Group:",choices=c(ALL=""),multiple=TRUE),
      # selectizeInput(inputId="patienttyperollup",label="Patient Type:",choices=c(ALL=""),multiple=TRUE),
      # selectizeInput(inputId="costtype",label="Cost Type:",choices=c(ALL=""),multiple=TRUE),
      # selectizeInput(inputId="qualityincidents",label="Quality Incidents:",choices=c(ALL=""),multiple=TRUE),
      # selectizeInput(inputId="dischargestatusgroup",label="Discharge Status:",choices=c(ALL=""),multiple=TRUE)
    ),
    box(
      width=9,
      title="Cost Driver Overview",
      solidHeader=TRUE,
      status="primary",
      collapsible=TRUE,
      tabsetPanel(type = "tabs",
                  tabPanel(
                    title="Box Plot",
                    plotOutput("costdriver_overview_plot")
                  ),
                  tabPanel(
                    title="Summary",
                    plotlyOutput(outputId="box_Imaging")
                  ),
                  tabPanel(
                    title="Table",
                    tableOutput("table")
                  )
        )
      )
    )
  )
#)



