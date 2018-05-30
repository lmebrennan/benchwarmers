## Appendectomy Drill Down
## Laura Brennan
## Strata Decision Technology
## May 2018

dbBody = dashboardBody(
  fluidRow(
    column(
      width=12, offset=0, align='center',
      tabItems(
        tabItem(
          tabName="TAB NAME",
          fluidRow(
            column(
              width=12, offset=0, align='center',
              box(
                title="ANALYSIS", status="success", solidHeader = TRUE, collapsible= TRUE,
                tabBox(
                  title=NULL,width=NULL,
                  tabPanel(title="TAB PANEL TITLE", plotlyOutput(outputId='distPlot'))
                )
              )
            )
          )
        )
      )
    )
  )
)
