### Laura Brennan
### Software Developer
### Strata Decision Technology
### CostBenchmarkingForChildren
###
### UI Code

#make sure Shiny package is in library
library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  titlePanel("Children's Hospital Benchmarking Initiative"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("customer_entity", "Select a hospital:", 
                     choices = c("Customer 1, Entity 1", "Customer 1, Entity 8",
                                 "Customer 3, Entity 2", "Customer 3, Entity 3",
                                 "Customer 4, Entity 5", "Customer 4, Entity 26",
                                 "Customer 4, Entity 6", "Customer 5, Entity 6",
                                 "Customer 6, Entity 1", "Customer 7, Entity 2",
                                 "Customer 9, Entity 2", "Customer 11, Entity 1",
                                 "Customer 12, Entity 1"))
  ),
  mainPanel()
  # theme = shinythemes::shinytheme("lumen"),
  # tabsetPanel(type="tabs",
  #     tabPanel("Benchmark",
  #        fluidRow(
  #          column(2,
  #                 h3("Choose Sample Population"),
  #                 selectizeInput("customer_entity", "Select a customer and entity:",
  #                                choices = c("Customer 1, Entity 1","Customer 2, Entity 2",
  #                                            "Customer 4 Entity 4")
  #                 )
  #           )
  #         )
  #       )
 )))
