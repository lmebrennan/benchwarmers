#### CREATE KPI BENCHMARKING APP ####
## R version 3.5.0

install.packages(c('devtools',
                   'shiny',
                   'shinythemes',
                   'shinyWidgets',
                   'shinyTree',
                   'cowplot',
                   'sparkline',
                   'htmltools',
                   'formattable',
                   'plotly',
                   'rlang',
                   'DT',
                   'data.table'
),
repos='https://cran.rstudio.com/')

library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(cowplot)
library(rlang)
library(civis)
library(shiny)
library(shinyTree)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(sparkline)
library(plotly)
library(htmltools)
library(formattable)
library(data.table)
library(stringr)

# read in data
kpi<-read_civis("kpi_benchmarking",database="Strata Decision Technologies")
names <- c('customer_entity' ,'admission_volume','kpi')
kpi[,names] <- lapply(kpi[,names], as.character)

#### UI #### 
ui <- fluidPage(
  theme = shinythemes::shinytheme("yeti"),
  tabsetPanel(type = "tabs",
              ## -----------< 1. Create Benchmark >-----------
              tabPanel("Create Benchmark",br(),
                       fluidRow(
                         ## -----------<< Column 1.1: Input Hospital and Benchmark Selections >>-----------
                         column(3,
                                # parameters for "Me"
                                h3("Hospital to Benchmark"),
                                # select customer / hospital system
                                selectizeInput("customer_entity", "Select a customer and entity to benchmark:", 
                                               choices =''),
                                # parameters for Baseline / hospitals to bencmark against
                                h3("Benchmark Hospitals"),
                                # select specific hospitals to compare against
                                uiOutput("benchmark_selector"),
                                # select average admissions/discharges to compare against
                                selectizeInput("admission_volume", "Select admissions/discharges volume:",
                                               choices = c(ALL = "", "less than 2,000", "2,000 - 10,000", "above 10,000"),
                                               multiple = TRUE),
                                # output characteristics about the benchmark
                                htmlOutput("benchmark_institutions"),  # institutions in the benchmark
                                htmlOutput("benchmark_admission"),
                                actionButton("hospital_refresh", "Compare")),
                        
                         ## -----------<< Column 1.4: Income Statement >>-----------
                         column(6, offset=1,br(),
                                h2(""),
                                h1(""),
                                htmlOutput("Table")
                                )
                         )
                       ),
              ## -----------< 2. Actual v.s. Budget >-----------
              tabPanel("Actual V.S. Budget",br(),
                         sidebarLayout(
                           sidebarPanel(
                            radioButtons("actual_budget", "KPIs",
                                      choiceNames = c("Inpatient Revenue per Inpatient Day","Revenue Growth Rate","NPR as % of Total Gross Charges",
                                                      "Bad Debt as % of NPR", "Charity as % of NPR", "Employment Expense as % of NPR",
                                                      "Salaries as % of NPR","Supplies as % of NPR","Purchased Services as % of NPR",
                                                      "Total Expenses as % of NPR","EBITDA per Adjusted Patient Day","TIE Ratio","Operating Margin",
                                                      "Net Income Margin","Average Length of Stay","Employment Expense per Adjusted Patient Day",
                                                      "Supply Expense per Adjusted Patient Day","Purchased Services per Adjusted Patient Day"),
                                      choiceValues = c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
                                                       "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
                                                       "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
                                                       "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday")),
                            width=3),
                         mainPanel(
                           plotlyOutput('plot',width = "100%", height = "700px")
                         )
                       )
              ),

              
                    
                   ## -----------< 2. Trend Over Year >-----------
              tabPanel("Trend Over Year",br(),
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("trend", "KPIs",
                                        choiceNames = c("Inpatient Revenue per Inpatient Day","Revenue Growth Rate","NPR as % of Total Gross Charges",
                                                        "Bad Debt as % of NPR", "Charity as % of NPR", "Employment Expense as % of NPR",
                                                        "Salaries as % of NPR","Supplies as % of NPR","Purchased Services as % of NPR",
                                                        "Total Expenses as % of NPR","EBITDA per Adjusted Patient Day","TIE Ratio","Operating Margin",
                                                        "Net Income Margin","Average Length of Stay","Employment Expense per Adjusted Patient Day",
                                                        "Supply Expense per Adjusted Patient Day","Purchased Services per Adjusted Patient Day"),
                                        choiceValues = c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
                                                         "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
                                                         "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
                                                         "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday")),
                           width=3),
                         mainPanel(
                           plotlyOutput('trendplot',width = "100%", height = "700px")
                         )
                       )
              )
  )
)
              

#### SERVER #### 
server <- function(input, output, session){
  
  ## -----------< UI Inputs and Outputs >-----------
  
  ## Dependent UI Inputs
  
  # Select customer and entity ("me")
  updateSelectizeInput(session,"customer_entity",
                       choices=unique(kpi[order(kpi$organization,kpi$entity),]$customer_entity),
                       server=TRUE)
  
  # Customer ID and Entity ID
  output$benchmark_selector = renderUI({
    selectizeInput(inputId = "customer_entity_benchmark", "Select customer(s) and entity(ies) to benchmark against:",
                   choices = c(ALL = "", kpi$customer_entity[kpi$customer_entity != input$customer_entity]),
                   multiple = TRUE)
  })
  
  ## UI output hospital benchmark information
  # Customer and Entity -- outputs the Customers(s) and Entity(ies) that make up the benchmark
  output$benchmark_institutions = renderText({
      paste("<b>Benchmark Institution(s):</b><br/>", 
            ifelse(is.null(input$customer_entity_benchmark) & is.null(input$admission_volume),
                   # if no inputs, then take all the hospitals that aren't the one selected
                   paste(as.vector(unique(kpi$customer_entity[kpi$customer_entity != input$customer_entity])), collapse = "<br/>"),
                   ifelse(is.null(input$admission_volume),
                          paste(as.vector(unique(kpi$customer_entity[kpi$customer_entity != input$customer_entity
                                                                    & kpi$customer_entity %in% input$customer_entity_benchmark])), collapse = "<br/>"),
                          paste(as.vector(unique(kpi$customer_entity[kpi$customer_entity != input$customer_entity
                                                                    & (kpi$customer_entity %in% input$customer_entity_benchmark
                                                                       | ((kpi$admission_volume %in% input$admission_volume | is.null(input$admission_volume))))])), 
                                collapse = "<br/>")
                   )))
  })

  # Admission/discharge volume -- outputs the admission/discharge volume of the Customer(s) & Entity(ies) selected
  output$benchmark_admission = renderText({
    paste("<b>Benchmark Admission Volume:</b><br/>", 
          ifelse(is.null(input$customer_entity_benchmark) & is.null(input$admission_volume),
                 paste(as.vector(unique(kpi$admission_volume[kpi$customer_entity != input$customer_entity])), collapse = ", "),
                 paste(as.vector(unique(kpi$admission_volume[kpi$customer_entity %in% input$customer_entity_benchmark | kpi$admission_volume %in% input$admission_volume])), collapse = ", ")))
  })
  
  
  
  ## -----------<< df >>-----------
  # Filters the full dataframe based off user inputs about how to filter the data
  # the data is also labeled as "Me" or "Baseline" to indicate which data go towards the benchmark, and which go to the hospital of interest
  
  df <- eventReactive(input$hospital_refresh, {
    df <- kpi
    
    ## "me" / hospital filter
    df$h1 <- ifelse(df$customer_entity == input$customer_entity, TRUE, FALSE)  # filter for input Customer ID and Entity ID
    
    
    ## benchmark filters
    if(!is.null(input$customer_entity_benchmark)){
      df$c1 <- ifelse(df$customer_entity %in% input$customer_entity_benchmark, TRUE, FALSE)
    } else {
      df$c1 <- TRUE
    }

    if(!is.null(input$admission_volume)){
      df$c2 <- ifelse(df$admission_volume %in% input$admission_volume, TRUE, FALSE)        # filter for hospital region
    } else {
      df$c2 <- TRUE
    }
    # master hospital benchmark filter
    # if only input customers/entities to benchmark against, only use that column to filter
    if(is.null(input$admission_volume) & !is.null(input$customer_entity_benchmark)){
      df$c3 <- ifelse(df$c1, TRUE, FALSE)
    } 
    # if input admission volume filter, but not customer entity filter, then only use those filters
    else if(!is.null(input$admission_volume) & is.null(input$customer_entity_benchmark)){
      df$c3 <- ifelse(df$c2, TRUE, FALSE)
    }
    # if input admission_volume filter and customer entity filter, then
    else if(!is.null(input$admission_volume) & !is.null(input$customer_entity_benchmark)){
      df$c3 <- ifelse(df$c1 | df$c2, TRUE, FALSE)
    }
    # if none selected; then else
    else {
      df$c3 <- TRUE
    }
    
    # filter for only hospital to benchmark & benchmark hospitals
    df <- df %>%
      filter(h1 | c3) %>%
      mutate(Group = factor(ifelse(h1, "Me", "Baseline"),  
                            levels = c("Baseline", "Me"),
                            ordered = TRUE))
    
    return(df)
  })
  
  ## -----------<< Income statement table with sparklines >>-----------
  
  income_statement <- eventReactive(input$hospital_refresh, {
    table_df<-df()
    dat<-subset(table_df,fiscalyear==2017 & timeclass=="A" & Group=="Me",select=c(kpi,value))
    names(dat)[2]<-"me_2017"
    
    
    spark_dat<-subset(table_df,((fiscalyear<=2017 & timeclass=="A")|fiscalyear==2018)&Group=="Me",
              select=c(fiscalyear,kpi,value))
    spark_dat<-spark_dat[order(spark_dat$kpi,spark_dat$fiscalyear),]
    names(spark_dat)[3]<-"value_me"
    
    spark_dat<-na.omit(spark_dat)
    
    box_dat<-subset(table_df,fiscalyear==2017 & timeclass=="A"&Group=="Baseline",
              select=c(customer_entity,kpi,value))
    names(box_dat)[3]<-"value_baseline"
    
    income_dat<-subset(Reduce(function(...) merge(..., all=TRUE), list(dat,spark_dat,box_dat)),
                       kpi %in% c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
                                  "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
                                  "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
                                  "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday")) 

    ord<-c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
           "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
           "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
           "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday")
    
    income_dat$kpi<-factor(income_dat$kpi, levels=ord,
                           labels=c("Inpatient Revenue per Inpatient Day","Revenue Growth Rate","NPR as % of Total Gross Charges",
                                    "Bad Debt as % of NPR", "Charity as % of NPR", "Employment Expense as % of NPR",
                                    "Salaries as % of NPR","Supplies as % of NPR","Purchased Services as % of NPR",
                                    "Total Expenses as % of NPR","EBITDA per Adjusted Patient Day","TIE Ratio","Operating Margin",
                                    "Net Income Margin","Average Length of Stay","Employment Expense per Adjusted Patient Day",
                                    "Supply Expense per Adjusted Patient Day","Purchased Services per Adjusted Patient Day"))
    names(income_dat)[1]<-"KPI"


        income_table <- income_dat %>% 
          group_by(KPI) %>%
          summarise(
            Me=paste(unique(me_2017),collapse=","),
            q25=quantile(value_baseline,0.25,na.rm=TRUE),
            q75=quantile(value_baseline,0.75,na.rm=TRUE),
            Baseline=spk_chr(value_baseline,type="box"),
            Trend=spk_chr(unique(value_me),type="line")
          )%>%
          mutate(
            Me=ifelse(KPI %in% c("Inpatient Revenue per Inpatient Day","EBITDA per Adjusted Patient Day",
                                 "Employment Expense per Adjusted Patient Day","Supply Expense per Adjusted Patient Day",
                                 "Purchased Services per Adjusted Patient Day"),paste("$",Me,sep=""),
                      ifelse(KPI %in% c("Average Length of Stay","TIE Ratio"),paste(Me),paste(Me,"%",sep=""))
            ),
            q25=paste("[",ifelse(KPI %in% c("Inpatient Revenue per Inpatient Day","EBITDA per Adjusted Patient Day",
                                            "Employment Expense per Adjusted Patient Day","Supply Expense per Adjusted Patient Day",
                                            "Purchased Services per Adjusted Patient Day"),paste("$",q25,sep=""),
                                 ifelse(KPI %in% c("Average Length of Stay","TIE Ratio"),paste(q25),paste(q25,"%",sep=""))),
                      ",",ifelse(KPI %in% c("Inpatient Revenue per Inpatient Day","EBITDA per Adjusted Patient Day",
                                            "Employment Expense per Adjusted Patient Day","Supply Expense per Adjusted Patient Day",
                                            "Purchased Services per Adjusted Patient Day"),paste("$",q75,sep=""),
                                 ifelse(KPI %in% c("Average Length of Stay","TIE Ratio"),paste(q75),paste(q75,"%",sep=""))),
                      "]")
            
          )%>%select(-q75)%>% `colnames<-`(c("KPI", "Me", "Percentile [25th,75th]","Baseline","Trend"))%>%
          
          format_table(align = "l")%>% 
          htmltools::HTML() %>%
          div() %>%
          spk_add_deps() %>%
          {column(width=10, .)}
    
      return(income_table)
    })
   output$Table<-renderUI({
     income_statement()
   })

  ## -----------<< Actual v.s. Budget Figures >>---------------------------------------------------------------------
   plot <- eventReactive(input$actual_budget, {
     figure_df<-df()
     dat_me<-filter(figure_df,Group=="Me")
     dat_me<-reshape(subset(dat_me,(fiscalyear>=2015 & fiscalyear<=2017)& 
                                         kpi %in% c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
                                                    "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
                                                    "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
                                                    "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday"),
                                       select=c(customer_entity,fiscalyear,timeclass,kpi,value)),idvar=c("customer_entity","fiscalyear","kpi"),timevar="timeclass",direction="wide")%>% 
       mutate(
         variance=ifelse(kpi %in% c("iprevperipday","revenue_growthrate","npr_totalgrosscharge",
                                    "ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin"),value.A-value.B,value.B-value.A),
         percent_variance=round((variance/value.B)*100,2),
         status=case_when(variance<0 ~ "Unfavorable",
                          variance>=0 ~ "Favorable"))
     
     # For metrics with negative values, make sure the status and percent_variance value are consistent
     dat_me$percent_variance<-ifelse((dat_me$status=="Unfavorable"&dat_me$percent_variance>0)|
                                     (dat_me$status=="Favorable"&dat_me$percent_variance<0),
                                     dat_me$percent_variance*(-1),dat_me$percent_variance)
     dat_me$variance_me<-dat_me$percent_variance
         
     
     dat_baseline<-filter(figure_df,Group=="Baseline")
     dat_baseline<-reshape(subset(dat_baseline,(fiscalyear>=2015 & fiscalyear<=2017)& 
                                              kpi %in% c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
                                                         "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
                                                         "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
                                                         "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday"),
                                            select=c(customer_entity,fiscalyear,timeclass,kpi,value)),idvar=c("customer_entity","fiscalyear","kpi"),timevar="timeclass",direction="wide")%>% 
       mutate(
         variance=ifelse(kpi %in% c("iprevperipday","revenue_growthrate","npr_totalgrosscharge",
                                    "ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin"),value.A-value.B,value.B-value.A),
         percent_variance=round((variance/value.B)*100,2),
         status=case_when(variance<0 ~ "Unfavorable",
                          variance>=0 ~ "Favorable"))
     
     # For metrics with negative values, make sure the status and percent_variance value are consistent
     dat_baseline$percent_variance<-ifelse((dat_baseline$status=="Unfavorable"&dat_baseline$percent_variance>0)|
                                       (dat_baseline$status=="Favorable"&dat_baseline$percent_variance<0),
                                     dat_baseline$percent_variance*(-1),dat_baseline$percent_variance)
     dat_baseline$variance_baseline<-dat_baseline$percent_variance
         
     
     dat_me$fiscalyear<-as.factor(dat_me$fiscalyear)
     dat_baseline$fiscalyear<-as.factor(dat_baseline$fiscalyear)
     
     
     p <- plot_ly(data = dat_baseline[which(dat_baseline$kpi==input$actual_budget),], x =~fiscalyear, y =~variance_baseline,type="scatter",color=~status,
                  mode = 'markers',colors=c("lime green","red"),marker=list(symbol="circle-open",size=8),text=~paste(customer_entity))%>%
       add_trace(data=dat_me[which(dat_me$kpi==input$actual_budget),],x=~fiscalyear,y=~variance_me,type="scatter",mode='markers',marker=list(color="black",symbol="x",size=10),showlegend=FALSE)%>%
       layout(xaxis=list(title="Fiscal Year"),yaxis=list(title="% Variance"))
   
     return(p)
   })
   
   output$plot <- renderPlotly({
     plot()
   })

   ## -----------<< Trend Over Year >>---------------------------------------------------------------------
   
   trendplot <- eventReactive(input$trend, {
   trend_df<-df()
   trend_me<-filter(trend_df,Group=="Me")
   trend_me<-subset(trend_me,(timeclass=="A"|fiscalyear==2018)& 
                      kpi %in% c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
                                 "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
                                 "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
                                 "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday"),
                    select=c(customer_entity,fiscalyear,kpi,value))
   
   trend_baseline<-filter(trend_df,Group=="Baseline")
   trend_baseline<-subset(trend_baseline,(timeclass=="A"|fiscalyear==2018)& 
                            kpi %in% c("iprevperipday","revenue_growthrate","npr_totalgrosscharge","baddebt_npr","charity_npr",
                                       "employmentexpense_npr","salaries_npr","supplies_npr","purchasedservice_npr",
                                       "totalexpense_npr","ebitda_adpatientday","tie_ratio","operatingmargin","netincomemargin",
                                       "alos","employmentexpense_adpatientday","supplyexpense_adpatientday","purchasedservice_adpatientday"),
                          select=c(customer_entity,fiscalyear,kpi,value))%>%
     group_by(kpi,fiscalyear)%>%
     summarise(
       average=round(mean(value,na.rm=TRUE),3),
       median=round(quantile(value,0.5,na.rm=TRUE),3),
       q25=round(quantile(value,0.25,na.rm=TRUE),3),
       q75=round(quantile(value,0.75,na.rm=TRUE),3)
     )
   
   trend_me$fiscalyear<-as.factor(trend_me$fiscalyear)
   trend_me<-trend_me[order(trend_me$kpi,trend_me$fiscalyear),]
   trend_me$value<-round(trend_me$value,3)
   trend_baseline$fiscalyear<-as.factor(trend_baseline$fiscalyear)
  
   trendplot <- plot_ly(data=trend_me[which(trend_me$kpi==input$trend),],x=~fiscalyear,y=~value,type='scatter',mode='lines',
                        line=list(color='coral'),name='Me')%>%
     add_trace(data = trend_baseline[which(trend_baseline$kpi==input$trend),],x=~fiscalyear,y=~median,type="scatter",mode='lines',line=list(color='green',dash='dash'),
               name='Median')%>%
     add_trace(data = trend_baseline[which(trend_baseline$kpi==input$trend),], x =~fiscalyear, y =~q75,type="scatter",mode="lines",
               line=list(color='transparent'),name="75th percentile",showlegend=FALSE)%>%
     add_trace(data= trend_baseline[which(trend_baseline$kpi==input$trend),],x=~fiscalyear,y=~q25,type="scatter",mode='lines',fill='tonexty',fillcolor='rgba(0,100,80,0.2)',line=list(color='transparent'),
               showlegend=FALSE,name='25th percentile',showlegend=FALSE)%>%
     layout(xaxis = list(title = "Fiscal Year",
                         showticklabels = TRUE,
                         zeroline = TRUE),
            yaxis = list(title = "",
                         showticklabels = TRUE,
                         zeroline = TRUE))
   
   
   return(trendplot)
})

output$trendplot <- renderPlotly({
  trendplot()
}) 

  ## -----------< Session >-----------
  session$allowReconnect("force")
}
 

#### RUN APP #### 
shinyApp(ui = ui, server = server)



