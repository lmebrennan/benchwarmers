#### HELPER FUNCTIONS FOR STRATA BENCHMARKING PILOT ####
## This script contains helper functions for calculating Strata benchmarks
## February 13, 2018


## Function to read in Strata files ##

readStrataFile <- function(filename){
  tryCatch({
    out <- read_csv(file = filename, col_types = cols(.default = col_character(), 
                                                      "Age" = col_double(),
                                                      "Charge" = col_double(),
                                                      "LengthOfStay" = col_double(),
                                                      "FixedDirectCost" = col_double(),
                                                      "FixedIndirectCost" = col_double(),
                                                      "VariableDirectCost" = col_double(),
                                                      "VariableIndirectCost" = col_double())) %>%
      filter(!grepl("affected", CustomerID))  # remove random metadata row
    return(out)
    
  }, error = function(err){
    print(paste("MY_ERROR:  ",err))
    out <- read_delim(file = filename, delim = "\t", col_types = cols(.default = col_character(),
                                                                      "Age" = col_double(),
                                                                      "Charge" = col_double(),
                                                                      "LengthOfStay" = col_double(),
                                                                      "FixedDirectCost" = col_double(),
                                                                      "FixedIndirectCost" = col_double(),
                                                                      "VariableDirectCost" = col_double(),
                                                                      "VariableIndirectCost" = col_double())) %>%
      filter(!grepl("affected", CustomerID))  # remove random metadata row
    return(out)
    
  }
  )
}



## Function to calculate Summary info ##

calcSummary <- function(df, summary_var, outlier_threshold = 2, grouping_vars = NULL){
  summary_var <- parse_quosure(summary_var)
  #grouping_vars <- quos(...)
  
  if(is.null(grouping_vars)){
    out <- df %>%
      summarise(min = round(summary(!!summary_var, na.rm = TRUE)[[1]], 2),
                q1 = round(summary(!!summary_var, na.rm = TRUE)[[2]], 2),
                median = round(summary(!!summary_var, na.rm = TRUE)[[3]], 2),
                mean = round(summary(!!summary_var, na.rm = TRUE)[[4]], 2),
                q3 = round(summary(!!summary_var, na.rm = TRUE)[[5]], 2),
                max = round(summary(!!summary_var, na.rm = TRUE)[[6]], 2),
                sd = round(sd(!!summary_var, na.rm = TRUE), 2),
                total = round(sum(!!summary_var, na.rm = TRUE), 2),
                obs = n()) %>%
      mutate(IQR = round(q3 - q1, 2),
             IQR_outlier_high = round(q3 + (1.5*IQR), 2),
             IQR_outlier_low = round(q1 - (1.5*IQR), 2),
             sd_outlier_high = round(mean + (outlier_threshold*sd), 2),
             sd_outlier_low = round(mean - (outlier_threshold*sd), 2))
  } else {
    out <- df %>%
      group_by(.dots = grouping_vars) %>%
      summarise(min = round(summary(!!summary_var, na.rm = TRUE)[[1]], 2),
                q1 = round(summary(!!summary_var, na.rm = TRUE)[[2]], 2),
                median = round(summary(!!summary_var, na.rm = TRUE)[[3]], 2),
                mean = round(summary(!!summary_var, na.rm = TRUE)[[4]], 2),
                q3 = round(summary(!!summary_var, na.rm = TRUE)[[5]], 2),
                max = round(summary(!!summary_var, na.rm = TRUE)[[6]], 2),
                sd = round(sd(!!summary_var, na.rm = TRUE), 2),
                total = round(sum(!!summary_var, na.rm = TRUE), 2),
                obs = n()) %>% ungroup() %>%
      mutate(IQR = round(q3 - q1, 2),
             IQR_outlier_high = round(q3 + (1.5*IQR), 2),
             IQR_outlier_low = round(q1 - (1.5*IQR), 2),
             sd_outlier_high = round(mean + (outlier_threshold*sd), 2),
             sd_outlier_low = round(mean - (outlier_threshold*sd), 2))
  }
  return(out)
}


## Function to calculate Sums info ##

groupingCalcs <- function(df, grouping_vars){
  
  out <- df %>%
    group_by(.dots = grouping_vars) %>%
    summarise(FixedDirectCost = sum(FixedDirectCost, na.rm = TRUE),
              FixedIndirectCost = sum(FixedIndirectCost, na.rm = TRUE),
              VariableDirectCost = sum(VariableDirectCost, na.rm = TRUE),
              VariableIndirectCost = sum(VariableIndirectCost, na.rm = TRUE)
    ) %>% ungroup() %>%
    gather("CostKey", "Costs", (ncol(.)-3):ncol(.)) %>%
    mutate(
      FixedVariable = case_when(
        CostKey == "VariableDirectCost" | CostKey == "VariableIndirectCost" ~ "Variable",
        CostKey == "FixedDirectCost" | CostKey == "FixedIndirectCost" ~ "Fixed"),
      DirectIndirect = case_when(
        CostKey == "VariableDirectCost" | CostKey == "FixedDirectCost" ~ "Direct",
        CostKey == "VariableIndirectCost" | CostKey == "FixedIndirectCost" ~ "Indirect")
    )
  
  return(out)
}


## Function to label APR-DRG codes ##
labelAPRDRG <- function(codes, values = FALSE){
  labelled_codes <- c("221 - Major Small and Large Bowel Procedures" = "221",
                      "225 - Appendectomy" = "225",
                      "303 - Dorsal and Lumbar Fusion Proc for Curvature of Back" = "303",
                      "420 - Diabetes" = "420",
                      "693 - Chemotherapy" = "693",
                      "696 - Other Chemotherapy" = "696")
  x <- match(codes, labelled_codes)
  if(values == FALSE){
    return(labelled_codes[x])
  }
  else if(values == TRUE){
    return(names(labelled_codes[x]))
  }
}



## Function to add facet to a plot ##
addFacet <- function(plot, facet_formula) {
  plot +
    facet_grid(facet_formula)
}


## Function to adjust plot scales ##
setY_AxisScale <- function(plot, logscale = FALSE){
  if(logscale){
    plot + 
      scale_y_log10(name = "Cost per Encounter\n($)",
                    labels = scales::dollar)
  }
  else {
    plot +
      scale_y_continuous(name = "Cost per Encounter\n($)",
                         labels = scales::dollar)
  }
  
  return(plot)
}



