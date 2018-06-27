###################################################################################################################################-
##### 3. FUNCTIONS -----------------------------------------------------------------------------------------------------------------
###################################################################################################################################-

##### _ a. General Functions ------------------------------------------------

# function to calculate summary info
calcSummary <- function(df, summary_var, outlier_threshold = 2, grouping_vars = NULL){
  summary_var <- parse_quosure(summary_var)
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


##### _ b. Graphing Functions ------------------------------------------------

# theming plots
theme_plot<-function(base_size = 10,legend.pos = 'bottom') {
  t = (theme_foundation(base_size = base_size)
       + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.x = element_text(vjust = -0.2),
               axis.title.y = element_text(angle = 90,vjust = 2),
               axis.text = element_text(),
               axis.line = element_line(colour = "black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour = "#f0f0f0"),
               panel.grid.minor = element_black(),
               legend.key = element_rect(colour=NA),
               legend.position = legend.pos,
               legend.title = element_text(face="italic"),
               plot.margin = unit(c(10,5,5,5),"mm"),
               strip.background = element_rect(colour = "#f0f0f0"),
               strip.text = element_text(face = "italic")
       )
  )
  return(t)
}

scale_fill_plot <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

scale_colour_plot <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

plot_custom <- function(p, saveTo = NULL, palette = 'tableau20', base_size=10, legend.pos = "right", color = TRUE, fill = FALSE) {
  out = p + theme_plot(base_size, legend.pos)
  if(color) out = out + scale_colour_tableau(palette = palette)
  if(fill) out = out + scale_fill_tableau(palette = palette)
  if(is.null(saveTo)) return(out)
  ggsave(saveTo, out)
  return(out)
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

