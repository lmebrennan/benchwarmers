options(prompt="R> ",
        defaultPackages = "")



shhh<-function(a.package){
  suppressWarnings(suppressPackageStartupMessages(
    library(a.package,character.only=TRUE)
  ))
}

auto.loads<-c("shiny",
              "civis",
              "")

.First <- function() {
  install.packages("shiny")
}

.Last <- function() {
  
}


