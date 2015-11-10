library(reshape2)
library(ggplot2)
library(dplyr)

load.data <- function() {
  options(timeout=600)
  print(system.time(temps <- read.csv(url("http://10.0.0.176:7645/t/data.csv"))))
  temps$Date <- as.Date(temps$Date)
  temps$Temperature <- temps$Value - 273.15
  temps
}

do.graph <- function(temps) {
  by.month <- 
    temps %>% 
    group_by(Date,Scenario) %>% 
    summarise(Mean=mean(Temperature), 
              Max=max(Temperature), 
              Min=min(Temperature))
  by.month$year <- as.Date(format(by.month$Date, "%Y-01-01"))

  by.year <- 
    by.month %>% 
    group_by(year, Scenario) %>% 
    summarise(Temperature=max(Max))

  ggplot(by.year, aes(x=year, y=Temperature, 
                      color=Scenario, group=Scenario)) + 
    geom_line() + 
    stat_smooth(method="loess", level=.8) + 
    ggtitle(sprintf("Maximum mean temperature for warmest month using model %s", temps$Model[1]))

}