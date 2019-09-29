################################################
# This script simply scrapes schedules for 
# every year from 2007 - 2019 (excluding 2012):
################################################

source("~/Desktop/NBA Projection System/functions/schedule_scraper.R")

years <- c(2007:2011,2013:2019)

for(yr in years){
  out <- sched(yr)
  file = paste('~/Desktop/NBA Projection System/data/schedule_', yr, '.csv')
  write.csv(out, file, row.names = F)
  print(yr)
}
