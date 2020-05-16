################################################
# Playoff Data Compiler
# 2/8/18
# Purpose: Use 'sched_scraper.R' to get all the 
# playoff results from 2003-2018.
################################################

setwd("~/Desktop/Threes and Layups Articles/Playoff Probabilities/Data Scraping Scripts")

source('sched_scraper.R')

################################################
# Section 1:
# Scrape each year and month, 1 at a time.
years <- seq(2018, 2003, by = -1) #2003 was the first year of the current playoff format
monthss <- c('april', 'may', 'june')

for(yr in years){
  for(mon in monthss){
    
    if(mon == 'april' & yr == '2018'){
      out <- sched_scraper(month = mon, year = yr, playoffs = T)
    }
    else{
      temp <- sched_scraper(month = mon, year = yr, playoffs = T)
      out <- rbind(out, temp)
    }
    
    print(yr)
    print(mon)
    
  }
  
  
}

for(yr in years){
print(length(out$year[out$year == yr]))
}

out$result <- out$h_score - out$a_score

################################################
# Section 2:
# Aggregate individual games into series.

out$series_code <- 0

for(i in 1:nrow(out)){
  temp <- c(out$away[i], out$home[i])
  temp <- temp[order(temp)]
  temp <- paste(temp[1], temp[2], out$year[i], sep = '_')
  out$series_code[i] <- temp
}

# Read in regular season point differentials:
setwd("/Users/thomasbassine/Desktop/Threes and Layups Articles/Playoff Probabilities/Data")
y <- read.csv('playoffs_2003_2018.csv',
              stringsAsFactors = F)
y$year <- 0
for(i in 1:nrow(y)){
  y$year[i] <- as.numeric(unlist(strsplit(y$series_code[i], split = '_'))[3])
}

y$away_join <- paste(y$away, '_', y$year, sep = '')
y$home_join <- paste(y$home, '_', y$year, sep = '')

# Merge regular season point differential on:
# Get a table of all point reg. season differentials:

w <- y[,c('away_url','away_net')]
ww <- y[,c('home_url','home_net')]
colnames(w) = c('url', 'pd')
colnames(ww) = c('url', 'pd')

w <- rbind(w, ww)
w <- unique(w)
w$away_pd <- w$pd
w$home_pd <- w$pd

out <- merge(out, w[,c('url','away_pd')], 
             by.x = 'away_url',
             by.y = 'url',
             all.x = T)
out <- merge(out, w[,c('url','home_pd')], 
             by.x = 'home_url',
             by.y = 'url',
             all.x = T)

out$pd_diff <- out$home_pd - out$away_pd
summary(fit <- lm(result ~ pd_diff, data = out))

setwd("~/Desktop/Threes and Layups Articles/Playoff Probabilities/what_if_playoffs_were_best_of_3")
# write out data:
write.csv(out, 'playoff_games_2003_2018.csv', row.names = F)


##########################################################
# Probability of winning a series based of different lengths:

prob_vic <- function(gms, pd_diff){
  out <- rep(0, 100000)
  for(i in 1:length(out)){
    rand <- rnorm(gms, mean = 0, sd = 12.68)
    hm_thresh <- 4.4 + 1.04 * pd_diff
    aw_thresh <- 4.4 - 1.04 * pd_diff
    div <- (gms + 1) / 2
    hm_wins <- sum(rand[1:div] <= hm_thresh) + sum(rand[(div+1):gms] >= aw_thresh)
    if(hm_wins >= div){
      out[i] = 1
    }
  }
  return(pct = mean(out))
}

prob_vic(3, 6)

pnorm(0, 4.4 + 1.04 * 7, 12.7)
