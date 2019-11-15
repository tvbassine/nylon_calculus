#######################################################
# This code is to determine 3-point attempt rate over 
# time in a season.
#######################################################

# Read in play-by-play data:

# 2010-2018
setwd("~/Desktop/Clean Play-By-Play")
out <- list()
out[[1]] <- read.csv("pbp2009_2010.csv", stringsAsFactors = F)
out[[2]] <- read.csv("pbp2010_2011.csv", stringsAsFactors = F)
out[[3]] <- read.csv("pbp2011_2012.csv", stringsAsFactors = F)
out[[4]] <- read.csv("pbp2012_2013.csv", stringsAsFactors = F)
out[[5]] <- read.csv("pbp2013_2014.csv", stringsAsFactors = F)
out[[6]] <- read.csv("pbp2014_2015.csv", stringsAsFactors = F)
out[[7]] <- read.csv("pbp2015_2016.csv", stringsAsFactors = F)
out[[8]] <- read.csv("pbp2016_2017.csv", stringsAsFactors = F)
out[[9]] <- read.csv("pbp2017_2018.csv", stringsAsFactors = F)

# 2019 and 2020
out[[10]] <- read.csv("games.2019.csv", stringsAsFactors = F)
out[[10]]$yr <- 2019
out[[11]] <- read.csv("games.2020.csv", stringsAsFactors = F)
out[[11]]$yr <- 2020

# Just keep play, id (the game # in a given year), and yr
library(dplyr)
for(i in 1:11){
  out[[i]] <- select(out[[i]], play, id, yr)
}

x <- do.call('rbind', out)

rm(out)

#######################################################
# Get shot distance and whether 2 or 3 point attempt for each 
# field goal attempt.

shot_dist_2_or_3 <- function(play){
  two <- grepl('2-pt', play)
  three <- grepl('3-pt', play)
  
  # Get shot distance
  if(grepl('at rim', play)){
    dist <- 0
  }
  else{
    play <- unlist(strsplit(play, split = 'from '))[2]
    play <- unlist(strsplit(play, split = 'ft'))[1]
    dist <- as.numeric(play)
  }
  
  
  return(out = data.frame(dist = dist,
                          two = two,
                          three = three))
} 

# Get all 2-pointers and 3-pointers
ind2 <- grep('2-pt', x$play)
ind3 <- grep('3-pt', x$play)
x <- x[c(ind2,ind3),]

x$dist <- NA
x$two <- NA
x$three <- NA

for(i in 1:nrow(x)){
  out <- shot_dist_2_or_3(x$play[i])
  x$dist[i] = out$dist
  x$two[i] = out$two
  x$three[i] = out$three
  print(i)
}

#####################################################
z <- unique(select(x, yr, id))
z$twos <- NA
z$threes <- NA
z$shots_0_3 <- NA

for(i in 1:nrow(z)){
  temp <- which(x$yr == z$yr[i] & x$id == z$id[i])
  z$twos[i] <- sum(x$two[temp])
  z$threes[i] <- sum(x$three[temp])
  z$shots_0_3[i] <- sum(x$dist[temp] <= 3)
}




# END HERE
#####################################################








# Get all age year and id combos:
z <- unique(select(x, yr, id))
z$twos <- NA
z$threes <- NA

# Get all 2 and 3 point shots by game:
for(i in 1:nrow(z)){
  out <- shot_attempts(x, z$yr[i], z$id[i])
  z$twos[i] <- out$twos
  z$threes[i] <- out$threes
  print(paste(z$yr[i], z$id[i]))
}

##########################################
tail(z)
setwd("~/Desktop/nylon_calculus/Season_trends")
write.csv(z,'two_and_three_pointers_by_game.csv', row.names = F)

z$cum3 <- 0
z$cum2 <- 0

for(i in 1:nrow(z)){
  z$cum3[i] <- sum( z$threes[z$yr == z$yr[i] & z$id <= z$id[i]] )
  z$cum2[i] <- sum( z$twos[z$yr == z$yr[i] & z$id <= z$id[i]] )
  print(i)
}

z$att3_rate <- z$cum3 / (z$cum2 + z$cum3)

plot(z$id[z$yr == 2017 & z$id >= 20],
     z$att3_rate[z$yr == 2017 & z$id >= 20], type = 'l',
     ylim = c(0.28, .38), cex = 1.5, col = 'blue',
     main = 'League Wide 3-Point Attempt Rate Through The Season',
     xlab = 'Game In Season',
     ylab = '3-Point Attempt Rate')
lines(z$id[z$yr == 2018 & z$id >= 20],
      z$att3_rate[z$yr == 2018 & z$id >= 20], type = 'l',
      ylim = c(0.28, .38),  cex = 1.5, col = 'blue')
lines(z$id[z$yr == 2019 & z$id >= 20],
      z$att3_rate[z$yr == 2019 & z$id >= 20], type = 'l',
      ylim = c(0.28, .38),  cex = 1.5, col = 'blue')
lines(z$id[z$yr == 2020 & z$id >= 20],
      z$att3_rate[z$yr == 2020 & z$id >= 20], type = 'l',
      ylim = c(0.28, .38),  cex = 1.5, col = 'blue')
for(i in seq(.28,.38,.01)){
  abline(h = i, lty = 2, col = 'grey')
}

text(600, .308, '2016-2017', col = 'red')
text(600, .331, '2017-2018', col = 'red')
text(600, .348, '2018-2019', col = 'red')
text(150, .375, '2019-2020', col = 'red')

##########################################
# Some thing I wanted to check:
y <- x[x$yr == 2010,]
# How many feet do 2-pointers go out to?
y <- y[grep('2-pt',y$play),]
sum(grepl('16 ft', y$play))
sum(grepl('17 ft', y$play))
sum(grepl('18 ft', y$play))
sum(grepl('19 ft', y$play))
sum(grepl('20 ft', y$play))
sum(grepl('21 ft', y$play))
sum(grepl('22 ft', y$play))
sum(grepl('23 ft', y$play))
sum(grepl('24 ft', y$play))
sum(grepl('25 ft', y$play))
