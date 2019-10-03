#############################################################
# Clean Season Win Totals
#############################################################

# Read in data:
setwd("~/Desktop/nylon_calculus/draft_pick_value_project/data")
years <- 2000:2019
out <- list()
count <- 1
for(yr in years){
  add_on <- substr(yr, 3, 4)
  temp <- read.csv(paste('misc_', add_on, '.csv', 
                         sep = ''), stringsAsFactors = F)
  temp$yr = yr
  out[[count]] <- temp
  count <- count + 1
}

# Combine all the years into one data.frame:
x <- do.call('rbind', out)

# Some cleaning of team names:
x$Team <- gsub('\\*', '',x$Team)

# Deal with teams who changed names
x$Team[x$Team == 'Charlotte Hornets' & x$yr <= 2002] <- 'New Orleans Pelicans'
x$Team[grep('New Jersey', x$Team)] <- 'Brooklyn Nets'
x$Team[grep('New Orleans', x$Team)] <- 'New Orleans Pelicans'
x$Team[grep('Vancouver Grizzlies', x$Team)] <- 'Memphis Grizzlies'
x$Team[grep('Seattle SuperSonics', x$Team)] <- 'Oklahoma City Thunder'
x$Team[grep('Charlotte', x$Team)] <- 'Charlotte Hornets'
x <- x[!(x$Team == 'League Average'),]

# Team and year:
x$tm_yr <- paste(x$Team, x$yr, sep = '_')

# Get win rank each year:
x$rank <- 0
for(i in unique(x$yr)){
  ind <- which(x$yr == i)
  x$rank[ind] <- rank(x$W[ind])
}

# Rank next year, and the year after, ... up to 7 years later:
x$rank_1 <- NA
x$rank_2 <- NA
x$rank_3 <- NA
x$rank_4 <- NA
x$rank_5 <- NA
x$rank_6 <- NA
x$rank_7 <- NA

for(i in 1:nrow(x)){
  try(x$rank_1[i] <- x$rank[x$Team == x$Team[i] & x$yr == (x$yr[i] + 1)])
  try(x$rank_2[i] <- x$rank[x$Team == x$Team[i] & x$yr == (x$yr[i] + 2)])
  try(x$rank_3[i] <- x$rank[x$Team == x$Team[i] & x$yr == (x$yr[i] + 3)])
  try(x$rank_4[i] <- x$rank[x$Team == x$Team[i] & x$yr == (x$yr[i] + 4)])
  try(x$rank_5[i] <- x$rank[x$Team == x$Team[i] & x$yr == (x$yr[i] + 5)])
  try(x$rank_6[i] <- x$rank[x$Team == x$Team[i] & x$yr == (x$yr[i] + 6)])
  try(x$rank_7[i] <- x$rank[x$Team == x$Team[i] & x$yr == (x$yr[i] + 7)])
}

# Write data to a csv:
setwd("~/Desktop/nylon_calculus/draft_pick_value_project/data")
write.csv(x, 'pre_processed_data.csv', row.names = F)

# END SCRIPT
###################################################################
# Plot some relationships!!

# Record 1 year away
plot(x$rank, x$rank_1)
cor(x$rank, x$rank_1) #0.58

# Record 2 years away
plot(x$rank, x$rank_2)
cor(x$rank, x$rank_2) #0.34

# Record 3 years away
plot(x$rank, x$rank_3)
cor(x$rank, x$rank_3) #0.16

# Record 5 years away
plot(x$rank, x$rank_5)
cor(x$rank, x$rank_5) #~0.01 (so not much)

# Does age of roster have any effect??
summary(lm(rank_3 ~ rank + Age, data = x)) 
#Age is significantly negative
#So, as we thought, older team tend to be worse in future years (and vice versa for younger).
# SE is roughly 8 spots, which is a ton

# What if we say the Lakers are the 5th best team this season??
# What would the expected pick that they give up 3 years from now?
# (I'll say age is 26.5)

41.98 + 0.27 * 26 - 1.10 * 26.5 #Expected to pick around 20 in 3 years
