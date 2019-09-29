######################################################################
# Rockets Research:
# Specifically, which duos in the last 20 years took the 
# largest share of their team's shot attempts?
# How good were those offenses?
# I want to use the usage rate this time (rather than shot attempts).
# 8/30/19
######################################################################

setwd("~/Desktop/Threes and Layups Articles/Player Growth/Data")
dir()
# Read in data:
x <- read.csv('advanced_1995_2019.csv', stringsAsFactors = F)

# Get a column for team and year:
x$team_id_yr <- paste(x$team_id, x$yr, sep = '_')

# Next, my code will go through each team and find it's two 
# highest shot guzzlers. (There will be a minimum games criteria).

y <- unique(x[, c('team_id', 'yr', 'team_id_yr')])
y <- y[y$team_id != '',]
head(y)
y <- y[y$yr >= 2000,]

# Initialize columns for leading shot takers:
y$leader_1 <- 0
y$leader_2 <- 0
y$usg_1 <- 0
y$usg_2 <- 0
y$ts_1 <- 0
y$ts_2 <- 0
y$mp_1 <- 0
y$mp_2 <- 0

mp_thresh <- 1800
for(i in 1:nrow(y)){
  w <- x[x$team_id_yr == y$team_id_yr[i],]
  # Scale mp in lockout shortened 2012:
  if(y$yr[i] == 2012){
    w$mp <- (82/66) * w$mp
  }
  
  # Narrow down the players to just those with the minimum mp played.
  w <- w[w$mp >= mp_thresh,]
  w <- w[order(w$usg_pct, decreasing = T),]
  
  y$leader_1[i] <- w$name[1]
  y$leader_2[i] <- w$name[2]
  y$usg_1[i] <- w$usg_pct[1]
  y$usg_2[i] <- w$usg_pct[2]
  y$ts_1[i] <- w$ts_pct[1]
  y$ts_2[i] <- w$ts_pct[2]
  y$mp_1[i] <- w$mp[1]
  y$mp_2[i] <- w$mp[2]
}

y$usg_tot <- y$usg_1 + y$usg_2
View(y) # Interestingly, the top 3 are:
# 1) Jordan and Richard Hamilton on the '02 Wiz
# 2) Westbrook and Durant on the '12 Thunder
# 3) Dwyane Wade and Beasley on the '09 Heat

###################################################
# Next question: Are there any comparable movements
# of high usage players onto teams with other high usage
# players?

# Identify the highest usage players who switched teams:

# Scale MP to account for 2012
x$mp2 <- x$mp
x$mp2[x$yr == 2012] <- (82/66) * x$mp2[x$yr == 2012]

# Step 1: Get all players who switched teams.
x2 <- x
x2$yr_next <- x2$yr - 1
x2$id_yr <- paste(x2$id, x2$yr_next, sep = '_')
x$id_yr <- paste(x$id, x$yr, sep = '_')

x3 <- merge(x, x2[,c('id_yr','team_id')],
            by.x = 'id_yr', by.y = 'id_yr', all.x = T)
# Remove players who did not play in next season:
x3 <- x3[!is.na(x3$team_id.y),]
# Ignore "total" rows:
x3 <- x3[!(x3$team_id.y == ''),]
x3 <- x3[!(x3$team_id.x == ''),]
# Filter by minutes played threshold in previous season:
x3 <- x3[x3$mp2 >= mp_thresh,]
# Filter to only those who changed teams:
x3 <- x3[x3$team_id.x != x3$team_id.y,]
# Down to 1017 players. Take a look at these:
View(x3[,c('name','yr','mp2','usg_pct','team_id.x','team_id.y')])

# Next step: Get the previous usage leader of their new team.

# See how the usage and efficiency of both players changed.