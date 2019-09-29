#######################################################
# Schedule Playground:
# Date: 8/24/19
# Purpose- I want to use this code to answer questions
# like 
# 1) What is a typical schedule like in terms of average 
# number of games won by an average team?
# 2) What is the source of discrepancies in schedule? 
# (Quantify impact of East vs. West, unbalanced shedules
# in terms of who you play 3 times vs. 4, back-to-backs)
# 3) Does having a front-loaded or back-loaded schedule make 
# a difference in terms of exceeding pre-season O/U?
# 4) Do tanking teams play measurably worse in, say, 
# the last 10 or 20 games?
#######################################################

# Step 1:
# Read in Schedules and Season O/U. Merge the 2.
setwd("~/Desktop/NBA Projection System/data")
x <- read.csv("data_2007_2019.csv", stringsAsFactors = F)

years <- c(2007:2011,2013:2019)
out <- list()

for(i in 1:length(years)){
  yr <- years[i]
  file = paste('~/Desktop/NBA Projection System/data/preseason_', yr, '.csv', sep = '')
  out[[i]] <- read.csv(file, stringsAsFactors = F)
  out[[i]]$year <- yr
}

w <- do.call('rbind', out)

w$team_year <- paste(w$Team, w$year, sep = '_')

w$pd <- 0

# Step 2:
# Come up with my basic point differential, home, back-to-back model.
# Home and away strength are the teams' final point differential.
# Home point differential advantage is the difference between the 2two.
summary(fit <- lm(h_margin ~ home_pd_adv + bb_id, data = x))

# Step 3: 
# Get an estimate of point diff from O/U (make sure to scale 
# based on total values for the year).

# How many total wins according to the lines each year?
tot_wins <- aggregate(w$W.L.O.U ~ w$year, w, mean)
colnames(tot_wins) = c('year', 'Average_O_U')
w <- merge(w, tot_wins, by = 'year', all.x = T)
w$o.u.modified <- w$W.L.O.U - w$Average_O_U + 41 # This is modified O/U if avg. line is scaled to 41 wins

# What is final point differential of each team on the year?
x <- merge(x, w[,c('team_year', 'o.u.modified')],
           by.x = 'away_year',
           by.y = 'team_year',
           all.x = T)

# Subset to just point differential and O/U:
x2 <- unique(x[, c('away_year', 'away_strength', 'o.u.modified')])

# What is relationship between O/U and average point diff?
summary(fit2 <- lm(away_strength ~ o.u.modified,
                   data = x2))

# Merge on home and away team's predicted point diff 
# based on O/U:
w$away_pd_ou <- w$o.u.modified * 0.34018 - 13.94727
w$home_pd_ou <- w$o.u.modified * 0.34018 - 13.94727

x <- merge(x, w[,c('team_year', 'away_pd_ou')],
           by.x = 'away_year',
           by.y = 'team_year',
           all.x = T)
x <- merge(x, w[,c('team_year', 'home_pd_ou')],
           by.x = 'home_year',
           by.y = 'team_year',
           all.x = T)

# Step 4: 
# Create function (and use) to get average number of wins an
# average team would get against each schedule.

# Function:
sched_strength <- function(x, year, team,
                           home_strength = 'home_pd_ou',
                           away_strength = 'away_pd_ou',
                           bb_id = 'bb_id',
                           date_range = 'all',
                           gm_num = 1:82){
  y <- x[x$year == year & (x$home == team | x$away == team) ,]
  
  if(date_range != 'all'){
    y <- y[y$dateR >= as.Date(date_range[1]) &
           y$dateR <= as.Date(date_range[2]) ,]
  }
  
  # Restrict to only the specified games
  y <- y[order(y$dateR, decreasing = F),]
  y$gm_num = 1:nrow(y)
  y <- y[y$gm_num %in% gm_num,]
  
  home_gms <- which(y$home == team)
  away_gms <- which(y$away == team)
  
  # Identify which columns have point dif and which have back-to-backs
  col_hm <- which(colnames(y) == home_strength)
  col_aw <- which(colnames(y) == away_strength)
  col_bb <- which(colnames(y) == bb_id)
  
  # Make the team we are trying to get the schedule strength for 
  # have "0" point diff
  y[home_gms, col_hm] <- 0
  y[away_gms, col_aw] <- 0
  
  # Get expected home score each game:
  y$exp_home_score <- 2.58956 + (0.96332 * (y[,col_hm] - y[,col_aw])) +
                      -1.72502 * y[,col_bb]
  
  # Get expected home win probability for each game:
  y$exp_home_win <- pnorm(y$exp_home_score, mean = 0, sd = 11.67)
  
  # Get expected number of wins for our team and total games played:
  exp_wins <- sum(y$exp_home_win[home_gms]) + sum(1 - y$exp_home_win[away_gms])
  
  # Return:
  return(out = list(exp_wins = exp_wins,
                    gp = nrow(y)))
}

# Apply function:
z <- unique(x[,c('home', 'year')])
colnames(z)[1] <- 'team'
z$exp_wins <- 0
z$gp = 0

for(i in 1:nrow(z)){
  out <- sched_strength(x = x, year = z$year[i], team = z$team[i])
  z$exp_wins[i] = out$exp_wins
  z$gp[i] = out$gp
}


# Step 5: 
# Affect of unbalanced scheds on O/U wins.
z$exp_wins_half1 <- 0
z$exp_wins_half2 <- 0

for(i in 1:nrow(z)){
  out <- sched_strength(x = x, year = z$year[i], team = z$team[i],
                        gm_num = 1:41)
  z$exp_wins_half1[i] = out$exp_wins
  out <- sched_strength(x = x, year = z$year[i], team = z$team[i],
                        gm_num = 42:82)
  z$exp_wins_half2[i] = out$exp_wins
}

# What is difference in schedule strength?
# (Positive means schedule gets easier,
# negative means more difficult)
z$half2_minus_half1 <- z$exp_wins_half2 - z$exp_wins_half1

# Does an unbalanced schedule correlate with over/under performing?:
z$team_year <- paste(z$team, '_', z$year, sep = '')
w <- merge(w, z[,c('team_year', 'exp_wins', 'gp',
                   'half2_minus_half1')],
           by = 'team_year', all.x = T)
w$actual_wins <- 0
for(i in 1:nrow(w)){
  w$actual_wins[i] = unlist(strsplit(w$Result[i], split = '-'))[1]
}

w$actual_wins <- as.numeric(w$actual_wins)
w$actual_minus_ou <- w$actual_wins - w$o.u.modified

# Is there any correlation between unbalanced schedule and performance?
cor(w$actual_minus_ou, w$half2_minus_half1) #~0.03, so, no
cor(w$actual_minus_ou, abs(w$half2_minus_half1))


# Step 6: 
# Tankers do worse at end of season?

# Step 7: 
# Read in this year's schedule and O/U to get each team's strength 
# of schedule.

# Read in this year's schedule:
setwd("~/Desktop/NBA Projection System/data")
x20 <- read.csv('schedule_ 2020 .csv', stringsAsFactors = F)
x20$bb_id <- x20$home_bb - x20$away_bb

# Read in O/U (and clean):
ou20 <- read.csv("NBA_over_unders_2020 - Data.csv", stringsAsFactors = F)
mean_wins <- mean(ou20$Westgate.Current)
ou20$o.u.modified <- ou20$Westgate.Current - mean_wins + 41
ou20$away_pd_ou <- ou20$o.u.modified * 0.34018 - 13.94727
ou20$home_pd_ou <- ou20$o.u.modified * 0.34018 - 13.94727

# The Sixers need to be renamed to the 76ers to match the schedule
ou20$Team[ou20$Team == 'Sixers'] <- '76ers'

# Attach away_pd_ou and home_pd_ou to 2020 schedule:
x20$away_pd_ou <- 0
x20$home_pd_ou <- 0
for(i in 1:nrow(ou20)){
  away_ind <- grep(ou20$Team[i], x20$away)
  print(length(away_ind))
  if(length(away_ind) == 0){print(paste(ou20$Team[i], 'is a problem'))}
  x20$away_pd_ou[away_ind] <- rep(ou20$away_pd_ou[i],41)
  home_ind <- grep(ou20$Team[i], x20$home)
  print(length(home_ind))
  x20$home_pd_ou[home_ind] <- rep(ou20$home_pd_ou[i],41)
}

# Step 8:
# Get each team's expected wins if they were of average strength:
z <- unique(x20[,c('home', 'year')])
colnames(z)[1] <- 'team'
z$exp_wins <- 0
z$gp = 0

for(i in 1:nrow(z)){
  out <- sched_strength(x = x20, year = z$year[i], team = z$team[i])
  z$exp_wins[i] = out$exp_wins
  z$gp[i] = out$gp
}

# Some Sanity Checking 
mean(z$exp_wins) #40.98, so we're good here
mean(x20$away_pd_ou)
mean(x20$home_pd_ou)
mean(ou20$away_pd_ou)


# Step 9:
# Analyze source of SOS differences.
# (Conference disparities, back-to-back, unbalanced scheds)

# Get total number of back-to-backs:
z$bb <- 0
z$bb_opp <- 0
for(i in 1:nrow(z)){
  away_ind <- which(x20$away == z$team[i])
  away_bb <- sum(x20$away_bb[away_ind])
  home_bb_opp <- sum(x20$home_bb[away_ind])
  home_ind <- which(x20$home == z$team[i])
  home_bb <- sum(x20$home_bb[home_ind])
  away_bb_opp <- sum(x20$away_bb[home_ind])
  
  z$bb[i] <- away_bb + home_bb
  z$bb_opp[i] <- away_bb_opp + home_bb_opp
}

z$bb_ad <-  z$bb_opp - z$bb

# Strength of Schedule 1st half vs. 2nd half:
z$exp_wins_half1 <- 0
z$exp_wins_half2 <- 0
z$half2_minus_half1 <- z$exp_wins_half2 - z$exp_wins_half1

for(i in 1:nrow(z)){
  out <- sched_strength(x = x20, year = z$year[i], team = z$team[i],
                        gm_num = 1:41)
  z$exp_wins_half1[i] = out$exp_wins
  out <- sched_strength(x = x20, year = z$year[i], team = z$team[i],
                        gm_num = 42:82)
  z$exp_wins_half2[i] = out$exp_wins
}

# Which opponents does each team play three times?
z$tms3 <- 0

for(i in 1:nrow(z)){
  away_ind <- which(x20$away == z$team[i])
  home_ind <- which(x20$home == z$team[i])
  tms <- c(x20$away[home_ind], x20$home[away_ind])
  tab <- table(tms)
  tms3  <- names(tab)[tab == 3]
  z$tms3[i] <- paste(tms3, collapse = ', ')
}

# Print Out Some Nice Data Tables For The Nylon Article:
head(z)
View(z)
z$conf <- c('E', 'E', 'E', 'W', 'W', 'W',
            'W', 'W', 'W', 'E', 'E', 'E',
            'E', 'W', 'E', 'W', 'W', 'W',
            'E', 'E', 'W', 'W', 'W', 'W',
            'E', 'E', 'E', 'E', 'W', 'E')
z <- z[order(z$exp_wins, decreasing = T),]
write.csv(z, 'sched_strength_2020.csv', row.names = F)

mean(z$exp_wins[z$conf == 'E'])
mean(z$exp_wins[z$conf == 'W'])
mean(z$exp_wins)
