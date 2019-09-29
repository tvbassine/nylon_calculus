########################################################
# Rockets Research:
# Specifically, which duos in the last 20 years took the 
# largest share of their team's shot attempts?
# How good were those offenses?
# 8/29/19
########################################################

setwd("~/Desktop/Threes and Layups Articles/Player Growth/Data")
dir()
# Read in data:
x <- read.csv('totals_1995_2019.csv', stringsAsFactors = F)

# Encode a column for true shooting attempts:
x$tsa <- x$fga + 0.44 * x$fta

# True Shooting Attempts per game:
x$tsa_game <- x$tsa / x$g

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
y$shot_1 <- 0
y$shot_2 <- 0
y$gp_1 <- 0
y$gp_2 <- 0
y$shot_tm <- 0

gp_thresh <- 65
for(i in 1:nrow(y)){
  w <- x[x$team_id_yr == y$team_id_yr[i],]
  # Get total true shooting attempts of the team:
  y$shot_tm[i] <- sum(w$tsa)
  # Narrow down the players to just those with the minimum games played.
  w <- w[w$g >= gp_thresh,]
  w <- w[order(w$tsa_game, decreasing = T),]
  
  y$leader_1[i] <- w$name[1]
  y$leader_2[i] <- w$name[2]
  y$shot_1[i] <- w$tsa_game[1]
  y$shot_2[i] <- w$tsa_game[2]
  y$gp_1[i] <- w$g[1]
  y$gp_2[i] <- w$g[2]
}

y$g <- 82
y$tsa_game <- y$shot_tm / y$g
y$prop_1 <- y$shot_1 / y$tsa_game
y$prop_2 <- y$shot_2 / y$tsa_game
y$prop_tot <- y$prop_1 + y$prop_2
View(y)
View(y[y$team_id_yr %in% c('HOU_2019','OKC_2019'),])
