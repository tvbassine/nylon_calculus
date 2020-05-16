############################################################
# What if Playoff Series were best of 3? Best of 5? 1 Game?
# Date: 3/30/2020
############################################################

# set seed for reproducibility
set.seed(16)

par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(1,1))

setwd("~/Desktop/Threes and Layups Articles/Playoff Probabilities/what_if_playoffs_were_best_of_3")
out <- read.csv('playoff_games_2003_2018.csv', stringsAsFactors = F)

# Probability of winning an individual playoff game:
summary(fit <- lm(result ~ pd_diff, data = out))

# Probability of winning a playoff series based on number of
# games (gms) and home team point diff - away team point diff (pd_diff)
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


# Use prob_vic to determine the probability of winning playoff 
# series under different conditions.
pd_diff <- seq(-2,10,1)
gms <- seq(1,7,2)

x <- data.frame(expand.grid(gms, pd_diff, stringsAsFactors = F))
colnames(x) <- c('gms','pd_diff')
x$pct <- 0

# Probability of winning the series of 3,5, or 7 games
for(i in 1:nrow(x)){
  if(x$gms[i] != 1){
    x$pct[i] = prob_vic(x$gms[i], x$pd_diff[i])
    print(i)
  }
}

# 1 game series:
for(i in 1:nrow(x)){
  if(x$gms[i] == 1){
    x$pct[i] = 1 - pnorm(0, 4.4 + 1.04 * x$pd_diff[i], 12.68)
    print(i)
  }
}

# Plotting probabilities:
plot(x$pd_diff[x$gms == 3], x$pct[x$gms == 3],
     ylim = c(0,1),
     type = 'b',
     pch = 19,
     col = 'blue',
     xlab = 'Home Team Point Differential - Away Team Point Differential',
     ylab = '% Chance of Winning',
     main = 'Probability of Winning a Playoff Series by Number of Games, \nSeries Played in Home Stadiums',
     yaxt = 'n')
lines(x$pd_diff[x$gms == 7], x$pct[x$gms == 7],
      ylim = c(0,1),
      type = 'b',
      pch = 19,
      col = 'green')
lines(x$pd_diff[x$gms == 1], x$pct[x$gms == 1],
      ylim = c(0,1),
      type = 'b',
      pch = 19,
      col = 'red')
axis(2, seq(0,1,.2), seq(0,100,20), las = 2)
for(j in seq(0, 1, .1)){
  abline(h = j, lty = 2)
}
legend('topleft', c('Best of 1 Game', 'Best of 3 Games', 'Best of 7 Games'),
       col = c('red','blue','green'),
       pch = 19)


##########################################################################
# Scenario where everything is on a neutral site:
prob_vic_no_home <- function(gms, pd_diff){
  out <- rep(0, 100000)
  for(i in 1:length(out)){
    rand <- rnorm(gms, mean = 0, sd = 12.68)
    hm_thresh <- 1.04 * pd_diff
    div <- (gms + 1) / 2
    hm_wins <- sum(rand[1:gms] <= hm_thresh) 
    if(hm_wins >= div){
      out[i] = 1
    }
  }
  return(pct = mean(out))
}

# What would happen if there were no home teams (just neutral site games)?
x$pct_no_home <- 0

for(i in 1:nrow(x)){
    x$pct_no_home[i] = prob_vic_no_home(x$gms[i], x$pd_diff[i])
    print(i)
}


plot(x$pd_diff[x$gms == 1], x$pct_no_home[x$gms == 1],
     ylim = c(0,1),
     type = 'b',
     pch = 19,
     col = 'black',
     xlab = 'Home Minus Away Regular Season Point Differential',
     ylab = '% Chance Home Court Team Wins Series',
     main = 'How wild would a single elimination tournament be?',
     yaxt = 'n')
lines(x$pd_diff[x$gms == 7], x$pct[x$gms == 7],
      ylim = c(0,1),
      type = 'b',
      pch = 19,
      col = 'red')
axis(2, seq(0,1,.2), seq(0,100,20), las = 2)
for(j in seq(0, 1, .1)){
  abline(h = j, lty = 2)
}
legend('topleft', c('Regular Best of 7 Games', '1 Game, Neutral Site'),
       col = c('red','black'),
       pch = 19)
text(8, .15, 'Data: basketball-reference')
text(8, .05, '@tvbassine')

par(mar=c(5.1, 4.1, 4.1, 2.1))

plot(x$pd_diff[x$gms == 3], x$pct_no_home[x$gms == 3],
     ylim = c(0,1),
     type = 'b',
     pch = 19,
     col = 'blue',
     xlab = 'Home Court Team Minus Away Regular Season Point Differential',
     ylab = '% Chance Home Court Team Wins Series',
     main = 'How would a neutral site and a shorter length \naffect an NBA playoff series?',
     yaxt = 'n')
lines(x$pd_diff[x$gms == 7], x$pct[x$gms == 7],
      ylim = c(0,1),
      type = 'b',
      pch = 19,
      col = 'red')
lines(x$pd_diff[x$gms == 5], x$pct_no_home[x$gms == 5],
     ylim = c(0,1),
     type = 'b',
     pch = 19,
     col = 'green')
axis(2, seq(0,1,.2), seq(0,100,20), las = 2)
for(j in seq(0, 1, .1)){
  abline(h = j, lty = 2)
}
legend('topleft', c('Regular Best of 7 Games', 'Best of 5 Games, Neutral Site', 'Best of 3 Games, Neutral Site'),
       col = c('red','green','blue'),
       pch = 19)
text(8, .15, 'Data: basketball-reference')
text(8, .05, '@tvbassine')
# Home team down 2 to 1:

# pd_diff <- 0
# out <- rep(0, 100000)
# for(i in 1:length(out)){
#   rand <- rnorm(4, mean = 0, sd = 12.68)
#   hm_thresh <- 4.4 + 1.04 * pd_diff
#   aw_thresh <- 4.4 - 1.04 * pd_diff
#   
#   hm_wins <- sum(rand[1:2] <= hm_thresh) + sum(rand[3:4] >= aw_thresh)
#   if(hm_wins >= 3){
#     out[i] = 1
#   }
# }
# mean(out)
