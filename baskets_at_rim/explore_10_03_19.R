###########################################################
# I'm curious about the relationship between baskets
# scored/allowed at the rim and team offense/defense.
###########################################################

setwd("~/Desktop/nylon_calculus/baskets_at_rim/data")
x <- read.csv('tm.data.05.19.csv', stringsAsFactors = F)

# Compute opponent baskets at rim
x$opp_rim_buckets <- x$opp_fga * x$opp_pct_fga_00_03 * x$opp_fg_pct_00_03
x$rim_buckets <- x$fga * x$pct_fga_00_03 * x$fg_pct_00_03

# Plot team rim buckets
View(cbind(x$tm[x$yr == 2019], x$rim_buckets[x$yr == 2019]))
y <- x[x$yr == 2019, c('tm','rim_buckets')]
y <- y[order(y$rim_buckets),]
y$name <- 0
for(i in 1:30){
  temp <- unlist(strsplit(y$tm[i], ' '))
  y$name[i] <- temp[length(temp)]
}
plot(1:30, 
     2 * y$rim_buckets,
     cex = 0,
     main = 'Points Per Game on Baskets Within 3 Feet of Rim, \n2018-2019',
     ylab = 'Points',
     yaxt = 'n', 
     xaxt = 'n',
     ylim = c(25,42),
     xlab = '')
text(1:30, 2 * y$rim_buckets, y$name, cex = .5)
axis(2, seq(24,42,2))

# Probably want to pace adjust this.

# Looks like a strong relationship for 2019. That is, less baskets at the rim 
# correspond 
# Cor = 0.77
yr <- 2018
plot(x$opp_rim_buckets[x$yr == yr], x$def_rtg[x$yr == yr],
     cex = 0,
     ylim = range(x$def_rtg[x$yr == yr]) + c(-1,1),
     xlim = range(x$opp_rim_buckets[x$yr == yr]) + c(-1,1))
text(x$opp_rim_buckets[x$yr == yr], x$def_rtg[x$yr == yr],
     x$tm[x$yr == 2019], cex = .5)
cor(x$opp_rim_buckets[x$yr == yr], x$def_rtg[x$yr == yr])

# What is relationship between 3's and defensive rating?
plot(x$opp_fg3[x$yr == 2019], x$def_rtg[x$yr == 2019])
cor(x$opp_fg3[x$yr == 2019], x$def_rtg[x$yr == 2019]) #0.34

# Get rim buckets allowed per 48 min and normalize by pace to 
# get rim buckets allowed per 100 possessions.
# compare with relative def. rating.


#################################################################
# Shot location and efficiency:

z <- x %>%
     group_by(yr) %>%
     mutate(avg_pct_fga_00_03 = mean(pct_fga_00_03),
            avg_pct_fga_03_10 = mean(pct_fga_03_10),
            avg_pct_fga_10_16 = mean(pct_fga_10_16),
            avg_pct_fga_16_xx = mean(pct_fga_16_xx),
            avg_fg_pct_00_03 = mean(fg_pct_00_03),
            avg_fg_pct_03_10 = mean(fg_pct_03_10),
            avg_fg_pct_10_16 = mean(fg_pct_10_16),
            avg_fg_pct_16_xx = mean(fg_pct_16_xx)) %>%
     select(yr, avg_pct_fga_00_03, avg_pct_fga_03_10, avg_pct_fga_10_16, avg_pct_fga_16_xx,
            avg_fg_pct_00_03, avg_fg_pct_03_10, avg_fg_pct_10_16, avg_fg_pct_16_xx) %>%
     unique()

par(mfrow = c(1,2))
plot(z$yr, z$avg_fg_pct_00_03,
     type = 'b', col = 'red',
     ylab = 'Field Goal % on Shots Within 3 Ft. of Rim',
     xlab = 'Year',
     pch = 19,
     main = 'Field Goal % on Shots Within 3 Feet., Since 2005')
text(c(2009), c(0.655), 'What happened in 2011??')
plot(z$yr, z$avg_pct_fga_00_03,
     type = 'b', col = 'blue',
     ylab = 'Proportion of FGA Within 3 Ft. of Rim',
     xlab = 'Year',
     main = 'Proportion of Attempts Within 3 Feet, Since 2005',
     pch = 19)
text(c(2010), c(0.28), '2011??')


###############################################################
# Turnovers per 100 possessions

x$tov_adv <- x$opp_tov - x$tov

View(x[x$yr == 2019, c('tm', 'opp_tov', 'tov', 'tov_adv')])
