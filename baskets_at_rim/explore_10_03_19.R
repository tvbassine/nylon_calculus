###########################################################
# I'm curious about the relationship between baskets
# scored/allowed at the rim and team offense/defense.
###########################################################

setwd("~/Desktop/nylon_calculus/baskets_at_rim/data")
x <- read.csv('tm.data.05.19.csv', stringsAsFactors = F)

# Compute opponent baskets at rim
x$opp_rim_buckets <- x$opp_fga * x$opp_pct_fga_00_03 * x$opp_fg_pct_00_03
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




