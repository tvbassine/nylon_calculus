###########################################################
# I'm curious about the relationship between baskets
# scored/allowed at the rim and team offense/defense.
###########################################################

setwd("~/Desktop/nylon_calculus/baskets_at_rim/data")
x <- read.csv('tm.data.05.19.csv', stringsAsFactors = F)

# Compute opponent baskets at rim
x$opp_rim_buckets <- x$opp_fga * x$opp_pct_fga_00_03 * x$opp_fg_pct_00_03
# Probably want to pace adjust this.

# Looks like a strong relationship for 2019.
# Cor = 0.77
plot(x$opp_rim_buckets[x$yr == 2019], x$def_rtg[x$yr == 2019],
     cex = 0,
     ylim = c(105, 119),
     xlim = c(13,20))
text(x$opp_rim_buckets[x$yr == 2019], x$def_rtg[x$yr == 2019],
     x$tm[x$yr == 2019], cex = .5)
cor(x$opp_rim_buckets[x$yr == 2019], x$def_rtg[x$yr == 2019])

# What is relationship between 3's and defensive rating?
plot(x$opp_fg3[x$yr == 2019], x$def_rtg[x$yr == 2019])
cor(x$opp_fg3[x$yr == 2019], x$def_rtg[x$yr == 2019]) #0.34
