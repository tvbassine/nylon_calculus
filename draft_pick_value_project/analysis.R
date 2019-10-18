################################################################
# Analyze Final Record Based on Record in the Past.
# Date: 9/30/19
# Thomas Bassine
################################################################

# Read in the data:
setwd("~/Desktop/nylon_calculus/draft_pick_value_project/data")
x <- read.csv('pre_processed_data.csv', stringsAsFactors = F)

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

################################################################
#### Beehive plot:

# I want to bin teams into the top 5 records, next 5, etc...,
# and then plot where these teams ended up a few years later.

library(beeswarm) #You will need to install this package.

# Bin current season rank into groups of 5
x$rank_binned <- NA
x$rank_binned[x$rank <= 5] <- '1-5' #Worst records this year
x$rank_binned[x$rank >= 6 & x$rank <= 10] <- '6-10'
x$rank_binned[x$rank >= 11 & x$rank <= 15] <- '11-15'
x$rank_binned[x$rank >= 16 & x$rank <= 20] <- '16-20'
x$rank_binned[x$rank >= 21 & x$rank <= 25] <- '21-25'
x$rank_binned[x$rank >= 26] <- '26-30' #Best records this year
x$rank_binned <- factor(x$rank_binned, 
                           levels = c('1-5','6-10','11-15',
                                      '16-20', '21-25', '26-30'))

# Use 'beeswarm' to visualize team rankings out in the future:
beeswarm(rank_1 ~ rank_binned, data = x, 
         log = F, pch = 16, col = rainbow(8),
         ylim = c(1,30),
         xlab = 'Draft Position This Season',
         ylab = 'Draft Position Next Season',
         main = 'How Does A Team\'s Draft Position Relate to Its Draft Position Next Year?')

# 3 Year Plot- I kind of like this one but it might be a bit too cluttered.
beeswarm(rank_3 ~ rank_binned, data = x, 
         log = F, pch = 16, col = rainbow(8),
         ylim = c(-2,30),
         yaxt = 'n',
         xlab = 'Draft Position This Season (Pre-Lottery)',
         ylab = 'Draft Position In 3 Seasons (Pre-Lottery)',
         main = 'How Does A Team\'s Draft Position Now Relate to \nIts Draft Position 3 Years From Now?')
axis(2, c(1, seq(5,30,5)))
text(2, -1, 'Worse Records')
text(5, -1, 'Better Records')
arrows(x0 = 1.2,
       y0 = -1,
       x1 = 0.5,
       y1 = -1,
       length = .15)
arrows(x0 = 5.8,
       y0 = -1,
       x1 = 6.5,
       y1 = -1,
       length = .15)

# 5 Year Plot (may be a little too cluttered)
beeswarm(rank_5 ~ rank_binned, data = x, 
         log = F, pch = 16, col = rainbow(8),
         ylim = c(-2,30),
         xlab = 'Draft Position This Season (Pre-Lottery)',
         ylab = 'Draft Position In 5 Seasons (Pre-Lottery)',
         main = 'We Have No Idea Where Anyone Will Draft In 5 Years!',
         yaxt = 'n')
axis(2, c(1, seq(5,30,5)))
text(2, -1, 'Worse Records')
text(5, -1, 'Better Records')
arrows(x0 = 1.2,
       y0 = -1,
       x1 = 0.5,
       y1 = -1,
       length = .15)
arrows(x0 = 5.8,
       y0 = -1,
       x1 = 6.5,
       y1 = -1,
       length = .15)

# Simple Plot of Correlation of this year draft position with future years:
cors <- rep(0,6)
cors[1] = cor(x$rank, x$rank_1, use = 'complete.obs')
cors[2] = cor(x$rank, x$rank_2, use = 'complete.obs')
cors[3] = cor(x$rank, x$rank_3, use = 'complete.obs')
cors[4] = cor(x$rank, x$rank_4, use = 'complete.obs')
cors[5] = cor(x$rank, x$rank_5, use = 'complete.obs')
cors[6] = cor(x$rank, x$rank_6, use = 'complete.obs')

par(mar = c(5.1, 5.1, 4.1, 2.1)) # changed left (2nd one) from 4.1 to 5.1

#Pretty Steep decline from 1 year to 3 years. 
#5 years out has little correlation with the current season.
# This is OK... might want to indicate 2000-2019 seasons and pre-lottery position.
plot(1:6, cors,
     type = 'b',
     pch = 19,
     cex = 1.5,
     ylim = c(0,1),
     xlab = 'Seasons Away From Current Season',
     ylab = 'Correlation of Future Draft \nPosition With Current Draft Position',
     main = 'How Does A Team\'s Draft Position In Future Years \nCorrelate With Their Current Season Draft Position?')

which(x$rank_5 == 1)


###############################################################
# Beta regression of future lottery position based on 
# x years ago.

library(betareg)

# Make draft position a percentile:
x$rank_percentile <- x$rank / 30.1

x$rank1_percentile <- x$rank_1 / 30.1

summary(fit <- betareg(rank1_percentile ~ rank_percentile,
                       data = x))

predict(fit, newdata = data.frame(rank_percentile = seq(1,30) / 30.5),
        type = 'response',) * 30.1
