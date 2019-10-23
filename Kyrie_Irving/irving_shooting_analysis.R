############################################
# The Nets Season is Starting.
# Let's check in on Kyrie Irving.
############################################

# Read in data:
setwd("~/Desktop/nylon_calculus/Kyrie_Irving")
x <- read.csv("irving_shooting_stats.csv", stringsAsFactors = F)

# Get total field goal attempts:
x$fga <- c(747, 1070, 1237, 1235, 879, 1420, 1087, 1241)

x$rim_fgpct <- x$X0.3.1
x$rim_fga <- x$fga * x$X0.3
x$nonrim2_fga <- x$fga * x$X3.10 + x$fga * x$X10.16 + x$fga * x$X16.3pt
x$nonrim2_fgpct <- (x$fga * x$X3.10 * x$X3.10.1 + x$fga * x$X10.16 * x$X10.16.1 + x$fga * x$X16.3pt * x$X16.3pt.1) / x$nonrim2_fga
x$three_fgpct <- x$X3P.1

par(mfrow = c(1,3))
plot(x$Age, x$rim_fgpct, 
     main = 'Kyrie!!',
     type = 'b',
     pch = 19, col = 'red')
plot(x$Age, x$nonrim2_fgpct, 
      type = 'b', col = 'blue', pch = 19)
plot(x$Age, x$three_fgpct, 
      type = 'b', col = 'black', pch = 19)

# I'm just going to plot Kyrie's 2-point stats!
x$two_fgpct <- c(.491, .474, .458, .491,
                 .502, 0.505, .541, .533)
x$year <- 2012:2019
par(mfrow = c(1,1))
plot(x$year, x$two_fgpct, type = 'b', pch = 19, col = 'red',
     ylim = c(0.45, .55),
     xlab = 'Season',
     ylab = '2-PT FG%',
     main = 'Kyrie Got Better At Shooting 2-Pointers!',
     yaxt= 'n')
axis(2, seq(.45, .55, .02))

# Kemba: 0.494
# Lillard: 0.499
# Steph: 0.525

points(2019, 0.494, pch = 19)
points(2019, 0.499, pch = 19)
points(2019, 0.525, pch = 19)
text(2018.5, 0.494, 'Kemba')
text(2018.5, 0.499, 'Dame')
text(2018.5, 0.525, 'Steph')

# Kyrie at the rim:
plot(x$year, x$nonrim2_fgpct)

# Percentage of all 2-point attempts at the rim:
x$pct_fga2_rim <- x$X0.3.1 / (x$X0.3.1+ x$X3.10 + x$X10.16 + x$X16.3pt)
