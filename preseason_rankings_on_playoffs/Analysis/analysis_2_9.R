################################################
# Playoff Series Analysis
# 2/9/18
# Purpose: Construct a simple model for playoff
# results based on the net ratings of the two 
# teams.
# I'm also curious how much the Warriors have 
# improved in the playoffs on their regular 
# season performance.
################################################

# Section 1:
# Read data and analysis.

# Read data:
setwd("/Users/thomasbassine/Desktop/Threes and Layups Articles/Playoff Probabilities/Data")
y <- read.csv('playoffs_2003_2018.csv',
              stringsAsFactors = F)
y$year <- 0
for(i in 1:nrow(y)){
  y$year[i] <- as.numeric(unlist(strsplit(y$series_code[i], split = '_'))[3])
}

# Logistic Regression on difference in net rating.
model <- glm(home_win ~ net,
             family=binomial(link='logit'),
             data=y)

summary(model)

pred <- predict(model, 
                newdata = data.frame(net = seq(-5,10,1)),
                type = 'response')

cbind( seq(-5,10,1), pred)
# 0 net rating diff = 57%
# 3 pts of diff = 76%

plot(seq(-5,10,1), pred, 'b', pch = 19,
     ylab = 'Probability Home Team Wins Series',
     xlab = 'Home Team Net Rating - Away Team Net Rating',
     ylim = c(0,1),
     main = 'Simple Model of an NBA Playoff Series',
     xaxt = 'n')
axis(1, seq(-4,10,2))
text(seq(-4,10,2), pred[seq(2,16,2)],
     labels = round(pred[seq(2,16,2)], 2), pos = 3)

#largest upsets:
z <- y
z$net[z$home_win == 1] <- -1 * z$net[z$home_win == 1]
head(z[order(z$net, decreasing = T),])

q <- z[order(z$net, decreasing = T), c('year',
                                  'away',
                                  'home',
                                  'winner',
                                  'net')]
colnames(q)[5] <- 'Reg_Season_MOV_Diff'
head(q, 15)

# Model of average home margin of victory based on 
# difference in net ratings

fit <- lm(avg_margin ~ net, data = y)
summary(fit) 
# Every point of net rating difference is worth 
# roughly 1 point per game. 

y$pred_margin <- predict(fit)
y$dif_margin <- y$avg_margin - y$pred_margin

plot(y$pred_margin, y$avg_margin, pch=19)
abline(a = .8, b = 1.07)

# The series which deviated most from my predictions!
head(y[order(abs(y$dif_margin), decreasing = T),
       c('year', 'away', 'home', 'avg_margin', 'dif_margin')], 20)

median(abs(y$avg_margin))
summary(abs(y$avg_margin))
########################################################

# Get most dominant playoff teams and teams which overperformed
# and underperformed expectation the most:

tms <- paste(y$away, y$year, sep = '_')
tms <- c(tms, paste(y$home, y$year, sep = '_'))
tms <- unique(tms)  

w <- data.frame(tms = tms, 
                avg_margin = 0,
                pred_margin = 0,
                stringsAsFactors = F)

w$year <- 0
w$tm <- 0
w$champ <- 0
w$reg_pd <- 0

for(i in 1:nrow(w)){
  w$year[i] = unlist(strsplit(w$tms[i], split = '_'))[2]
  w$tm[i] =  unlist(strsplit(w$tms[i], split = '_'))[1]
}

for(i in 1:nrow(w)){
  ind <- which(y$year == w$year[i] & 
               (y$away == w$tm[i] | y$home == w$tm[i])
              )
  temp <- y[ind,]
  ind1 <- which(temp$away == w$tm[i])
  temp$pred_margin[ind1] <- -1 * temp$pred_margin[ind1]
  temp$avg_margin[ind1] <- -1 * temp$avg_margin[ind1]
  
  print(i)
  
  w$avg_margin[i] <- mean(temp$avg_margin)
  w$pred_margin[i] <- mean(temp$pred_margin)
  
  wins <- sum(temp$winner == w$tm[i])
  if(wins == 4){
    w$champ[i] <- 1
  }
  
  w$reg_pd[i] = temp$home_net[1]
  if(temp$away[1] == w$tm[i]){
    w$reg_pd[i] = temp$away_net[1]
  }
}

w$dif <- w$avg_margin - w$pred_margin

w <- w[order(w$avg_margin, decreasing = T),]
head(w)

head(w[order(w$dif, decreasing = T),], 40)
head(w[order(w$avg_margin, decreasing = T),], 20)

ch <- w[w$champ == 1,]
ch[order(ch$avg_margin, decreasing = T),]
ch[order(ch$dif, decreasing = T),]



########################################################
# Year over year graph:

w$pch <- 1
w$pch[w$champ == 1] <- 15

w$col = 'grey'
w$col[w$champ == 1] <- 'red'

plot(w$year, w$reg_pd, pch = w$pch,
     col = w$col,
     xlim = c(2003, 2018))

# Logistic Regression of championship by point differential:
model <- glm(champ ~ reg_pd,
             family=binomial(link='logit'),
             data=w)

summary(model)

pred <- predict(model, 
                newdata = data.frame(reg_pd = seq(0,13,1)),
                type = 'response')

# Bucks estimated championship odds
predict(model, 
        newdata = data.frame(reg_pd = 12),
        type = 'response')

plot( seq(0,10,1), pred[1:11],
      xlab = 'Regular Season MOV',
      ylab = 'NBA Championship Probability',
      main = 'Probability of Winning NBA Championship, \nBased on Regular Season MOV',
      col = 'red',
      pch = 19,
      type = 'b',
      ylim = c(0,1))

abline(h = 0.2, lty = 2)
abline(h = 0.4, lty = 2)
abline(h = 0.6, lty = 2)
abline(h = 0.8, lty = 2)

########################################################
# Golden State playoff series:
gs <- y[y$year >= 2015 & (y$home == 'Golden State Warriors' | y$away == 'Golden State Warriors'),]
head(gs)
# Reverse the margins for the 2018 Rockets series because Golden State was the away team:
gs$avg_margin[3] <- 9
gs$pred_margin[3] <- -1 * gs$pred_margin[3]
gs$dif_margin[3] <- -1 * gs$dif_margin[3]

hist(gs$avg_margin)
plot(gs$year, gs$avg_margin)

mean(gs$dif_margin)
aggregate(dif_margin ~ year, data = gs, mean)
aggregate(avg_margin ~ year, data = gs, mean)

q <- gs[order(gs$avg_margin, decreasing = F),
   c('away', 'home', 'year', 'games', 'avg_margin')]
q

#####################################################
# What is the distribution of margin of victory in a 
# playoff series

summary(abs(y$avg_margin))
hist(abs(y$avg_margin))
