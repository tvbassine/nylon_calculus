################################################
# Playoff Analysis (Regular season + preseason)
# 2/20/20
# Purpose: Do preseason title odds add meaningful 
# value to playoff predictions?
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

y$away_join <- paste(y$away, '_', y$year, sep = '')
y$home_join <- paste(y$home, '_', y$year, sep = '')

# Read and concatenate title odds data:
z <- read.csv("Preseason NBA Title Odds - 2017-2018.csv", stringsAsFactors = F)
temp <- read.csv("Preseason NBA Title Odds - 2016-2017.csv", stringsAsFactors = F)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2015-2016.csv", stringsAsFactors = F)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2014-2015.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2013-2014.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2012-2013.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2011-2012.csv", stringsAsFactors = F)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2010-2011.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2009-2010.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2008-2009.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2007-2008.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2006-2007.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)
temp <- read.csv("Preseason NBA Title Odds - 2005-2006.csv", stringsAsFactors = F,
                 header = F)
temp <- temp[1:30,]
colnames(temp) <- colnames(z)
z <- rbind(z, temp)

z$Title.Odds <- as.numeric(z$Title.Odds)
z$Year <- as.numeric(z$Year)

get_prob <- function(j){
  if(j > 0){out <- 100 / (j+100)}
  else{out <- -1*j / (-1*j + 100)}
  return(out)
}

z$title_prob <- 0
for(i in 1:nrow(z)){
  z$title_prob[i] <- get_prob(z$Title.Odds[i])
}

z$away_join <- paste(z$Team, '_', z$Year, sep = '')
z$home_join <- z$away_join

#Join title odds onto playoff results:
y <- merge(y, z[,c('away_join', 'title_prob')],
           by = 'away_join', 
           all.x = T)
y <- merge(y, z[,c('home_join', 'title_prob')],
           by = 'home_join', 
           all.x = T)
colnames(y)[16] <- 'title_prob_away'
colnames(y)[17] <- 'title_prob_home'
y$title_prob_diff <- y$title_prob_home - y$title_prob_away
y$title_prob_diff2 <- sign(y$title_prob_diff) * y$title_prob_diff^2

y <- y[y$year %in% 2006:2018,]
y$log_title_prob_home <- log(100*y$title_prob_home + 1)
y$log_title_prob_away <- log(100*y$title_prob_away + 1)
y$log_title_prob_diff <- y$log_title_prob_home - y$log_title_prob_away
head(y)

summary(fit <- lm(avg_margin ~ net + title_prob_diff, data = y)) #0.21 R squared
cor(y$title_prob_diff, y$net)

summary(fit <- lm(avg_margin ~ net, data = y)) #0.16 R squared

plot(fit)
plot(y$title_prob_diff, y$net)

# Use log title probability:
summary(fit <- lm(avg_margin ~ net + log_title_prob_diff, data = y))


# how about no Cavs or Warriors?
summary(fit <- lm(avg_margin ~ net + log_title_prob_diff, data = y[y$year %in% 2006:2014,]))
# significance of title odds goes away

# How about probability of home victory in the series?
model <- glm(home_win ~ net + log_title_prob_diff,
             family=binomial(link='logit'), data=y)
summary(model)

# Some test examples:
predict(model, newdata = data.frame(net = 0.2, 
                                    log_title_prob_diff = log(100 * .25 + 1) - log(100 * .012 + 1)),
        type = 'response')

predict(model, newdata = data.frame(net = 0, 
                                    log_title_prob_diff = log(100 * .50 + 1) - log(100 * .10 + 1)),
        type = 'response')

# How would this simple model predict a playoff series based on participants:
z <- data.frame(net = rep(0,51),
           a = seq(.01,.51, .01),
           b = rep(.01,51))
z$title_prob_diff <- 100* (z$a - z$b)
z$log_title_prob_diff <- log(100 * z$a + 1) - log(100 * z$b + 1) 
z$predicted_home_prob <- predict(model, newdata = z, type = 'response')
View(z)

plot(z$title_prob_diff, 100* z$predicted_home_prob, ylim = c(0,100),
     xlab = 'Difference in Preseason Title %',
     ylab = '% Chance of Winning Series')
