library('dplyr')
library('corrplot')
library('ggplot2')
library('stats')
library('rpart')
library('rattle')
library('rpart.plot')
library('RColorBrewer')
library('randomForest')

bike_data
head(bike_data)
dim(bike_data)
names(bike_data)
is.integer(bike_data)
bike_data <- data.frame(bike_data)
str(bike_data)
bike_data$temp <- (bike_data$temp)*41
bike_data$atemp <- (bike_data$atemp)*50
bike_data$windspeed <- (bike_data$windspeed)*67
bike_data$hum <- (bike_data$hum)* 100
head(bike_data)
h_graph <- hist(bike_data$cnt , breaks=25 , ylab="Frequency of rental" , xlab="Total bike rental count" , 
                main="Distribution of total bike rental count" , col="blue")
xfit <- seq(min(bike_data$cnt) , max(bike_data$cnt) , length=50)
yfit <- dnorm(xfit , mean=mean(bike_data$cnt) , sd=sd(bike_data$cnt))
yfit <- yfit$diff(h_graph$mids[1:2])*length(bike_data$cnt)
lines(xfit , yfit , col="red" , lwd=3)
bike_data$cnt
mean(bike_data$cnt)
var(bike_data$cnt)
res <- c(mean(bike_data$cnt) , var(bike_data$cnt))
c(mean=r[1] , var=r[2] , ratio= r[2]/r[1])
boxplot(bike_data$cnt ~ bike_data$dteday , xlab="Dates" , ylab="Count of users")
boxplot(bike_data$cnt ~ bike_data$workingday , xlab="Working day" , ylab="Count of users")
boxplot(bike_data$cnt ~ bike_data$holiday , xlab="Holiday" , ylab="Count of users")
boxplot(bike_data$cnt ~ bike_data$weathersit , xlab="Weather" , ylab="Count of users")
boxplot(bike_data$cnt ~ bike_data$yr , xlab="Year" , ylab="Count of users")

cor.temp <- cor.test(x=bike_data$temp , y=bike_data$cnt)
cor.temp

cor.atemp <- cor.test(x=bike_data$atemp , y=bike_data$cnt)
cor.atemp

substr <- data.frame(bike_data$cnt , bike_data$temp , bike_data$hum , bike_data$atemp , 
                     bike_data$windspeed)
cor(substr)

plot(x= bike_data$temp , y=bike_data$cnt , main="Correlation" , col="red")
abline(lm(bike_data$cnt ~ bike_data$temp) , col="blue")
legend("Topleft" , legend= paste("cor=" , round(cor(bike_data$temp , bike_data$cnt),2) , sep="") , 
       lty=1 , col="blue")
plot(x=bike_data$atemp , y=bike_data$cnt , main="Correlation" , col="red")
abline(lm(bike_data$cnt ~ bike_data$atemp) , col= "blue")
legend("Topleft" , legend=paste("cor=" , round(cor(bike_data$atemp , bike_data$cnt) , 2) , 
                                sep="") , lty=1 , col="blue")

bike_data$season= as.factor(bike_data$season)
bike_data$weathersit = as.factor(bike_data$weathersit)
bike_data$holiday = as.factor(bike_data$holiday)
bike_data$workingday = as.factor(bike_data$workingday)
bike_data$yr  = as.factor(bike_data$yr)

poi_model <- glm(cnt~ yr+ season + holiday + workingday + weathersit + temp +atemp + hum + 
                     windspeed , family=poisson , bike_data=bike_data)

summary(poi_model)
exp(poi_model$coef)
warpbreaks
breaks_model <- glm(breaks~ wool*tension , warpbreaks , family=poisson)
summary(breaks_model)
summary(poi_model)
deviance(poi_model)
df.residual(poi_model)
tf <- log(bike_data$cnt)
lin_model <- lm(tf ~ yr + season+ holiday + workingday +weathersit + temp +
                    atemp + hum + windspeed , bike_data=bike_data)
adv_lin_model <- lm(tf ~ yr + season + holiday + workingday + weathersit + temp +
                        hum + windspeed  , bike_data=bike_data)
c(adjusted.R_squared = summary(adv_lin_model)$adj.r.squared)
tm <- log(bike_data$temp)
hm <- bike_data$hum^10
ws <- bike_data$windspeed^2
lin_model_trans <- lm(tf ~ yr + season + holiday + workingday + weathersit + temp +
                          hm + windspeed  , bike_data=bike_data)
lin_model_trans2 <- lm(tf , yr+season + holiday + workingday + weathersit + temp + 
                           hum + ws , bike_data=bike_data)
lin_model_trans3 <- lm(tf , yr+season + holiday + workingday + weathersit + tm + 
                           hum + windspeed , bike_data=bike_data)
lin_model_trans4 <- lm(tf , yr+season + holiday + workingday + weathersit + tm + 
                           hum + ws , bike_data=bike_data)
c(adjusted.R_squared = summary(lin_model_trans4)$adj.r.squared)

rent <- bike_data$cnt
head(rent)
hist(rent , xlab="Total rental " , ylab="Frequency" , main="Bike Rental" , 
     breaks=20 , col="green")
abline(v=mean(rent) , col="blue" , lty=1 , lwd=3)
text(x=4000 , y=85 , labels= paste("Mean=" , round(mean(winter) , 2) , 
                                   sep="") , col="red")
ggplot(bike_data , aes(dteday , cnt)) + geom_point()

library('MASS')

neg_bin.model <- glm.nb(cnt ~ yr + season + holiday + workingday + weathersit + temp + 
                            atemp + hum +windspeed , bike_data=bike_data , link=log)
neg_bin.model2 <-glm.nb(cnt ~ yr + season + holiday + workingday + weathersit + atemp +
                            hum + windspeed  , bike_data=bike_data , link=log)
summary(neg_bin.model)
summary(neg_bin.model2)

poi_model2<- glm(cnt ~ dteday + season + holiday + temp + workingday + weathersit + atemp + hum
              + windspeed, family = "poisson", bike_data = bike_data)

pchisq(2 * (logLik(poi_model2) - logLik(neg_bin.model2)), df = 1, lower.tail = FALSE)

(est <- cbind(Estimate = coef(neg_bin.model2), confint(neg_bin.model2)))

exp(est)                       


















