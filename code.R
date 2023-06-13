#######################################################################
###   Advanced Econometrics - Airline Customer satisfaction
###
###       Aleksander Wieliński  -     420 272
###       Jakub Gazda           -     419 272
###
#######################################################################

### LIBRARIES
#######################################################################

#install.packages("dplyr")
library("dplyr")
#install.packages("corrplot")
library('corrplot')
#install.packages("margins")
library("margins")
#install.packages("plm")
library("plm")
#install.packages("ResourceSelection")
library("ResourceSelection")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("lattice")
library("lattice")
#install.packages("caret")
library("caret")
#install.packages("lmtest")
library("lmtest")
#install.packages("DescTools")
#install.packages("RDCOMClient", repos="http://www.omegahat.net/R")
library("DescTools")
#install.packages('aods3')
library("aods3")

#######################################################################


### DATA LOADING AND PREPARATION

setwd(getwd())
data <- as.data.frame(read.csv("Invistico_Airline.csv"))

# Random seed - this ensures consistent data across devices
set.seed(123)

# Splitting the dataset while maintaining the target variable distribution.
# This is done to save memory and computational resources and allow for a fast analysis.
trainIndex <- createDataPartition(data$satisfaction, p = 0.005, list = FALSE, times = 1)

# Create the smaller subset by selecting the instances based on the indices
data <- data[trainIndex, ]

# Missing values removal
na_count <- sum(is.na(data))
print(na_count)
data<-na.omit(data)
#View(data)

# Binary encoding 
data$satisfaction<-ifelse(data$satisfaction == "satisfied", 1,0)
data$Gender<-ifelse(data$Gender == "Male", 1,0)
data$Customer.Type<-ifelse(data$Customer.Type == "Loyal Customer", 1,0)
data$Type.of.Travel<-ifelse(data$Type.of.Travel == "Personal Travel", 1,0)
data$Class <- as.integer(factor(data$Class, levels = c("Eco", "Eco Plus", "Business"), labels = c(1, 2, 3)))

variables <- c("Seat.comfort", "Departure.Arrival.time.convenient", "Food.and.drink", 
               "Gate.location", "Inflight.wifi.service", "Inflight.entertainment", 
               "Online.support", "Ease.of.Online.booking","On.board.service", "Leg.room.service", 
               "Baggage.handling", "Checkin.service", "Cleanliness", "Online.boarding")

str(data)
summary(data)

x <- data[(data$Age==0), ]
x

### Exploratory Data Analysis

hist(data$Departure.Delay.in.Minutes)

hist(log1p(data$Departure.Delay.in.Minutes))

hist(BoxCox(data$Departure.Delay.in.Minutes, 0))


hist(data$Arrival.Delay.in.Minutes)

hist(log1p(data$Arrival.Delay.in.Minutes))

hist(BoxCox(data$Arrival.Delay.in.Minutes, 0))

# Heatmap - correlations between variables
par(mfrow = c(1,1))
cor_matrix <- cor(data)
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 0)

#print_correlation_table <- function(data, variables) {
#  cor_matrix <- cor(data[, variables])
#  print(cor_matrix)
#}

par(mar = c(2, 2, 2, 2))
columns1 <- c("Age", "Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
par(mfrow=c(2,2))

# Histograms for the columns in columns1
for (col in columns1) {
  hist(data[[col]], main = col, xlab = "", col = "lightblue")
}
# Boxplots for the columns in columns1
for (col in columns1) {
  boxplot(data[[col]], main = col, col = "lightblue")
}

columns2 <- c("satisfaction", "Gender", "Customer.Type","Type.of.Travel", "Class", 
              "Seat.comfort", "Departure.Arrival.time.convenient","Food.and.drink", 
              "Gate.location","Inflight.wifi.service", "Inflight.entertainment", "Online.support", 
              "Ease.of.Online.booking","On.board.service", "Leg.room.service", "Baggage.handling", 
              "Checkin.service", "Cleanliness","Online.boarding")

par(mfrow = c(4, 5))

# Bar plots for the columns in columns2
for (col in columns2) {
  barplot(table(data[[col]]), main = col, xlab = "", col = "lightblue")
}

for (col in c("Flight.Distance")) { #, "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes"
  threshold <- quantile(data[[col]], 0.95)
  data <- data[data[[col]] <= threshold, ]
}

columns3 <- c("Age", "Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
par(mfrow=c(2,2))
# Histograms for the columns in columns1
for (col in columns3) {
  hist(data[[col]], main = col, xlab = "", col = "lightblue")
}

par(mfrow = c(1,1))
# A non linear relation is introduced in order to normalize the distribution
# of those 2 variables.

data$Departure.Delay.in.Minutes <- log(data$Departure.Delay.in.Minutes+0.001)
data$Arrival.Delay.in.Minutes <- log(data$Arrival.Delay.in.Minutes+0.001)

### Assesing models

source("linktest.R")

## OLS

lpm <- lm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance
               +Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location
               +Food.and.drink*Gate.location+Inflight.wifi.service+Inflight.entertainment
               +Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service
               +Baggage.handling+Checkin.service+Cleanliness*On.board.service
               +Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes
               +Departure.Delay.in.Minutes, data=data)

summary(lpm)

# -----
# Ad. b

# specification test
resettest(lpm, power=2:3, type="fitted")

# -----
# Ad. c

# heteroscedasticity
lpm.residuals = lpm$residuals
plot(lpm.residuals~Gender, data=data)
plot(lpm.residuals~Customer.Type, data=data)

plot(lpm.residuals~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance
     +Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location
     +Food.and.drink*Gate.location+Inflight.wifi.service+Inflight.entertainment
     +Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service
     +Baggage.handling+Checkin.service+Cleanliness*On.board.service
     +Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes
     +Departure.Delay.in.Minutes, data=data)

bptest(lpm.residuals~Gender, data=data)
bptest(lpm.residuals~Gender+Customer.Type, data=data)

bptest(lpm.residuals~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance
       +Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location
       +Food.and.drink*Gate.location+Inflight.wifi.service+Inflight.entertainment
       +Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service
       +Baggage.handling+Checkin.service+Cleanliness*On.board.service
       +Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes
       +Departure.Delay.in.Minutes, data=data)

#View(lpm$fitted.values)

# -----
# Ad. d

# White's estimator of the variance-covariane matrix
robust_vcov = vcovHC(lpm, data = olympics, type = "HC")
coeftest(lpm, vcov.=robust_vcov)

# to compare the simple lpm and the one with a robust vcov matrix
#install.packages("stargazer")
library("stargazer")
robust.lpm = coeftest(lpm, vcov.=robust_vcov)
stargazer(lpm, robust.lpm, type="text")

## Logit Modelling

mylogit <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance
               +Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location
               +Food.and.drink*Gate.location+Inflight.wifi.service+Inflight.entertainment
               +Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service
               +Baggage.handling+Checkin.service+Cleanliness*On.board.service
               +Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes
               +Departure.Delay.in.Minutes, data=data, 
                family=binomial(link="logit"))

# Model Summary
summary(mylogit)
logit_summary <- summary(mylogit)

# R^2 statistics
r_squared_logit <- logit_summary$deviance/logit_summary$null.deviance
print(r_squared_logit)
PseudoR2(mylogit, "all")

# Linktest - yhat significant | yhat2 insignificant -> cannot reject H0
linktest_result_logit = linktest(mylogit)
summary(linktest_result_logit)

# Stargaze

stargazer(mylogit, type="text")

## Probit Modelling

#myprobit <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance+Seat.comfort+Departure.Arrival.time.convenient+Food.and.drink+Gate.location+Inflight.wifi.service+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Online.boarding+Arrival.Delay.in.Minutes+Departure.Delay.in.Minutes, data=data, 
#                family=binomial(link="probit"))
#no diff when 1,0 are 50-50, information criteria: we should use AIC to be sure, BIC(SBC) - its better, the lower the value the better 
myprobit <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class
                +Flight.Distance+Seat.comfort*Food.and.drink
                +Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location
                +Inflight.wifi.service+Inflight.entertainment+Online.support
                +Ease.of.Online.booking+On.board.service+Leg.room.service
                +Baggage.handling+Checkin.service+Cleanliness*On.board.service
                +Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes
                +Departure.Delay.in.Minutes, data=data, 
               family=binomial(link="probit"))

# Model Summary
summary(myprobit)
probit_summary <- summary(myprobit)

# R^2 statistics
r_squared_probit <- probit_summary$deviance/probit_summary$null.deviance
print(r_squared_probit)
PseudoR2(myprobit, "all")
#can interprate McKelvy.Zavoina if the latent variable is observed the model explainx x% of observations, count is the correct R2 explaining x% of the observation, adj.count is saying % ofcorrectly predicted observations with given variance

# Linktest - yhat significant | yhat2 insignificant -> cannot reject H0
linktest_result_probit = linktest(myprobit)
summary(linktest_result_probit)

stargazer(myprobit, type="text")

### Diagnostics

# can also use Wald test for beta2=beta3=0
#H <- rbind(c(0,1,0,0), c(0,0,1,0))
# h %*% coef(dem.probit)

# DECLARE TERMS
#?wald.test
#wald_results = wald.test(myprobit, )
#wald_results

# Goodness of fit test
gof_results = gof(myprobit)
gof_results


### General to Specific procedure

#for 0,005 of dataset
# general to specific thatway= h0 is beta=0(for additional variables) and with p-value<0.05 we reject h0. Joint insignificance of all variables test against null
null_probit = glm(satisfaction~1, data=data, family=binomial(link="probit"))
lrtest(myprobit, null_probit)
#bez inflight wifi service
myprobit1 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes+Departure.Delay.in.Minutes, data=data, 
                family=binomial(link="probit"))
lrtest(myprobit, myprobit1)
summary(myprobit1)
#bez inflight.entertainment
myprobit2 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Cleanliness*On.board.service+Online.boarding+Arrival.Delay.in.Minutes+Departure.Delay.in.Minutes, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit1, myprobit2)
### the change was too significant, go to the next biggest pvalue

#bez gate location
myprobit3 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes+Departure.Delay.in.Minutes, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit1, myprobit3)
### the change was too significant, go to the next biggest pvalue

#bez class
myprobit4 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment+Arrival.Delay.in.Minutes+Departure.Delay.in.Minutes, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit1, myprobit4)
summary(myprobit4)
# bez arrival delay
myprobit5 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment+Departure.Delay.in.Minutes, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit4, myprobit5)
summary(myprobit5)
# bez baggage handling
myprobit6 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment+Departure.Delay.in.Minutes, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit5, myprobit6)
summary(myprobit6)
# bez departure delay in min
myprobit7 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit6, myprobit7)
summary(myprobit7)
#bez online support
myprobit8 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Ease.of.Online.booking+On.board.service+Leg.room.service+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit7, myprobit8)
summary(myprobit8)
#bez online boarding
myprobit9 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Ease.of.Online.booking+On.board.service+Leg.room.service+Checkin.service+Cleanliness*On.board.service, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit8, myprobit9)
### the change was too significant, go to the next biggest pvalue

#bez age
myprobit10 <- glm(satisfaction~Gender+Customer.Type+Type.of.Travel+Flight.Distance+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Ease.of.Online.booking+On.board.service+Leg.room.service+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment, data=data, 
                 family=binomial(link="probit"))
lrtest(myprobit8, myprobit10)
summary(myprobit10)
#bez flight distance
myprobit11 <- glm(satisfaction~Gender+Customer.Type+Type.of.Travel+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Ease.of.Online.booking+On.board.service+Leg.room.service+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment, data=data, 
                  family=binomial(link="probit"))
lrtest(myprobit10, myprobit11)
summary(myprobit11)
# bez on board service
myprobit12 <- glm(satisfaction~Gender+Customer.Type+Type.of.Travel+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Ease.of.Online.booking+On.board.service+Leg.room.service+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment, data=data, 
                  family=binomial(link="probit"))
lrtest(myprobit11, myprobit12)
### the change was too significant, go to the next biggest pvalue\

#bez leg room service
myprobit13 <- glm(satisfaction~Gender+Customer.Type+Type.of.Travel+Seat.comfort*Food.and.drink+Departure.Arrival.time.convenient*Gate.location+Food.and.drink*Gate.location+Inflight.entertainment+Ease.of.Online.booking+On.board.service+Checkin.service+Cleanliness*On.board.service+Online.boarding*Inflight.entertainment, data=data, 
                  family=binomial(link="probit"))
lrtest(myprobit11, myprobit13)
summary(myprobit13)

linktest_result = linktest(myprobit13)
model_summary13 <- summary(myprobit13)
r_squared13 <- model_summary13$deviance/model_summary13$null.deviance
print(r_squared13)
PseudoR2(myprobit13, "all")
bptest(myprobit13, data=data)

#Hosmer-Lemshow test and Osius-Rojek test? https://statisticalhorizons.com/hosmer-lemeshow/  https://statisticalhorizons.com/alternatives-to-the-hosmer-lemeshow-test/
gof.results = gof(myprobit13)
gof.results
# Get predicted probabilities from the model
predicted_probs <- predict(myprobit13, type = "response")
# Perform the Hosmer-Lemeshow test
hosmer_lemeshow <- hoslem.test(data$satisfaction, predicted_probs)
# Print the test results
print(hosmer_lemeshow)


stargazer(lpm, mylogit, myprobit, myprobit13, type="text")

info_criterion <- data.frame(model = c("lpm", "mylogit", "myprobit", "myprobit13"),
                             AIC = c(AIC(lpm), AIC(mylogit), AIC(myprobit), AIC(myprobit13)),
                             BIC = c(BIC(lpm), BIC(mylogit), BIC(myprobit), BIC(myprobit13))   
                             )

info_criterion


# odds ratio ONLY FOR LOGITS
#
#

#white estimator needed?
#robust_vcov = vcovHC(myprobit, data = data, type = "HC")
#coeftest(myprobit, vcov.=robust_vcov)

# Calculate marginal effects
marginal_effects <- margins(myprobit13, data = data)
# Print the marginal effects
print(marginal_effects)
summary(marginal_effects)
