library("dplyr")
library('corrplot')
library("lmtest")
#install.packages("margins")
library("margins")
library("plm")
#install.packages("ResourceSelection")
library(ResourceSelection)

setwd("C:/Users/48796/OneDrive/Pulpit/STUDIA/ROK 4/SEM 2/A. Econometrics/project")

data <- as.data.frame(read.csv("Invistico_Airline.csv"))
View(data)

# Display the count of NAs
na_count <- sum(is.na(data))
print(na_count)
data<-na.omit(data)
View(data)

#rows_with_na<- data[!complete.cases(data), ]
# Print rows with NAs
#print(rows_with_na)

#BINARY ENCODING 
data$satisfaction<-ifelse(data$satisfaction == "satisfied", 1,0)
data$Gender<-ifelse(data$Gender == "Male", 1,0)
data$Customer.Type<-ifelse(data$Customer.Type == "Loyal Customer", 1,0)
data$Type.of.Travel<-ifelse(data$Type.of.Travel == "Personal Travel", 1,0)
data$Class <- as.integer(factor(data$Class, levels = c("Eco", "Eco Plus", "Business"), labels = c(1, 2, 3)))
#variables <- c("Seat.comfort", "Departure.Arrival.time.convenient", "Food.and.drink", "Gate.location", "Inflight.wifi.service", "Inflight.entertainment", "Online.support", "Ease.of.Online.booking","On.board.service", "Leg.room.service", "Baggage.handling", "Checkin.service", "Cleanliness", "Online.boarding")
#for (variable in variables) {
#  data <- data %>% 
#    mutate(!!variable := ifelse(!!sym(variable) %in% c(0, 1, 2), 0, 1))
#}


str(data)
summary(data)

x <- data[(data$Age==0), ]
x

par(mfrow = c(1,1))
cor_matrix <- cor(data)
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 0)

print_correlation_table <- function(data, variables) {
  cor_matrix <- cor(data[, variables])
  print(cor_matrix)
}

par(mar = c(2, 2, 2, 2))
columns1 <- c("Age", "Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
par(mfrow=c(2,2))
# Histograms for the columns in columns1
for (col in columns1) {
  hist(data[[col]], main = col, xlab = "", col = "lightblue")
}
for (col in columns1) {
  boxplot(data[[col]], main = col, col = "lightblue")
}
columns2 <- c("satisfaction", "Gender", "Customer.Type","Type.of.Travel", "Class", "Seat.comfort", "Departure.Arrival.time.convenient","Food.and.drink", "Gate.location","Inflight.wifi.service", "Inflight.entertainment", "Online.support", "Ease.of.Online.booking","On.board.service", "Leg.room.service", "Baggage.handling", "Checkin.service", "Cleanliness","Online.boarding")
par(mfrow = c(4, 5))  # Adjust the layout of the plots
# Bar plots for the columns in columns2
for (col in columns2) {
  barplot(table(data[[col]]), main = col, xlab = "", col = "lightblue")
}

for (col in c("Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")) {
  threshold <- quantile(data[[col]], 0.95)
  data <- data[data[[col]] <= threshold, ]
}

columns3 <- c("Age", "Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
par(mfrow=c(2,2))
# Histograms for the columns in columns1
for (col in columns3) {
  hist(data[[col]], main = col, xlab = "", col = "lightblue")
}


#no diff when 1,0 are 50-50, information criteria: we should use AIC to be sure, BIC(SBC) - its better, the lower the value the better 
#myprobit <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel+Class+Flight.Distance+Seat.comfort+Departure.Arrival.time.convenient+Food.and.drink+Gate.location+Inflight.wifi.service+Inflight.entertainment+Online.support+Ease.of.Online.booking+On.board.service+Leg.room.service+Baggage.handling+Checkin.service+Cleanliness+Online.boarding+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data=data, 
#                family=binomial(link="probit"))
myprobit <- glm(satisfaction~Gender+Customer.Type+Age*Flight.Distance+Type.of.Travel*Class+Flight.Distance+Seat.comfort+Food.and.drink+Departure.Arrival.time.convenient+Gate.location+Inflight.wifi.service*Online.support+Online.support*Ease.of.Online.booking+Seat.comfort*Inflight.entertainment+On.board.service+Leg.room.service+Checkin.service+Cleanliness+Baggage.handling+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data=data, 
                family=binomial(link="probit"))
summary(myprobit)

model_summary <- summary(myprobit)
# Extract the R-squared statistic
r_squared <- model_summary$deviance/model_summary$null.deviance
# Print the R-squared statistic
print(r_squared)
PseudoR2(myprobit)
#can interprate McKelvy.Zavoina if the latent variable is observed the model explainx x% of observations, count is the correct R2 explaining x% of the observation, adj.count is saying % ofcorrectly predicted observations with given variance

# we dont use ramsey reset, we use LINKTEST, modoelhas correct form if yhat is significant and yhat2 is not
#resettest(myprobit, power=2:3, type="fitted")
source("linktest.R")
linktest_result = linktest(myprobit)
# yhat and yhat2 are significat that mean we reject h0 about proper form
summary(linktest_result)
# can also use Wald test for beta2=beta3=0
H <- rbind(c(0,1,0,0), c(0,0,1,0))
# h %*% coef(dem.probit)
wald.test.results = wald.test(b = coef(dem.probit), 
                              Sigma = vcov(dem.probit), L = H)
wald.test.results

# general to specific thatway= h0 is beta=0(for additional variables) and with p-value<0.05 we reject h0. Joint insignificance of all variables test against null
null_probit = glm(satisfaction~1, data=data, family=binomial(link="probit"))
lrtest(myprobit, null_probit)
myprobit1 <- glm(satisfaction~Gender+Customer.Type+Age+Type.of.Travel*Class+Flight.Distance+Seat.comfort*Food.and.drink+Food.and.drink*Departure.Arrival.time.convenient*Gate.location+Inflight.wifi.service*Online.support*Ease.of.Online.booking+Inflight.entertainment+On.board.service+Leg.room.service+Checkin.service+Cleanliness*Baggage.handling+Departure.Delay.in.Minutes*Arrival.Delay.in.Minutes+Online.boarding*Online.support+Online.boarding*Inflight.wifi.service+Online.boarding*Ease.of.Online.booking, data=data, 
                family=binomial(link="probit"))
lrtest(myprobit, myprobit1)

# odds ratio ONLY FOR LOGITS
#
#

#Hosmer-Lemshow test and Osius-Rojek test? https://statisticalhorizons.com/hosmer-lemeshow/  https://statisticalhorizons.com/alternatives-to-the-hosmer-lemeshow-test/
gof.results = gof(myprobit)
gof.results$gof
# Get predicted probabilities from the model
predicted_probs <- predict(myprobit, type = "response")
# Perform the Hosmer-Lemeshow test
hosmer_lemeshow <- hoslem.test(data$satisfaction, predicted_probs)
# Print the test results
print(hosmer_lemeshow)


bptest(myprobit, data=data)
#white estimator needed?
robust_vcov = vcovHC(myprobit, data = data, type = "HC")
coeftest(myprobit, vcov.=robust_vcov)

# Calculate marginal effects
marginal_effects <- margins(myprobit, data = data)
# Print the marginal effects
print(marginal_effects)
summary(marginal_effects)
