cruise <- read.fwf("http://www.stat.ufl.edu/~winner/data/cruise_ship.dat", width=c(20,20,rep(8,7)),
                   col.names=c("ship", "cline", "age", "tonnage", "passengers", "length", "cabins", "passdens", "crew"))
head(cruise)

#### Goal: predict number of crew needed for a cruise ship

####### Fit "Full" model
fit0 <- lm(crew ~ age + tonnage + passengers + length + cabins + passdens,data=cruise)
summary(fit0)

######### Perform Backward Elimination, Forward Selection, and Stepwise Regression
######### Based on Model AIC (not individual regression coefficients)
######### fit1 and fit2 represent "extreme" models
library(MASS)
fit1 <- lm(crew ~ age + tonnage + passengers + length + cabins + passdens, data=cruise)
fit2 <- lm(crew ~ 1, data=cruise) #fits the model using no predictors
fit3 <- stepAIC(fit1,direction="backward",scope=list(upper=fit1,lower=fit2)) #start with full, work down
fit4 <- stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2)) #start with nothing, work up
fit5 <- stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2)) #start with nothing, work up, go both ways

summary(fit3)
summary(fit4)
summary(fit5)
#all gave same results in this case
