copier=read.csv("http://www.stat.ufl.edu/~athienit/STA4210/Examples/copiers.csv",skip=1,header=TRUE)

reg=lm(Time~Copiers,data=copier)
summary(reg)

plot(copier$Copiers,copier$Time,pch=16,xlab="Quantity",ylab="Time (in minutes)",main="Scatterplot")
abline(reg)
abline(h = mean(copier$Time))

-0.5802 + 15.0352 * 7

SSE = 8.914^2 * 43
SST = SSE / (1 - .9575)
SSR = SST - SSE

15.0352 - 0.4831 * qt(.975, 43)
15.0352 + 0.4831 * qt(.975, 43)

confint(reg, level=0.95, type="Wald")

sxx = 44 * var(copier$Copiers)
xbar = mean(copier$Copiers)
s = 8.914
s * sqrt((1/45) + ((5 - xbar)^2 / sxx))

-0.5802 + 15.0352*5 - qt(.975, 43) * s * sqrt((1/45) + ((5 - xbar)^2 / sxx))
-0.5802 + 15.0352*5 + qt(.975, 43) * s * sqrt((1/45) + ((5 - xbar)^2 / sxx))

predict.lm(reg, se.fit=TRUE, newdata=data.frame(Copiers=5), interval="confidence",
level=0.95)

s * sqrt(1 + (1/45) + ((7 - xbar)^2 / sxx))

-0.5802 + 15.0352*7 - qt(.975, 43) * s * sqrt(1 + (1/45) + ((7 - xbar)^2 / sxx))
-0.5802 + 15.0352*7 + qt(.975, 43) * s * sqrt(1 + (1/45) + ((7 - xbar)^2 / sxx))

predict.lm(reg, se.fit=TRUE, newdata=data.frame(Copiers=7), interval="prediction",
level=0.95)


#####checking assumptions

re=rstandard(reg) #gives standardized residuals

#plot histogram/density of standardized residuals
plot(density(re), ylim=c(0,0.5))
hist(re, freq=FALSE, border='blue', add=TRUE)
#compare to true normal distribution
plot(function(t)dnorm(t),from=-3,to=3, col='red',add=TRUE)

#create QQ plot of standardized residuals
qqnorm(re,datax=TRUE,col=2)
qqline(re,datax=TRUE)

#check for independence
plot(re,type="o",pch=22,xlab="Order",ylab="std res",main="Independence")
#type = "o" tells it to plot all the points in order and connect them with a line
abline(h=0)

#check for constant variance/model fit
fitted <- fitted.values(reg) #alternatively, reg$fit gives same thing
plot(re~fitted,ylim=c(-3,3),xlab=expression(hat(y)),ylab="std res",main="Constant Variance/Model Fit")
abline(h=0)

#for the future; easy code to use
source("http://www.stat.ufl.edu/~athienit/check.R") #R-script for graphically checking assumptions
check(reg)

#box-cox
#install.packages('car', repos='http://cran.us.r-project.org')
#run above line of code if you haven't installed the package "car" before
library(car)
bc2=powerTransform(reg)
summary(bc2)
#fix y, run new model
yT=bcPower(copier$Time,1.1165)
reg2 <- lm(yT ~ copier$Copiers)
summary(reg2)
check(reg2)

#install.packages('MASS', repos='http://cran.us.r-project.org')
#run above line of code if you haven't installed the package "MASS" before
library(MASS)
bc1=boxcox(reg)
lambda=bc1$x[which.max(bc1$y)];lambda


##another example of checking assumptions
dat=read.table("http://www.stat.ufl.edu/~athienit/STA6166/wordrecall.txt",header=TRUE)
head(dat)
reg=lm(prop~time,data=dat)
plot(prop~time,data=dat)
abline(reg)
#check(reg, tests=TRUE) #need packages lawstat, Hmisc, lattice, survival, Formula, ggplot2 for tests=TRUE
check(reg)

#clearly an issue with independence, model fit. Try box-cox transformation for x
library(car)
bc=powerTransform(dat$time)
summary(bc)
#xT <- bcPower(dat$time, 0) #since fitted value is about lambda = 0, can just transform using
xT <- log(dat$time)
reg_bc <- lm(dat$prop ~ xT)
summary(reg_bc)
plot(dat$prop~xT)
abline(reg_bc)
check(reg_bc)





