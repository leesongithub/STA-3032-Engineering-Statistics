energy <- read.table("http://www.stat.ufl.edu/~winner/data/resid_energy.dat",
         header=F,col.names=c("yr","pop","gdp","hhsz","hhinc","costelec",
            "costngas","costhoil","consume"))
head(energy)
energy$avecost <- (energy$costelec + energy$costngas + energy$costhoil)/3
#resident population, GDP, household size, household income, cost of electricity,
#cost of natural gas, cost of heating oil, energy consumption

e.mod1 <- lm(consume ~ pop, data = energy) 
summary(e.mod1)
e.mod2 <- lm(consume ~ pop + gdp, data = energy)
summary(e.mod2)
e.mod3 <- lm(consume ~ pop + gdp + hhsz, data = energy)
summary(e.mod3)
e.mod4 <- lm(consume ~ pop + gdp + hhsz + avecost, data = energy)
summary(e.mod4)




dat=read.table("http://www.stat.ufl.edu/~winner/data/heatcapacity.dat",header=FALSE)
names(dat) <- c("index", "heat_capacity", "temperature")
reg <- lm(heat_capacity ~ temperature, data = dat)
summary(reg)
plot(heat_capacity ~ temperature, data = dat)
abline(reg)
source("http://www.stat.ufl.edu/~athienit/check.R") 
check(reg)

#check for box-cox transformation since model fit is primary issue
library(car)
bc=powerTransform(dat$temperature)
summary(bc)
#confidence interval includes 1; no better transformation suggested

#fit a polynomial term possibly
dat$temp2 <- (dat$temperature)^2
reg2 <- lm(heat_capacity ~ temperature + temp2, data = dat)
summary(reg2)
check(reg2)



#AIC values
AIC(e.mod1)
AIC(e.mod2)
AIC(e.mod3)
AIC(e.mod4)


#confounding
summary(e.mod1)
e.mod1a <- lm(consume ~ gdp, data = energy)
summary(e.mod1a)
summary(e.mod2)

#F test
#H0: beta_2 = beta_4 = 0
#fit full model, reduced model
lmfull <- lm(consume ~ pop + gdp + hhsz + avecost, data = energy)
summary(lmfull)
SSEfull <- (.4037)^2 * 22
lmred <- lm(consume ~ pop + hhsz, data = energy)
summary(lmred)
SSEred <- (.4063)^2 * 24
F <- ((SSEred - SSEfull) / 2) / (SSEfull / 22)
#find p-value
1 - pf(F, 2, 22)
#fail to reject

#alternatively, can use following R code:
anova(lmred,lmfull)
#gives same F statistic and p value; fail to reject H0


#####notes example

linthurst <- read.table("http://www.stat.ufl.edu/~athienit/STA6166/linthurst.txt",
                        row.names=1,  # interpret column 1 as row names
                        skip=1,
                        col.names=c("obsnum","loc","type","biomass",
                                    "salinity","pH","K","Na","Zn"))
head(linthurst)


# to look at correlation between two single predictors:
cor(linthurst$pH, linthurst$Zn)
# correlation matrix
round(cor(linthurst[c("biomass","salinity","pH","K","Na","Zn")]),3)  
#symmetric, so you only need to look at top-right half, above the 1's
#look for large absolute values, i.e. close to 1 or -1, for potential collinearity

linthurst.model <- lm(biomass ~ salinity + pH + K + Na + Zn, data=linthurst)
summary(linthurst.model)
AIC(linthurst.model)
anova(linthurst.model) #SSE is under Sum Sq Residuals; MSE is Mean Sq Residuals

# Can we remove salinity , K and Zn simultaneously?
# Not including Na as it is correlated with K.
linthurst.model.r <- lm(biomass ~ pH + Na, data=linthurst)
summary(linthurst.model.r)
anova(linthurst.model.r)
AIC(linthurst.model.r)

anova(linthurst.model.r, linthurst.model)

check(linthurst.model.r)

# Remove Na
linthurst.model.r2 <- lm(biomass ~ pH, data = linthurst)
summary(linthurst.model.r2)
AIC(linthurst.model.r2)
# So final model to use is linthurst.model.r or linthurst.model.r2? 
#.08 is only barely insignificant, and AIC lower. probably linthurst.model.r

# Fit biomass for pH=4.15 AND Na=10000. and create a C.I. and a P.I.
newdata=data.frame(pH=4.15,Na=10000)
predict(linthurst.model.r, newdata, interval="confidence",level=0.95)
predict(linthurst.model.r, newdata, interval="prediction",level=0.95)

check(linthurst.model.r)


###### Single indicator variable example

dat=read.csv("http://www.stat.ufl.edu/~athienit/STA6166/safe_reg.csv",header=TRUE)
head(dat); tail(dat)

library(car)
scatterplot(y~x1|x2,smooth=FALSE,reg.line=FALSE,data=dat)

reg1=lm(y~x1+x2, data=dat)
summary(reg1)

reg2=lm(y~x1+x2+x1:x2, data=dat) #same as lm(y~x1+x2+x1*x2) or lm(y~x1*x2). ":" means interaction
summary(reg2)

#confidence interval for beta_1 + beta_3
vc=vcov(reg2);vc
(.019749 - .010957) - qt(.975, 36) * sqrt(vc[2,2] + vc[4,4] + 2*vc[2,4])
(.019749 - .010957) + qt(.975, 36) * sqrt(vc[2,2] + vc[4,4] + 2*vc[2,4])
#slicker code to do the same thing
sum(reg2$coefficients[c(2,4)])+c(1,-1)*qt(0.025,reg2$df.residual)*sqrt(vc[2,2]+vc[4,4]+2*vc[2,4])


###### Multiple Indicator Variables example

Dose=rep(c(0.2,0.4,0.8,1.6),each=3)
Product=rep(c("A","B","C"),4)
y=c(2.0,1.8,1.3,4.3,4.1,2.0,6.5,4.9,2.8,8.9,5.7,3.4)
ds=data.frame(Dose=(Dose),Product=factor(Product),Response=y)
xtabs(Response~Dose+Product,data=ds)
ds

#concise code to do everything
modelfull=lm(Response~Dose*Product,data=ds) #automatically leaves out first factor
summary(modelfull)
anova(modelfull)

#alternatively, you can create indicator variables yourself
ds$A <- ifelse(ds$Product == "A", 1, 0)
ds$B <- ifelse(ds$Product == "B", 1, 0)
modelfull2 <- lm(Response ~ Dose + A + B + Dose*A + Dose*B, data = ds)

#model 1: intercept for product A is 1.9783
#model 2: intercept for product A is 1.3217 + 0.6565 = 1.9782 (same, rounding)
#model 1: slope for product A is 4.5957
#model 2: slope for product A is 1.4043 + 3.1913 = 4.5956 (same, rounding)

#proceed from here using model 1.
summary(modelfull)

#3 models:
#A: Response = 1.9783 + 4.5957 * Dose
#B: Response = (1.9783 + 0.4087) + (4.5957 - 2.2783) * Dose
#C: Response = (1.9783 - 0.6565) + (4.5957 - 3.1913) * Dose

#Question: are the slopes of product A and B significantly different? same for A and C?
#Question: are the slopes of product B and C significantly different?
#H: beta_2 + beta_5 = beta_2 + beta_6
#H: beta_5 = beta_6 .... H: beta_5 - beta_6 = 0
#Make a CI for beta_5 - beta_6, see if 0 falls in it. need var-covar matrix

vc=vcov(modelfull);round(vc,3)

#CI: note V(X - Y) = V(X) + V(Y) - 2cov(X,Y)
(-2.2783 - (-3.1913)) - qt(.975, 6) * sqrt(vc[5,5] + vc[6,6] - 2 * vc[5,6])
(-2.2783 - (-3.1913)) + qt(.975, 6) * sqrt(vc[5,5] + vc[6,6] - 2 * vc[5,6])
#FAIL to reject H0. cannot conclude significantly different slopes.








