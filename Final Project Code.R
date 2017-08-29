MLB_2012 <- read.csv("~/Stats 20/Updated^2 MLB Data.csv")
attach(MLB_2012)

MLB_2012$ERAratio <- cbind(HomeERA/AwayERA)

MLB_2012$hwp <- cbind(W/(W+L)) #add win percentage column

MLB_2012$PF <- cbind((((RSH+RAH)/(81))/((RSA+RAA)/(81))))
head(MLB_2012)

#PLOT 1: PF vs. ERA

mod1 <- lm(MLB_2012$PF ~ MLB_2012$ERAratio)
par(mfrow=c(1,1))
plot(MLB_2012$ERAratio, MLB_2012$PF,
     xlab = "Earned Run Average",
     ylab = "Park Factor")
abline((mod1), col="red")


par(mfrow=c(2,2))
plot(mod1)

summary(mod1)

mod1
anova(mod1)


MLB_2012$OBPratio <- cbind(HomeOBP/AwayOBP)

#PLOT 2: PF vs. OBP
par(mfrow=c(1,1))
mod2 <- lm(MLB_2012$PF ~ MLB_2012$OBPratio)
plot(MLB_2012$OBPratio, MLB_2012$PF,
     xlab = "On Base Percentage",
     ylab = "Park Factor")
abline((mod2), col="blue")

summary(mod2)
anova(mod2)


par(mfrow=c(2,2))
plot(mod2)
summary(mod2)
mod2


#PLOT 3: PF vs. SLG
par(mfrow=c(1,1))

MLB_2012$SLGratio <- cbind(HomeSLG/AwaySLG)
mod3 <- lm(MLB_2012$PF ~ MLB_2012$SLGratio)
plot(MLB_2012$SLGratio, MLB_2012$PF,
     xlab = "SLG",
     ylab = "Park Factor")

abline(mod3, col="green")

par(mfrow=c(2,2))
plot(mod3)
summary(mod3)



#PLOT 4: PF vs. Outfield
par(mfrow=c(1,1))

mod4 <- lm(MLB_2012$PF ~ MLB_2012$OF.Area)
plot(MLB_2012$OF.Area, MLB_2012$PF,
     xlab = "Outfield Area",
     ylab = "Park Factor")
abline(mod4, col="blue")

par(mfrow=c(2,2))
plot(mod4)

summary(mod4)
anova(mod4)
mod4


abline((mod4), col="orange")

#PLOT 5: PF vs. hwp
par(mfrow=c(1,1))

mod5 <- lm(MLB_2012$PF ~ MLB_2012$hwp)
plot(MLB_2012$hwp, MLB_2012$PF,
     xlab = "Home Win Percentage",
     ylab = "Park Factor")
abline((mod5), col="red")


summary(mod5)
plot(mod5)
anova(mod5)
mod5

################
#Multiple linear regression

#We want to prove that which of the many predictors we have is a significant predictor for our Park Factor.
#So we create a full model of multiple predictors with Park Factor as the response variable.



fullmultiplemlb <- lm(MLB_2012$PF ~ MLB_2012$ERAratio + MLB_2012$OBPratio + MLB_2012$SLGratio +
                        MLB_2012$OF.Area + MLB_2012$hwp)
summary(fullmultiplemlb)
plot(fullmultiplemlb)

#Looking at the p-values, with p values less that 0.05, we can say that the predictors ERA ratio, OBPratio,
#and SLGratio are significant predictors in our MLR. 


reducedmultiplemlb <- lm(MLB_2012$PF ~ MLB_2012$ERAratio + MLB_2012$OBPratio + MLB_2012$SLGratio)
summary(reducedmultiplemlb)

anova(reducedmultiplemlb,fullmultiplemlb)

#Looking at the ANOVA table, because the F stat of 1.9688 has a p-value of 0.1417, we cannot reject the null
#hypothesis (where all the slopes are equal to each other) at the significance level of 5%. So the variables
#Outfield Area and Home Winning Percentage do not contribute significant information to the Park Factor. 

#When we compare the adjusted R^2 value from the full model to the reduced model, we notice a very insignificant
#difference. From 0.7528 in our Full model to 0.7509 in our reduced model.

#And looking at our reduced model, we notice and can conclude again that ERAratio, OBPratio, and SLGratio are
#significant predictors for our response variable, Park Factor.



par(mfrow=c(2,2))
plot(fullmultiplemlb)

#The variable SLG has the largest effect on PF with a large regression coefficient of 0.844545. While the variable
#of home winning percentage has the least effect on PF with a regression coefficient of -0.094. Judging by the
#p-values we can keep the predictors ERA ratio, OBP ratio, and SLG ratio, but most likely better to remove our OF.Area
#and home winning percentage. Changes in the good predictor's value are related to changes in the PF. While changes
#in our OF.Area and home winning percentage are not associated with changes in our PF.