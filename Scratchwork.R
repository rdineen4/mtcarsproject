##Load mtcars data
data(mtcars)
View(mtcars)
help(mtcars)

summary(mtcars)
fit <- lm(mpg ~ am, data = mtcars)
plot(mtcars$am, mtcars$mpg, pch = 21)
abline(fit)

##exploratory boxplot
mtcars$am[mtcars$am == 0] <- "automatic"
mtcars$am[mtcars$am == 1] <- "manual"

library(ggplot2)
ggplot(data = mtcars, mapping = aes(factor(am), mpg)) +
    geom_boxplot() +
    xlab("transmission")

##exploratory plot
library(ggpubr)
plot2 <- ggplot(data = mtcars, mapping = aes(wt, mpg, color = factor(am))) +
    geom_point(size = 3, color = "black") +
    geom_point(size = 2) +
    geom_smooth(method = "lm", formula = y ~ log(x)) +
    labs(x = "Weight (in 1000s of lbs)", y = "MPG", color = "Transmission")
    

plot1 <- ggplot(data = mtcars, mapping = aes(wt, mpg)) +
    geom_point(size = 3, color = "black") +
    geom_point(size = 2, aes(color = factor(am))) +
    geom_smooth(method = "lm", formula = y ~ log(x)) +
    labs(x = "Weight (in 1000s of lbs)", y = "MPG", color = "Transmission")

ggarrange(plot1, plot2, common.legend = TRUE)
   

#Very few low-weight manual cars, very few high-weight manual cars

library(dplyr)
library(ggplot2)
autocars <- mtcars %>% filter(am == 0)
manualcars <- mtcars %>% filter(am == 1)

##more fits
allfit <- lm(mpg ~ ., data = mtcars)
summary(allfit)

wtamfit <- lm(mpg ~ factor(am) + wt, data = mtcars)
summary(wtamfit)
plot(wtamfit, which = 1)

##auto fits
autowtfit <- lm(mpg ~ wt - 1, data = autocars)
summary(autowtfit)
plot(autowtfit)

autoOriginFit <- lm(I(mpg - mean(mpg)) ~ I(wt - mean(wt)) - 1, data = autocars)
summary(autoOriginFit)

plot((autocars$wt - mean(autocars$wt)), (autocars$mpg - mean(autocars$mpg)), pch = 21)
abline(autoOriginFit)

##manual fits
manualwtfit <- lm(mpg ~ wt -1, data = manualcars)
summary(manualwtfit)
plot(manualwtfit)

manualOriginFit <- lm(I(mpg - mean(mpg)) ~ I(wt - mean(wt)) - 1, data = manualcars)
summary(manualOriginFit)

plot((manualcars$wt - mean(manualcars$wt)), (manualcars$mpg - mean(manualcars$mpg)), pch = 21)
abline(manualOriginFit)

##vifs
library(car)
vif(allfit)

##log lms and resids
logfit <- lm(mpg ~ factor(am) + log(wt), data = mtcars)
summary(logfit)
plot(logfit, which = 1)

logManualFit <- lm(mpg ~ log(wt), data = manualcars)
plot(logManualFit, which = 1)

logAutoFit <- lm(mpg ~ log(wt), data = autocars)
plot(logAutoFit, which = 1)

##Adding stuff?

cor(mtcars$mpg, mtcars)
cor(mtcars$am, mtcars)
cor(mtcars$wt, mtcars$am)
cor(mtcars$mpg, mtcars) - cor(mtcars$am, mtcars)

logfit3 <- lm(mpg ~ factor(am) + log(wt) + cyl, data = mtcars)
summary(logfit3)
anova(logfit, logfit3)
plot(logfit3, which = 1)

noTransFit <- lm(mpg ~ log(wt) + cyl, data = mtcars)
summary(noTransFit)
anova(noTransFit, logfit3) #transmission is almost insignificant to the model

wtfit <- lm(mpg ~ wt, mtcars)
summary(wtfit)
wtcylfit <- lm(mpg ~ wt + cyl, mtcars)
summary(wtcylfit)
logwtcylfit <- lm(mpg ~ log(wt) + cyl, mtcars)
summary(logwtcylfit)

anova(wtfit, wtcylfit, logwtcylfit)

plot(logwtcylfit, which = 1)

idkfit <- lm(mpg ~ am, mtcars)
summary(idkfit)
plot(idkfit,which = 1)
