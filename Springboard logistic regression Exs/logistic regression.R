library(ggmap)
library(devtools)
library(tidyr)
library(readr)
library(ggplot2)
getwd()
setwd("~/Desktop/logistic_regression/dataSets2")
#   Load the National Health Interview Survey data:
NH11 <- readRDS("NatHealth2011.rds")
labs <- attributes(NH11)$labels
# Let's predict the probability of being diagnosed with hypertension based on age, sex, sleep, and bmi
# check stucture of hypev
str(NH11$hypev) 
# check levels of hypev
levels(NH11$hypev) 
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,data=NH11, family="binomial")
coef(summary(hyp.out))
#transform the coefficients to make them easier to interpret
hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab
# Create a dataset with predictors set at desired levels
predDat <- with(NH11,expand.grid(age_p = c(33, 63),sex = "2 Female",bmi = mean(bmi, na.rm = TRUE),sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",se.fit = TRUE, interval="confidence",newdata = predDat))

library(effects)
plot(allEffects(hyp.out))


