# check stucture of everwk
str(NH11$everwrk) 
# check levels of everwk
levels(NH11$everwrk) 
# collapse all missing values to NA
NH11 <- transform(NH11,everwrk = factor(everwrk,levels = c("1 Yes", "2 No")),
        r_maritl = droplevels(r_maritl))
# run our regression model
hyp.out2 <- glm(everwrk~age_p+r_maritl,data=NH11, family="binomial")
summary(hyp.out2)
#Predict the probability of working for each level of marital status
library(effects)
data.frame(Effect("r_maritl", hyp.out2))


