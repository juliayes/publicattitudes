#New script for support for YJ policies paper

#load data file
dflabel <- read.csv("labellingpilot3.csv")

#testing for normality
install.packages("moments")
library(moments)

skewness(dflabel$policy1)
kurtosis(dflabel$policy1)
skewness(dflabel$policy2)
kurtosis(dflabel$policy2)
skewness(dflabel$policy3)
kurtosis(dflabel$policy3)
skewness(dflabel$policy4)
kurtosis(dflabel$policy4)
skewness(dflabel$policy5)
kurtosis(dflabel$policy5)
skewness(dflabel$policy6)
kurtosis(dflabel$policy6)


#demographic descriptives
mean(dflabel$age, na.rm = TRUE)
summary(dflabel$age, na.rm=TRUE)
sd(dflabel$age, na.rm = TRUE)
mean(dflabel$politicalid, na.rm = TRUE)
summary(dflabel$politicalid, na.rm=TRUE)
sd(dflabel$politicalid, na.rm = TRUE)
mean(dflabel$rehab1, na.rm = TRUE)
summary(dflabel$rehab1, na.rm = TRUE)
sd(dflabel$rehab1, na.rm = TRUE)
mean(dflabel$rehab2, na.rm = TRUE)
summary(dflabel$rehab2, na.rm = TRUE)
sd(dflabel$rehab2, na.rm = TRUE)

tabulate(dflabel$gender)
tabulate(dflabel$ethnicity)
tabulate(dflabel$education)

#support for policies descriptives  
library(dplyr)

variables <- c('policy1', 'policy2', 'policy3', 'policy4', 'policy5', 'policy6','politicalid','rehab1','rehab4','policysupport') 

summary_stats <- dflabel %>%
  summarise(across(all_of(variables), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)), .names = "{col}_{fn}"))
print(summary_stats)

library(ggplot2)

for (var in variables) {
  p <- ggplot(dflabel, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  
  print(p)  # Explicitly print each plot
}

tabulate(dflabel$policy1)
tabulate(dflabel$policy2)
tabulate(dflabel$policy3)
tabulate(dflabel$policy4)
tabulate(dflabel$policy5)
tabulate(dflabel$policy6)

#recode demographics
dflabel$female <- ifelse(dflabel$gender == 2, 1 , 0 )
dflabel$european <- ifelse(dflabel$ethnicity ==1,1,0)
dflabel$maoripacifika <- ifelse(dflabel$ethnicity ==2 | dflabel$ethnicity ==4,1,0)
dflabel$asian <- ifelse(dflabel$ethnicity ==3,1,0)
dflabel$MELAA <- ifelse(dflabel$ethnicity ==5,1,0)
dflabel$other <- ifelse(dflabel$ethnicity==6,1,0)
dflabel$educated <- ifelse(dflabel$education ==1 | dflabel$education ==2 | dflabel$education ==3 | dflabel$education ==4,1,0)

#cronbach's alpha for support for YJ policies
install.packages("psych")
library(psych)
alpha(dflabel[,c("policy1","policy2","policy3","policy4","policy5","policy6")])

#create mean support for YJ policies 
library(dplyr)
dflabel <- dflabel %>% mutate(policysupport=(policy1 + policy2 + policy3 + policy4 + policy5 + policy6)/6)
mean(dflabel$policysupport)
range(dflabel$policysupport)
sd(dflabel$policysupport)

#correlations
cor.test(dflabel$policysupport, dflabel$politicalid,method=c("pearson"))
cor.test(dflabel$policysupport, dflabel$rehab1,method=c("pearson"))
cor.test(dflabel$policysupport, dflabel$rehab4,method=c("pearson"))
cor.test(dflabel$politicalid, dflabel$rehab1,method=c("pearson"))
cor.test(dflabel$politicalid, dflabel$rehab4,method=c("pearson"))
cor.test(dflabel$rehab1, dflabel$rehab4,method=c("pearson"))

#linear regression models
library(QuantPsyc)
lm1 <- lm(policysupport ~ condition2 + age + female + maoripacifika + asian + MELAA + other + educated, data=dflabel)
summary(lm1)
lm.beta(lm1)
lm2 <- lm(policysupport ~ condition2 + age + female + maoripacifika + asian + MELAA + other + educated + politicalid, data=dflabel)
summary(lm2)
lm.beta(lm2)
lm3 <- lm(policysupport ~ condition2 + age + female + maoripacifika + asian + MELAA + other +educated + politicalid + rehab1 + rehab4, data=dflabel)
summary(lm3)
lm.beta(lm3)


modeleducation <- lm(policysupport ~ condition * educated, data = dflabel)
summary(modeleducation)

library(interactions)
interact_plot(modeleducation, pred = condition, modx = educated, plot.points=TRUE, legend.main="Education")

