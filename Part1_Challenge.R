library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(emmeans)

raw_data <- read.csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/data1.csv")
#Need to convert Subject, Item and Condition to factors

tidied_data <- raw_data %>%
  mutate(Subject = factor(Subject),
         Item = factor(Item),
         Condition = factor(Condition))

#Let's plot the data to see if we can see if a linear model will fit
tidied_data %>%
  ggplot(aes(x = Condition, y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .1) +
  theme_minimal() +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = "none") +
  ylab("Reaction Time (ms)") +
  coord_flip()
#From the plot it appears that there is a difference in the RT means, although
#there is a lot of data overlap

#Let's build a model for predicting RT
RT_model <- lmer(RT ~ Condition + (1 | Subject) + (1 | Item), 
                 data = tidied_data)
summary(RT_model)

#Random effects:
#Groups   Name        Variance Std.Dev.
#Subject  (Intercept)  78471   280.1   
#Item     (Intercept) 117874   343.3   
#Residual             399974   632.4 

#Fixed effects:
#               Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)    1366.19      83.20   50.54  16.420  < 2e-16
#ConditionRare   200.28      47.88  622.18   4.183 3.29e-05

#So from this model there is a ~200ms difference in reaction time between rare
#and common words with rare words being more quickly reacted to. P < 0.05
#therefore this is significant

check_model(RT_model)
#The model is ok, most of the data fits it. There are a few influential 
#observations (163, 192, 120, 221, 96 and 22). The normality of residuals also
#is poor after the second standard normal distribution quantile




