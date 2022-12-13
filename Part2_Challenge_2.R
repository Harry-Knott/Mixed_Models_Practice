library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(pbkrtest)

#Read in raw data:
raw_happy_sad_data <- read.csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/accuracy_data.csv")
head(raw_happy_sad_data)
str(raw_happy_sad_data)
#Subject, Face, FaceExpression all need to be factor:

tidied_face_data <- raw_happy_sad_data %>%
  mutate(Subject = factor(Subject),
         Face = factor(Face),
         FaceExpression = factor(FaceExpression))

tidied_face_data %>%
  ggplot(aes(x = FaceExpression, y = Acc, colour = FaceExpression)) +
  geom_jitter(width = .2) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal () +
  labs(x = "Facial Expression of Image",
       y = "Accuracy of participant about image (1 = correct, 0 = incorrect)",
       title = "Accuracy of participants at determining the emotional state of a face")

#From the plot, almost everyone was correct, some people were wrong most of 
#which said a face was happy when it was actually sad
#The means do look different, with sad almost exactly at 1 and happy
#being around 0.9, so there potentially is some signficance here

#Let's make a model and see
model <- glmer(Acc ~ FaceExpression + 
                 (1 | Subject) +
                 (1 | Face),
               data = tidied_face_data,
               family = binomial)
summary(model)

#Random effects:
#Groups  Name        Variance  Std.Dev. 
#Face    (Intercept) 1.966e-14 1.402e-07
#Subject (Intercept) 1.512e-01 3.889e-01
#Number of obs: 955, groups:  Face, 33; Subject, 32

#Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.5838     0.2118  12.198  < 2e-16 ***
#FaceExpressionSad   1.7037     0.4187   4.069 4.71e-05 ***

model1 <- glmer(Acc ~ FaceExpression + 
                 (1 | Subject),
               data = tidied_face_data,
               family = binomial)
summary(model1)

#Didn't get the singlar error like before
#Random effects:
#Groups  Name        Variance Std.Dev.
#Subject (Intercept) 0.1512   0.3889  
#Number of obs: 955, groups:  Subject, 32

#Fixed effects:
#                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.5838     0.2118  12.198  < 2e-16 ***
#FaceExpressionSad   1.7037     0.4187   4.069 4.71e-05 ***

model2 <- glmer(Acc ~ FaceExpression +
                  (1 | Face),
                data = tidied_face_data,
                family = binomial)
summary(model2)
#This model also not happy, so have to use model1

#Model1 states that Sad facial expression resulted in higher accuracy
#This was signficant as the p value was 4.71e-05










