library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(pbkrtest)

#Read in raw data:
raw_timing_data <- read.csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/factorial_data.csv")
#Subject, Item, Prime and Target are all not factors, so let's change that

tidied_timing_data <- raw_timing_data %>%
  mutate(Subject = factor(Subject),
         Item = factor(Item),
         Prime = factor(Prime),
         Target = factor(Target))

#Everything is in correct data format, so let's visualize the data incuding
#means with standard error
#Potentially missing datapoints, so let's see where they are

tidied_timing_data %>%
  group_by(Prime, Target) %>%
  summarise(mean_Time = mean(Time),
            sd_Time = sd(Time))

#Prime    Target   mean_Time sd_Time
#<fct>    <fct>        <dbl>   <dbl>
#1 Negative Negative        NA      NA
#2 Negative Positive        NA      NA
#3 Positive Negative        NA      NA
#4 Positive Positive        NA      NA

#There are missing datapoints in all cases
library(visdat)
vis_miss(tidied_timing_data)
#1.9% of all Time data is missing, let's count how many that is

tidied_timing_data %>%
  filter(is.na(Time)) %>%
  count()
#121 Time datapoints are misssing...

tidied_timing_data %>%
  filter(!is.na(Time)) %>%
  group_by(Prime, Target) %>%
  summarise(mean_Time = mean(Time),
            sd_Time = sd(Time))
#  Prime    Target   mean_Time sd_Time
#<fct>    <fct>        <dbl>   <dbl>
#1 Negative Negative      321.    167.
#2 Negative Positive      376.    219.
#3 Positive Negative      361.    191.
#4 Positive Positive      337.    203.

#^Looks like means are relatively similar, SDs are pretty different
#Let's code the ggplot to skip the missing Time data

tidied_timing_data %>%
  filter(!is.na(Time)) %>%
  ggplot(aes(x = Prime:Target, y = Time, colour = Prime:Target)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  #guides() +
  labs(title = "Time taken to respond to target word, based upon priming word",
       x = "Priming Word (left), Target Word (right)",
       y = "Time taken to respond to target word (ms)") +
  coord_flip()
#Looks like Negative:Negative and Negative:Positive will be significant, others
#looks much less likely, let's make a model and see
#We need to set the contrasts:
contrasts(tidied_timing_data$Prime) <- matrix(c(0.5, -0.5))
contrasts(tidied_timing_data$Target) <- matrix(c(0.5, -0.5))

#Let's build a maximal model and see if it works
Part1_model <- lmer(Time ~ Prime * Target +
                      (1 + Prime * Target | Subject) +
                      (1 + Prime * Target | Item),
                    data = tidied_timing_data)
#boundary (singular) fit: see help('isSingular')
#Warning message:
#Model failed to converge with 1 negative eigenvalue: -1.3e+01 

#Maximal model does not work

simp_Part1_model <- lmer(Time ~ Prime * Target +
                           (1 + Prime * Target | Subject) +
                           (1 + Prime + Target | Item),
                         data = tidied_timing_data)
check_model(simp_Part1_model)
#Doesn't work

simp_Part1_model_2 <- lmer(Time ~ Prime * Target +
                           (1 + Prime + Target | Subject) +
                           (1 + Prime * Target | Item),
                         data = tidied_timing_data)
check_model(simp_Part1_model_2)
#Doesn't work

simp_Part1_model_3 <- lmer(Time ~ Prime * Target +
                             (1 + Prime + Target | Subject),
                           data = tidied_timing_data)
#Doesn't work

simp_Part1_model_4 <- lmer(Time ~ Prime * Target + 
                             (1 | Subject) +
                             (1 | Item),
                           data = tidied_timing_data)
summary(simp_Part1_model_4)
#This is as complex a model as I can get without it not working
#Let's compare to a null model
simp_Part1_model_null <- lmer(Time ~ 1 +
                                (1 + Prime + Target | Subject) +
                                (1 + Prime * Target | Item),
                              data = tidied_timing_data)

anova(simp_Part1_model_4, simp_Part1_model_null)
#                      npar   AIC   BIC  logLik deviance Chisq Df Pr(>Chisq)
#simp_Part1_model_4       7 15392 15427 -7688.9    15378                    
#simp_Part1_model_null   18 15416 15507 -7689.9    15380     0 11          1

#Model4 is barely better than a null model
summary(simp_Part1_model_4)

#Random effects:
#Groups   Name        Variance Std.Dev.
#Subject  (Intercept)  6121     78.24  
#Item     (Intercept)  1413     37.59  
#Residual             31007    176.09  
#Number of obs: 1159, groups:  Subject, 40; Item, 32

#Fixed effects:
#                 Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)     344.6124    14.9824   50.8068  23.001  < 2e-16 ***
#Prime1            0.2717    10.3688 1089.1868   0.026 0.979101    
#Target1         -16.9031    10.3787 1090.0633  -1.629 0.103680    
#Prime1:Target1  -78.7537    20.7330 1088.9179  -3.798 0.000154 ***          



emmeans(simp_Part1_model_4, pairwise ~ Prime * Target, adjust = "none")

#$emmeans
#Prime    Target   emmean   SE   df lower.CL upper.CL
#Negative Negative    317 17.6 94.8      282      351
#Positive Negative    356 17.4 92.3      321      390
#Negative Positive    373 17.5 92.8      338      408
#Positive Positive    333 17.4 92.4      299      368

#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95 

#$contrasts
#contrast                              estimate   SE   df t.ratio p.value
#Negative Negative - Positive Negative    -39.1 14.7 1088  -2.660  0.0079
#Negative Negative - Negative Positive    -56.3 14.8 1089  -3.814  0.0001
#Negative Negative - Positive Positive    -16.6 14.7 1090  -1.129  0.2592
#Positive Negative - Negative Positive    -17.2 14.6 1088  -1.175  0.2401
#Positive Negative - Positive Positive     22.5 14.6 1088   1.541  0.1236
#Negative Positive - Positive Positive     39.6 14.6 1089   2.711  0.0068

#Negative:Negative reaction time was ~40ms faster than Positive:Negative, it 
#was also singificant

#N:N, N:P ~55ms faster, also significant
#N:P, P:P ~40ms slower, also significant

#All other differences seen were not significant










