#Tachinid Competitive and Development Project - Data Exploration
#Keaton Wilson
#2018-02-13
#keatonwilson@me.com

#Packages
library(tidyverse)

#Importing the master data file
tach_master = read_csv(file = "Data/TachDataFull.csv")

#Cleaning up a bit
tach_master = tach_master %>%
  select(-SiblingNumber, -NutritionIndex)
tach_master$Sex = factor(tach_master$Sex)

#First steps: 1. Let's get sibling numbers for each group so we can calculate the nutritional index.
siblings = tach_master %>%
  group_by(CaterpillarID) %>%
  summarize(sib_number = max(FlyID))

#left_Join this
tach_master = tach_master %>%
  left_join(siblings)

#Generating a Nutritional Index that is the head capsule width divided by the number of siblings in a cohort
tach_master = tach_master %>%
  mutate(nut_index = HeadCapsuleWidth/sib_number)

#Let's impute NA values. 
library(caret)
#install.packages("VIM")
library(VIM)


tach_master_impute = as.tibble(kNN(tach_master, variable = colnames(tach_master[8:13]), numFun = "weightedMean"))
tach_master_impute$FlyWeight = as.numeric(tach_master_impute$FlyWeight)



#Exploratory plots!

#Fly weight as a function of sibling number
ggplot(tach_master_impute, aes(x = sib_number, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1)) +
  xlab("Cohort Size") +
  ylab("Fly Weight (mg)")

#Fly weight as a function of head capsule size
ggplot(tach_master_impute, aes(x = HeadCapsuleWidth, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1)) +
  xlab("Host head-capsule width (mm)") +
  ylab("Fly Weight (mg)")

#best model seems to be one that includes both, additively.
lm1 = lm(FlyWeight ~ sib_number + HeadCapsuleWidth, data = tach_master_impute)
summary(lm1)
#Also interesting to note how sex doesn't come into play here... it makes the model worse. Indicating that this relationship isn't
#different among the sexes

#Also need to show that there is a positive correlation between head-capsule width and weight @ wander
#Read in bigger data set

head_cap_wander = read_csv(file = "Data/Parasitized Caterpillar Data.csv")
ggplot(head_cap_wander, aes(x = HeadCapsuleWidth, y = WanderWeight)) +
  geom_jitter(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm") +
  xlab("Head Capsule Width (mm)") +
  ylab("Weight at wandering (g)")

lm_head_cap = lm(WanderWeight ~ HeadCapsuleWidth, data = head_cap_wander)

#Not great - but there are a lot of other variables embedded in that - perhaps it's better to use other examples from the literature.

#Ok, now the fun stuff.

#Head weight versus body weight 
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(HeadWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(-1.5,3)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Head Weight (mg)")
  
#No difference between males and females
lmhvb = lm(log(HeadWeight) ~ log(FlyWeight), data = tach_master_impute)
summary(lmhvb)
confint(lmhvb)
#Let's do a Wald's test to check and see if the slope is sig. diff from 1
library(car)
linearHypothesis(lmhvb, "log(FlyWeight) = 1")

#Slope is significantly less than 1. 


#Thorax versus Body  
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(ThoraxWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(-1.5,3)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Thorax Weight (mg)")

#Differences between males and females
lmtvbvs = lm(log(ThoraxWeight) ~ Sex/log(FlyWeight) -1, data = tach_master_impute)
summary(lmtvbvs)
#Testing differences from a slope of 1
linearHypothesis(lmtvbvs, "SexF:log(FlyWeight) = 1")
linearHypothesis(lmtvbvs, "SexM:log(FlyWeight) = 1")


#Yep, Males are significantly higher, no difference in slope though.

#Abdomen versus Body
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(AbWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(-1.5,3)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Abdomen Weight (mg)")

#Things are reversed. Females are significantly higher, no difference in slopes. 
lmavbvs = lm(log(AbWeight) ~ log(FlyWeight) + Sex, data = tach_master_impute)


#Wings versus Body
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(WingWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3,3)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Wing Weight (mg)")

#No difference between males and females

#Legs versus Body
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(as.numeric(LegsWeight)), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3,3)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Legs Weight (mg)")


#Ok, these are rad, and Goggy will love them, I think. 


#Can we do them all on one plot!?
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(HeadWeight), shape = Sex)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") +
  geom_point(aes(x = log(FlyWeight), y = log(ThoraxWeight)), size = 2, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, aes(x = log(FlyWeight), y = log(ThoraxWeight))) +
  geom_point(aes(x = log(FlyWeight), y = log(AbWeight)), size = 2, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, aes(x = log(FlyWeight), y = log(AbWeight))) +
  geom_point(aes(x = log(FlyWeight), y = log(WingWeight)), size = 2, alpha = 0.5, shape = 4) +
    geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1, x = log(FlyWeight), y = log(WingWeight))) +
  geom_point(aes(x = log(FlyWeight), y = log(as.numeric(LegsWeight))), size = 2, alpha = 0.5, shape = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1, x = log(FlyWeight), y = log(as.numeric(LegsWeight)))) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(0, 3), ylim = c(-3,3)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Body Part Weight (mg)") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 2.2, label = "Thorax", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 1.5, label = "Abdomen", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 0.5, label = "Head", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 0.1, label = "Legs", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = -1.3, label = "Wings", size = 10), color = "black")


#Better way to make the plot above - but with changes described by Robbie
tach_master_impute
tach_master_impute_long = tach_master_impute %>%
  select(Sex, FlyWeight:LegsWeight) %>%
  gather(key = "organ", value = "organ_weight", -Sex, -FlyWeight)
tach_master_impute_long$organ = factor(tach_master_impute_long$organ)
tach_master_impute_long$organ_weight = as.numeric(tach_master_impute_long$organ_weight)

tach_master_impute_long %>%
  filter(organ == "ThoraxWeight" | organ == "AbWeight") %>%
ggplot(aes(x = log(FlyWeight), y = log(organ_weight), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", aes(lty = Sex)) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 3) +
  coord_cartesian(xlim = c(0, 3), ylim = c(-3,3)) +
  scale_color_discrete(guide = FALSE) +
  scale_shape_discrete(labels = c("Female", "Male")) +
  scale_linetype_discrete(guide = FALSE) +
  geom_label(show.legend = FALSE, aes(x = 3, y = 2.2, label = "Thorax",
                                      size = 9), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 1.5, label = "Abdomen",
                                      size = 9), color = "black")
  

#Didn't he do relative body part size plotted against size?

ggplot(tach_master_impute, aes(x = FlyWeight, y = HeadWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), aes(group = 1), color = "black")

ggplot(tach_master_impute, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2))

ggplot(tach_master_impute, aes(x = FlyWeight, y = AbWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm")


#Seems like there might a wing-weight that is fucked up - outlier removed - should probably remove all data where the wing category is False
ggplot((tach_master_impute %>% filter(WingWeight/FlyWeight < 0.1)), aes(x = FlyWeight, y = WingWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm")


ggplot(tach_master_impute, aes(x = FlyWeight, y = as.numeric(LegsWeight)/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm")


#Comparing body parts
ggplot(tach_master_impute, aes(x = ThoraxWeight/FlyWeight, y = AbWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm")






